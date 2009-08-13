(in-package :blockfort)

(defparameter *heap* nil)

;;; interface
(defgeneric heap-close (heap)
  (:documentation "Closes the heap and all the log and data files associated with it."))
  
(defgeneric heap-read-sequence (heap sequence &key heap-start start end transaction)
  (:documentation "Destructively modifies SEQUENCE by replacing the elements of sequence bounded by START and END
with elements read from the heap starting from HEAP-START."))

(defgeneric heap-write-sequence (heap sequence &key heap-start start end transaction)
  (:documentation "Writes the elements of SEQUENCE bounded by START and END into the persistent heap HEAP
starting HEAP-START bytes into the heap."))

(defgeneric heap-begin-transaction (heap)
  (:documentation "Starts a transaction on the given heap.  Returns a transaction."))

(defgeneric heap-commit-transaction (heap transaction)
  (:documentation "Commits the provided transaction"))

(defgeneric heap-rollback-transaction (heap transaction)
  (:documentation "Undoes the actions of the transaction"))

(defgeneric heap-size (heap)
  (:documentation "Returns the number of elements (octets) in a heap."))

(defgeneric heap-expand (heap octet-count)
  (:documentation "Expands the heap by OCTET-COUNT number of octets."))

(defclass persistent-heap ()
  ((heap-env-directory
    :initform nil :initarg :heap-environment :type (or pathname string)
    :reader heap-environment
    :documentation "The directory where the persistent heap environment is stored locally.")
   (data-file
    :initform nil :initarg :data-file
    :reader heap-data-file
    :documentation "The file that stores all the data")
   (transaction-log
    :initform nil :initarg :transaction-log
    :reader heap-transaction-log :reader heap-log
    :documentation "The transaction log for rolling back and recovering the database."))
  (:documentation "An ACID-compliant, concurrently accessible heap that will persist across sessions."))

(defmethod print-object ((heap persistent-heap) stream)
   (print-unreadable-object (heap stream :type t :identity t)
     (prin1 (heap-environment heap)  stream)))

(defun open-heap (environment-dir)
  (setf *heap*
    (make-instance 'persistent-heap :heap-environment environment-dir)))

(defun close-heap (heap)
  (heap-close heap))



;;; macros

(defmacro with-transaction ((transaction-var heap) &body body)
  (let ((transaction-var transaction-var) ;(gensym "TRANSACTION"))
	(heap-var (gensym "HEAP")))
    `(let* ((,heap-var ,heap)
	    (,transaction-var (begin-transaction ,heap-var)))
       (unwind-protect (progn ,@body)
	 (commit-transaction ,transaction-var)))))

(defmacro with-heap-data-stream ((stream-var heap) &body body)
  "Gets us access to the data stream for the heap, performing any necessary locking."
  `(let ((,stream-var (binary-file-stream (heap-data-file ,heap))))
     ,@body))

;;; implementation
(defmethod initialize-instance :after ((heap persistent-heap) &rest args)
  (declare (ignore args))

  (setf (slot-value heap 'data-file)
	(make-instance 'binary-file
		       :path (merge-pathnames (heap-environment heap) "db-data")
		       :if-exists :overwrite))

  (setf (slot-value heap 'transaction-log)
	(make-instance 'transaction-log
		       :path (merge-pathnames (heap-environment heap) "db-log")
		       :db heap)))

(defmethod heap-close ((heap persistent-heap))
  (log-close (heap-transaction-log heap))
  (close-file (heap-data-file heap)))


(defmethod heap-read-sequence ((heap persistent-heap) sequence &key (heap-start 0) (start 0) end transaction)
  "Currently ignores transaction argument."
  (declare (ignore transaction))
  (with-heap-data-stream (stream heap)
    (file-position stream heap-start)
    (read-sequence sequence stream :start start :end end)))

(defmethod heap-write-sequence ((heap persistent-heap) sequence &key heap-start (start 0) end transaction)
  "Writes a byte sequence to the heap starting at offset HEAP-START"
  (declare (optimize debug))
  (when (null heap-start) (setf heap-start 0))
  (with-heap-data-stream (stream heap)
    (file-position stream heap-start)
;    (format t "Logging modification...~%")
    (let* ((db-element
	    (cons (or heap-start 0)
		  (if end (- end start) (length sequence))))
	   (old-value
	    (let ((seq (make-array (if end (- end start) (length sequence))
				   :initial-element 0 :element-type '(unsigned-byte 8))))
	      (read-sequence seq stream)
	      seq))
	   (new-value
	    (if (or end start) (subseq sequence start end) sequence)))
      (log-modification (heap-transaction-log heap) transaction db-element old-value new-value))
;    (format t "Flushing...~%")
    (log-flush (heap-transaction-log heap))
    (file-position stream heap-start)
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
    (write-sequence sequence stream :start (or start 0) :end end)
    (finish-output stream)))

(defmethod db-redo-modification ((heap persistent-heap) transaction-log modification-log-entry)
  (let ((new-value (log-entry-new-value modification-log-entry))
	(db-element (log-entry-database-element modification-log-entry)))
    (with-heap-data-stream (stream heap)
      (file-position stream (car db-element))
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
      (write-sequence new-value stream)
      (finish-output stream))))

(defmethod db-undo-modification ((heap persistent-heap) transaction-log modification-log-entry)
  (let ((old-value (log-entry-old-value modification-log-entry))
	(db-element (log-entry-database-element modification-log-entry)))
    (with-heap-data-stream (stream heap)
      (file-position stream (car db-element))
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
      (write-sequence old-value stream)
      (finish-output stream))))

;(defgeneric db-undo-modification (database transaction-log modfication-log-entry)
;  (:documentation "Undoes the modification to the database."))

(defmethod db-element-as-octets ((heap persistent-heap) heap-range)
  (concatenate 'vector
	       (integer-to-octet-vector 4 (car heap-range))
	       (integer-to-octet-vector 4 (cdr heap-range))))

(defmethod db-element-from-octets ((heap persistent-heap) octets)
  (cons (octet-vector-to-integer (subseq octets 0 4))
	(octet-vector-to-integer (subseq octets 4 8))))

(defmethod db-value-as-octets  ((heap persistent-heap) value)
  value)

(defparameter *transaction-counter-hack* 1)

(defmethod heap-begin-transaction ((heap persistent-heap))
  (let ((txn-id (incf *transaction-counter-hack*)))
    (log-begin-transaction (heap-log heap) txn-id)
    txn-id))

(defmethod heap-commit-transaction ((heap persistent-heap) transaction)
  (log-commit-transaction (heap-log heap) transaction)
  (log-flush (heap-log heap)))

(defmethod heap-size ((heap persistent-heap))
  (with-heap-data-stream (stream heap)
    (file-length stream)))

(defmethod heap-expand ((heap persistent-heap) octet-count)
  ;; TODO this should most certainly occur with a transaction
  (with-heap-data-stream (stream heap)
    (file-position stream (file-length stream))
    (loop :for i :from 0 :upto (- octet-count 1)
       :do  (write-byte 0 stream))))
    
    

