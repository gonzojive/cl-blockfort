(in-package :blockfort)

(defparameter *store* nil)
;;;;
;;;; INTERFACE
;;;;

;;;; Opening and closing the store
(defgeneric store-open (store &key if-exists &allow-other-keys)
  (:documentation "Opens the store after it has initially been created"))

(defgeneric store-close (store)
  (:documentation "Closes the store and all the log and data files associated with it."))

;;;; Abstract transactions on the store
(defgeneric store-begin-transaction (store transaction)
  (:documentation "Starts a transaction on the given store.  Returns a transaction."))

(defgeneric store-commit-transaction (store transaction)
  (:documentation "Commits the provided transaction"))

(defgeneric store-abort-transaction (store transaction)
  (:documentation "Undoes the actions of the transaction"))

;;;; Locks
(defgeneric store-acquire-locks-for-action (store operation &key &allow-other-keys)
  (:documentation "Acquires a set of locks on an element in the given
data store, configurable by keyword options. "))

(defgeneric store-release-locks (store locks &key &allow-other-keys)
  (:documentation "Releases the locks acquired early by the store."))

;;;; I/O on the elements in a data store
(defgeneric store-perform-action (store transaction operation &key &allow-other-keys))

(defgeneric store-perform-node-operation (store node operation &key &allow-other-keys)
  (:documentation "Performs some sort of operation on the given node
  in the data store. An example of an operation might be a READ or
  WRITE of a particular database element.  "))

(defgeneric store-read-sequence (store sequence &key store-start start end transaction &allow-other-keys)
  (:documentation "Destructively modifies SEQUENCE by replacing the
elements of sequence bounded by START and END with elements read from
the store starting from STORE-START."))

(defgeneric store-write-sequence (store sequence &key store-start start end transaction &allow-other-keys)
  (:documentation "Writes the elements of SEQUENCE bounded by START
and END into the persistent store STORE starting STORE-START bytes
into the store.  Before a sequence may be written the size of the heap
must be large enough to account for all the new bytes."))

(defgeneric store-node-size (store node)
  (:documentation "Returns the number of elements (octets) in a store."))

(defgeneric store-node-expand (store octet-count)
  (:documentation "Expands the store by OCTET-COUNT number of octets."))

;;;; Implementation

(defclass abstract-store () ())

(defclass store (abstract-store)
  ((store-env-directory
    :initform nil :initarg :store-environment :type (or pathname string)
    :reader store-environment
    :documentation "The directory where the persistent store environment is stored locally.")
   (data-file
    :initform nil :initarg :data-file
    :reader store-data-file
    :documentation "The file that stores all the data")
   (metadata-path
    :initform nil :initarg :metadata-path
    :reader store-metadata-path
    :documentation "The file that stores information about the data
store, like counter numbers and the id the local node.")
   (transaction-log
    :initform nil :initarg :transaction-log
    :reader store-transaction-log :reader store-log
    :documentation "The transaction log for rolling back and
    recovering the database."))
  (:documentation "An ACID-compliant, concurrently accessible store
  that will persist across sessions."))

(defmethod print-object ((store store) stream)
   (print-unreadable-object (store stream :type t :identity t)
     (prin1 (store-environment store)  stream)))

;;;; Interface function
(defun close-store (store)
  (store-close store))

;;;; Interface function
(defun open-store (store-spec &rest store-open-args)
  (destructuring-bind (store-class-designator environment-dir &rest store-initargs)
      store-spec
    (let* ((store (apply #'make-instance
			 store-class-designator
			 :store-environment environment-dir
			 store-initargs)))
      (apply 'store-open store store-open-args)
      (setf *store* store))))

(defmethod store-open ((store store) &key if-exists &allow-other-keys)
  (let ((file-if-exists (case if-exists
			  (:open :overwrite)
			  (:error :error)
			  (:supersede :supersede))))
    (log-open (store-log store) :if-exists if-exists)
    
    nil))


;;; macros

(defmacro with-transaction ((transaction-var store)
			    &body body)
  `(let ((,transaction-var (store-begin-transaction ,store)))
     (multiple-value-prog1 (progn ,@body)
       (store-commit-transaction ,transaction-var))))

(defmacro with-store-data-stream ((stream-var store) &body body)
  "Gets us access to the data stream for the store, performing any necessary locking."
  `(let ((,stream-var (binary-file-stream (store-data-file ,store))))
     ,@body))


;;; implementation
(defmethod initialize-instance :after ((store store) &rest args)
  (declare (ignore args))

  (setf (slot-value store 'metadata-path)
	(merge-pathnames (store-environment store) "db-metadata"))

  (setf (slot-value store 'data-file)
	(make-instance 'binary-file
		       :path (merge-pathnames (store-environment store) "db-data")))

  (setf (slot-value store 'transaction-log)
	(make-instance 'transaction-log
		       :path (merge-pathnames (store-environment store) "db-log")
		       :db store)))

(defmethod store-close ((store persistent-store))
  (log-close (store-transaction-log store))
  (close-file (store-data-file store)))


(defmethod store-read-sequence ((store persistent-store) sequence &key (store-start 0) (start 0) end transaction)
  "Currently ignores transaction argument."
  (declare (ignore transaction))
  (with-store-data-stream (stream store)
    (file-position stream store-start)
    (read-sequence sequence stream :start start :end end)))

(defun store-read-n-byte-sequence (store n &key store-start)
  "Reads N bytes from STORE starting at STORE-START (or 0 if store start is not defined.  Returns
the sequence.  Ensures that exactly N bytes are read."
  (let* ((seq (make-array n :element-type '(unsigned-byte 8)))
	 (num-bytes-read (store-read-sequence store seq :store-start store-start)))
    (when (not (= n num-bytes-read))
      (signal (make-condition 'read-sequence-error :store store)))
    seq))

(defmethod store-write-sequence ((store persistent-store) sequence &key store-start (start 0) end transaction)
  "Writes a byte sequence to the store starting at offset STORE-START"
  (declare (optimize debug))
  (when (null store-start) (setf store-start 0))
  (with-store-data-stream (stream store)
    (file-position stream store-start)
;    (format t "Logging modification...~%")
    (let* ((db-element
	    (cons (or store-start 0)
		  (if end (- end start) (length sequence))))
	   (old-value
	    (let ((seq (make-array (if end (- end start) (length sequence))
				   :initial-element 0 :element-type '(unsigned-byte 8))))
	      (read-sequence seq stream)
	      seq))
	   (new-value
	    (if (or end start) (subseq sequence start end) sequence)))
      (log-modification (store-transaction-log store) transaction db-element old-value new-value))
;    (format t "Flushing...~%")
    (log-flush (store-transaction-log store))
    (file-position stream store-start)
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
    (write-sequence sequence stream :start (or start 0) :end end)
    (finish-output stream)))

(defmethod db-redo-modification ((store persistent-store) transaction-log modification-log-entry)
  (let ((new-value (log-entry-new-value modification-log-entry))
	(db-element (log-entry-database-element modification-log-entry)))
    (with-store-data-stream (stream store)
      (file-position stream (car db-element))
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
      (write-sequence new-value stream)
      (finish-output stream))))

(defmethod db-undo-modification ((store persistent-store) transaction-log modification-log-entry)
  (let ((old-value (log-entry-old-value modification-log-entry))
	(db-element (log-entry-database-element modification-log-entry)))
    (with-store-data-stream (stream store)
      (file-position stream (car db-element))
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
      (write-sequence old-value stream)
      (finish-output stream))))

;(defgeneric db-undo-modification (database transaction-log modfication-log-entry)
;  (:documentation "Undoes the modification to the database."))

(defmethod db-element-as-octets ((store persistent-store) store-range)
  (let ((array (make-array 8 :element-type '(unsigned-byte 8))))
    (integer-into-octet-vector array 4 (car store-range))
    (integer-into-octet-vector array 4 (cdr store-range) :start 4)
    array))

(defmethod db-element-from-octets ((store persistent-store) octets)
  (cons (octet-vector-to-integer (subseq octets 0 4))
	(octet-vector-to-integer (subseq octets 4 8))))

(defmethod db-value-as-octets  ((store persistent-store) value)
  value)

(defparameter *transaction-counter-hack* 1)

(defmethod store-begin-transaction ((store persistent-store))
  (let ((txn-id (incf *transaction-counter-hack*)))
    (log-begin-transaction (store-log store) txn-id)
    txn-id))

(defmethod store-commit-transaction ((store persistent-store) transaction)
  (log-commit-transaction (store-log store) transaction)
  (log-flush (store-log store)))

(defmethod store-size ((store persistent-store))
  (with-store-data-stream (stream store)
    (file-length stream)))

(defmethod store-expand ((store persistent-store) octet-count)
  ;; TODO this should most certainly occur with a transaction
  (with-store-data-stream (stream store)
    (file-position stream (file-length stream))
    (loop :for i :from 0 :upto (- octet-count 1)
       :do  (write-byte 0 stream))))
    
    

