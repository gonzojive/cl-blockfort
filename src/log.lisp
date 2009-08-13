(in-package :blockfort)

;;;;; db-level api to undo-redo logging

(defclass transaction-log ()
  ((log-file
    :initarg :file
    :reader log-file
    :documentation "The binary file where the log is stored.")
   (db
    :initarg :db
    :accessor log-db
    :documentation "The database this transaction log is logging.  TRANSACTION-LOG
does not rely upon a particular database implementation.  Instead, it relies on several
generic functions that take the database as an argument that a database must implement.
TRANSACTION-LOG is thus block-store agnostic.")
   (unflushed-log-entries
    :initform nil
    :accessor unflushed-log-entries
    :documentation "Entries that have not yet been flushed."))
   
  (:documentation "An undo-redo log for the transactional aspect of the persistent heap."))

(defmethod initialize-instance :after ((log transaction-log) &key path)
  (setf (slot-value log 'log-file) 
	(make-instance 'binary-file :path path :if-exists :append)))

;;; generic implemented by the log manager
;; FLUSH (v.) means that pending log entries are written to disk
;; LOG (v.) means that log entries are added to the pending log entries to write to disk.

(defgeneric log-begin-transaction (transaction-log transaction)
  (:documentation "Logs, but does not flush, the beginning of a transaction."))

(defgeneric log-commit-transaction (transaction-log transaction)
  (:documentation "Logs, but does not flush, the commit of a transaction."))

(defgeneric log-flush (transaction-log)
  (:documentation "Guaranteed to flush the log to disk."))

(defgeneric log-modification (transaction-log transaction database-element old-value new-value)
  (:documentation "Logs a modification to the database element."))

(defgeneric log-recover (transaction-log database)
  (:documentation "Recovers the log to the last consistent state.  That is, it undoes all uncommited transaction
and redoes any committed transactions."))

(defgeneric log-close (transaction-log)
  (:documentation "Close all the associated log files."))

;; generics implemented by the db, not the log manager
(defgeneric db-redo-modification (database transaction-log modfication-log-entry)
  (:documentation "Redoes the modification to the database."))

(defgeneric db-undo-modification (database transaction-log modfication-log-entry)
  (:documentation "Undoes the modification to the database."))

(defgeneric db-element-as-octets (database database-element)
  (:documentation "Returns the database element of the given database as a vector of bytes."))

(defgeneric db-element-from-octets (database octets)
  (:documentation "Returns the database element of the given database from a vector of bytes."))

(defgeneric db-value-as-octets (database value)
  (:documentation "This is called by the log manager when a MODIFICATION-LOG-ENTRY must be written
to disk.  It is called with either the LOG-ENTRY-OLD-VALUE or LOG-ENTRY-NEW-VALUE of a 
MODIFICATION-LOG-ENTRY."))

;;;; Log entries

(defclass log-entry ()
  ((flushed?
    :initarg :flushed? :accessor log-entry-flushed? :initform nil
    :documentation "T if this entry has been written to disk.  NIL otherwise.")
   (transaction-log
    :initarg :transaction-log :accessor log-entry-transaction-log
    :documentation "The transaction log associated with this log entry."))
  (:documentation "A log entry in a transaction log."))

(defclass logged-transaction ()
  ((transaction-id
    :initarg :transaction-id :initform nil :accessor transaction-id
    :documentation "The ID of the transaction.  This is written to the log."))

  (:documentation "A LOGGED-TRANSACTION can be subclassed by a user or used as is.  The only
requirement of this object is that is supply a unique transaction that is not shared by any
concurrently executing transaction."))

(defclass single-transaction-log-entry (log-entry)
  ((transaction
    :initarg :transaction :type logged-transaction :accessor log-entry-transaction
    :documentation "The LOGGED-TRANSACTION that corresponds to this log entry."))
  (:documentation "A log entry related to a single transaction"))
  
(defclass modification-log-entry (single-transaction-log-entry)
  ((database-element
    :initarg :database-element :accessor log-entry-database-element
    :documentation "This log entry documents the writing of certain data to this database element.
This slot is purposefully abstract in order for this logging library to be general.")
   (old-value
    :initarg :old-value :accessor log-entry-old-value
    :documentation "The data overwritten, if any.")
   (new-value
    :initarg :new-value :accessor log-entry-new-value
    :documentation "The data to be written or already written."))

  (:documentation "A log entry corresponding to writing a piece of data to a database.  This
log entry must be created and flushed before that data is actually inserted into the database."))

(defclass begin-transaction-log-entry (single-transaction-log-entry)
  ()
  (:documentation "Corresponds to the beginning of a transaction.  The only restriction is that
it created and flushed before any write entry or commit entry is flushed."))

(defclass commit-transaction-log-entry (single-transaction-log-entry)
  ()
  (:documentation "Corresponds to the beginning of a transaction.  The only restriction is that
it created and flushed before any write entry or commit entry is flushed."))

(defclass abort-transaction-log-entry (single-transaction-log-entry)
  ()
  (:documentation "Corresponds to the abortion of a transaction.  When this has been
written to a log, it means that the modifications in the corresponding transaction have
been undone."))

(defclass begin-checkpoint-log-entry (log-entry)
  ((executing-transaction
    :initarg :transactions :accessor checkpoint-transactions
    :documentation "The LOGGED-TRANSACTION that correspond to this log entry."))

  (:documentation "Corresponds to the beginning of a checkpoint while the given transactions
are running."))

(defclass end-checkpoint-log-entry (log-entry)
  ((executing-transaction
    :initarg :transactions :accessor checkpoint-transactions
    :documentation "The LOGGED-TRANSACTIONS that correspond to this log entry."))

  (:documentation "Corresponds to the end of a checkpoint.."))

;; log entry generics
(defgeneric write-log-entry (log-entry stream)
  (:documentation "Writes the given log entry to the stream STREAM."))


(defun integer-to-octet-vector (octet-count integer)
  "Given an integer and the number of bytes of desired output, returns a vector of lenghth
BYTE-COUNT with a Little Endian integer in it."
  (let ((array (make-array octet-count :element-type '(unsigned-byte 8))))
    (dotimes (i octet-count)
      (setf (elt array i)
	    (ldb (byte 8 (* 8 i)) integer)))
    array))

(defun octet-vector-to-integer (vector &optional signed?)
  "Given a byte vector in Little Endian form, returns a signed integer."
  (let ((unsigned-value 0))
    (dotimes (i (length vector))
      (incf unsigned-value (ash (elt vector i) (* 8 i))))
    (if (and signed?
	     (>= unsigned-value (ash 1 (1- (* 8 (length vector))))))
	(- unsigned-value (ash 1 (* 8 (length vector))))
	unsigned-value)))

;; logging details
(defconstant +field-size-octet-count+ 8
  "Number of bytes to use to store the size of each 'field' in a log entry.")

(defun write-binary-field-to-log-stream (vector log-stream)
  "Writes a vector of octets to the stream LOG-STREAM.  Formatted with a leading binary
byte count followed by the byte array."
  (let ((length (length vector)))
    (write-sequence (integer-to-octet-vector +field-size-octet-count+ length) log-stream)
    (write-sequence vector log-stream)
    (+ +field-size-octet-count+ length)))

(defun read-binary-field-from-log-stream (log-stream)
  "Reads a vector of octets from the stream LOG-STREAM.  Reads however many bytes 
are encoded in the field."
  (flet ((read-vector (byte-count)
	   (let* ((seq (make-array byte-count :element-type '(unsigned-byte 8)))
		  (bytes-read (read-sequence seq log-stream)))
	     (when (not (= byte-count bytes-read))
	       (error "Expected to read ~A bytes but read ~A" byte-count bytes-read))
	     (values seq bytes-read))))
    (let ((length (octet-vector-to-integer (read-vector +field-size-octet-count+))))
      (values
       (read-vector length)
       (+ +field-size-octet-count+ length)))))

(defun write-integer-field-to-log-stream (integer octet-count log-stream)
  "Writes an integer of a certain size from the log stream."
  (write-sequence (integer-to-octet-vector octet-count integer) log-stream))

(defun read-integer-field-from-log-stream (octet-count log-stream)
  "Reads an integer of a certain size from the log stream."
  (flet ((read-vector (byte-count)
	   (let* ((seq (make-array byte-count :element-type '(unsigned-byte 8)))
		  (bytes-read (read-sequence seq log-stream)))
	     (when (not (= byte-count bytes-read))
	       (error "Expected to read ~A bytes but read ~A" byte-count bytes-read))
	     (values seq bytes-read))))
    (octet-vector-to-integer (read-vector octet-count))))

;; so-called symbol integers are used to encode and decode symbols
(defparameter *integer-symbol-alist*
  '((1 . begin-transaction-log-entry)
    (2 . commit-transaction-log-entry)
    (3 . begin-checkpoint-log-entry)
    (4 . end-checkpoint-log-entry)
    (5 . modification-log-entry)
    (6 . abort-transaction-log-entry)))

(defconstant +symbol-integer-octet-count+ 4)
(defconstant +transaction-id-octet-count+ 8)

(defun integer-symbol (integer)
  (cdr (assoc integer *integer-symbol-alist*)))

(defun symbol-integer (symbol)
  (car (rassoc symbol *integer-symbol-alist*)))

(defun object-type-as-symbol-integer (object)
  (symbol-integer (class-name (class-of object))))

(defmethod write-log-entry ((log-entry log-entry) binary-stream)
  log-entry)

(defmethod write-log-entry :after ((log-entry log-entry) binary-stream)
  "The most generic writer simply writes the type of log entry it is. This should be called
before all else."
  (write-integer-field-to-log-stream
   (object-type-as-symbol-integer log-entry)
   +symbol-integer-octet-count+
   binary-stream))



(defmethod write-log-entry :after ((log-entry single-transaction-log-entry) binary-stream)
  (write-integer-field-to-log-stream
   (transaction-id (log-entry-transaction log-entry))
   +transaction-id-octet-count+
   binary-stream))

(defmethod write-log-entry :after ((log-entry modification-log-entry) binary-stream)
  
  (write-binary-field-to-log-stream
   (db-element-as-octets
    (log-db (log-entry-transaction-log log-entry)) 
    (log-entry-database-element log-entry))
   binary-stream)

  (write-binary-field-to-log-stream
   (db-value-as-octets
    (log-db (log-entry-transaction-log log-entry)) 
    (log-entry-old-value log-entry))
   binary-stream)

  (write-binary-field-to-log-stream
   (db-value-as-octets
    (log-db (log-entry-transaction-log log-entry)) 
    (log-entry-new-value log-entry))
   binary-stream))

(defmethod log-read-log-entry ((log transaction-log) binary-stream)
;  (let ((fpos (file-position binary-stream))
;	(flen (file-length binary-stream)))
;    (format t "Log Entry Read FPOS ~A FLEN ~A ~%" fpos flen))
  (if (= (file-position binary-stream) (file-length binary-stream))
      nil
      (let* ((int-field (read-integer-field-from-log-stream +symbol-integer-octet-count+ binary-stream))
	     (log-entry-type (integer-symbol int-field)))
	(when (not log-entry-type)
	  (error "NULL log entry type.  Symbol int ~A" int-field))
	(let ((entry (make-instance log-entry-type :flushed? t :transaction-log log)))
	  (read-log-entry entry binary-stream)
	  entry))))
    
(defmethod read-log-entry ((log-entry log-entry) binary-stream)
  "The most generic reader does nothing since it is done in LOG-READ-LOG-ENTRY."
  nil)

(defmethod read-log-entry :after ((log-entry single-transaction-log-entry) binary-stream)
  "The most generic writer simply writes the type of log entry it is. This should be called
before all else."
;  (format t "   Single Transaction Log Entry Read FPOS ~A FLEN ~A ~%" (file-position binary-stream) (file-length binary-stream))
  (let ((transaction-id (read-integer-field-from-log-stream +transaction-id-octet-count+ binary-stream)))
    (when (not transaction-id)
      (error "NULL transaction ID read from log file.. corruption with log?"))
    (setf (log-entry-transaction log-entry)
	  (make-instance 'logged-transaction :transaction-id transaction-id))
    log-entry))


(defmethod read-log-entry :after ((log-entry modification-log-entry) binary-stream)
;  (format t "   Modification Log Entry Read FPOS ~A FLEN ~A ~%" (file-position binary-stream) (file-length binary-stream))
  (flet ((read-valid-data-field ()
	   (let ((data (read-binary-field-from-log-stream binary-stream)))
	     (assert data)
	     data)))
  (let ((db-element
	 (db-element-from-octets
	  (log-db (log-entry-transaction-log log-entry)) 
	  (read-valid-data-field))))

    (let ((old-value (read-valid-data-field))
	  (new-value (read-valid-data-field)))

      (setf (log-entry-database-element log-entry) db-element
	    (log-entry-old-value log-entry) old-value
	    (log-entry-new-value log-entry) new-value)))))

;; generic implemented by the log manager

;(defgeneric log-find-or-make-transaction (log transaction-id)
;  (:documentation "Given a transaction ID, finds or creates the active transaction with that ID")
;  (:method  ((log transaction-log) (transaction-id integer))
;    (find transaction-id


(defmethod log-begin-transaction ((log transaction-log) (transaction-id integer))
  (push (make-instance 'begin-transaction-log-entry
		       :transaction (make-instance 'logged-transaction :transaction-id transaction-id)
		       :transaction-log log)
	(unflushed-log-entries log)))

(defmethod log-commit-transaction ((log transaction-log) (transaction-id integer))
  (push (make-instance 'commit-transaction-log-entry
		       :transaction (make-instance 'logged-transaction :transaction-id transaction-id)
		       :transaction-log log)
	(unflushed-log-entries log)))

(defmethod log-modification ((log transaction-log) (transaction-id integer) db-element old-value new-value)
  (push (make-instance 'modification-log-entry
		       :transaction (make-instance 'logged-transaction :transaction-id transaction-id)
		       :transaction-log log
		       :database-element db-element
		       :old-value old-value
		       :new-value new-value)
	(unflushed-log-entries log)))

(defmethod log-flush ((log transaction-log))
  (file-position (binary-file-stream (log-file log)) (file-length (binary-file-stream (log-file log))))
  (dolist (log-entry (reverse (unflushed-log-entries log)))
    (when (not (log-entry-flushed? log-entry))
      (write-log-entry log-entry (binary-file-stream (log-file log)))
      (finish-output (binary-file-stream (log-file log)))
      (setf (log-entry-flushed? log-entry) t)))
  (setf (unflushed-log-entries log) nil))


(defmethod log-recover ((log transaction-log) database)
  ;; loop through each log record.  When a 'begin' log entry is found,
  ;; push a transaction onto the stack.  coupled with that entry,
  ;; push modification entries preserving their order
  ;; when a commit is found, write redo all the modifications oldest to newest
  ;; when the end of the log is found, look at the transaction stack
  ;; and undo all the modifications newest to oldest
  (let ((transaction-stack nil) ; alist of transaction id to list of modifications newest to oldest
	(stream (binary-file-stream (log-file log))))
    ;; seek to the beginning of the file
    (file-position stream 0)
    (loop :for entry = (log-read-log-entry log stream)
       :when (null entry)
       :return nil
       :do
       (typecase entry
	 (begin-transaction-log-entry
	  (push (cons (transaction-id (log-entry-transaction entry)) nil) transaction-stack))
;	  (format t "Begin TXN ~A. new stack ~A~%"
;		  (transaction-id (log-entry-transaction entry)) transaction-stack))
	 (modification-log-entry
	  ;; modification stack is newest first
	  (push entry
		(cdr (assoc (transaction-id (log-entry-transaction entry)) transaction-stack))))
	 (abort-transaction-log-entry
	  (setf transaction-stack
		(remove (transaction-id (log-entry-transaction entry)) transaction-stack :key #'car)))
	 (commit-transaction-log-entry
	  (let ((modification-entries
		 (cdr (assoc (transaction-id (log-entry-transaction entry)) transaction-stack))))
	    (dolist (modification-entry (nreverse modification-entries))
	      (db-redo-modification database log modification-entry))
	    (setf transaction-stack
		  (remove (transaction-id (log-entry-transaction entry)) transaction-stack :key #'car))))))
;	    (format t "Committed TXN ~A. New stack ~A~%"
;		  (transaction-id (log-entry-transaction entry)) transaction-stack)))))
    (let ((undone-transactions 0))
      (dolist (uncommitted-transaction transaction-stack)
	(incf undone-transactions)
	(dolist (modification-entry (cdr uncommitted-transaction))
	  (db-undo-modification database log modification-entry))
	(file-position stream (file-length stream))
	(write-log-entry (make-instance 'abort-transaction-log-entry
					:transaction (make-instance 'logged-transaction
								    :transaction-id (car uncommitted-transaction))
					:transaction-log log)
			 stream)))
    database))
    
(defmethod log-close ((log transaction-log))
  (close-file (log-file log)))