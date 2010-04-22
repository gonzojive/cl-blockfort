(cl:defpackage :cl-transaction-log
    (:nicknames :transaction-log :txn-log)
  (:use :cl :alexandria :anaphora)
  (:export #:transaction-log
           #:undo/redo-log
           #:log-open
           #:log-close
           #:log-begin-transaction
           #:log-commit-transaction
))

(in-package :cl-transaction-log)

;;;;; db-level api to undo-redo logging
(defparameter *todo-output* *standard-output*)

(defun todo (format-control &rest format-args)
  (apply 'format *todo-output* (format nil "TODO: ~A" format-control) format-args))

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
    :documentation "Entries that have not yet been flushed.")
   (append-lock
    :initarg :append-lock :initform (bordeaux-threads:make-lock)
    :accessor log-append-lock
    :documentation "Must be held to write to the log.")
   (push-unflushed-log-entry-lock
    :initform (bordeaux-threads:make-lock)
    :accessor unflushed-log-entry-lock
    :documentation "Must be held to push a log entry onto the queue of unflushed entries")))

(defclass undo/redo-log (transaction-log)
  ()
  (:documentation "An undo-redo log for the transactional aspect of the persistent heap."))

;;; generic implemented by the log manager
;; FLUSH (v.) means that pending log entries are written to disk
;; LOG (v.) means that log entries are added to the pending log entries to write to disk.
(defgeneric log-open (transaction-log &key if-exists &allow-other-keys)
  (:documentation "Opens the log file so that operations can be performed on it."))

(defgeneric log-close (transaction-log)
  (:documentation "Close all the associated log files."))

(defgeneric log-begin-transaction (transaction-log transaction)
  (:documentation "Logs, but does not flush, the beginning of a transaction."))

(defgeneric log-commit-transaction (transaction-log transaction)
  (:documentation "Logs, but does not flush, the commit of a transaction."))

(defgeneric log-flush (transaction-log)
  (:documentation "Synchronous flush.  Guaranteed to flush the log to disk."))

(defgeneric log-modification (transaction-log transaction database-element old-value new-value)
  (:documentation "Logs a modification to the database element."))

(defgeneric log-recover (transaction-log database)
  (:documentation "Recovers the log to the last consistent state.  That is, it undoes all uncommited transactions
and redoes all committed transactions."))

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
requirement of this object is that it supply a unique transaction that is not shared by any
concurrently executing transaction."))

(defclass single-transaction-log-entry (log-entry)
  ((transaction
    :initarg :transaction :type logged-transaction :accessor log-entry-transaction
    :documentation "The LOGGED-TRANSACTION that corresponds to this log entry."))
  (:documentation "A log entry related to a single transaction"))

(defgeneric log-entry-transaction-id (log-entry)
  (:documentation "Returns the log entry transaction ID associated with the log entry."))

(defclass modification-log-entry (single-transaction-log-entry)
  ())

(defclass undo/redo-modification-log-entry (modification-log-entry)
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
it is created and flushed before any write entry or commit entry is flushed."))

(defclass commit-transaction-log-entry (single-transaction-log-entry)
  ()
  (:documentation "Corresponds to the beginning of a transaction.  The only restriction is that
it is flushed as soon as it appears in the log."))

(defclass abort-transaction-log-entry (single-transaction-log-entry)
  ()
  (:documentation "Corresponds to the abortion of a transaction.  When this has been
written to a log, it means that the modifications in the corresponding transaction have
been undone."))


(defclass distributed-log-entry (log-entry)
  ()
  (:documentation "All log entries that specifically relate to distributed transactions
are subclasses of this class."))

(defclass coordinator-log-entry (distributed-log-entry single-transaction-log-entry)
  ()
  (:documentation "A log entry only placed at the site of the coordinator."))

(defclass prepare-log-entry (coordinator-log-entry)
  ()
  (:documentation "The coordinator of a distributed, two-phase commit writes
a PREPARE entry to the log."))

(defclass ready-log-entry (distributed-log-entry single-transaction-log-entry)
  ()
  (:documentation "The coordinator of a distributed, two-phase commit writes
a READY entry to the log after Phase I of the two-phase commit."))

(defclass do-not-commit-log-entry (distributed-log-entry single-transaction-log-entry)
  ()
  (:documentation "The coordinator of a distributed, two-phase commit writes
a DON'T COMIT entry to the log after Phase I of the two-phase commit if it decides
to abort the transaction."))

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

;; logging details
(defconstant +field-size-octet-count+ 8
  "Number of bytes to use to store the size of each 'field' in a log entry.")

(defconstant +checksum-octet-count+ 4
  "Number of bytes to use to store the checksum of the data for a particular field.")

(defun write-binary-field-to-log-stream (vector log-stream)
  "Writes a vector of octets to the stream LOG-STREAM.  Formatted with a leading binary
byte count followed by the byte array."
  (declare (optimize debug))
  (let* ((length (length vector))
	 (length-octets (integer-to-octet-vector +field-size-octet-count+ length))
	 (crc (crc32 vector))
	 (crc-octets (integer-to-octet-vector +checksum-octet-count+ crc)))
    ;(format t "Writing field ~A ~A ~A~%" length vector crc-octets)
    (write-sequence length-octets log-stream)
    (write-sequence crc-octets log-stream)
    (write-sequence vector log-stream)
    (+ +field-size-octet-count+ +checksum-octet-count+ length)))

(defun read-binary-field-from-log-stream (log-stream)
  "Reads a vector of octets from the stream LOG-STREAM.  Reads however many bytes 
are encoded in the field."
  (flet ((read-vector (byte-count)
	   (let* ((seq (make-array byte-count :element-type '(unsigned-byte 8)))
		  (bytes-read (read-sequence seq log-stream)))
	     (when (not (= byte-count bytes-read))
	       (error "Expected to read ~A bytes but read ~A" byte-count bytes-read))
	     (values seq bytes-read))))
    (let* ((length (octet-vector-to-integer (read-vector +field-size-octet-count+)))
	   (crc-octets (read-vector +checksum-octet-count+))
	   (data (read-vector length))
	   (crc (octet-vector-to-integer crc-octets))
	   (data-crc (crc32 data)))
      ;(format t "Read field ~A ~A ~A~%" length data crc-octets)
      (if (eql crc data-crc)
	  (values
	    data
	    (+ +field-size-octet-count+ +checksum-octet-count+ length))
	  (error "Checksum failed on field.  Expected ~A but got ~A" crc data-crc)))))

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
    (3 . undo/redo-modification-log-entry)
    (4 . abort-transaction-log-entry)
    ;; distributed transactions
    (5 . prepare-log-entry)
    (6 . ready-log-entry)
    (7 . do-not-commit-log-entry)
    ;; checkpointing
    (8 . begin-checkpoint-log-entry)
    (9 . end-checkpoint-log-entry)
    ))

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
  "Writes the ID of the transaction to the stream for any single-transaction-log-entry."
  (write-integer-field-to-log-stream
   (transaction-id (log-entry-transaction log-entry))
   +transaction-id-octet-count+
   binary-stream))

(defmethod write-log-entry :after ((log-entry modification-log-entry) binary-stream)
  "A modification writes (1) the database element, (2) the old value, and (3) the new value
to the stream."
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


(defmethod read-log-entry :after ((log-entry undo/redo-modification-log-entry) binary-stream)
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

(defun queue-up-log-entry (log entry)
  (bordeaux-threads:with-lock-held ((unflushed-log-entry-lock log))
    (push entry (unflushed-log-entries log))))

(defmethod log-begin-transaction ((log transaction-log) (txn logged-transaction))
  (queue-up-log-entry log
		      (make-instance 'begin-transaction-log-entry
				     :transaction txn
				     :transaction-log log)))

(defmethod log-begin-transaction ((log transaction-log) (transaction-id integer))
  (let ((txn  (make-instance 'logged-transaction :transaction-id transaction-id)))
    (log-begin-transaction log txn)))

(defmethod log-commit-transaction ((log transaction-log) (txn logged-transaction))
  (queue-up-log-entry
   log
   (make-instance 'commit-transaction-log-entry :transaction txn :transaction-log log)))
	
(defmethod log-commit-transaction ((log transaction-log) (transaction-id integer))
  (log-commit-transaction log (make-instance 'logged-transaction :transaction-id transaction-id)))
	
(defmethod log-flush ((log transaction-log))
  (bordeaux-threads:with-lock-held ((log-append-lock log))
    (let ((log-stream (binary-file-stream (log-file log)))
	  (unflushed-log-entries (reverse
				  (bordeaux-threads:with-lock-held ((unflushed-log-entry-lock log))
				    (unflushed-log-entries log)))))
      ;; We need to ensure the log entries are actually flushed before
      ;; we clear out the queue.  There might be an error that prevents
      ;; a particular log entry from being written to disk.  Imagine
      ;; the race condition:
      ;; 1. Thread A queues up a bunch of log entries
      ;; 2. Thread B queues up a bunch of log entries and flushes half of them
      ;;    and then we run out of disk space.  We throw an error in thread B
      ;; 3.  Thread A calls log-flush and does not err.  Eek!
      ;;
      ;; To avoid this we have a flag for each entry that indicates whether it has been
      ;; flushed yet, and we only set that after calling FINISH-OUTPUT successfully
      ;; after writing the entry to the log.
      ;; move to end of file so we can append
      (file-position log-stream  (file-length log-stream))

      ;; flush all the log entries
      (let ((written-but-not-flushed nil))
	(flet ((write-not-flushed ()
		 (finish-output (binary-file-stream (log-file log)))
		 (dolist (log-entry written-but-not-flushed)
		   (setf (log-entry-flushed? log-entry) t))))
	       ;; loop through the log entries and write but do not flush them to disk
	  (unwind-protect
	       (dolist (log-entry unflushed-log-entries)
		 (when (not (log-entry-flushed? log-entry))
		   (write-log-entry log-entry log-stream)
		   (push log-entry written-but-not-flushed)))
	    (write-not-flushed)
	    (bordeaux-threads:with-lock-held ((unflushed-log-entry-lock log))
	      (removef (unflushed-log-entries log) t :key #'log-entry-flushed?))))))))


(defmethod log-modification ((log undo/redo-log) (transaction-id integer) db-element old-value new-value)
  (queue-up-log-entry
   log
   (make-instance 'undo/redo-modification-log-entry
		  :transaction (make-instance 'logged-transaction :transaction-id transaction-id)
		  :transaction-log log
		  :database-element db-element
		  :old-value old-value
		  :new-value new-value)))

(defclass transaction-recovery-info ()
  ((transaction-id
    :initform nil :initarg :transaction-id
    :accessor transaction-id)
   (modifications
    :initform nil :initarg :modifications
    :accessor modifications
    :documentation "Modifications, earliest last.")
   (ready
    :initform nil :initarg :ready
    :accessor ready
    :documentation "Was this TXN put into a ready state?")
   (committed
    :initform nil :initarg :committed
    :accessor complete
    :documentation "Was this TXN committed?")))

(defun log-maybe-redo-transactions (log database txns-earliest-first)
  "Maybe redoes the earliest committed transaction recursively,
returning the modified list of transactions that remain in the
earliest-first list, order preserved."
  (let ((popped? (when-let (earliest (first txns-earliest-first))
		   (when (complete earliest)
		     (dolist (modification-entry (reverse (modifications earliest)))
		       (db-redo-modification database log modification-entry))
		     t))))
      (if (not popped?)
	  txns-earliest-first
	  (log-maybe-redo-transactions log database (rest txns-earliest-first)))))

;; A note on recovery form page 905 of Database Systems: The Complete Book:
;; The undo/redo recover policy is:
;; 1. Redo all the committed transactions in the order earliest-first, and
;; 2. undo all the incomplete transactions in the order latest-first
(defmethod log-recover ((log undo/redo-log) database)
  ;; loop through each log record.  When a 'begin' log entry is found,
  ;; push a transaction onto the stack.  coupled with that entry, push
  ;; modification entries preserving their order when a commit is
  ;; found, write redo all the modifications oldest to newest when the
  ;; end of the log is found, look at the transaction stack and undo
  ;; all the modifications newest to oldest, unless a READY entry 
  (let ((transactions nil) ; not yet redone/undone transaction infos, earlist-starting LAST
	(stream (binary-file-stream (log-file log))))
    (flet ((recovery-info (entry)
	     (find (log-entry-transaction entry) transactions)))
    ;; seek to the beginning of the file
      (file-position stream 0)
      (loop :for entry = (log-read-log-entry log stream)
	    :when (null entry)
	    :return nil
	    :do
	    (typecase entry
	      (begin-transaction-log-entry
		 (push (make-instance 'transaction-recovery-info
				      :transaction-id (transaction-id (log-entry-transaction entry)))
		       transactions))
					;(format t "Begin TXN ~A. new stack ~A~%"
					;(transaction-id (log-entry-transaction entry)) transaction-stack))
	      (commit-transaction-log-entry
		 (setf (complete  (recovery-info entry)) t)
		 (setf transactions
		       (nreverse (log-maybe-redo-transactions log database (nreverse transactions)))))
					;(format t "Committed TXN ~A. New stack ~A~%"
					;(transaction-id (log-entry-transaction entry)) transaction-stack)))))
	      
	      (modification-log-entry
		 ;; modification stack is earliest LAST
		 (push entry (modifications (recovery-info entry))))

	      (abort-transaction-log-entry
		 ;; aborted transactions act as if they never happend
		 ;; (we have already corrected for them)
		 (removef transactions (recovery-info entry)))
	 
	      (ready-log-entry
		 ;; set the ready flag for processing during
		 ;; incomplete-txn processing phase
		 (setf (ready (recovery-info entry)) t))

	      (prepare-log-entry
		 ;; ignored.  This is redundant with begin-transaction-log-entry.
		 )))
      (let ((undone-count 0))
	(assert (every (complement #'complete) transactions))
	(dolist (uncommitted-transaction transactions)
	  (incf undone-count)
	  ;; undo modifications, latest first
	  (dolist (modification-entry (modifications uncommitted-transaction))
	    (db-undo-modification database log modification-entry))
	
	  (bordeaux-threads:with-lock-held ((log-append-lock log))
	    (file-position stream (file-length stream))
	    (write-log-entry (make-instance 'abort-transaction-log-entry
					    :transaction (make-instance 'logged-transaction
									:transaction-id (car uncommitted-transaction))
					    :transaction-log log)
			     stream))))
      database)))


;;;; Log initialization, opening, and closing
(defmethod initialize-instance :after ((log transaction-log) &key path &allow-other-keys) 
  (setf (slot-value log 'log-file) (make-instance 'binary-file :path path)))

(defmethod log-open ((log transaction-log) &key if-exists if-does-not-exist &allow-other-keys)
  (todo "Check to make sure that the log is not corrupted when first opening.")
  (with-open-file (s (binary-file-path (log-file log))
		     :direction :io
		     :if-exists if-exists
		     :if-does-not-exist if-does-not-exist))
  log)

(defmethod log-close ((log transaction-log))
  (todo "Close the log file"))