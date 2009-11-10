(in-package :blockfort)

(defparameter *store* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Store Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Opening and closing the store
(defgeneric store-open (store &key if-exists if-does-not-exist &allow-other-keys)
  (:documentation "Opens the store after it has initially been
created.  Valid values for IF-EXISTS are :overwrite, :error,
and :supercede.  :supercede erases the old database, while :overwrite
opens it for IO.

Valid values for IF-DOES-NOT-EXIST are :error and :create, defaulting to :create."))

(defgeneric store-close (store)
  (:documentation "Closes the store and all the log and data files associated with it."))

;;;; Abstract transactions on the store
(defgeneric store-begin-transaction (store transaction)
  (:documentation "Starts a transaction on the given store.  Returns a transaction."))

(defgeneric store-commit-transaction (store transaction)
  (:documentation "Commits the provided transaction"))

(defgeneric store-abort-transaction (store transaction)
  (:documentation "Undoes the actions of the transaction"))

;;;; Actions (I/O) on the elements in a data store
(defgeneric store-perform-action (store transaction action &key &allow-other-keys))

;;;; Locks
(defgeneric store-acquire-locks-for-action (store operation &key &allow-other-keys)
  (:documentation "Acquires a set of locks on an element in the given
data store, configurable by keyword options. "))

(defgeneric store-release-locks (store locks &key &allow-other-keys)
  (:documentation "Releases the locks acquired early by the store."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Transactions Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric transaction-id (transaction)
  (:documentation "A globally unique transaction ID number.  This works globally by using
the first 2 bytes to encode the node id and the last 6 bytes for a unique counter (by default)."))

(defclass abstract-transaction () ())
(defclass transaction (logged-transaction abstract-transaction)
  ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Node Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric node-perform-action (node transaction action &key &allow-other-keys)
  (:documentation "Performs some sort of operation on the given node
in the data store. An example of an operation might be a READ or WRITE
of a particular database element."))

(defgeneric node-read-sequence (node sequence &key store-start start end transaction &allow-other-keys)
  (:documentation "Destructively modifies SEQUENCE by replacing the
elements of sequence bounded by START and END with elements read from
the store starting from STORE-START."))

(defgeneric node-close (node &key &allow-other-keys)
  (:documentation "Only relevant to local nodes.  Closes open files."))

(defgeneric node-open (node &key &allow-other-keys)
  (:documentation "Only relevant to local nodes.  Opens relevant files."))

(defgeneric node-write-sequence (node sequence &key store-start start end transaction &allow-other-keys)
  (:documentation "Writes the elements of SEQUENCE bounded by START
and END into the persistent store STORE starting STORE-START bytes
into the store.  Before a sequence may be written the size of the heap
must be large enough to account for all the new bytes."))

(defgeneric node-size (node)
  (:documentation "Returns the number of elements (octets) in a store."))

(defgeneric node-resize (node new-size &key &allow-other-keys)
  (:documentation "Expands or contracts the store by adding/removing
bytes at the end of the address space."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Syntax Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-transaction ((transaction-var store)
			    &body body)
  `(let ((,transaction-var (store-begin-transaction ,store)))
     (multiple-value-prog1 (progn ,@body)
       (store-commit-transaction ,transaction-var))))

(defmacro with-node-data-stream ((stream-var node) &body body)
  "Gets us access to the data stream for the store, performing any necessary locking."
  `(let ((,stream-var (binary-file-stream (node-data-file ,node))))
     ,@body))

(defun close-store (store)
  (store-close store))

(defun open-store (store-spec &rest store-open-args)
  (destructuring-bind (store-class-designator environment-dir &rest store-initargs)
      store-spec
    (let* ((store (apply #'make-instance
			 store-class-designator
			 :store-environment environment-dir
			 store-initargs)))
      (apply 'store-open store store-open-args)
      (setf *store* store))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Store Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass abstract-store () ())

(defclass store (abstract-store)
  ((store-env-directory
    :initform nil :initarg :store-environment :type (or pathname string)
    :reader store-environment
    :documentation "The directory where the persistent store environment is stored locally.")
   (local-node
    :initform nil :initarg :local-node
    :accessor store-local-node
    :documentation "The node that stores data on the local file system.")
   (metadata-path
    :initform nil :initarg :metadata-path
    :reader store-metadata-path
    :documentation "The file that stores information about the data
store, like counter numbers and the id the local node.")
   (transaction-log
    :initform nil :initarg :transaction-log
    :reader store-transaction-log :reader store-log
    :documentation "The transaction log for rolling back and
    recovering the database.")
   (txn-counter
    :initform 0 :initarg :txn-id-counter
    :documentation "Keeps track of the next id to assign to a transaction.
Must hold the trans")
   (txn-start-lock
    :initform (bordeaux-threads:make-lock) :initarg :txn-id-lock
    :documentation "Lock that must be held to start a new transaction.")
   (network
    :initform nil :initarg :store-network
    :accessor store-network
    :documentation "The network is responsible for sending messages
from node to node in the distributed database."))
  (:documentation "An ACID-compliant, concurrently accessible store
  that will persist across sessions."))

(defmethod print-object ((store store) stream)
   (print-unreadable-object (store stream :type t :identity t)
     (prin1 (store-environment store)  stream)))

(defun read-metadata-file (store)
  "Reads the metadata file of the data store, returning an alist of
keys and values."
  (with-open-file (s (store-metadata-path store))
    (read s)))

(defmethod initialize-instance :after ((store store) &rest args)
  (declare (ignore args))

  (with-accessors ((metadata-path store-metadata-path)
		   (local-node store-local-node)
		   (env store-environment)
		   (log store-transaction-log))
      store

    (setf (slot-value store 'metadata-path)
	  (merge-pathnames env "db-metadata"))

    (setf (store-local-node store)
	  (make-instance 'local-node
			 :store store
			 :data-file (make-instance 'binary-file
						   :path (merge-pathnames env "db-data"))))

    (setf (slot-value store 'transaction-log)
	  (make-instance 'transaction-log
			 :path (merge-pathnames env "db-log")
			 :db store))))

(defmethod store-open ((store store) &key (if-exists :overwrite) (if-does-not-exist :create) &allow-other-keys)
  (declare (type (member :overwrite :error :supercede) if-exists)
	   (type (member :error :create) if-does-not-exist))
  (log-open (store-log store) :if-exists if-exists :if-does-not-exist if-does-not-exist)
  (node-open (store-local-node store) :if-exists if-exists :if-does-not-exist if-does-not-exist)
  ;; TODO FIXME ensure that node-id for node is valid and also that
  ;; the transaction counter is set correctly (use the metadata file
  ;; and possibly the server we are connecting to if this is not a new
  ;; database)
  (setf (node-id (store-local-node store)) 0)
  store)

(defmethod store-close ((store store))
  (log-close (store-transaction-log store))
  (node-close (store-local-node store))
  store)

(defparameter *transaction-counter-hack* 1)

(defun globally-unique-id (node locally-unique-id)
  (let ((node-id (node-id node)))
    (logior (ash node-id (* 8 6)) locally-unique-id)))

(defmethod store-begin-transaction ((store store) transaction)
  (bordeaux-threads:with-lock-held ((slot-value store 'txn-start-lock))
      (setf (transaction-id transaction)
	    (globally-unique-id (store-local-node store)
				(incf (slot-value store 'txn-counter))))
      (log-begin-transaction (store-log store) transaction))
  transaction)

(defmethod store-commit-transaction ((store store) transaction)
  (log-commit-transaction (store-log store) transaction)
  (log-flush (store-log store)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Node Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass store-node ()
  ((store
    :initarg :store :initform nil
    :accessor node-store
    :documentation "The store that owns this node.")
   (node-id
    :initarg :node-id :initform nil
    :accessor node-id
    :type (or null (unsigned-byte 16))
    :documentation "Unique identifier for this node.  2-byte integer."))
  (:documentation ""))

(defclass local-node (store-node)
  ((data-file
    :initform nil :initarg :data-file
    :reader node-data-file
    :documentation "The file that stores all the data"))
  (:documentation "A local node has a bunch of data associated with it
that we can write to without necessarily going across the
network (though for replicated cases it may be necessary)."))

(defclass remote-node (store-node)
  ())

(defclass node-action ()
  ((node
    :initarg :node :initform nil
    :accessor action-node))
  (:documentation "An action on a single node."))

(defmethod node-open ((node local-node) &key if-exists if-does-not-exist &allow-other-keys)
  (declare (type (member :supersede :overwrite :error) if-exists)
	   (type (member :create :error) if-exists))
  (let* ((data-path (binary-file-path (node-data-file node))))
    (with-open-file (s data-path
		       :direction :io
		       :if-exists if-exists
		       :if-does-not-exist if-does-not-exist)
      node)))


(defmethod node-close ((node local-node) &key &allow-other-keys)
  ;; TODO close the data stream
  )

#+nil ;; was node-open
(progn
  (let ((existsp (open data-path  :direction :probe)))
    (cond
      ((and existsp (eql :error if-exists))
       (error "Database file already exists"))

      ((or (and existsp       (eql :supersede if-exists))
	   (and (not existsp) (eql :create if-does-not-exist)))
       ;; create/supercede the file now (later we open it with :overwrite
       (with-open-file (s data-path :direction :output :if-exists :supersede)))

      ((and existsp (eql :overwrite if-exists))
       ;; make sure we can actually open up the file later
       (with-open-file (s data-path :direction :io :if-exists :overwrite)))

      ((and (not existsp) (eql :error if-does-not-exist))
       (error 'db-does-not-exist-error))))
  node)
  