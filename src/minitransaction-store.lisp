(in-package :blockfort)

;;;; Minitransactions
(defclass minitransaction ()
  ((conditional-action
    :initarg :conditional-action :initform nil
    :accessor minitransaction-conditional-action
    :documentation "This is the action executed by the minitransaction at commit time."))
  (:documentation "A minitransaction is an optimist transaction that
performs a short-lived atomic, conditional action at commit-time.  All
database reads/writes throughout the duration of the transaction
happen without locks and accumulate in the atomic conditional action
executed at commit-time.

Minitransactions can be used to, for example, execute an optimistic
operation on a series of database elements and at the end atomically
check/increment their version numbers iff their version numbers
correspond to the version numbers read during the optimistic phase of
the minitransaction."))

;;;; 

(defclass atomic-conditional-action ()
  ((test-actions
    :initarg :test-actions :initform nil
    :accessor atomic-conditional-test-actions
    :documentation "A list of actions that must succeed in order to subsequently 
perform the success actions")
   (success-actions
    :initarg :success-actions :initform nil
    :accessor atomic-conditional-success-actionsn
    :documentation "A list of actions that execute when the test succeeds."))
  (:documentation "Used to implement minitransactions, where a node in
the store will atomically perform some test actions, if they all
succeed, will then perform a set of success actions (independent reads
and writes)."))

(defmacro with-locks-for-store-action ((store action &key) &body body)
  (once-only (store action)
    (with-unique-names (locks)
      `(let ((,locks (store-acquire-locks-for-action ,store ,action)))
	 (unwind-protect (progn ,@body)
	   (store-release-locks ,store ,locks))))))

(defmethod store-commit-transaction ((store store) (transaction minitransaction))
  (store-perform-action store
			transaction
			(minitransaction-conditional-action transaction)))

(defmethod store-perform-action :around ((store store) (txn minitransaction) (action atomic-conditional-action) &key &allow-other-keys)
  ;; TODO check for sound operation and rollback appropriately
  (with-locks-for-store-action (store action)
    (call-next-method)))

(defmethod store-perform-action ((store store) (tx minitransaction) (action atomic-conditional-action) &key &allow-other-keys)
  (if (every (curry 'store-perform-action store)
	     (atomic-conditional-test-actions action))
      (progn
	(dolist (success-action (atomic-conditional-success-actions action))
	  (store-perform-action store success-action))
	t)
      nil))

(defclass comparison-action ()
  ((first-element
    :initarg :first-element
    :accessor comparison-first-element)
   (second-element
    :initarg :first-element
    :accessor comparison-second-element)))

;;; implementation
(defmethod initialize-instance :after ((store store) &rest args)
  (declare (ignore args))

  (setf (slot-value store 'data-file)
	(make-instance 'binary-file
		       :path (merge-pathnames (store-environment store) "db-data")))

  (setf (slot-value store 'transaction-log)
	(make-instance 'transaction-log
		       :path (merge-pathnames (store-environment store) "db-log")
		       :db store)))

