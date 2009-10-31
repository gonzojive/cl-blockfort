(in-package :cl-blockfort)

(defclass lock ()
  ()
  (:documentation "Abstract lock class."))

(defclass shared-lock (lock)
  ((system-lock
    :initarg :system-lock
    :initform nil
    :accessor system-lock
    :documentation "Each lock object uses a system lock object as the underlying locking mechanism.")
   (system-lock-name
    :initarg :system-lock-name
    :initform (symbol-name (gensym "LOCK"))
    :accessor system-lock-name
    :documentation "Each lock has an underlying system name (a symbol) that uniquedly defines
the lock")
   (exclusively-available-condv
    :initform (bordeaux-threads:make-condition-variable)
    :accessor exclusively-available-condv
    :documentation "This condition is signaled when an exclusive lock is released or all shared
locks are released.  In either case, an exclusive lock will then be available.
Requires that system-lock be held in order to read/write.")
   (exclusive-lock-heldp
    :initform nil
    :accessor exclusive-lock-heldp
    :documentation "This is non-nil if the lock is being held exclusively (and no shared locks
may be aquired, or additional exclusive locks.
Requires that system-lock be held in order to read/write.")
   (shared-count
    :initform 0
    :accessor shared-count
    :documentation "Contains the # of shared locks.
Requires that system-lock be held in order to read/write."))
  (:documentation "A shared lock allows many threads to hold a shared lock at once (presumably for 
reading a value), but only one thread to hold an 'exclusive lock' (presumably for writing).
While an exclusive lock is held, no shared locks may be held."))

(defmethod initialize-instance :after ((lock shared-lock) &key &allow-other-keys)
  ;; set up the system lock and condition variables
  (setf (system-lock lock)
	(bordeaux-threads:make-lock (system-lock-name lock))))

(defgeneric acquire-shared-lock (lock access-type)
  (:documentation "Acquires an instance of a shared-lock.

ACCESS-TYPE is presumably :shared or :exclusive. "))

(defgeneric release-shared-lock (lock access-type)
  (:documentation "Releases an instance of a shared-lock.

ACCESS-TYPE is presumably :shared or :exclusive. "))

(defmethod acquire-shared-lock ((lock shared-lock) (access-type (eql :shared)))
  "Acquires a shared lock."
  (bordeaux-threads:with-lock-held ((system-lock lock))
    (loop :until (not (exclusive-lock-heldp lock))
	  :do (bordeaux-threads:condition-wait (exclusively-available-condv lock)
					       (system-lock lock)))
    (incf (shared-count lock))))

(defmethod acquire-shared-lock ((lock shared-lock) (access-type (eql :exclusive)))
  "Acquires an exclusive lock."
  (bordeaux-threads:with-lock-held ((system-lock lock))
    (loop :until (and (not (exclusive-lock-heldp lock))
		      (= 0 (shared-count lock)))
	  :do (bordeaux-threads:condition-wait (exclusively-available-condv lock)
					       (system-lock lock)))
    (setf (exclusive-lock-heldp lock) t)))

(defmethod release-shared-lock ((lock shared-lock) (access-type (eql :shared)))
  "Release a shared lock."
  (bordeaux-threads:with-lock-held ((system-lock lock))
    ;; decrease the lock count and notify others that the exlclusive
    ;; lock is available, if necessary
    (decf (shared-count lock))
    (when (= 0 (shared-count lock))
      (bordeaux-threads:condition-notify (exclusively-available-condv lock)))))

(defmethod release-shared-lock ((lock shared-lock) (access-type (eql :exclusive)))
  "Release an exclusive lock."
  (bordeaux-threads:with-lock-held ((system-lock lock))
    ;; decrease the lock count and notify others that the exlclusive
    ;; lock is available, if necessary
    (setf (exclusive-lock-heldp lock) nil)
    (bordeaux-threads:condition-notify (exclusively-available-condv lock))))

(defmacro with-shared-lock-held ((lock access-type) &body body)
  "Given a lock and access type, acquires the lock with the given access type, evaluates body,
and returns."
  (let ((lock-var (gensym "lock"))
	(access-type-var (gensym "access-type")))
    `(let ((,lock-var ,lock)
	   (,access-type-var ,access-type))
       (acquire-shared-lock ,lock-var ,access-type-var)
       (unwind-protect (progn ,@body)
	 (release-shared-lock ,lock-var ,access-type-var)))))
