(in-package :blockfort)

(defvar *thread-local-hash* (trivial-garbage:make-weak-hash-table :weakness :key)
  "Global table that maps each thread to a hash table with thread-local info.")

(defvar *thread-local-hash-lock* (bordeaux-threads:make-lock)
  "Lock for reading from *thread-local-hash*")

(defun thread-local-value (key)
  "Returns the value, in the currently-running thread, of the value of KEY"
  (let* ((thread (bordeaux-threads:current-thread))
	 (local-hash
	  (bordeaux-threads:with-lock-held (*thread-local-hash-lock*)
	    (gethash thread *thread-local-hash*))))
    (if local-hash
	(gethash key local-hash)
	(values nil nil))))

(defun (setf thread-local-value) (value key)
  ""
  (let* ((thread (bordeaux-threads:current-thread))
	 (local-hash
	  (bordeaux-threads:with-lock-held (*thread-local-hash-lock*)
	    (or (gethash thread *thread-local-hash*)
		(progn (let ((local-hash (make-hash-table)))
			 (setf (gethash thread *thread-local-hash*) local-hash)
			 local-hash))))))
    (setf (gethash key local-hash) value)))

(defmacro thread-local-binding (name)
  `(thread-local-value ',name))

(defun thread-finalize (thread function)
  "Executes function on thread when CURRENT-THREAD will never return
the value of thread anymore."
  (trivial-garbage:finalize thread function))