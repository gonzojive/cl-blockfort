(in-package :bfort-tests)
(in-suite cl-blockfort-tests)

(deftest shared-lock-read1 ()
  (let ((lock (make-instance 'blockfort::shared-lock)))
    (let ((guarded-value 55))
      (blockfort::with-shared-lock-held (lock :shared)
	(blockfort::with-shared-lock-held (lock :shared)
	  (is 55 guarded-value))))))

(defmacro with-thread-testing-environment (empty &body body)
  (declare (ignore empty))
  `(progn ,@body))
;; this does not work
;;  `(let ((bordeaux-threads:*default-special-bindings*
;;	  (cons (cons stefil::*global-context* 'stefil::*global-context*)
;;		bordeaux-threads:*default-special-bindings*)))
;;     ,@body)

(defun make-test-thread (fn &key name)
  (let* ((gc-boundp  (boundp 'stefil::*global-context*))
	 (gc (when gc-boundp stefil::*global-context*)))
    (bordeaux-threads:make-thread
     #'(lambda ()
	 (if gc-boundp
	     (let ((stefil::*global-context* gc))
	       (funcall fn))
	     (funcall fn)))
     :name name)))

(deftest threads-dynamic-context-ok? ()
  "Tests threading."
  (let ((bound-in-main-threadp (boundp 'stefil::*global-context*))
	(bound-in-new-threadp nil))
    (with-thread-testing-environment ()
      (let ((thread
	     (make-test-thread
	      #'(lambda ()
		  (setf bound-in-new-threadp
			(boundp 'stefil::*global-context*))))))
	(bordeaux-threads:join-thread thread)))
    (is (eql bound-in-main-threadp
	     bound-in-new-threadp))))

(deftest shared-lock-thread1 ()
  (let* ((lock (make-instance 'blockfort::shared-lock))
	 (guarded-value 55)
	 (threads nil))
    (dotimes (threadnum 32)
      (with-thread-testing-environment ()
	(push
	 (make-test-thread
	  #'(lambda ()
	      (dotimes (i 500)
		(blockfort::with-shared-lock-held (lock :shared)
		  (is 55 guarded-value)))))
	 threads)))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))))

(deftest shared-lock-thread2 ()
  (let* ((lock (make-instance 'blockfort::shared-lock))
	 (guarded-value 55)
	 (threads nil))
    (blockfort::with-shared-lock-held (lock :exclusive)
      ;; launch all the threads
      (dotimes (threadnum 32)
	(with-thread-testing-environment ()
	  (push (make-test-thread
		 #'(lambda ()
		     (blockfort::with-shared-lock-held (lock :shared)
		       (is 62 guarded-value))))
		threads)))
      ;; now update the guarded value to 62
      (sleep 0.1) ;; give the threads some time to activate
      (setf guarded-value 62)
      ;; release the lock
      )
      
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))))

(deftest shared-lock-thread3 ()
  (let* ((lock (make-instance 'blockfort::shared-lock))
	 (guarded-value :original)
	 (threads nil))

    (blockfort::with-shared-lock-held (lock :shared)
      ;; launch all the threads
      (dotimes (threadnum 1)
	(with-thread-testing-environment ()
	  (push (make-test-thread
		 #'(lambda ()
		     (blockfort::with-shared-lock-held (lock :exclusive)
		       ;; modify the original value
		       (is :original guarded-value)
		       (setf guarded-value :modified-once))))
		threads)))
      ;; now update the guarded value to 62
      (sleep 0.1) ;; give the othe read some time to activate
      (is guarded-value :original)
      ;; release the lock
      )
    ;; wait for the threads to complete modifying the guarded value
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))

    (is guarded-value :modified-once)))

(deftest with-shared-lock-test1 ()
  (let* ((lock (make-instance 'blockfort::shared-lock)))
    (blockfort::with-shared-lock-held (lock :exclusive)
      (is (blockfort::exclusive-lock-heldp lock)))
    (blockfort::with-shared-lock-held (lock :shared)
      (is (not (blockfort::exclusive-lock-heldp lock))))))
    