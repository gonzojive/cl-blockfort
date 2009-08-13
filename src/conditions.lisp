(in-package :blockfort)

(define-condition blockfort-condition ()
  ())

(define-condition blockfort-error (blockfort-condition error)
  ())

(define-condition heap-error (blockfort-error)
  ((heap :initarg :heap :initform nil :accessor condition-heap))
  (:report (lambda (condition stream)
	     (format stream "Error with heap ~A" (condition-heap condition)))))

