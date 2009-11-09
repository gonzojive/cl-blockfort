(in-package :blockfort)

(define-condition blockfort-condition ()
  ())

(define-condition blockfort-error (blockfort-condition error)
  ())

(define-condition db-does-not-exist-error (blockfort-error) ())

(define-condition log-condition (blockfort-condition) ())

(define-condition log-error (blockfort-error) ())

(define-condition log-does-not-exist-error (log-error) ())

(define-condition heap-error (blockfort-error)
  ((heap :initarg :heap :initform nil :accessor condition-heap))
  (:report (lambda (condition stream)
	     (format stream "Error with heap ~A" (condition-heap condition)))))

(define-condition heap-access-error (heap-error)
  ((heap :initarg :heap :initform nil :accessor condition-heap))
  (:report (lambda (condition stream)
	     (format stream "Access error with heap ~A" (condition-heap condition)))))

(define-condition read-sequence-error (heap-access-error)
  ((heap :initarg :heap :initform nil :accessor condition-heap))
  (:report (lambda (condition stream)
	     (format stream "Failed to read from heap ~A" (condition-heap condition)))))

