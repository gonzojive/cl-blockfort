(defpackage cl-blockfort
  (:use :common-lisp :alexandria)
  (:nicknames :blockfort :bfort)
  (:export
   ;; macros
   #:with-transaction
   ;; classes
   #:persistent-heap
   ;; conditions
   #:blockfort-condition
   #:heap-error
   #:heap-access-error
   #:read-sequence-error
   ;;; heap operations
   #:open-heap
   #:heap-close
   #:heap-read-sequence
   #:heap-read-n-byte-sequence
   #:heap-write-sequence
   #:heap-environment
   #:heap-expand
   #:heap-size
   #:heap-begin-transaction #:heap-commit-transaction #:heap-rollback-transaction
   ;; variables
   #:*heap*
   ))

(in-package :blockfort)






