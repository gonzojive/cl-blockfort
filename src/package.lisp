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

   ;; serialization
   #:integer-to-octet-vector
   #:integer-into-octet-vector
   #:octet-vector-to-integer

   ;; files
   #:binary-file
   #:binary-file-path
   #:binary-file-stream
   #:open-thread-local-stream
   ))

(cl:defpackage :cl-transaction-log
    (:nicknames :transaction-log :txn-log)
  (:use :cl :alexandria :anaphora)
  (:export #:transaction-log
           #:undo/redo-log
           #:log-open
           #:log-close
           #:log-begin-transaction
           #:log-commit-transaction
           #:log-modification
           #:log-flush
           #:log-recover
           #:log-db
           ;; generic implemented by db
           #:db-redo-modification
           #:db-undo-modification
           #:db-element-as-octets
           #:db-element-from-octets
           #:db-value-as-octets
           #:db-value-from-octets

           ;; log entries
           #:log-entry-old-value
           #:log-entry-new-value
           #:log-entry-database-element

           ))

(in-package :blockfort)






