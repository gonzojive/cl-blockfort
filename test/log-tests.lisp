(in-package :bfort-tests)
(in-suite cl-transaction-log-tests)

(declaim (optimize (debug 3)))

(defparameter *test-db1-pathname*
  (merge-pathnames "db1/" *test-directory-pathname*))

(defclass vecdb ()
  ((array :initarg :array :initform nil :accessor db-array)
   (disk-array :initarg :disk-array :initform nil :accessor db-disk-array)
   (log :initarg :log :initform nil :accessor db-log)))

(defparameter *txn-counter* 1)
(defclass vecdb-transaction ()
  ((id :initarg :id :initform (incf *txn-counter*) :accessor txn-id)
   (writes :initarg :writes :initform nil :accessor txn-writes)))

(defun vecdb-open ()
  (let ((db
         (make-instance 'vecdb
                        :array (make-array 10 :initial-element 0)
                        :disk-array (make-array 10 :initial-element 0)
                        :log (make-instance 'undo/redo-log
                                            :path *test-db1-pathname*))))
    (setf (log-db (db-log db)) db)
    (log-open (db-log db) :if-exists :supersede :if-does-not-exist :create)
    db))

(defun vecdb-with-transaction (db fn)
  "Calls fn in the context of a new transaction."
  (let ((transaction (make-instance 'vecdb-transaction)))
    (log-begin-transaction (db-log db) (txn-id transaction))
    (let ((err nil))
      (handler-case (funcall fn transaction)
        (error (e)
          (setf err e)
          (log-flush (db-log db))
          (log-recover (db-log db) db)
          (error e)))
      (unless err
        (log-commit-transaction (db-log db) (txn-id transaction))
        (log-flush (db-log db))
        (dolist (write (reverse (txn-writes transaction)))
          (setf (elt (db-disk-array db) (car write)) (cdr write)))))))


(defun vecdb-value (db index &optional diskp)
  (elt (if diskp (db-disk-array db) (db-array db))
       index))

(defun vecdb-update (db transaction index new-value)
  "Updates the INDEX'th value of the vector to be new value."
  (let ((old-value (vecdb-value db index)))
    (log-modification (db-log db) (txn-id transaction) index old-value new-value)
    (push (cons index new-value) (txn-writes transaction))
    (setf (elt (db-array db) index) new-value)))

(defmethod db-redo-modification ((database vecdb) transaction-log modification-log-entry)
  (let ((index (log-entry-database-element modification-log-entry)  )
        (new-value (log-entry-new-value modification-log-entry)))
    (setf (elt (db-disk-array database) index) new-value
          (elt (db-array database) index) new-value)))

(defmethod db-undo-modification ((database vecdb) transaction-log modification-log-entry)
  (let ((index (log-entry-database-element modification-log-entry)  )
        (old-value (log-entry-old-value modification-log-entry)))
    (setf (elt (db-disk-array database) index) old-value
          (elt (db-array database) index) old-value)))

(defmethod db-element-as-octets ((database vecdb) database-element)
  (integer-to-octet-vector 8 database-element))

(defmethod db-element-from-octets ((database vecdb) octets)
  (octet-vector-to-integer octets))

(defmethod db-value-as-octets ((database vecdb) value)
  (integer-to-octet-vector 8 value))

(defmethod db-value-from-octets ((database vecdb) octets)
  (octet-vector-to-integer octets))

;; utilities
(defun random-byte-vector (length)
  (let ((vector (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (elt vector i) (random 255)))
    vector))

(deftest testing-environment-works ()
  (is (= 5 5))
  (is (null nil))
  t)

(deftest logging-as-usual-works ()
  "Make sure we can proceed as usual if there is no error."
  (let ((db (vecdb-open)))
    (is db)
    (is (db-log db))
    (vecdb-with-transaction 
     db
     #'(lambda (txn)
         (is (eql 0 (vecdb-value db 2)))
         (vecdb-update db txn 2 6)
         (is (eql 6 (vecdb-value db 2)))
         (vecdb-update db txn 3 8)
         (is (eql 8 (vecdb-value db 3)))))
    (is (eql 6 (vecdb-value db 2)))
    (is (eql 8 (vecdb-value db 3)))
    
    (vecdb-with-transaction
     db
     #'(lambda (txn)
         (vecdb-update db txn 1 5)
         (is (eql 5 (vecdb-value db 1)))))

    (is (eql 5 (vecdb-value db 1)))))

(define-condition intended-error (error)
  ())

(deftest test-logging-with-rollback ()
  "Make sure errors are rolled back as appropriate."
  (let ((db (vecdb-open)))
    (is db)
    (is (db-log db))
    (vecdb-with-transaction 
     db
     #'(lambda (txn)
         (is (eql 0 (vecdb-value db 2)))
         (vecdb-update db txn 2 6)
         (is (eql 6 (vecdb-value db 2)))
         (vecdb-update db txn 3 8)
         (is (eql 8 (vecdb-value db 3)))))
    (is (eql 6 (vecdb-value db 2)))
    (is (eql 8 (vecdb-value db 3)))
    
    (handler-case 
        (vecdb-with-transaction
         db
         #'(lambda (txn)
             (vecdb-update db txn 1 5)
             (is (eql 5 (vecdb-value db 1)))
             (error 'intended-error "This was thrown intentionally")))
      (intended-error (e) (declare (ignore e))))
    
    (is (eql 0 (vecdb-value db 1)))))


(defun make-simple-array (seq)
  (make-array (length seq) :element-type '(unsigned-byte 8) :initial-contents seq))

(deftest heap-rw1 ()
  (let ((data (make-simple-array #(200 201 202 203 204 205 206 207))))
    ;; open it up once
    (let ((heap (open-heap *test-db1-pathname*)))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data :heap-start 0 :transaction transaction)
	(is (equalp data (heap-read-n-byte-sequence heap (length data) :heap-start 0)))
	(heap-commit-transaction heap transaction))
      (is (equalp data (heap-read-n-byte-sequence heap (length data) :heap-start 0))))))

(deftest heap-reopens ()
  (let ((data (make-simple-array #(200 201 202 203 204 205 206 207))))
    ;; open it up once
    (let ((heap (open-heap *test-db1-pathname* :if-exists :supersede)))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data :heap-start 0 :transaction transaction)
	(heap-commit-transaction heap transaction))
      (heap-close heap))
    (let ((heap (open-heap *test-db1-pathname*)))
      (is (equalp data (heap-read-n-byte-sequence heap (length data) :heap-start 0))))))

(deftest heap-reopens-supercede ()
  (let ((data (make-simple-array #(200 201 202 203 204 205 206 207))))
    ;; open it up once
    (let ((heap (open-heap *test-db1-pathname* :if-exists :supersede)))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data :heap-start 0 :transaction transaction)
	(heap-commit-transaction heap transaction))
      (heap-close heap))
    (let ((heap (open-heap *test-db1-pathname* :if-exists :supersede)))
      (is (eql 'does-raise-error
	       (handler-case (heap-read-n-byte-sequence heap (length data) :heap-start 0)
		 (read-sequence-error () 'does-raise-error)))))))

(deftest recover-uncommitted ()
  "Open up a heap store,
1. Begin Transaction 1
2. put some data in from 0-7
3. Commit Transaction 1
4. Begin Transaction 2
5. Put some data2 in from 0-7
6. Recover the database
7. Check to make sure the data is data1 not data2"

  (let ((data1 (make-simple-array #(200 201 202 203 204 205 206)))
	(data2 (make-simple-array #(100 101 102 103 104 105 106))))
    ;; 
    (let ((heap (open-heap *test-db1-pathname* :if-exists :supersede)))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data1 :heap-start 0 :transaction transaction)
	(heap-commit-transaction heap transaction))
      (is (equalp data1 (heap-read-n-byte-sequence heap (length data1) :heap-start 0)))
      ;;
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data2 :heap-start 0 :transaction transaction))

      (blockfort::log-recover (blockfort::heap-log heap) heap)
      
      (is (equalp data1 (heap-read-n-byte-sequence heap (length data1) :heap-start 0)))
      (heap-close heap))))

(deftest recover-committed ()
  "Open up a heap store,
1. Begin Transaction 1
2. put some data in from 0-7
3. Commit Transaction 1
4. Begin Transaction 2
5. Put some data2 in from 0-7
6. Recover the database
7. Check to make sure the data is data1 not data2"

  (let ((data1 (make-simple-array #(200 201 202 203 204 205 206)))
	(data2 (make-simple-array #(100 101 102 103 104 105 106))))
    ;; 
    (let ((heap (open-heap *test-db1-pathname* :if-exists :supersede)))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data1 :heap-start 0 :transaction transaction)
	(heap-commit-transaction heap transaction))
      (is (equalp data1 (heap-read-n-byte-sequence heap (length data1) :heap-start 0)))
      ;;
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data2 :heap-start 0 :transaction transaction)
	(heap-commit-transaction heap transaction))

      (blockfort::log-recover (blockfort::heap-log heap) heap)
      
      (is (equalp data2 (heap-read-n-byte-sequence heap (length data2) :heap-start 0)))
      (heap-close heap))))
