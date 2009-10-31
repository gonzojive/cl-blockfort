(in-package :bfort-tests)
(in-suite cl-blockfort-tests)

(defparameter *test-db1-pathname*
  (merge-pathnames "db1/" *test-directory-pathname*))


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
