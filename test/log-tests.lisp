(in-package :bfort-tests)
(in-suite cl-blockfort-tests)

;; utilities
(defun random-byte-vector (length)
  (let ((vector (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (elt vector i) (random 255)))
    vector))

(defun heap-read-n-byte-sequence (heap n &key heap-start)
  (let ((seq (random-byte-vector n)))
    (heap-read-sequence heap seq :heap-start heap-start)
    seq))

(deftest testing-environment-works ()
  (is (= 5 5))
  (is (null nil))
  t)

(deftest heap-rw1 ()
  (let ((data #(200 201 202 203 204 205 206 207)))
    ;; open it up once
    (let ((heap (open-heap "test/db1/")))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data :heap-start 0 :transaction transaction)
	(is (equalp data (heap-read-n-byte-sequence heap (length data) :heap-start 0)))
	(heap-commit-transaction heap transaction))
      (is (equalp data (heap-read-n-byte-sequence heap (length data) :heap-start 0))))))

(deftest heap-reopens ()
  (let ((data #(200 201 202 203 204 205 206 207)))
    ;; open it up once
    (let ((heap (open-heap "test/db1/")))
      (let ((transaction (heap-begin-transaction heap)))
	(heap-write-sequence heap data :heap-start 0 :transaction transaction)
	(heap-commit-transaction heap transaction))
      (heap-close heap))
    (let ((heap (open-heap "test/db1/")))
      (is (equalp data (heap-read-n-byte-sequence heap (length data) :heap-start 0))))))

(deftest recover-uncommitted ()
  "Open up a heap store,
1. Begin Transaction 1
2. put some data in from 0-7
3. Commit Transaction 1
4. Begin Transaction 2
5. Put some data2 in from 0-7
6. Recover the database
7. Check to make sure the data is data1 not data2"

  (let ((data1 #(200 201 202 203 204 205 206 207))
	(data2 #(100 101 102 103 104 105 106 107)))
    ;; 
    (let ((heap (open-heap "test/db1/")))
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
