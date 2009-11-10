(in-package :blockfort)

(defclass block-action (node-action)
  ((start
    :initarg :start :initform nil
    :accessor action-block-start)
   (end
    :initarg :end :initform nil
    :accessor action-block-end))
  (:documentation "Operations on blocks (contiguous regions of memory
in a particular node that start at byte offset START and end at byte
offset END."))

(defmethod block-length ((action block-action))
  (- (action-block-end action)
     (action-block-start action)))

(defclass read-block-action (block-action)
  ((output-seq
    :initarg :output-sequence :initform nil
    :accessor action-output-sequence)
   (output-seq-start
    :initarg :output-start :initform 0
    :accessor action-output-start
    :documentation "Offset within the data sequence from which to start writing data."))
  (:documentation "Read a block from a node"))

(defclass write-block-action (block-action)
  ((input-seq
    :initarg :input :initform nil
    :accessor action-input-sequence)
   (input-seq-start
    :initarg :input-start :initform 0
    :accessor action-input-start
    :documentation "Offset within the input sequence from which to
start writing data to the node's address space."))
  (:documentation "Action writes a sequence of bytes to a node."))

(defmethod node-perform-action ((node local-node) txn (action read-block-action) &key &allow-other-keys)
  ;; Performs the read without any protections
  ;; TODO maybe we need to lock the size of the database at least?  We might
  ;; be able to get away with just signaling an error
  (with-node-data-stream (stream node)
    (file-position stream (action-block-start action))
    (read-sequence (action-output-sequence action)
		   stream
		   :start (action-output-start action)
		   :end (+ (action-output-start action)
			   (block-length action)))))

(defmethod node-perform-action ((node local-node) txn (action write-block-action) &key old-value &allow-other-keys)
  ;; Performs the write without any protections (except logging)
  ;; TODO maybe we need to lock the size of the database at least?  We might
  ;; be able to get away with just signaling an error
  (with-node-data-stream (stream node)
    ;; FIXME: Since we use an undo/redo log we need to flush a log
    ;; entry before we change the disk.  We will need to generalize
    ;; this code to support other forms of logging in the future

    ;; FIXME Note that we assume here that we have the lock to write
    ;; to the disk, or that it is okay to be reading corrupted data
    ;; from the disk.  It seems strange to put down a log entry when
    ;; we may be reading corrupted data.  We should investigate this.

    (let* ((db-element (cons (action-block-start action)
			     (action-block-end action)))
	   (old-value (or old-value
			  (let ((seq (make-array (block-length action)
						 :element-type '(unsigned-byte 8))))
			    (node-perform-action node txn
						 (make-instance 'read-block-action
								:block-start (action-block-start action)
								:block-end (action-block-end action)
								:output seq)))))
	   (new-value (subseq (action-input-sequence action)
			      (action-input-start action)
			      (+ (action-input-start action) (block-length action))))
	   (log  (store-transaction-log (node-store node))))
      (log-modification log txn db-element old-value new-value)
;    (format t "Flushing...~%")
      (log-flush log)

    ;; Write the sequence to the file
      (file-position stream (action-block-start action))
      (write-sequence (action-input-sequence action)
		      stream
		      :start (action-output-start action)
		      :end (+ (action-output-start action)
			      (block-length action)))
    ;; no need to flush for undo/redo log
      )))


;;;; Log interaction for block operations
(defmethod db-redo-modification ((node local-node) transaction-log modification-log-entry)
  (let ((new-value (log-entry-new-value modification-log-entry))
	(db-element (log-entry-database-element modification-log-entry)))
    (with-node-data-stream (stream node)
      (file-position stream (car db-element))
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
      (write-sequence new-value stream)
      (finish-output stream))))

(defmethod db-undo-modification ((node local-node) transaction-log modification-log-entry)
  (let ((old-value (log-entry-old-value modification-log-entry))
	(db-element (log-entry-database-element modification-log-entry)))
    (with-node-data-stream (stream node)
      (file-position stream (car db-element))
;    (format t "Writing sequence ~A to ~A...~%" sequence stream)
      (write-sequence old-value stream)
      (finish-output stream))))

(defmethod db-element-as-octets ((node local-node) (block-range cons))
  (let ((array (make-array 8 :element-type '(unsigned-byte 8))))
    (integer-into-octet-vector array 4 (car block-range))
    (integer-into-octet-vector array 4 (cdr block-range) :start 4)
    array))

(defmethod db-element-from-octets ((node local-node) octets)
  (cons (octet-vector-to-integer (subseq octets 0 4))
	(octet-vector-to-integer (subseq octets 4 8))))

(defmethod db-value-as-octets  ((node local-node) value)
  value)