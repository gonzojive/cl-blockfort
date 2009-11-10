;(defgeneric db-undo-modification (database transaction-log modfication-log-entry)
;  (:documentation "Undoes the modification to the database."))

(defmethod store-size ((store persistent-store))
  (with-store-data-stream (stream store)
    (file-length stream)))

(defmethod store-expand ((store persistent-store) octet-count)
  ;; TODO this should most certainly occur with a transaction
  (with-store-data-stream (stream store)
    (file-position stream (file-length stream))
    (loop :for i :from 0 :upto (- octet-count 1)
       :do  (write-byte 0 stream))))
    
    

