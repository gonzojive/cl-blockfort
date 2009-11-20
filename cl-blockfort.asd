(defpackage org.iodb.cl-blockfort.system
  (:use #:cl #:asdf))

(in-package :org.iodb.cl-blockfort.system)

(defsystem cl-blockfort
  :description "An ACID-compliant, persistent, and eventually distributed heap."
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "LGPL"
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "conditions" :depends-on ("package"))
			 (:file "crc" :depends-on ("package"))
			 (:file "files" :depends-on ("package" "conditions" "basic-threading"))
			 ;; distributed network nodes
			 (:file "network" :depends-on ("package" "conditions" "basic-threading" "serialize"))
			 ;; Concurrency
			 (:file "basic-threading" :depends-on ("package"))
			 (:file "local-locks" :depends-on ("package"))
			 (:file "distributed-locks" :depends-on ("package" "network"))
			 ;; Logging
			 (:file "serialize" :depends-on ("files" "package"))
			 (:file "log" :depends-on ("files" "package" "crc" "basic-threading" "serialize"))
			 
			 (:file "store" :depends-on ("log" "files" "network" "distributed-locks"))

	       )))
  :depends-on ("bordeaux-threads" "trivial-garbage" "alexandria" "usocket"))

(defsystem cl-blockfort-tests
  :components ((:module "test"
                        :components ((:file "test-package")
				     #+nil
				     (:file "log-tests" :depends-on ("test-package"))
				     #+nil
				     (:file "locks-tests" :depends-on ("test-package"))
				     )))
  :depends-on ("cl-blockfort" "stefil"))
