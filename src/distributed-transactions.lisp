(in-package :cl-blockfort)

;;; two-phase locking id described in chapter 19 of the complete databases book (pg 1024)
;;; Phase I:
;;;   1.  Coordinator places a log record <Prepare T> on the log at its site
;;;   2.  Coordinator sends to each component's sit (in principle including itself) the
;;;   message `prepare T'
;;;   3.  Each site receiving the essage prepare T decides whether to commit or abort its
;;;   component of T.
;;;   4.  If a site wants to commit its component, it must enter a state called precommitted
;;;   Once in the precommitted state, the site cannot abort its component of T without a 
;;;   directive to do so from the coordinator.  However, the site does not _commit_ yet
;;;      a.   Ensure the local component of T will not have to abort, even if there is a
;;;      system failure followed by recovery at the site.
;;;      b.  Place and record <Ready T> at the local log site
;;;      c.  Send the coordinator a `Ready T' message
;;;   5.  Send If the site does not want to commit its component of T, then it logs the
;;;   record <Don't commit T> and sends the message `don't commit T' t the coordinator.
;;;   It is safe to abort the component at this time, since T will surely be aborted.
;;; Phase II: Begins when `ready T'
;;;   1.  

(defclass distributed-message ()
  ()
  (:documentation "Represents a message sent from one part of the database to another,
e.g. the ready and don't commit messages."))

(defclass single-transaction-distributed-message (distributed-message)
  ((transaction
    :initarg :transaction :type message-transaction :accessor message-transaction
    :documentation "The MESSAGE-TRANSACTION that corresponds to this distributed message."))
  (:documentation "Represents a message sent from one part of the database to another,
e.g. the ready and don't commit messages, that relate to a single transaction."))

(defclass distributed-log-entry (log-entry)
  ()
  (:documentation "All log entries that specifically relate to distributed transactions
are subclasses of this class."))

(defclass prepare-log-entry (distributed-log-entry single-transaction-log-entry)
  ()
  (:documentation "The coordinator of a distributed, two-phase commit writes
a PREPARE entry to the log "))

(defclass ready-log-entry (distributed-log-entry single-transaction-log-entry)
  ()
  (:documentation "The coordinator of a distributed, two-phase commit writes
a READY entry to the log after Phase I of the two-phase commit."))

(defclass do-not-commit-log-entry (distributed-log-entry single-transaction-log-entry)
  ()
  (:documentation "The coordinator of a distributed, two-phase commit writes
a DON'T COMIT entry to the log after Phase I of the two-phase commit if it decides
to abort the transaction."))


