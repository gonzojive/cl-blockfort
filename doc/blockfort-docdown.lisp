(defpackage :blockfort.doc
    (:use :docdown :cl :alexandria))

(in-package :blockfort.doc)


(defun output-docs ()
  (with-open-file (stream (asdf:system-relative-pathname (asdf:find-system :cl-blockfort)
							 "www/index.html")
			  :direction :output :if-exists :supersede)
    (write-string (generate-html-page 'index) stream)))

(progn
  (defdoc index :page
    (:title "Blockfort")
    (:systems :cl-blockfort); :cl-tidy.doc)
    (:content
     "#### Blockfort: an [ACID](#acid)-compliant transactional database engine

Blockfort is a low-level, distributed transactional data store
programmed in Common Lisp and intended for use by more abstract
database systems.

There are many forms of databases, from pointer-based object databases
to simple B-Tree stores like Berekely DB to monstrous black boxes like
Oracle's products.  Blockfort attempts to support a wide range of
these high-level database abstractions and provide all the necessary
primitives.

Importantly, Blockfort is a distributed database system that is
intended to scale well to many database nodes.  Of course, blockfort
supports single-node operation, but the ability to scale a system is
important and so Blockfort acknowledges a possible distributed nature
and provides the appropriate primitives for high-level paradigms to
operate.

All the code is maintained in a git repository.  To
obtain the library, use the following command:

    git clone git://github.com/gonzojive/cl-blockfort.git

")

    (:sections
     (defdoc guide :section
       (:title "User Guide")
       (:content "")
       (:sections
        (defdoc download :section
          (:title "Download and Installation")
          (:content "All the code is maintained in a git repository.  To
obtain the library, use the following command:

    git clone git://github.com/gonzojive/cl-blockfort.git

You can also browse the code at [http://github.com/gonzojive/cl-blockfort](http://github.com/gonzojive/cl-blockfort).
"))))
     (defdoc concepts :section
       (:title "Concepts")
       (:content "")
       (:sections
        (defdoc acid :section
          (:title "ACID")
          (:content "Blockfort guarantees that certain behaviors are ACID compliant in its
own sense and allows applications that call into Blockfort to
guarantee ACID compliance in a way that makes sense for the
application.  ACID transactions guarantee the following properties:

- *Atomicity* -- The transaction executes either all or none of its operations.

- *Consistency* -- The database is in a \"consistent\" state before and after the
transaction executes.

- *Isolation* -- Concurrent transactions appear to be executing one after another,
and they do not reflect intermediate changes made by each other.

- *Durability* -- After a transaction has successfully committed, the database will
reflect its changes.

Atomicity and Durability are almost exclusively the responsibility of
Blockfort, while Isolation and Consistency rely on the interaction of
the database client and Blockfort.  These interactions will become
clearer later in this chapter.

"))
        (defdoc architecture :section
          (:title "System Architecture")
          (:content "To accomplish ACID compliance, Blockfort relies
on two basic principles: [transaction logging](#transactions) for
[recovering](#recovery) from failures, and a concurrency scheduler for
dealing with concurrently-executing transactions.  How ACID compliance
is achieved in light of the rather agnostic nature of Blockfort in
regards to the higher-level structure of its data should become clear
after reading about blocks, concurrency control, and the interface
between the two in Blockfort.

![System architecture diagram](architecture.png)

The above diagram illustrates the basic architecture of a
blockstore-based database system.  For each computer involved in
conducting transactions on the database, there is a [server](#server) object
that
")
          (:sections
           (defdoc server :section
             (:title "Servers")
             (:content "Servers are the nodes on a distributed
database that communicate with each other to effect database
transactions.  We will document this further as more becomes clear
about how they work."))
           (defdoc store :section
             (:title "Stores")
             (:content "A store makes database changes materialize in
a persistent way.  A store need not know how it communicates with
other stores (e.g. over ethernet)--that is the responsibility of the
server.  A store must manage the data in the database by making sense
of [transactions](#transactions) and their component actions."))

           (defdoc transactions :section
             (:title "Transactions")
             (:content "
In Blockfort, a *transaction* is an protocol that wraps a set of
*actions* and guarantees atomicity, consistency, isolation, and
durability.  Many different types and subtypes of transactions exist,
and Blockfort attempts to abstract transactions enough to support many
of the different types of transactions without overgeneralization.

- *Lock-based transactions*:  Nearly all concurrent database
systems use locks to some degree to implement transactions, though how
exactly the locking scheme works varies widely.  Two-phase locking
\(2PL\) guarantees serializability of transactions by requiring that no
lock may be acquired after a single lock has been released.  A tree
protocol for locking requires far less contention for B-Trees by
violating 2PL and allowing locks after unlocks.  In both of these
cases, the locking scheme is pessimistic, acquiring locks as data is
read and written, and releasing the locks either at the end of the
transaction \(2PL\) or as nodes on the tree are traversed \(tree
protocol\).

- *Optimistic transactions*:  Optimistic transactions assume that
data accesses will not conflict and checks these assumptions by
performing an atomic validation step during commit.  Specific
mechanisms in this league include timestamp methods and validation
methods.  In many implementations of optimistic transactions, locks
are still necessary but only during the commit phase.

The details of the [transaction protocol](#transaction-protocol) are
described later."))
           (defdoc logs :section
             (:title "Logging")
             (:content "Blockfort provides undo/redo logging to
support recovery from database crashes and thus satisfy the durability
criterion of [ACID](#acid).  A logging package provides the logging
functionality in a highly modular way.

"))
           
           #+nil
           (defdoc block :section
             (:title "Blocks")
             (:content "
### *Note: this section ain't right*

*Blocks* are the basic unit of data in Blockfort.  A block is a
vector of contiguous (binary) data that persists after a computer
turns off and on.  Note that a Blockfort block is distinct from a
*disk block*, though the two may correspond physically in certain
cases. The interface to Blockfort does not assume anything about the
structure of blocks or place any extra metadata into blocks when they
are stored on the disk.

Each node in a distributed Blockfort environment has its own
*address space* in which blocks are allocated.  The units of an
address spess are 8-bit bytes.  A block in Blockfort is identified by
its node and offset into its node's address space.  Replication at the
block level is left up to the application.  Replication at the
*node* level may potentially be implemented by Blockfort in the
future (i.e. replicating the entire address space of a node
vs. replicating a single block)."))

           ))))


        #+nil
        (defdoc transaction-protocol :section
             (:title "Transaction Protocol")
             (:content "Blockfort attempts to support many classes of transaction with a
generic transaction protocol.  The basic principles of this protocol
are as follows:

A transaction begins with a call to the generic function
@code{store-start-transaction}.

@item
While a transaction is active, actions may be performed on the
database calling the generic function @code{(store-perform-action
store transaction action)}.  
@itemize
@item Eager-locking transactions
like 2PL and the tree-protocol will presumably acquire (and
potentially release) locks as actions are performed.
@item Optimistic transactions
will presumably accumulate read and write objects that will require
some form of validation during the commit phase of the transaction.
@end itemize

@item
A transaction attempts to commit with a call to the generic function
@code{store-commit-transaction}.

@item
A transaction aborts with a call to the generic function
@code{store-rollback-transaction}.  As soon as such a call is made,
the transaction is invalidated and any actions that took place during
the transaction are undone.
@end enumerate

Using this protocol a single Blockfort data store may implement a wide
range of possible transactions.  There are also some built-in
transaction types, but these may be overruled entirely.

@subsection Action protocol
Actions are an abstraction of database operations like reads and
writes.  They may also be more complex actions, such as ``atomic,
conditional read/write'' that encapsulate more complex behavior.

For example, an action as it is passed to @code{store-perform-action}
may be an instance of the built-in class @code{read-block-action} or
@code{write-block-action} which further decompose themselves and
recursively call @code{store-perform-action}.

The action protocol consists of two different components to ensure
that a transaction can handle an action in an ACID-compliant manner:

@enumerate
@item Database updates.
An action may need to update the database and thus needs to provide an
implementation of the update procedure.  Often an action decomposes
into primitive reads and writes.
@item Log updates.
An action needs to ensure that its database updates may be reversed
and serialized into a log entry.
@end enumerate

@subsection Distributed Transactions
Blockfort is a distributed database framework provides a protocol for
communicating custom actions between nodes..  Blockfort uses two-phase
commit (2PC) to perform resilient atomic transactions across many
nodes.

Two-phase commit is a protocol for performing some set of
actions atomically over a number nodes.  There is a single coordinator
of the distributed transaction that send a query to commit to all of
the cohorts (who must also participate in the transaction).  Each
cohort then attempts to fulfill its end of the bargain.  If a cohort
fails, it will return NO to the coordinator; otherwise it will return
YES and write something to its logs to ensure both that, even in the
case of a crash, the cohort can commit the transaction, and also
information to its logs to allow it to revert the partially-fulfilled
transaction.  The coordinator received all of the YES/NO responses
back, and if they are all YES it sends a COMMIT message to each
cohort; otherwise it sends an ABORT.

Actions performed during a transaction may correspond to physical
operations at multiple nodes in the distributed system.  A message
must be sent from one node to another to reques that particular
actions be taken at the destination node.  Thus, for actions that do
not decompose into primitive block read/write actions, the actions
must be made to conform to a message-passing protocol.

Blockfort does not specify a default protocol for communicating
between nodes, though a default implementation is supplied for a
TCP/IP-based communication layer.

TODO what exactly is the specification for this?

"))

     (defdoc logging :section
       (:title "Undo/Redo Logging")
       (:content "To ensure durability of transactions, Blockfort
maintains a disk log that can recover the entire database using just
the log files.  The log is a separate package, CL-TRANSACTION-LOG,
from the rest of blockfort to maintain modularity.  It works with an
abstract idea of a database and only requires that a few functions be
implemented and conventions be followed in order to make a database
fully recoverable from a crash.")


       (:sections
        (defdoc logging-rule :section
          (:title "Logging Rule")
          (:content "The log mechanism at the moment is an undo/redo
log, which has the following logging rule:

>  **Logging Rule**: Before modifying any database element X on disk
>  because of changes made by some transaction T, it is necessary that
>  the update record appear on the disk.

In addition, Blockfort enforces a strict commit rule:

>  **Commit Rule**: A commit record must be flushed to disk before the
>  user is informed that a transaction committed.

The strict commit semantics are necessary for certain applications and
not for others, but we employ it here.  They ensure that when a
transaction commits, it will be recovered frm the log by a commit.



"))
        (defdoc logging-transactions :section
          (:title "Logging transactions")
          (:content "The most common actions performed on a log are
actions that occur during database transactions.  The functions most
important to this interaction are:

1. [log-begin-transaction](#log-begin-transaction) -- begins a transaction
2. [log-commit-transaction](#log-commit-transaction) -- commits a transaction
3. [log-modification](log-modification) -- logs a modification to disk

Note that for all of these functions, a transaction is assumed to be
an integer.
")
          (:sections
           (defdoc cl-transaction-log:log-begin-transaction :generic)
           (defdoc cl-transaction-log:log-commit-transaction :generic)
           (defdoc cl-transaction-log:log-modification :generic)))

        (defdoc logging-recovery :section
          (:title "Recovering a database from a log")
          (:content "Recovering is easy!

[log-recover](#log-recover) recovers a database from a log file.
")
          (:sections
           (defdoc cl-transaction-log:log-recover :generic)))

        (defdoc logging-implementation-protocol :section
          (:title "Implementation Protocol")
          (:content "The log of the system requires that a database
system obey a minimal protocol in order to be recoverable.  A database
must implement several methods in order to comply with the procol.  In
addition, it must interact with the logger according to the protocol
described so far (i.e. use the transaction methods appropriately).")

          (:sections
           (defdoc logging-implementation-protocol-methods :section
             (:title "Methods that a database must implement")
             (:content "The following methods are those that any database
that wished to comply with the logging protocol must implement.")
             (:sections

              (defdoc cl-transaction-log:db-undo-modification :generic)
              (defdoc cl-transaction-log:db-redo-modification :generic)
           
              (defdoc cl-transaction-log:db-element-as-octets :generic)
              (defdoc cl-transaction-log:db-element-from-octets :generic)
           
              (defdoc cl-transaction-log:db-value-as-octets :generic)
              (defdoc cl-transaction-log:db-value-from-octets :generic)

              ))
           (defdoc logging-implementation-protocol-mle :section
             (:title "Log entries")
             (:content "The non-drowsy reader will notice that the
db-* functions take a modification-log-entry argument.  This is a
instance of a log entry class that has the following important
functions:")
             (:sections
              (defdoc cl-transaction-log:log-entry-database-element :generic)
              (defdoc cl-transaction-log:log-entry-old-value :generic)
              (defdoc cl-transaction-log:log-entry-new-value :generic)))))
           
))

           
        
     ))

  (output-docs))

   
