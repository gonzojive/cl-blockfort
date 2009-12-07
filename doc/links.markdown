# List of papers

"Consensus on Transaction Commit":http://research.microsoft.com/apps/pubs/default.aspx?id=64636

The distributed transaction commit problem requires reaching agreement
on whether a transaction is committed or aborted. The classic
Two-Phase Commit protocol blocks if the coordinator
fails. Fault-tolerant consensus algorithms also reach agreement, but
do not block whenever any majority of the processes are
working. Running a Paxos consensus algorithm on the commit/abort
decision of each participant yields a transaction commit protocol that
uses 2F +1 coordinators and makes progress if at least F +1 of them
are working. In the fault-free case, this algorithm requires one extra
message delay but has the same stable-storage write delay as Two-
Phase Commit. The classic Two-Phase Commit algorithm is obtained as
the special F = 0 case of the general Paxos Commit algorithm.


"Sinfonia: A New Paradigm for BUilding Scalable Distributed Systems":http://www.google.com/url?sa=t&source=web&ct=res&cd=11&ved=0CDAQFjAK&url=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fpeople%2Faguilera%2Fsinfonia-tocs-to-appear2009.pdf&ei=t8YcS8jwBIqisgPl6uWRBw&usg=AFQjCNGX1WAWueqUL1fTs24YAaoA3rLZFA

We propose a new paradigm for building scalable distributed
systems. Our approach does not require dealing with message-passing
protocols—a major complication in existing distributed sys-
tems. Instead, developers just design and manipulate data structures
within our service called Sinfonia. Sinfonia keeps data for
applications on a set of memory nodes, each exporting a linear address
space. At the core of Sinfonia is a new minitransaction primitive that
enables efficient and consistent access to data, while hiding the
complexities that arise from concurrency and failures.  Using
Sinfonia, we implemented two very different and complex applications
in a few months: a cluster file system and a group communication
service. Our implementations perform well and scale to hundreds of
machines.
