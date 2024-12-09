# AppManager

The SDK is designed for Hydra applications, where Hydra Heads are
operated by designated delegates. A delegate can be anyone who wants
to participate as a provider of computational resources to host Hydra
nodes. Delegates must form a group upfront to maintain Hydra
consensus. Upon forming a group, all members need to specify
information about their peers - such as Hydra node addresses, public
keys, etc. That way there exists a strict correspondence between
delegate configurations to start a functioning Hydra Head.

Each application instance should monitor its underlying Hydra node and
have access to information about other Hydra Head participants.
AppManager is an opinionated interface for managing multiple app
instances within a delegate group. This interface is utilized by 
applications like `hydra-auction-offchain`, enabling delegates to host
multiple Layer-2 auctions simultaneously.

At the core of AppManager is the concept of slots. When delegates
decide to form a group, they agree on the configurations for their
future Hydra nodes. Since each delegate have to specify information
about their peers (hydra-node address, public keys, etc.), there must
be strict correspondence between delegate configurations to start
a working Hydra Head. This is where slots come into play. Each slot
represents a set of delegate configurations sufficient to spin up a
properly configured Hydra Head. In hydra-auction-offchain, slot
numbers are implicitly derived from the configurations provided to
the delegate-server, with the first configuration corresponding to
slot 0, the second to slot 1, and so forth. Users are expected to
reserve slots before making an initial Layer-1 commitment, such as
announcing an auction. Upon reservation, they receive secrets from
each delegate, which can later be provided to host a Layer-2
application in the reserved slot.

One clear drawback of this approach is the potential for malicious
actors to reserve all available slots within a delegate group,
effectively paralyzing its operations. In real-world applications, a
flooding detection mechanism should be implemented to prevent this
scenario, although there seems to be no obvious incentive for anyone
to carry out such an attack.

Coming with the limitations mentioned, this approach simplifies things
since it neither requires communication and synchronization between
delegates during runtime nor does it rely on a central server to
orchestrate the initialization of Hydra Heads, making it a great fit
for hydra-auction-offchain and hopefully other Hydra applications.
