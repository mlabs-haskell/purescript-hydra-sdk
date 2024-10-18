# AppManager

AppManager is an opinionated interface for managing multiple app
instances, with each instance running its own Hydra node. This
interface is utilized in hydra-auction-offchain, enabling delegates to
host multiple Layer-2 auctions at the same time.

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
scenario, although there seem to be no obvious incentive for anyone to
carry out such an attack.

Despite its limitations, this approach does not require communication
or synchronization between delegates, nor does it rely on a central
server to orchestrate the initialization of Hydra Heads, making it a
great fit for hydra-auction-offchain.
