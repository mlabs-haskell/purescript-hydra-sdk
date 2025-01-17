# purescript-hydra-sdk

[Cardano Hydra](https://hydra.family/head-protocol/)
SDK (Software Development Kit) for PureScript. This library offers
various interfaces to facilitate the rapid development
of Hydra-based applications.

**Table of Contents**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Compatibility](#compatibility)
- [Preliminaries](#preliminaries)
- [Development Workflows](#development-workflows)
- [Getting Started with Example](#getting-started-with-example)
- [SDK Core Functionality](#sdk-core-functionality)
- [SDK Additionals: AppManager](#sdk-additionals-appmanager)
- [Applications](#applications)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Compatibility

| hydra-sdk   | hydra-node   | cardano-node |
| ----------- | ------------ | ------------ |
| **`0.1.0`** | **`0.19.0`** | **`10.1.2`** |


## Preliminaries

Since using this SDK implies a certain degree of understanding of the Hydra
protocol, it is advisable to get familiar with the ["Hydra: Fast Isomorphic State Channels" paper](https://iohk.io/en/research/library/papers/hydra-fast-isomorphic-state-channels/)
and the [Hydra Head protocol documentation](https://hydra.family/head-protocol/docs/)
before proceeding with the **Getting Started** guide below.

## Development Workflows

Before executing most of the commands listed below, first enter the Nix
development shell by running `nix develop`.

* **Build docs and open them in the browser**: `make docs`
* **Build the project**: `make build`
* **Format code**: `make format`

## Getting Started with Example

The simplest way to get a sense of how this SDK can help build Hydra-based
applications is to run our minimal example and then adapt or extend it to suit
your specific requirements.

Go to `example/minimal` and follow the step-by-step guide below to spin up
a cluster of two nodes, with each node running the minimal example logic.

1. Enter the Nix development environment by running `nix develop` from the root
directory of this repository. This will put you in the shell with all the
necessary executables required to continue with the setup procedure.

2. In [example/minimal/docker/cluster/](example/minimal/docker/cluster/) you
can find configuration files for both nodes. The only field that needs to be
updated here is the `blockfrostApiKey`, which should be set to a valid
Blockfrost API key **for preprod**.
Visit the [Blockfrost website](https://blockfrost.io/) to generate a fresh API key.

3. Execute `make gen-keys` to generate the necessary Cardano and Hydra keys
required by the underlying Hydra nodes. Cardano keys are used to authenticate
on-chain transactions, while the Hydra keys are used for multi-signing snapshots
within a Hydra Head. This command will output two Cardano preprod addresses
that must be pre-funded with sufficient tADA to run properly functioning Hydra
nodes.
Use the [Testnets faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)
to request tADA.

4. Finally, execute `make run-example` to launch a Hydra Head
with two participants both running the minimal example logic.

NOTE: Hydra nodes require a fully synchronized Cardano node to operate
correctly. No additional setup actions need to be performed to
configure the Cardano node as it is already included in the Docker
Compose configuration.
However, node synchronization may take several hours on the first run.
Keep in mind that Hydra nodes are configured to run only once
the Cardano node is fully synchronized,
and the `cardano-node` output is suppressed to not
interfere with useful Hydra Head logs.
As a result, there will be no output during synchronization.
Instead, use `docker logs` or run `cardano-cli query tip`
from within the `cardano-node` Docker container
to track the synchronization progress.

## SDK Core Functionality

The SDK exposes the following top-level modules which constitus it API which
is considered to be relatvely stable. You also can use `Internal` modules
at yor own risk.

* `HydraSdk.Process` module provides an interface for spinning up a hydra-node
as a Node.js subprocess.

* `HydraSdk.NodeApi` module exports functions for connecting to the Hydra Node
WebSocket API and sending HTTP requests. The primary function provided by this
module is `mkHydraNodeApiWebSocket`, which establishes a WebSocket connection to
the hydra-node, attaches specified handlers for incoming messages, and returns a
`HydraNodeApiWebSocket` record with type-safe actions for interacting with the
Hydra Node API. It also allows to specify retry strategies for Hydra
transactions that may be silently dropped by cardano-node, particularly for
Close and Contest transactions.

* `HydraSdk.Types` module re-exports various Hydra domain-specific types
(such as `HydraHeadStatus` and `HydraNodeApi_InMessage`), along with other
utility types (e.g., `HostPort` and `Network`) used by the components of this
library.

* Lastly, `HydraSdk.Lib` module contains useful helpers like codecs for many types,
logging action and some others.

## SDK Additionals: AppManager

Under `Extra` folder, modules with additional functionality are located,
currently being the only one for managing multiple app instances.

Originally the SDK was designed for Hydra applications, where Hydra Heads were
operated by designated delegates. In that model a delegate can be anyone who wants
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

## Applications

Please refer to [hydra-auction-offchain](https://github.com/mlabs-haskell/hydra-auction-offchain)
for a full-fledged example that utilizes this SDK.