# purescript-hydra-sdk

[Cardano Hydra](https://hydra.family/head-protocol/)
SDK (Software Development Kit) for PureScript. This library offers
various interfaces to facilitate rapid development of Hydra-based
applications.

**Table of Contents**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Applications](#applications)
- [Preliminaries](#preliminaries)
- [Getting Started](#getting-started)
- [Functionality](#functionality)
- [Development Workflows](#development-workflows)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

### Applications

Please refer to [hydra-auction-offchain](https://github.com/mlabs-haskell/hydra-auction-offchain)
for a full-fledged example that utilizes this SDK.

### Preliminaries

Since using this SDK implies a certain degree of understanding of the Hydra
protocol, it is advisable to get familiar with the ["Hydra: Fast Isomorphic State Channels" paper](https://iohk.io/en/research/library/papers/hydra-fast-isomorphic-state-channels/)
and the [Hydra Head protocol documentation](https://hydra.family/head-protocol/docs/)
before proceeding with the **Getting Started** guide below.

### Getting Started

The simplest way to get a sense of how this SDK can help build Hydra-based
applications is to run our minimal example and then adapt or extend it to suit
your specific requirements. Follow the step-by-step guide below to spin up
a cluster of two nodes, with each node running the minimal example logic.

1. Enter the Nix development environment by running `nix develop` from the root
directory of this repository. This will put you in the shell with all the
necessary executables required to continue with the setup procedure.

2. In [example/minimal/docker/cluster/](example/minimal/docker/cluster/) you
can find configuration files for both nodes. The only field that needs to be
updated here is the `blockfrostApiKey`, which should be set to a valid
Blockfrost API key **for preprod**. Visit the [Blockfrost website](https://blockfrost.io/)
to generate a fresh API key.

3. Execute `make gen-keys` to generate the necessary Cardano and Hydra keys
required by the underlying Hydra nodes. Cardano keys are used to authenticate
on-chain transactions, while the Hydra keys are used for multi-signing snapshots
within a Hydra Head. This command will output two Cardano preprod addresses
that must be pre-funded with sufficient tADA to run properly functioning Hydra
nodes. Use the [Testnets faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)
to request tADA.

4. Finally, execute `make run-example` to launch a Hydra Head with two
participants running the minimal example logic.

NOTE: Hydra nodes require a fully-synchronized Cardano node to operate
correctly. No additional setup actions need to be performed to
configure the Cardano node as it is already included in the Docker
Compose configuration. However it may take several hours on the first
run to synchronize the node. Keep in mind that Hydra nodes
are configured to run only once the Cardano node is fully
synchronized, and the `cardano-node` output is suppressed to not
interfere with useful Hydra Head logs. As a result, there will be no
output during synchronization. Instead, use `docker logs` or run
`cardano-cli query tip` from within the `cardano-node` Docker
container to track the synchronization progress. 

### Functionality

The `HydraSdk.Process` module provides an interface for spinning up a hydra-node
as a Node.js subprocess.

The `HydraSdk.NodeApi` module exports functions for connecting to the Hydra Node
WebSocket API and sending HTTP requests. The primary function provided by this
module is `mkHydraNodeApiWebSocket`, which establishes a WebSocket connection to
the hydra-node, attaches specified handlers for incoming messages, and returns a
`HydraNodeApiWebSocket` record with type-safe actions for interacting with the
Hydra Node API. It also allows to specify retry strategies for Hydra
transactions that may be silently dropped by cardano-node, particularly for
Close and Contest transactions.

The `HydraSdk.Types` module re-exports various Hydra domain-specific types
(such as `HydraHeadStatus` and `HydraNodeApi_InMessage`), along with other
utility types (e.g., `HostPort` and `Network`) used by the components of this
library.

`HydraSdk.Extra.AppManager` provides an opinionated interface for managing
multiple Hydra application instances, with each instance running a separate
hydra-node process, as implemented in [hydra-auction-offchain](https://github.com/mlabs-haskell/hydra-auction-offchain).
For more information, refer to the [AppManager README](src/Extra/README.md).

### Development Workflows

Before executing most of the commands listed below, first enter the Nix
development shell by running `nix develop`.

**Build the project** (requires Nix shell): `make build`  
**Format code** (requires Nix shell): `make format`  
**Build docs and open them in the browser**: `make docs`
