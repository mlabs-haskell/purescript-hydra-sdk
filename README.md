# purescript-hydra-sdk

[Cardano Hydra](https://hydra.family/head-protocol/)
SDK (Software Development Kit) for PureScript. This library offers
various interfaces to facilitate rapid development of Hydra-based
applications.

**Table of Contents**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Applications](#applications)
- [Functionality](#functionality)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

### Applications

Please refer to [hydra-auction-offchain](https://github.com/mlabs-haskell/hydra-auction-offchain)
for a full-fledged example that utilizes this SDK.

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
