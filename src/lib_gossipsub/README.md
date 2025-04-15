# Gossipsub Library

This library provides an implementation of the [libp2p's Gossipsub
protocol](https://docs.libp2p.io/concepts/pubsub/overview/), a pub-sub protocol
designed for efficient message dissemination in decentralized networks. This
library implements the [version v1.1 of the
spec](https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.1.md).

The library is basically standalone: it does not depend on other components. It
is designed to be integrated into a custom P2P stack. It does not handle
networking itself. Instead, it leaves all message transmission and reception, and
peer and topic management to the caller, allowing for full flexibility in
choosing the transport layer.

Its main purpose is to provide the P2P communication protocol for Tezos' Data
Availability Layer (DAL).

## Implementation Details

The implementation is centered around several key interfaces:

### `AUTOMATON_SUBCONFIG` and `AUTOMATON_CONFIG` interfaces

Configuration modules providing type definitions for: peers, topics, time
(stamps) and time spans, message ids, and (full) messages.

### `AUTOMATON` interface

The core and pure state machine defining the Gossipsub logic.
* It is fully functional and deterministic (with explicit `Random.State` usage).
* It handles P2P events: receipt of control messages (`IHAVE`, `IWANT`, `GRAFT`,
  `PRUNE`, `PING`) and of (full/application) messages; connection and
  disconnection events.
* It handles application events (topic join/leave, publish message), and time
  tick events (the so-called "heartbeat" logic).
* It responds to each event by updating the state and emitting an output.
* It manages a message cache.
* It handles peer scoring via a pluggable `Score` module.

### `WORKER` interface

The `WORKER` interface is a higher-level layer built on top of the `AUTOMATON`
interface, designed to integrate the Gossipsub protocol into a live system.  It
receives inputs from both the P2P and application layers, processes them using
the automaton, and emits corresponding actions back to the P2P layer (messages,
connections, disconnections) and the application layer (full messages).

The WORKER module is designed to be agnostic to the underlying concurrency model.
Rather than depending on a specific concurrency library (such as Lwt), it requires
the host application to supply an I/O monad and stream implementation.
This design allows the application to retain control over the event loop and
execution model, enabling the worker to integrate cleanly into diverse runtime
environments.

The pluggable `Stream` module provides FIFO streams for message passing between
layers.  These streams are used to: receive inputs from the P2P layer
(`p2p_input`) and from the application layer (`app_input`), and emit messages to
the P2P output stream (`p2p_output`) and the application output stream
(`app_output`).

### Peer scoring

The implementation supports detailed peer scoring (spec §6.1) through the `SCORE` interface, tracking:
* time in mesh (P1)
* first message deliveries (P2)
* mesh message deliveries (P3)
* mesh failure penalties (P3b)
* invalid message penalties (P4)
* application-specific scores (P5)
* behaviour penalty (P7)

The scoring mechanism for IP colocation penalty (P6) is not implemented.

Scores decay over time and are refreshed periodically during heartbeats.

### Observability

The `Introspection` sub-modules allow to access part of the internal states for
observability purposes, for instance for providing Prometheus metrics.
A full view of the Gossipsub state is available through `Introspection.view`:
* peers and their connection metadata
* mesh membership per topic
* fanout and seen messages
* current scores and backoff timers
* access counters and message cache contents

### File Structure:

- `gossipsub_automaton.ml`: Encodes the protocol logic of Gossipsub as a
  deterministic state machine, mapping events to actions in a purely functional
  style.
- `state_monad.ml`: Implements a pure state monad used to compose and sequence
  updates to the Gossipsub automaton's internal state.
- `message_cache.ml`: Tracks recently seen messages using a sliding time window,
  to support gossiping and prevent reprocessing of previously handled messages.
- `peers_score.ml`: Implements peer scoring based on Gossipsub's reputation
  system to assess and track peer behavior.
- `gossipsub_worker.ml`: Coordinates the operational logic of Gossipsub and acts
  as the integration layer between the application and the protocol state
  machine.

### Usage

To use the library one needs to create a `Worker` module whose signature is
`Gossisub_intf.WORKER` (see above for details).  One must therefore provide
modules for:
* peer IDs,
* topic types,
* time and time spans,
* messages and their IDs (including validation),
* I/O monad and stream primitives.
