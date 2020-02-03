# Tezos

## Introduction

Tezos is a distributed consensus platform with meta-consensus
capability. Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also comes to consensus about how the
protocol and the nodes should adapt and upgrade. For more information about
the project, see https://tezos.com.

## Getting started

Instructions to
[install](https://tezos.gitlab.io/introduction/howtoget.html), [start
using](https://tezos.gitlab.io/introduction/howtouse.html), and
[taking part in the
consensus](https://tezos.gitlab.io/introduction/howtorun.html) are
available at https://tezos.gitlab.io/.

## The Tezos software

This git repository contains the source code, the tests, and the
developer documentation of the Tezos software running on the nodes of
the main Tezos network and on [various Tezos test
networks](https://tezos.gitlab.io/introduction/test_networks.html).

The documentation for developers of the Tezos software is available
online at https://tezos.gitlab.io/. This documentation is always in
sync with the master branch which may however be slightly
desynchronized with the code running on the live networks.

The source code of Tezos is placed under the [MIT Open Source
License](https://opensource.org/licenses/MIT).

## Contributing

### Development of the Tezos protocol

The core of the Tezos software that implements the economic ruleset is
called the *protocol*. The protocol can be updated through [Tezos
on-chain voting
procedure](https://tezos.gitlab.io/whitedoc/voting.html). Protocol
contributors are encouraged to synchronize their contributions to
minimize the number of protocol proposals that the stakeholders have
to study and to maximize the throughput of the voting procedure.

### Development of the Tezos shell

Except for the protocol, all development of the Tezos code happens on
GitLab at https://gitlab.com/tezos/tezos. Merge requests
(https://gitlab.com/tezos/tezos/-/merge_requests) should usually
target the `master` branch; see [the contribution
instructions](https://tezos.gitlab.io/developer/contributing.html).

The issue tracker at https://gitlab.com/tezos/tezos/issues can be used
to report bugs and to request new simple features. The [Tezos Agora
forum](https://forum.tezosagora.org/) is another great place to
discuss the future of Tezos with the community at large.

## Community

Links to community websites are gathered in the following community portals:
- https://www.tezos.help/
- https://developers.tezos.com/ (for developers of applications built on Tezos)
