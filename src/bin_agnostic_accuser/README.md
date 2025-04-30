# Agnostic Accuser

## Overview

Agnostic Accuser is a protocol-independent binary that dynamically selects the
appropriate denunciation process based on the active protocol. It monitors the state of
the blockchain and automatically switches to the correct process when a new
protocol is encountered (for example, during migrations, or at startup).

It is designed in a similar fashion to the **Agnostic Baker**, meaning that users
will no longer need to use two accuser binaries for protocol migration, as this will
be performed automatically by the agnostic accuser.

## Usage

In the same fashion as for the agnostic baker, to run the accuser we just replace
the old `octez-accuser-<protocol>` binary with `octez-accuser` and keep the
same arguments:

```bash
octez-accuser [OCTEZ-ACCUSER-COMMANDS]
```

Furthermore, for the `run` command of the accuser, the agnostic baker is equipped
with a command to replicate the behaviour of the accuser binary:

```bash
octez-baker run accuser [OCTEZ-ACCUSER-COMMANDS]
```

## How it works

The mechanism behind the agnostic accuser is the same as the one for the agnostic
baker, so you can refer to that section for more in-depth information.
