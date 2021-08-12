Various
=======

Environment for writing Michelson contracts
-------------------------------------------

Here is how to setup a practical environment for
writing, editing and debugging Michelson programs.

Install `Emacs <https://www.gnu.org/software/emacs/>`_ and configure
it to use the `MELPA <https://melpa.org/#/getting-started>`_ package
repository.

Inside Emacs, install the ``michelson-mode`` package and its
dependency `deferred <https://github.com/kiwanami/emacs-deferred>`_ by
running ``M-x package-install-file``; the package file is located in
the ``emacs`` folder of the Tezos code base.

Set up the `Michelson mode
<https://gitlab.com/tezos/tezos/tree/master/emacs>`_ to use the Tezos
client in :doc:`mockup mode <mockup>` (to typecheck Michelson
scripts without interacting with a Tezos node) by adding in your
``.emacs`` file:

::

   (setq michelson-client-command "tezos-client --base-dir /tmp/mockup --mode mockup --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK")
   (setq michelson-alphanet nil)

Note that the Michelson mode will be chosen automatically by Emacs for
files with a ``.tz`` or ``.tez`` extension.

We can now open our favourite contract
:src:`./tests_python/contracts_alpha/attic/id.tz` in Emacs
and, when moving the cursor on
a Michelson instruction, in the bottom of the windows Emacs should
display the state of the stack before (left) and after (right) the
application of the instruction.
The Emacs mode automatically type-checks your program and reports
errors; once you are happy with the result you can ask the client to
run it locally:

::

   tezos-client run script ./tests_python/contracts_alpha/attic/id.tz \
                on storage '"hello"' and input '"world"'

.. _tezos-admin-client:

Admin Client
------------

The admin client enables you to interact with the peer-to-peer layer in order
to:

- check the status of the connections
- force connections to known peers
- ban/unban peers

A useful command to debug a node that is not syncing is:

::

   tezos-admin-client p2p stat

.. _tezos_binaries_signals_and_exit_codes:

Tezos binaries: signals and exit codes
--------------------------------------

Signals:
Upon receiving ``SIGINT`` (e.g., via Ctrl+C in an interactive session) or
``SIGTERM`` (e.g., via ``systemctl stop``) the process will exit (with code 64 or
255, see details below). Note that sending the same signal a second time (after
a one (1) second grace period) will terminate the process immediately,
interrupting the normal clean-up functions of clean-up (in this case the exit
code will be 255).

Exit codes:
The meaning of exit codes is presented in the following table. The action column
indicates a recommended course of action.

+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| exit code   |  meaning                                                                         | action                                                           |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 0           | the process exited successfully                                                  | nothing                                                          |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 1–125       | something went unexpectedly                                                      | check output/log to see if you forgot an argument or some such   |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 126         | an exception was not handled                                                     | report a bug                                                     |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 127         | the process received a signal (e.g., via Ctrl-C)                                 | nothing                                                          |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 128         | the process was about to exit successfully but an error occurred during exit     | check output/logs, clean-up leftover files, open a bug report    |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 129–253     | like 1–125 and an error occurred during exit                                     | check output/logs, clean-up leftover files, open a bug report    |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 254         | like 126 and an error and an error occurred during exit                          | check output/logs, clean-up leftover files, open a bug report    |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+
| 255         | like 127 but an error and an error occurred during exit (e.g., ``kill -9``)      | check output/logs, clean-up leftover files                       |
+-------------+----------------------------------------------------------------------------------+------------------------------------------------------------------+

Failing_noop operation
----------------------

Starting with protocol 009 a `Failing_noop` operation is added. This operation
is not executable in the protocol and will always fail when injected. It allows
to sign an arbitrary string that cannot be misinterpreted in the protocol.

The client has commands to sign a message with a given key or to check that
message has been signed by a given key. These commands create a `failing_noop`
operation from the message that is being signed or checked.

::

   tezos-client sign message "hello world" for <account>

   tezos-client check that message "hello world" was signed by <account> to
   produce <signature>
