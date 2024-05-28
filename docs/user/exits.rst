.. _tezos_binaries_signals_and_exit_codes:

Exit codes and signals
======================

This page documents the exit codes possibly returned by the Octez binaries, as well as the signals they handle.

Exit codes
----------

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

Signals
-------

Upon receiving ``SIGINT`` (e.g., via Ctrl+C in an interactive session) or
``SIGTERM`` (e.g., via ``systemctl stop``) the process will exit (with code 64 or
255, see details below). Note that sending the same signal a second time (after
a one (1) second grace period) will terminate the process immediately,
interrupting the normal clean-up functions of clean-up (in this case the exit
code will be 255).
