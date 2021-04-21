.. _cli_commands_shell:

**********************
Command Line Interface
**********************

This document is a prettier output of the documentation produced by
the command line ``man`` of the different Tezos binaries. You can
obtain similar pages using shell commands such as:

::

   tezos-client -protocol ProtoALphaALph man -verbosity 3
   tezos-admin-client man -verbosity 3

The rest of this page documents the protocol-independent tools.
The protocol-dependent tools are documented :ref:`here <cli_commands>`.


.. _admin_client_manual:

Admin-client manual
===================

.. raw:: html
         :file: ../api/tezos-admin-client.html


.. _signer_manual:

Signer manual
=============

.. raw:: html
         :file: ../api/tezos-signer.html


.. _benchmark_tool_manual:

Benchmark tool manual
=====================

.. raw:: html
         :file: ../api/tezos-snoop.html


.. _codec_manual:

Codec manual
============

.. raw:: html
         :file: ../api/tezos-codec.html


.. _node_manual:

Node manual
===========

The comand line of the Tezos node is not currently documented as a web page, but
you can obtain it in Unix manual format by running the node with no arguments::

  tezos-node
