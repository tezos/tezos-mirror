**********************
Command Line Interface
**********************

This document is a prettier output of the documentation produced by
the command ``man`` of the different Octez binaries. You can
obtain similar pages using shell commands such as:

::

   octez-admin-client man -verbosity 3

The rest of this page documents the protocol-independent tools.
The protocol-dependent tools are documented :doc:`here <../active/cli-commands>`.


.. _admin_client_manual:

Admin-client manual
===================

.. raw:: html
         :file: ../api/octez-admin-client.html


.. _signer_manual:

Signer manual
=============

.. raw:: html
         :file: ../api/octez-signer.html


.. _benchmark_tool_manual:

Benchmark tool manual
=====================

.. raw:: html
         :file: ../api/octez-snoop.html


.. _codec_manual:

Codec manual
============

.. raw:: html
         :file: ../api/octez-codec.html


.. _node_manual:

Node manual
===========

The command line of the Octez node is documented in Unix ``man`` format, as shown below. You can also obtain this manual by running ``octez-node --help``.

The manual briefly shows the available node commands.
Each command accepts its own set of options and arguments, that you can discover by running ``octez-node <command> --help``.
For more details on the node invocation and configuration, see :doc:`../user/node-configuration`.

.. include:: ../api/octez-node.txt

.. _dal_node_manual:

DAL node manual
===============

The command line of the Octez DAL node is documented in Unix ``man`` format, as shown below. You can also obtain this manual by running ``octez-dal-node --help``.

The manual briefly shows the available DAL node commands.
Each command accepts its own set of options and arguments, that you can discover by running ``octez-dal-node <command> --help``.

.. include:: ../api/octez-dal-node.txt

.. _smart_rollup_node_manual:

Smart rollup node manual
========================

.. raw:: html
         :file: ../api/octez-smart-rollup-node.html
