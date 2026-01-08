**********************
Command Line Interface
**********************

This document is a prettier output of the documentation produced by
the command ``man`` of the different Octez binaries. You can
obtain similar pages using shell commands such as:

::

   octez-admin-client man -verbosity 3

.. note::

	The rest of this page documents the protocol-independent tools, whose features do not vary with each protocol.
	The protocol-dependent tools are documented :doc:`here <../active/cli-commands>`.


.. _admin_client_manual:

Admin-client manual
===================

This is the manual page of the command line tool ``octez-admin-client``. See :ref:`octez-admin-client` for a general description of this tool.

.. raw:: html
         :file: ../api/octez-admin-client.html


.. _signer_manual:

Signer manual
=============

This is the manual page of the command line tool ``octez-signer``. See :ref:`signer` for a general description of this tool.

.. raw:: html
         :file: ../api/octez-signer.html


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

The command line of the :doc:`Octez DAL node <../shell/dal_node>` is documented in Unix ``man`` format, as shown below. You can also obtain this manual by running ``octez-dal-node --help``.

The manual briefly shows the available DAL node commands.
Each command accepts its own set of options and arguments, that you can discover by running ``octez-dal-node <command> --help``.

.. include:: ../api/octez-dal-node.txt

.. _smart_rollup_node_manual:

Smart rollup node manual
========================

This is the manual page of the :doc:`../shell/smart_rollup_node`.

.. raw:: html
         :file: ../api/octez-smart-rollup-node.html

.. _baker_manual:

Baker manual
============

This is the manual page of the baker command line tool. See :ref:`baker_run` for a general description of this tool.

.. note::
	This unique baker executable is meant to replace the protocol-specific bakers for all the protocols it supports, such as ``octez-baker-PsRiotum`` and ``octez-baker-alpha``.

.. raw:: html
         :file: ../api/octez-baker.html

.. _accuser_manual:

Accuser manual
==============

This is the manual page of the accuser command line tool. See :ref:`accuser_run` for a general description of this tool.

.. note::
	This unique accuser executable is meant to replace the protocol-specific accusers for all the protocols it supports, such as ``octez-accuser-PsRiotum`` and ``octez-accuser-alpha``.

.. raw:: html
         :file: ../api/octez-accuser.html

.. _benchmark_tool_manual:

Benchmark tool manual
=====================

This is the manual page of the command line tool ``octez-snoop``. See :doc:`../developer/snoop` for a general description of this tool.

.. raw:: html
         :file: ../api/octez-snoop.html


.. _codec_manual:

Codec manual
============

This is the manual page of the command line tool ``octez-codec``. See :ref:`octez-codec` for a general description of this tool.

.. raw:: html
         :file: ../api/octez-codec.html


Protocol-dependent tools
========================

For manuals of the tools that may vary with each protocol, see :doc:`here <../active/cli-commands>`.
