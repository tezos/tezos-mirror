**********************
Command Line Interface
**********************

This document is a prettier output of the documentation produced by
the command ``man`` of the different Octez binaries. You can obtain similar pages
using shell commands such as (:ref:`indicating the appropriate protocol <octez_client_protocol>`):

::

   octez-client -protocol <protocol_hash> man -verbosity 3

.. note::

	The rest of this page documents the protocol-dependent tools, that is, those whose behavior or features may be specific to each protocol.
	The protocol-independent tools are documented :doc:`here <../shell/cli-commands>`.


.. _client_manual_alpha:

Client manual
=============

This is the manual page of the command line tool ``octez-client``. See :ref:`howtouse_tezos_client` for a general description of this tool.

.. raw:: html
         :file: octez-client.html


.. _baker_manual_alpha:

Baker manual
============

This is the manual page of the baker command line tool. See :ref:`baker_run` for a general description of this tool.

.. raw:: html
         :file: octez-baker.html


.. _accuser_manual_alpha:

Accuser manual
==============

This is the manual page of the accuser command line tool. See :ref:`accuser_run` for a general description of this tool.

.. raw:: html
         :file: octez-accuser.html

Protocol-independent tools
==========================

For manuals of the tools that do not vary with each protocol, see :doc:`here <../shell/cli-commands>`.