**********************
Command Line Interface
**********************

This document is a prettier output of the documentation produced by
the command ``man`` of the different Octez binaries. You can obtain similar pages
using shell commands such as (:ref:`indicating the appropriate protocol <octez_client_protocol>`):

::

   octez-client -protocol <protocol_hash> man -verbosity 3

The rest of this page documents the protocol-dependent tools.
The protocol-independent tools are documented :doc:`here <../shell/cli-commands>`.


.. _client_manual:
.. _client_manual_rio:

Client manual
=============

.. raw:: html
         :file: octez-client.html


.. _baker_manual_rio:

Baker manual
============

This is the manual page of the baker command line tool. See :ref:`baker_run` for a general description of this tool.

.. note::
	The protocol-independent baker executable now available is meant to replace all the protocol-specific bakers like this one. Please refer to :ref:`its manual <baker_manual>`.

.. raw:: html
         :file: octez-baker.html


.. _accuser_manual_rio:

Accuser manual
==============

.. note::
	The protocol-independent accuser executable now available is meant to replace all the protocol-specific accusers like this one. Please refer to :ref:`its manual <accuser_manual>`.

.. raw:: html
         :file: octez-accuser.html
