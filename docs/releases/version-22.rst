Version 22.1
============

Changes
-------

Summary
~~~~~~~

Version 22 introduces the following changes or new features:
  (1) Support of the **Rio Protocol proposal** (See :ref:`the protocol support section <protocol_support_v22>`)
  (2) Source code is compiled with the **OCaml 5 compiler** (See :ref:`the compiler version section <compiler_version_v22>`)
  (3) The baker requires an endpoint of a running DAL node or the ``--without-dal`` option to start. (See :ref:`the DAL node section <dal_node_v22>`)
  (4) The default :doc:`history mode <../user/history_modes>` is now ``Rolling`` instead of ``Full``.
  (5) **New experimental agnostic baker executable**. (See :ref:`the experimental agnostic baker section <agnostic_baker_v22>`)
  (6) Release of the new Debian and RPM packages. (See :ref:`the packages section <packages_v22>`)

Octez v22.1
"""""""""""

Octez v22.1 is a minor releases aiming to fix some issues reporting when compiling from sources.
In particular, it allows to install (or update) the ``ocamlfind`` dependency correctly to its latest version.
It also fixes an issue with Homebrew formulas, which was affecting deployment on Mac OS X systems.

In addition, v22.1 improves the snapshot import UX and fixes an issue in the rollup affecting the execution of outbox messages during chain reorganisations.

.. _protocol_support_v22:

Protocol Support
~~~~~~~~~~~~~~~~

Version 22 contains a new version (V14) of the protocol environment.
As a result, Octez version 22 is the first version compatible with all protocols that require this environment, such as `the Rio protocol proposal <https://research-development.nomadic-labs.com/rio-announcement.html>`__.

.. _compiler_version_v22:

Compiler Version
~~~~~~~~~~~~~~~~

Starting from Octez version 22, the source code can be compiled with version 5 of the OCaml compiler. The distribution of Octez, starting from this version, will be compiled with OCaml version ``5.2.1`` instead of ``4.14.2``.

.. _dal_node_v22:

DAL operations and Bakers
~~~~~~~~~~~~~~~~~~~~~~~~~

The baker option ``--without-dal``, introduced in :doc:`Octez-v21.3 <version-21>`, is now **mandatory** if not providing an endpoint of a DAL node.

See :doc:`the DAL node documentation <../shell/dal_node>` for futher details.

.. _agnostic_baker_v22:

Experimental Agnostic Baker
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Octez v22 introduces a **protocol-agnostic baker as experimental**, named ``octez-experimental-agnostic-baker``.

The Agnostic Baker is a protocol-independent binary that dynamically determines and executes the appropriate baking binary based on the active protocol.

It continuously monitors the blockchain state and automatically transitions to the correct binary whenever
a new protocol is detected by the node, such as during migrations or at startup.

The v22 agnostic baker is **EXPERIMENTAL ONLY** and should **NOT** be used on ``mainnet``.
You can use the agnostic baker on test networks and give feedback.

You can find more information on the agnostic baker in the corresponding :src:`README <src/bin_agnostic_baker/README.md>` file.

Minimal hardware specifications
-------------------------------

Our benchmarks suggest the following *minimal* specs for Octez node and baker operators:

- 3 CPU cores: 2 needed by the node and 1 needed by the baker (arm64 or amd64/x86-64)
- 8GB of RAM + 8GB of swap (or 16GB of RAM)
- 100GB SSD storage (or similar I/O performance)
- A low-latency reliable internet connection

This configuration has been tested for running an Octez node in :doc:`rolling history mode <../user/history_modes>`.
Other more specific uses may need adequate configurations.
For instance:

- running a node in full or archive mode requires extra storage space;
- bakers participating to the DAL should consult :ref:`The DAL node requirements <dal_node_specs>`.

Update Instructions
-------------------

To update from sources:

.. code-block:: shell

  git fetch
  git checkout octez-v22.1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v22.1`` Docker images of Octez.


.. _packages_v22:

Packages
~~~~~~~~

In Octez v22, the new set of packages, that was introduced in the previous version of Octez, **replaces** the old one in the APT repository. Check :ref:`the documentation <installing_packages>` for more details.

Therefore, upgrading to the new packages is done as usual with ``apt``.
Note however that the Zcash parameters are in a different package now, which needs to overwrite files from the old package::

    $ sudo apt update
    $ sudo apt upgrade octez-baker -o DPkg::options::="--force-overwrite"

Furthermore, RPM packages are now available in a **dnf repository**. Installation instructions are detailed in :ref:`the documentation <installing_packages>`.

When upgrading to v22, you can start or restart the ``octez-baker.service`` that runs all bakers for supported protocols.

Alternatively, you can run them individually. For instance, to start only the baker for the Quebec protocol, you can use the command::

  $ sudo systemctl start octez-baker@PsQuebec

Changelog
---------

- `Version 22.1 <../CHANGES.html#version-22-1>`_
- `Version 22.0 <../CHANGES.html#version-22-0>`_
- `Version 22.0~rc3 <../CHANGES.html#version-22-0-rc3>`_
- `Version 22.0~rc2 <../CHANGES.html#version-22-0-rc2>`_
- `Version 22.0~rc1 <../CHANGES.html#version-22-0-rc1>`_
