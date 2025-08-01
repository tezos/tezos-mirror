Version 7.5
===========

Version 7.0 notably introduces the multinetwork node.

Version 7.1 fixes a few compilation issues that users encountered with version 7.0,
and fixes a few client commands.

Version 7.2 fixes an issue that could cause baking to fail when validating some
smart contracts, and fixes how arguments are passed by the tezos-docker-manager.sh
script when using Docker images.

Version 7.3 fixes a couple of security issues.

Version 7.4 adds the Delphi protocol and the Delphinet built-in network configuration.

Version 7.5 fixes some issues with gas cost computation in the client for Delphi.

Update Instructions
-------------------

To update from sources:

.. code-block:: shell

  git fetch
  git checkout v7.5
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v7.5`` Docker images of Tezos.

New Versioning Scheme
---------------------

Starting from this release, we are using a new versioning scheme to name
our releases. Each release is now named "Version X.Y" where X is the major
version number and Y is the minor version number. Minor releases mostly
backport bug fixes into previous major releases.

Before releasing a new major version, we publish release candidates.
For instance, before releasing 7.0 we published 7.0~rc1, the first release
candidate for version 7.0. Once release candidates have been out
long enough to be sufficiently tested and are considered stable,
we publish actual releases. For instance, 7.0~rc1 was published
April 15th 2020, and 7.0 was published May 5th 2020.

Additionnally, we provide a ``latest-release`` branch which will always
be equal to the latest release. Release candidates are not considered
to be releases in this sense, so ``latest-release`` will never
point to a release candidate. In other words, ``latest-release`` points
to the latest stable release. Currently, it thus points to version 7.5.

If you are used to the ``mainnet`` and ``mainnet-staging`` branches,
you can consider release candidates to be the new ``mainnet-staging``
branches, and the ``latest-release`` branch to be the new ``mainnet``
branch.

Note for Remote Signer Users
----------------------------

Note for users of ``tezos-signer``: the 7.0 (or above) client, baker, endorser
and accuser need the 7.0 signer (or above) to work. They are in particular not
compatible with the ``mainnet`` version of ``tezos-signer``. So remember to
update your remote signer too!

Multinetwork Node
-----------------

The node can now be used with any network, including Mainnet (the
default network) and test networks such as Carthagenet or even custom
ones. See the :doc:`../user/multinetwork` documentation page for more information.

Thanks to this, the same release can be used for all networks
instead of having one branch per network. This allows to name this release
Version 7.0 instead of Mainnet April 2020, as releases are no longer tied to Mainnet.
If you are following the ``mainnet`` branch, you should instead follow the
``latest-release`` branch.

If you are using the Docker script (``alphanet.sh``), note that
this script has been renamed ``tezos-docker-manager.sh``. The ``alphanet.sh``
script is still available in the Docker image for the auto-update mechanism.
See :ref:`howtoget` for more information.

Changelog
---------

- `Version 7.5 <../CHANGES.html#version-7-5>`_
- `Version 7.4 <../CHANGES.html#version-7-4>`_
- `Version 7.3 <../CHANGES.html#version-7-3>`_
- `Version 7.2 <../CHANGES.html#version-7-2>`_
- `Version 7.1 <../CHANGES.html#version-7-1>`_
- `Version 7.0 <../CHANGES.html#version-7-0>`_
