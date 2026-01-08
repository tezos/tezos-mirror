Release System
==============

Octez
-----

Octez releases include all the software needed to run the distributed
consensus platform with its meta-consensus capability. This
meta-consensus capability is supported by a protocol that is used to
achieve consensus not only about the state of its ledger, but also about
upgrading the protocol itself, subject to an on-chain voting procedure.
This evolving nature of the protocol implies that at each point there
are several important protocols, including a currently active protocol
(on the main Tezos network) and other protocols under test, development,
or being voted on-chain. Thus, **protocols are versioned independently
from releases** (see :doc:`../protocols/naming`). For more details on
the distinction between the protocol and the rest of the Tezos node
software, see :doc:`../shell/the_big_picture`.

Although a node can compile and upgrade to a new protocol
on the fly, each release contains several embedded protocol versions.
Therefore, Octez releases are published not only when new features are
added or bugs are fixed, but also when new protocols are proposed or
adopted.

Octez releases are named using the ``octez-v<major>.<minor>`` numbering scheme.
There are also release candidates suffixed by ``~rc<N>``, and beta
versions suffixed by ``~beta<N>``.

Release candidates and beta versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- **Release candidates** are not releases per se. They are published
  versions of release branches that are believed to be ready but require
  further testing by the broader community. After a few days or weeks,
  release candidates are either considered stable releases
  (losing their ``~rc<N>`` suffix), or a new release candidate is published
  with an incremented ``N``. Usually, only major releases have release candidates.

- **Beta versions** are early pre-versions of a major release. Unlike release
  candidates, these are not considered ready for production use, but contain
  the main features of the next major release that are "stable enough" for
  public testing, including on public test networks.

Both RC and Beta versions are **not recommended** for use on Tezos Mainnet.

Major releases
~~~~~~~~~~~~~~

A major release of Octez, ``octez-v<major>.0``, is a distribution of all Octez components.
Major version numbers are incremented for distributions that include significant breaking changes,
or when compatibility across all components is required for a new protocol.

Minor releases
~~~~~~~~~~~~~~

A minor release, ``<component>-v<major>.<minor>``, is a distribution of a specific Octez component.
In the specific case where ``component`` is ``Octez``, only the Layer 1 related software is distributed.
Minor versions usually include bug fixes and minor improvements.
:ref:`The next section<component_releases>` provides more details on components and their releases.

.. _component_releases:

Components
----------

An Octez component is defined as a sub-part of the monorepo (https://gitlab.com/tezos/tezos). For example: the Layer 1 executables, the rollup node, or Teztale.

All components are released together with each Octez major release.
However, each component has its own minor release cycle, meaning that components
can be released independently with minor releases, following their own
incrementation schedule.

Distribution
------------

Releases are available in several forms:

-  **Source code**, from the Octez code repository
   (https://gitlab.com/tezos/tezos). Tags for each release are available
   prefixed by ``octez-v`` or ``<component>-v``.
   There is also a ``latest-release`` tag pointing to
   the latest **stable release** (excluding release candidates).
   Additionally, the version under development is available in the
   ``master`` branch.
-  **Packaged forms**: standalone binaries (with no dependencies),
   binary or source packages, and Docker containers.

The packaged forms are updated from the source as follows:

-  :ref:`Static binaries<getting_static_binaries>`:

   - Updated automatically at each release
   - Available on the `Release page <https://octez.tezos.com/releases/>`_
-  :ref:`Docker containers <using_docker_images>`:

   - Two kinds of images are generated automatically:

     - Release images: at each release, including release candidates
     - Master images: at each merge commit (after each merge request is merged)
   - Available on `DockerHub <https://hub.docker.com/r/tezos/tezos>`_
-  :ref:`OPAM packages<install_opam_packages>`:

   - Updated at each stable release
   - Published on `the Tezos fork of the opam repository <https://github.com/tezos/opam-repository>`_
-  :ref:`APT and DNF binary packages<installing_deb>`:

   - Updated at each release and release candidate
   - Available on the `Nomadic Labs package repository <https://packages.nomadic-labs.com/>`_

For installing Octez from these different forms of releases, see
:doc:`../introduction/howtoget`.
