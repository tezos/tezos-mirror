Release System
==============

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

Although a node is able to compile and upgrade to a new protocol
on the fly, each release contains several embedded protocol versions.
Consequently, Octez releases are created not only when new features are
added or bugs are fixed, but also when new protocols are proposed or
adopted.

Starting with version 7.0, releases are named using a
``<major>.<minor>`` numbering scheme. There are also release candidates
suffixed by ``~rc<N>``, and beta versions suffixed by ``~beta<N>``.

- **The major version number is incremented when a new release branch is
  created from master.** Such branches thus include many new
  features and possibly significant breaking changes, such as
  improvements to the storage layer requiring a manual
  upgrade that can take significant time.

- **The minor version number is incremented when changes are backported
  to an existing release branch.** Such versions usually include mostly
  bug fixes and minor improvements. But they can also include new
  economic protocols and/or new versions of the economic protocol
  environment, so that users who do not want to migrate to newer major
  versions yet (because of the possible breaking changes) can still
  run a node and a baker for these new protocols.

- Release candidates are not releases per se. They are published
  versions of release branches that are believed to be ready but that
  require further testing by the community at large. After a few days
  or weeks, release candidates are either considered to be stable, in
  which case they are actually released (losing their ``~rc<N>``
  suffix), or a new release candidate is published with an increment
  of ``N``. Usually, only major releases have release candidates.

- Beta versions are early pre-versions of a major release. Unlike release candidates, these are not believed to be ready, but contain main features of the next major release which are "stable enough" for public testing, including public test networks.
  It is not recommended to use them on Tezos Mainnet.

Releases are available in several forms:

-  in source form, from the Octez code repository
   (https://gitlab.com/tezos/tezos). Tags for each release are available
   prefixed by ``v``, and there is also a ``latest-release`` tag, pointing to
   the latest **stable release** (i.e., excluding release candidates).
   Additionally, the version under development is available in the
   ``master`` branch.
-  packaged in various forms: as standalone binaries (with no dependencies),
   as binary or source packages, and as Docker containers.

The packaged forms are updated from the source form as follows:

-  static binaries:

   -  automatically, at each stable release
   -  For recent release candidates, the static binaries are available
      as GitLab artifacts, pointed to in the `release page of the repository <https://gitlab.com/tezos/tezos/-/releases>`_.

-  Docker containers: two kinds of images are generated, automatically:

   -  release images: at each release, including release candidates

   -  master images: at each merge commit (i.e. after each merge request is merged)

-  OPAM packages:

   -  at each stable release
   -  This is currently a manual process performed by
      `Nomadic Labs <https://www.nomadic-labs.com/>`_.

-  APT and DNF binary packages:

   -  at each release, including release candidates
   -  The process is currently performed manually by
      `Serokell <https://serokell.io>`_.

For installing Octez from these different forms of releases, see
:doc:`../introduction/howtoget`.
