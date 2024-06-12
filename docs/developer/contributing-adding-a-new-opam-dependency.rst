How to add or update opam dependencies
======================================

When a merge request (MR) introduces a new dependency to an opam package, or
updates an existing dependency to a different version of an opam package,
additional steps must be taken in the development and merge process.
This document explains those steps.

If you have already read this guide and only need a refresher, skip to the
:ref:`TL;DR <tldr>`.

Background
----------

The Octez project is built under a system that is somewhat stricter than
the default for OCaml projects. The goal is to make sure that users, developers
and the CI use the exact same dependencies, with the exact same versions.
To this end:

- the set of opam dependencies and their exact version number is stored in
  :src:`an opam lock file <opam/virtual/octez-deps.opam.locked>`;
- the hash of the commit to use from the public opam repository is stored
  in :src:`scripts/version.sh` in the variable ``opam_repository_tag``;
- ``make build-deps`` and ``make build-dev-deps`` use the lock file and the hash
  to select dependencies;
- the CI builds and uses Docker images using this information.

.. note::

    The CI Docker images contain additional dependencies
    such as ``odoc`` which are needed by the CI but not to build Octez.

The rest of this document explains the process of adding, removing or
updating dependencies from the point-of-view of a developer (you). The
instructions below assume you have already :ref:`set up your work
environment <build_from_sources>` but that you installed *development*
dependencies (``make build-dev-deps`` instead of ``make build-deps``).

Local work
----------

The simplest way of using a new dependency on the Octez codebase when working
locally (i.e., on your own machine) is to install it using ``opam``.

Because you have used ``make build-dev-deps`` in order to install the
Octez dependencies, you have access to the default opam repository.

**Install your dependency:** ``opam install foo``

**Add dependencies to build files:** both opam files and dune files must
be updated.
Add the dependency to the relevant declarations in ``manifest/``. And
then use ``make -C manifest`` to update the opam and dune files accordingly.

For example, if you are modifying the Shell using the new
dependency, you must add an entry in the ``~deps`` list of the
``let octez_shell =`` entry of the :src:`manifest/product_octez.ml` and then run
``make -C manifest``. You should see the changes propagated onto
:src:`opam/octez-libs.opam` and :src:`src/lib_shell/dune`,
as well as :src:`opam/virtual/octez-deps.opam`.

You can work on your feature, using the types and values provided by
your new dependency.

Making an MR
------------

Even though you can compile and run code locally, the CI will likely fail.
This is because its set of available dependencies is now different from yours.
You must follow the steps below in order to produce the necessary Docker images,
allowing your work to eventually be merged.

First, in your local copy of Octez, **update the**
``opam_repository_tag`` **variable in the** :src:`scripts/version.sh`
**file**. You should set this variable to the commit hash of a recent version of
the ``master`` branch of
`the default opam repository <https://github.com/ocaml/opam-repository/commits/master>`__.
(Note: this is not always necessary, but it is simpler for you to do so
than to check whether it is necessary to do so.)

Second, update the opam lock file. The safest way to do that is to
**execute the** :src:`scripts/update_opam_lock.sh` **script**.
It will ask opam to upgrade all Octez dependencies,
making sure that unwanted package versions are not selected for dependencies,
and will update the lock file accordingly.
Note that the diff may include a few more changes than what you strictly need.
Specifically, it might include some updates of some other dependencies. This is
not an issue in general but it might explain some changes unrelated to your work.

.. note::

    If you do not wish to upgrade all dependencies,
    you can also just run ``opam lock opam/virtual/octez-deps.opam``
    followed by ``mv octez-deps.opam.locked opam/virtual``,
    or even edit the lock file manually.
    Neither of these guarantees that packages are available in the commit
    identified by ``opam_repository_tag`` of the public opam repository,
    and even so, you may end up with unwanted versions of dependencies;
    so you should review the resulting lock file even more carefully.
    Editing the lock file manually is even less safe than running ``opam lock``
    as it does not guarantee that the set of dependencies is actually
    a valid solution that the opam solver could have chosen.

Third, commit the change of ``scripts/version.sh`` and the updated lockfiles
with a title along the lines of “CI: use dependency ``foo``”.

Finally, **push these changes and open an MR**.

.. _tldr:

TL;DR
-----

As a developer:

- You have an Octez MR from ``<your-organisation>/tezos:<your-branch>``
  onto ``tezos/tezos:master`` introducing a dependency to ``foo``.
- You amend the ``manifest/`` files to declare the dependency.
- You propagate the changes to ``opam`` and ``dune`` files by running ``make -C manifest``.
- You update the ``opam_repository_tag`` to the commit hash of
  a recent version of the public default opam repository.
- You update :src:`opam/virtual/octez-deps.opam.locked`,
  for instance by executing :src:`scripts/update_opam_lock.sh`.
- You push the changes to your Octez MR.

As a merger there are no special steps to take:

- You test, review, etc. the code.
- You assign the Octez MR to Marge Bot.
