How to add or update opam dependencies
======================================

When a merge request (MR) introduces a new dependency to an opam package, or
updates an existing dependency to an different version of an opam package,
additional steps must be taken in the development and merge process.
This document explains those steps.

If you have already read this guide and only need a refresher, skip to the
:ref:`TL;DR <tldr>`.

Background
----------

The Tezos project is built under a system that is somewhat stricter than
the default for OCaml project. Specifically, the Tezos project maintains
a dedicated opam package repository that is a strict subset of the opam
default one; all binaries are built with dependencies from this subset
only.

For this reason, adding or updating a dependency requires to work both
on `the main codebase <https://gitlab.com/tezos/tezos>`__ and on `the
dedicated opam package
repository <https://gitlab.com/tezos/opam-repository>`__. Moreover, work
between those two components must happen in a specific order.

The rest of this document explains the process from the point-of-view of
a developer (you). The instructions below assume you have already
:ref:`set up your work environment <build_from_sources>`
but that you installed *development* dependencies
(``make build-dev-deps`` instead of ``make build-deps``).


Local work
----------

The simplest way of working locally (i.e., on your own machine) on the
Tezos codebase, using a new dependency is to install it using ``opam``.

Because you have used ``make build-dev-deps`` in order to install the
Tezos dependencies, you have access to the default opam repository in
addition to the dedicated one.

**Install your dependency:** ``opam install foo``

Add the dependency to the relevant opam and dune files within the Tezos
codebase. For example, if you are modifying the Shell using the new
dependency, you must add an entry in the ``depends`` section of
``src/lib_shell/tezos-shell.opam`` and add an entry in the ``libraries``
section of ``src/lib_shell/dune``.

**Add dependencies to build files:** both opam files and dune files must
be updated.

You can work on your feature, using the types and values provided by
your new dependency.

Creating a draft MR (optional)
------------------------------

If you need to share your work in the early stages of development to
gather feedback, you can start by creating a work-in-progress (Draft) MR.
This MR will include a hack (described below) to bypass the normal CI
process. Because of this, it will not be mergeable as is but it will
pass the CI and allow you to work collaboratively with other
developers.

To give the CI access to your new dependency, **insert temporary opam commands to
the CI yaml files.** The opam commands have the following form:

::

       - opam pin --no-action --yes add foo.<version> <repository for foo>
       - opam upgrade --yes foo

In the case where your new dependency also brings some new transitive
dependencies, you need to pin all of them and upgrade all of them. E.g., if
`foo` depends on `bar` you should use the following chunk:

::

       - opam pin --no-action --yes add foo.<version> <repository for foo>
       - opam pin --no-action --yes add bar.<version> <repository for bar>
       - opam upgrade --yes foo bar

In the case where your new dependency brings many transitive dependencies, this
hack might not be worth it. You can skip this Draft MR and go directly for the
finalised MR as per the section below.

If you do decide to open the Draft MR anyway, you must insert the yaml chunk into
multiple places in the CI yaml files. The list of places that you need to insert
this chunk into may change with the CI. At the time of writing, you must insert it
into the following places:

-  the ``before_script`` section of the ``.build_template`` rule of
   ``.gitlab/ci/build.yml``,
-  the ``script`` section of the ``build_arm64`` rule of
   ``.gitlab/ci/build.yml``,
-  the ``script`` section of the ``documentation:build`` rule of
   ``.gitlab/ci/doc.yml``,
-  the ``before_script`` section of the ``integration_template`` rule of
   ``.gitlab/ci/integration.yml``,
-  the ``before_script`` section of the ``tezt:main`` rule of
   ``.gitlab/ci/tezt.yml``,
-  the ``before_script`` section of the ``tezt:manual:migration`` rule
   of ``.gitlab/ci/tezt.yml``, and
-  the ``before_script`` section of the ``test-script-gen-genesis`` rule
   of ``.gitlab/ci/test.yml``.

With this ``opam pin`` hack, it can be tested but it cannot be merged
onto master. You can push you branch to Gitlab and open an MR.

- Do not forget to mark your MR as Draft on Gitlab.
- You should also use one dedicated commit to introduce the ``opam pin`` hack. Use an easily identifiable title for the commit. It will be easy to remove afterwards.

Finalising the MR
-----------------

The ``opam pin`` CI yaml hack is satisfactory for a Draft MR. But it is
not mergeable. In order to get to a mergeable MR, you must perform the
following steps.

First, in your local copy of Tezos, **update the**
``full_opam_repository_tag`` **variable in the** ``scripts/version.sh``
**file**. You
should set this variable to the hash of the ``HEAD`` commit on
`the default opam repository <https://github.com/ocaml/opam-repository/commits/master>`__.
(Note: this is not always necessary, but it is simpler for you to do so
than to check whether it is necessary to do so.)

Second, still in your local copy of Tezos, **execute the**
``./scripts/update_opam_repo.sh`` **script**. This script will create a file
called ``opam_repo.patch`` that represent the diff between the current
dedicated opam repository and the dedicated opam repository that your MR
needs.

Note that the diff may include a few more changes. Specifically, it
might include some updates of some other dependencies. This is not an
issue in general but it might explain some changes unrelated to your
work.

Third, **create an MR on the dedicated opam repository that includes
your patch.** This is the *opam repository MR*, its role is to prepare
the environment for your existing *Tezos MR*.

In order to create the opam repository MR:

- If it hasn’t already been done, fork `the dedicated opam repository <https://gitlab.com/tezos/opam-repository>`__ under your own organisation (i.e., to ``https://gitlab.com/<your-organisation>/opam-repository``).
- If you haven’t already done so, clone your organisation’s repository.
- Create a branch off of the Tezos organisation’s ``master`` and switch to it (``git fetch "git@gitlab.com:tezos/opam-repository.git" master`` and ``git checkout -b add-dependency-to-foo FETCH_HEAD``).
- Apply the patch generated by ``./scripts/update_opam_repo.sh`` (``git apply <path-to-file>/opam_repo.path``).
- Push your branch (``git push``).
- Create the opam repository MR from this branch.

Fourth, back in your local copy of Tezos, **update the variables in the**
:src:`.gitlab/ci/templates.yml` **and** :src:`scripts/version.sh` **files**. Specifically, set
the ``build_deps_image_version`` and the ``opam_repository_tag`` variables
to the hash of the ``HEAD`` commit of the opam repository MR. Commit
this change with a title along the lines of “CI: use dependency
``foo``”.

Fifth, still in your local copy of Tezos, **temporarily patch some variables in the**
``.gitlab/ci/templates.yml`` **and** ``scripts/version.sh`` **files**. Specifically, set
the variables ``build_deps_image_name`` to
``registry.gitlab.com/<your-organisation>/opam-repository`` and
``opam_repository_url`` to
``https://gitlab.com/<your-organisation>/opam-repository``. Commit
this change with a title that makes it obvious that it is temporary and
needs to be removed before being merged – e.g., by using one of the
following prefix TEMPORARY, WIP, HACK, TOBEREMOVED.

Together, the commits in the fourth and fifth steps allow the CI of
your Tezos MR to obtain docker images that were built by the CI of your
opam repository MR. One commit updates the version numbers (the commit
hashes), the other commit temporarily points the CI towards your
organisation’s Gitlab docker registry. The second commit is necessary at this
stage because the opam repository MR is coming from your own
organisation. However, this commit will be removed once the opam
repository MR is merged into the Tezos project’s dedicated opam
repository.

Sixth, still in your local copy of Tezos, **push these changes and open
or update the MR**. Add a link in the description of the Tezos MR to the opam repository
MR. This gives reviewers the necessary context. Note that the CI will
fail (specifically, it will fail to load the Docker images) until CI of
the opam repository MR has successfully run in its entirety.

That’s it. You now have two MRs:

- The *opam-repository MR* from ``<your-organisation>/opam-repository`` against ``tezos/opam-repository`` updates the environment in which the Tezos libraries and binaries are built.
- The *tezos MR* from ``<your-organisation>/tezos`` against ``tezos/tezos`` uses this new environment.

Merging the MR
--------------

This section is for the :doc:`Octez merge team <merge_team>`. It is the last step in the lifetime
of the MRs you have opened. Understanding the basics of this process may
help you when communicating with the reviewers and the mergers of your
MR. Understanding all the minutiae and details is not necessary. For
this reason, this final section is addressed to whichever member of the
Octez merge team takes care of this MR (you).

After the iterative review-comment-edit process has reached a satisfying
fixpoint, you can merge the two MRs opened by the developer. To avoid
interference with other MRs, it is better to perform all the steps
described below relatively quickly (the same day).

First, **merge the opam-repository MR**. This will trigger the CI
that builds the new docker images and uploads them to the Tezos
organisation’s registry.

Second, **fix the tezos MR**. Specifically you need to:

- Remove the temporary commit that points the CI to the developer’s organisation registry.
- Amend the commit that sets the commit hash in ``.gitlab/ci/templates.yml`` and ``scripts/version.sh``.
  Specifically, amend the commit to set the variables to the commit hash of the ``HEAD`` commit on the
  ``master`` branch of the ``tezos/opam-repository`` repository. This ``HEAD`` commit is the one obtained from
  merging the MR in the previous step.

Third, wait for the ``opam-repository`` CI to finish, and **run the CI
on the tezos MR**. Make sure that you also run the opam stage of the
CI.

Fourth, assuming the CI succeeds, simply **merge the tezos MR**. You
should also mention that there has been a dependency update on the
``#devteam`` channel of the developper Slack. The message you send
should invite people to execute ``make build-dev-deps``.

.. _tldr:

TL;DR
-----

As a developer:

- You have a Tezos MR from ``<org>/tezos`` against ``tezos/tezos`` introducing a
  dependency to ``foo``.
- You amend the ``opam`` and ``dune`` files to declare the dependency.
- You update the ``full_opam_repository_tag`` to a commit hash from the public
  default opam repository.
- You execute ``./scripts/update_opam_repo.sh``.
- You open an opam repository MR from ``<org>/opam-repository`` against
  ``tezos/opam-repository`` that includes the generated patch.
- You update ``build_deps_image_version`` and ``opam_repository_tag`` to the
  hash of the ``HEAD`` commit of your opam repository MR.
- You update ``build_deps_image_name`` and ``opam_repository_url`` to your
  organisation's registry.
- You push the changes to your Tezos MR, carefully separating temporary and
  permanent changes in distinct commits.
- You update the description of your Tezos MR and set the opam repository MR as
  a dependency.

As a merger:

- You test, review, etc. the code.
- You merge the opam repository MR.
- You update the Tezos MR to point to the new opam repository hash and remove
  the temporary move to the registry of the developer's organisation.
- You wait for the opam repository CI to complete.
- You merge the Tezos MR.
