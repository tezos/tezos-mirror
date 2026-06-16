Packaging Revisions
===================

Overview
--------

A **packaging revision** is a rebuild of the release assets
(binaries, packages, Docker images) for an already-published stable version,
where the Octez source code remains identical to the original release.
Packaging revisions do not apply to beta or release candidate versions.

Packaging revisions are generally used in the following situations:

- a build failure prevented one or more artifacts from being published correctly
- a package needs to be fixed (e.g. metadata, install scripts) without changing the compiled binaries
- vulnerabilities are detected in dependencies (e.g. base Docker images) and
  the artifacts need to be rebuilt against patched versions.

A packaging revision does **not** create a new GitLab release, does not
publish new OPAM packages, and does not modify any source code or protocol.

Naming
------

Packaging revisions are identified by git tags of the form
``octez-v<major>.<minor>-<N>``, where ``N`` is the build number
(a positive integer incremented for each successive revision of the same version).
For example, ``octez-v24.2-1`` is the first packaging revision of ``octez-v24.2``,
and ``octez-v24.2-2`` is the second.

These tags are based on the same Octez source code as the original release tag.

Published Assets
----------------

A packaging revision may update any subset of the release assets,
depending on what needs to be fixed.

Static binaries
~~~~~~~~~~~~~~~

Static binaries for x86_64 and arm64 are rebuilt and published to the
`Release page <https://octez.tezos.com/releases/>`_, replacing the binaries
previously associated with the version. The existing GitLab release entry
for the version is also updated to point to the new binaries.

Docker images
~~~~~~~~~~~~~

Docker images are first published under the revision tag
(e.g. ``tezos/tezos:octez-v24.2-1``), then promoted to the canonical version
tag (e.g. ``tezos/tezos:octez-v24.2``) on
`DockerHub <https://hub.docker.com/r/tezos/tezos>`_.
This means that users pulling the canonical tag will transparently get the
latest revised image.

Packages
~~~~~~~~

Updated APT packages are published to the
`Nomadic Labs package repository <https://packages.nomadic-labs.com/>`_.
Users who have configured this repository will receive the updated packages
on their next upgrade.

Release page
~~~~~~~~~~~~

The `Release page <https://octez.tezos.com/releases/>`_ is regenerated to
reflect the new revision. The revision is recorded in ``versions.json`` and
the RSS feed is updated with a new entry for the packaging revision.
