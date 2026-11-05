Smart Rollup node releases
==========================

The Octez Smart Rollup node has its own release. It is not part of an Octez
suite release. This page describes how to release it.

Version scheme
--------------

A release tag is ``octez-smart-rollup-node-vYYYYMMDD``. ``YYYYMMDD`` is the
release date. A second release on the same day adds a ``.N`` suffix, where
``N`` starts at 1 and has no leading zero, for example
``octez-smart-rollup-node-v20260724.1``. A prerelease adds ``-rcN`` or
``-betaN``.

There is no release branch and no backport. To release a fix, you release
again. The node version does not follow the Octez suite version.

What a release produces
-----------------------

A release pipeline runs when you push a release tag. It produces:

- a GitLab release with a generic package of the static binaries;
- static binaries for ``x86_64`` and ``arm64``;
- the Docker images ``tezos/tezos``, ``tezos/tezos-bare`` and
  ``tezos/tezos-debug``, tagged ``octez-smart-rollup-node-vYYYYMMDD``. A
  release also moves the ``octez-smart-rollup-node-latest`` tag. A prerelease
  does not move it;
- an entry on the release page
  https://octez.tezos.com/releases/octez-smart-rollup-node/, with a link to the
  changelog.

Compatibility with Octez suite versions
---------------------------------------

The node supports a range of protocols. State this range in the changelog of
each release. The release date does not tell the user which Octez suite version
the node works with, so the changelog must.

Changelog
---------

Keep the changelog in :doc:`smart_rollup_node_changelog`. In the tagged commit,
add a section under the heading ``Version YYYYMMDD (YYYY-MM-DD)``, with the
version of the tag. State the protocol range and the user-facing changes. The
release page links to this section.

Release procedure
-----------------

#. Update :doc:`smart_rollup_node_changelog` in the commit you release.
#. Push the tag ``octez-smart-rollup-node-vYYYYMMDD`` to ``tezos/tezos``.
#. Watch the ``octez-smart-rollup-node.release`` pipeline.
#. Start the manual ``octez-smart-rollup-node.release-page-deploy-assets`` job.
   The site is then rendered again.
#. Check the release page shows the new version and that every download link
   resolves.

Prereleases
-----------

Push a tag with an ``-rcN`` or ``-betaN`` suffix. This runs the
``octez-smart-rollup-node.prerelease`` pipeline. It publishes the release like a
release pipeline, but it does not move the ``octez-smart-rollup-node-latest``
Docker tag.

Testing the pipeline
--------------------

Push a release tag to a personal fork of the repository. This runs the
``octez-smart-rollup-node.test_release`` pipeline. It writes to
``release-page-test.nomadic-labs.com`` and does not push to Docker Hub. Start
the manual deploy job. Then check the test page shows the version and that every
download link resolves.

Troubleshooting
---------------

- A download link returns 404: the release page directory name and the S3
  upload path must match. Both come from the tag. Check
  ``scripts/rollup_node/releases/deploy_release_page_assets.sh``.
- The version does not appear: the
  ``octez-smart-rollup-node.release-page-deploy-assets`` job failed. Read its
  log.
