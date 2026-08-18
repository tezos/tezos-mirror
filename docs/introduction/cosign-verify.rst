Verifying Octez Docker Images with Cosign
==========================================

`Cosign <https://github.com/sigstore/cosign>`_ is a tool developed by `Sigstore
<https://www.sigstore.dev/>`_ to sign and verify container images and other
artifacts. This document provides instructions on how to verify Docker images of Octez
signed using Cosign.

Prerequisites
-------------

Before you can verify Docker images signed with Cosign, ensure you have
Docker and Cosign installed on your system.

- `Docker Installation Guide <https://docs.docker.com/get-started/get-docker/>`_
- `Cosign Installation Guide <https://docs.sigstore.dev/cosign/system_config/installation/>`_

Obtaining the Public Key
------------------------

To verify a signed Docker image, you need the public key that corresponds to
the private key used for signing. The pem certificate is available at
https://keyserver.nomadic-labs.com/cosign/nl-prod-docker-sign-key.pem

Saving the public key:

.. code-block:: bash

    # Save the public key to a file (e.g., octez.pub)
    curl -o octez.pub https://keyserver.nomadic-labs.com/cosign/nl-prod-docker-sign-key.pem

Verifying the Docker Image
--------------------------

To verify the Octez Docker image, follow these steps:

1. **Pull the Docker Image** (if not already pulled):

   .. code-block:: bash

       docker pull tezos/tezos-bare:master

2. **Use Cosign to Verify the Image**:

   Replace the image name with the name of your Docker image and tag with the
   specific tag ( for example ``tezos/tezos:22.0`` )

   .. code-block:: bash

       cosign verify --key octez.pub tezos/tezos-bare:master

   Or more directly:

   .. code-block:: bash

       cosign verify --key https://keyserver.nomadic-labs.com/cosign/nl-prod-docker-sign-key.pem tezos/tezos-bare:master

3. **Check the Output**:

   You can use tools like ``jq`` to parse the json output of Cosign:

   .. code-block:: bash

       cosign verify --key https://keyserver.nomadic-labs.com/cosign/nl-prod-docker-sign-key.pem tezos/tezos-bare:master | jq

   If the verification is successful, Cosign will output the signatures and their claims in JSON format:

   .. code-block:: bash

       The following checks were performed on each of these signatures:
       - The cosign claims were validated
       - Existence of the claims in the transparency log was verified offline
       - The signatures were verified against the specified public key

   .. code-block:: JSON

       [
         {
           "critical": {
             "identity": {
                 "docker-reference": "<image>"
             },
             "image": {
               "docker-manifest-digest": "sha256:<digest>"
             },
             "type": "cosign container image signature"
           },
           "optional": {
             "Bundle": {
               "SignedEntryTimestamp": "<timestamp's signature>",
               "Payload": {
                 "body": "<payload>",
                 "integratedTime": "<time>",
                 "logIndex": "<logIndex>",
                 "logID": "<logId>"
               }
             }
           }
         }
       ]

   If the verification fails, an error message will be displayed indicating the failure reason.

Inspecting the SLSA Provenance Attestation
------------------------------------------

Octez Docker images carry a `SLSA <https://slsa.dev>`_ provenance attestation
emitted at build time by `BuildKit
<https://docs.docker.com/build/buildkit/>`_. It records the inputs to the
build (source revision, build args, builder version, layer digests) and is
attached to the image index as an OCI (`Open Container Initiative
<https://opencontainers.org>`_) `referrer
<https://github.com/opencontainers/distribution-spec/blob/main/spec.md#listing-referrers>`_
— a separate manifest that points back at the image it describes.

To dump the raw SLSA predicate for a given platform (``tezos/tezos:master`` is
a multi-platform tag, so buildx returns a map keyed by ``os/arch``; pick the
entry you want with ``jq``):

.. code-block:: bash

    docker buildx imagetools inspect --format '{{json .Provenance}}' tezos/tezos:master \
      | jq '.["linux/amd64"].SLSA // .SLSA'

The ``jq`` filter is needed because ``{{json .Provenance}}`` returns a
wrapper, not the bare predicate that SLSA tooling expects; the filter unwraps
it for both single- and multi-platform tags.

Each platform image has exactly one attestation manifest, marked
``vnd.docker.reference.type=attestation-manifest`` in the image index (visible
with ``docker buildx imagetools inspect --raw``); it carries both the SLSA
provenance and the SBOM described below as `in-toto
<https://in-toto.io/docs/what-is-in-toto/>`_ payloads
(``application/vnd.in-toto+json``).

.. note::

   These are OCI-standard in-toto referrer attestations attached by BuildKit,
   **not** cosign `DSSE <https://github.com/secure-systems-lab/dsse>`_
   attestations created by ``cosign attest`` (a different envelope format,
   stored differently). As a consequence ``cosign verify-attestation`` and
   `slsa-verifier <https://github.com/slsa-framework/slsa-verifier>`_
   ``verify-image`` — which look up cosign-created attestations — do not apply
   here and will report "no matching attestations".

   Integrity instead follows from the image *signature*: the parent OCI index
   is signed with cosign and the index commits to the digest of every referrer,
   so ``cosign verify`` (which walks the index recursively) transitively covers
   the attestation manifests. The predicate content is inspected with
   ``docker buildx imagetools inspect`` as shown above.

Inspecting the Software Bill of Materials (SBOM)
------------------------------------------------

Octez Docker images also carry an `SBOM
<https://en.wikipedia.org/wiki/Software_supply_chain>`_ (software bill of
materials) produced at build time by the `syft
<https://github.com/anchore/syft>`_ scanner bundled with BuildKit. It
enumerates the packages installed in the image — the inventory to consult to
answer "is this image affected by advisory X?" — as an `SPDX
<https://spdx.dev>`_ document, carried in the same attestation manifest as the
provenance.

To dump it as a bare SPDX document for a given platform (same per-platform
wrapper as above):

.. code-block:: bash

    docker buildx imagetools inspect --format '{{json .SBOM}}' tezos/tezos:master \
      | jq '.["linux/amd64"].SPDX // .SPDX' > sbom.spdx.json

The document can be validated and scanned, for instance with `sbom-utility
<https://github.com/CycloneDX/sbom-utility>`_ and `grype
<https://github.com/anchore/grype>`_:

.. code-block:: bash

    # Schema validation
    sbom-utility validate --input-file sbom.spdx.json
    # Vulnerability scan from the SBOM
    grype sbom:sbom.spdx.json

.. note::

   `trivy <https://github.com/aquasecurity/trivy>`_ cannot scan this SPDX
   document: it does not read the OS distribution from the packages' `purl
   <https://github.com/package-url/purl-spec>`_ qualifiers and reports
   ``Detected OS family="none"``. With trivy, scan the image directly
   instead (``trivy image tezos/tezos:master``).
