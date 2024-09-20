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
