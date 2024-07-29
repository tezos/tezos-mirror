Verifying Octez Docker Images with Cosign
==========================================

`Cosign <https://github.com/sigstore/cosign>`_ is a tool developed by `Sigstore
<https://sigstore.dev/>`_ to sign and verify container images and other
artifacts. This document provides instructions on how to verify Docker images of Octez
signed using Cosign.

Prerequisites
-------------
Before you can verify Docker images signed with Cosign, ensure you have
Docker and Cosign installed on your system.

- `Docker Installation Guide <https://docs.docker.com/get-docker/>`_
- `Cosign Installation Guide <https://docs.sigstore.dev/cosign/installation/>`_

Obtaining the Public Key
------------------------
To verify a signed Docker image, you need the public key that corresponds to
the private key used for signing. The pem certificate is available at
https://storage.googleapis.com/nl-prod-sign-keyring/nl-prod-docker-sign-key.pem

Saving the public key:

.. code-block:: bash

    # Save the public key to a file (e.g., octez.pub)
    curl -O https://storage.googleapis.com/nl-prod-sign-keyring/nl-prod-docker-sign-key.pem octez.pub

Verifying the Docker Image
--------------------------
To verify the Octez Docker image, follow these steps:

1. **Pull the Docker Image** (if not already pulled):

   .. code-block:: bash

       docker pull tezos/tezos-bare:master

2. **Use Cosign to Verify the Image**:

   .. code-block:: bash

       cosign verify -key octez.pub tezos/tezos-bare:master

   Replace the image name with the name of your Docker image and tag with the
   specific tag ( for example ``tezos/tezos:22.0`` )

3. **Check the Output**:

- If the verification is successful, Cosign will output the signatures and their claims.
- If the verification fails, an error message will be displayed indicating the failure reason. You can use tools like ``jq`` to parse the json output of Cosign.
