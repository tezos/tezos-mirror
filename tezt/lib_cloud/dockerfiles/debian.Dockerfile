# When copying this image, feel free to change to debian:stable if you
# encounter any dependency issue
FROM debian:sid as raw

RUN apt-get update && apt-get install -y \
    # netbase is needed to handle transport services
    netbase \
    # python3 is not strictly needed but could be used to run an html server for example
    python3 \
    # openssh-server is needed for sshd
    openssh-server \
    # Tezos dependencies
    libgmp-dev curl libev-dev libhidapi-dev \
    # With the proxy mode we may want to run docker inside docker
    docker.io screen \
    # Necessary certificates for mirages dependencies
    ca-certificates \
    # DL3015: Use --no-install-recommends
    --no-install-recommends && \
    # DL3009: Delete the apt-get lists after Installing
    rm -rf /var/lib/apt/lists/* && \
    # To create and make the /usr/bin/python binary points to /usr/bin/python3 binary
    update-alternatives --install /usr/bin/python python /usr/bin/python3 1 && \
    # This directory is necessary for running sshd
    mkdir -p /run/sshd && \
    # A server ssh also requires a key. We generate one for all the main schemes
    mkdir -p /root/.ssh && \
    ssh-keygen -A
# The public key giving access in ssh to the container
ARG SSH_PUBLIC_KEY
# The key is added to already existing keys
RUN echo $SSH_PUBLIC_KEY >> /root/.ssh/authorized_keys
# Copy zcash params from local machine to the remote one
ARG ZCASH_PARAMS_PATH
COPY $ZCASH_PARAMS_PATH /usr/local/share/zcash-params
# Copy DAL trusted setup from local macine to the remote machine
ARG DAL_TRUSTED_SETUP_PATH
COPY $DAL_TRUSTED_SETUP_PATH /usr/local/share/dal-trusted-setup
# We run the ssh server but not as a daemon on the port 30000
CMD ["-D", "-p", "30000", "-e"]
ENTRYPOINT ["/usr/sbin/sshd"]
