# When copying this image, feel free to change to debian:stable if you
# encounter any dependency issue
FROM ubuntu:questing AS base

# ignore "Pin versions in apt get install" hadolint warning
# hadolint ignore=DL3008
RUN apt-get update && apt-get install -y \
    # netbase is needed to handle transport services
    netbase \
    # openssh-server is needed for sshd
    openssh-server \
    # Tezos dependencies
    libgmp-dev curl libev-dev libhidapi-dev \
    # Teztale dependencies
    libpq-dev \
    # Dependencies needed for tezt-cloud
    docker.io docker-cli screen file \
    # iproute2 installs traffic control tooling
    iproute2 \
    # Can be used to monitor process individually
    prometheus-process-exporter \
    # emacs can be useful for debugging
    emacs \
    # wget can be used to import snapshots
    wget \
    # Necessary certificates for mirages dependencies
    ca-certificates \
    # jq is useful for pretty-printing json
    jq \
    # nginx is used as reverse proxy to balance the load between DAL nodes
    nginx \
    # libfaketime allows us to set the current date inside docker
    libfaketime \
    # DL3015: Use --no-install-recommends
    --no-install-recommends && \
    # DL3009: Delete the apt-get lists after Installing
    rm -rf /var/lib/apt/lists/* && \
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
# In order to use libfaketime with ssh calls, we need to set few things
RUN sed -i 's/#PermitUserEnvironment no/PermitUserEnvironment yes/' /etc/ssh/sshd_config && \
    echo 'LD_PRELOAD=/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1' >> /root/.ssh/environment
# We run the ssh server but not as a daemon on the port 30000
CMD ["-D", "-p", "30000", "-e"]
ENTRYPOINT ["/usr/sbin/sshd"]

FROM base AS full
# Path where binaries should be stored on the docker container
ARG BINARIES_DESTINATION_PATH
COPY ./octez-node $BINARIES_DESTINATION_PATH/octez-node
COPY ./octez-client $BINARIES_DESTINATION_PATH/octez-client
COPY ./octez-admin-client $BINARIES_DESTINATION_PATH/octez-admin-client
COPY ./octez-baker $BINARIES_DESTINATION_PATH/octez-baker
COPY ./octez-dal-node $BINARIES_DESTINATION_PATH/octez-dal-node
COPY ./octez-teztale-archiver $BINARIES_DESTINATION_PATH/octez-teztale-archiver
COPY ./octez-teztale-server $BINARIES_DESTINATION_PATH/octez-teztale-server
# Feel free to complete with the binaries you need

#################################################################################
# Lines below are added to the original ubuntu_plucky.Dockerfile file that is copy/paste
# above. Mind keeping the above ubuntu_plucky.Dockerfile copy synchronised with the
# content of this file.
#################################################################################
COPY ./docker_embedded_snapshot_file /tmp/docker_embedded_snapshot_file
