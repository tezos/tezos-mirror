# When copying this image, feel free to change to debian:stable if you
# encounter any dependency issue
FROM debian:sid AS base

# ignore "Pin versions in apt get install" and "Delete the apt-get
# lists after installing something" hadolint warnings. The apt-get
# list will be deleted later.
# hadolint ignore=DL3008,DL3009
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
    # wget is needed to get the microsoft keys and repo (in this dockerfile) and also to import snapshots (in some scenario)
    wget \
    # Necessary certificates for mirages dependencies
    ca-certificates \
    # jq is useful for pretty-printing json
    jq \
    # nginx is used as reverse proxy to balance the load between DAL nodes
    nginx \
    # libfaketime allows us to set the current date inside docker
    libfaketime \
    # tzkt depends on PGSQL
    postgresql \
    # to run commands as postgres user
    sudo \
    # to clone Tzkt and faucet sources
    git \
    # to run the faucet application
    npm \
    # to generate SSL certificates
    certbot \
    python3-certbot-nginx \
    # to build Umami
    nodejs \
    # DL3015: Use --no-install-recommends
    --no-install-recommends

# Add Microsoft keys and repo, to be able to install dotnet to run TZKT

# ignore "Pin versions in apt get install" hadolint warning.
# hadolint ignore=DL3008
RUN wget -q https://packages.microsoft.com/config/debian/12/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    rm packages-microsoft-prod.deb && \
    # dotnet is used to compile and run TZKT
    apt-get update && apt-get install -y dotnet-sdk-9.0 \
    # DL3015: Use --no-install-recommends
    --no-install-recommends  && \
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

# Build Umami
RUN git clone https://github.com/trilitech/umami-v2 /tmp/umami-v2
WORKDIR /tmp/umami-v2
RUN npm install -g yarn pnpm turbo
RUN pnpm install

FROM base AS full
# Path where binaries should be stored on the docker container
ARG BINARIES_DESTINATION_PATH
COPY ./smart-rollup-installer $BINARIES_DESTINATION_PATH/smart-rollup-installer
COPY ./evm_kernel.wasm $BINARIES_DESTINATION_PATH/evm_kernel.wasm
COPY ./octez-evm-node  $BINARIES_DESTINATION_PATH/octez-evm-node
