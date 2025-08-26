FROM archlinux:latest AS raw

# Fixes packages fetching stalling, provided by ChatGPT
RUN echo "Server = https://mirror.osbeck.com/archlinux/\$repo/os/\$arch" > /etc/pacman.d/mirrorlist && \
    # Disable ParallelDownloads
    sed -i '/^ParallelDownloads/s/^/#/' /etc/pacman.conf && \
    # Install haveged to boost entropy and run it
    pacman -Sy --noconfirm haveged && \
    haveged -w 1024

# Update package database and install dependencies

RUN pacman -Syu --noconfirm --noprogressbar\
    python \
    openssh \
    curl libev hidapi \
    postgresql-libs \
    docker screen \
    logrotate \
    emacs \
    wget \
    jq \
    nginx git

RUN ln -sf /usr/bin/python3 /usr/bin/python && \
    # Setup SSH
    mkdir -p /run/sshd /root/.ssh && \
    ssh-keygen -A

# Prepare nginx directories for DAL reverse proxy, which are not created by default
RUN mkdir -p /etc/nginx/sites-available && mkdir -p /etc/nginx/sites-enabled
RUN rm -f /etc/nginx/nginx.conf &&\
    echo -e "events {\n      worker_connections  1024;\n}\n\
             http {\n    include sites-enabled/*;\n}" > /etc/nginx/nginx.conf



# Add public key for SSH access
ARG SSH_PUBLIC_KEY
RUN echo "$SSH_PUBLIC_KEY" >> /root/.ssh/authorized_keys

# Copy Zcash and DAL trusted setup data
ARG ZCASH_PARAMS_PATH
RUN echo "$ZCASH_PARAMS_PATH"
COPY $ZCASH_PARAMS_PATH /usr/local/share/zcash-params

ARG DAL_TRUSTED_SETUP_PATH
RUN echo "$DAL_TRUSTED_SETUP_PATH"
COPY $DAL_TRUSTED_SETUP_PATH /usr/local/share/dal-trusted-setup

# Run SSH server in foreground on port 30000
CMD ["-D", "-p", "30000", "-e"]
ENTRYPOINT ["/usr/bin/sshd"]


FROM raw AS full

# Add binaries
ARG BINARIES_DESTINATION_PATH
COPY ./octez-node $BINARIES_DESTINATION_PATH/octez-node
COPY ./octez-dal-node $BINARIES_DESTINATION_PATH/octez-dal-node
COPY ./octez-client $BINARIES_DESTINATION_PATH/octez-client
COPY ./octez-baker $BINARIES_DESTINATION_PATH/octez-baker
COPY ./octez-smart-rollup-node $BINARIES_DESTINATION_PATH/octez-smart-rollup-node
COPY ./smart-rollup-installer $BINARIES_DESTINATION_PATH/smart-rollup-installer
COPY ./octez-evm-node  $BINARIES_DESTINATION_PATH/octez-evm-node
COPY ./octez-teztale-archiver $BINARIES_DESTINATION_PATH/octez-teztale-archiver
COPY ./octez-teztale-server $BINARIES_DESTINATION_PATH/octez-teztale-server
COPY ./floodgate $BINARIES_DESTINATION_PATH/floodgate
