# The release tag to be used (like 'latest', 'master', etc)
ARG RELEASE_TAG

# Start from some release
FROM tezos/tezos:$RELEASE_TAG

# Set back the user for executing the following commands
USER root

# SSH key that will be used for the SSH server
ARG SSH_PUBLIC_KEY

# This is extracted from the link below
# https://dev.to/yakovlev_alexey/running-ssh-in-an-alpine-docker-container-3lop
RUN mkdir -p /root/.ssh \
    && chmod 0700 /root/.ssh \
    && echo "$SSH_PUBLIC_KEY" > /root/.ssh/authorized_keys \
    && apk add openrc openssh \
    && ssh-keygen -A \
    && echo -e "PasswordAuthentication no" >> /etc/ssh/sshd_config \
    && mkdir -p /run/openrc \
    && touch /run/openrc/softlevel

CMD ["-D", "-p", "30000", "-e"]
ENTRYPOINT ["/usr/sbin/sshd"]
