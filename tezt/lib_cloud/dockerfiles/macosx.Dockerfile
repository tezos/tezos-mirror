# This image needs to be updated from time to time, as the code evolves.
# FIXME: Can we automatically get one, instead of hardcoding?
FROM us-central1-docker.pkg.dev/nl-gitlab-runner/registry/tezos/tezos/ci/build@sha256:c826a00735d97e66b56e00ebe52f7d32c19b059acc5a073f3a4e96057a63342c AS step1

FROM step1 AS step2
WORKDIR /build
RUN git init tezos
WORKDIR /build/tezos
RUN git remote add origin https://gitlab.com/tezos/tezos.git
# This commit was hardcoded. It is supposed to be a recent commit, if the user
# encounters errors, because of incompatibilities between current master and 
# given commit, please change the commit locally to something more recent.
# FIXME: Can we get the latest commit instead?
RUN git fetch --depth 1 origin 093ed36b2ee3b5882434d18008c8e41f8a12bf25
RUN git checkout 093ed36b2ee3b5882434d18008c8e41f8a12bf25
RUN ./scripts/slim-mode.sh on
RUN eval $(opam env) && make octez-node

# This step might not be necessary for some environments, depending on docker.
# Overall, it should speed things up for local changes to octez.
FROM step2

ARG COMMIT
RUN git fetch --depth 1 origin $COMMIT
RUN git checkout $COMMIT
RUN eval $(opam env) && make octez-node octez-dal-node octez-client octez-experimental-agnostic-baker


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

