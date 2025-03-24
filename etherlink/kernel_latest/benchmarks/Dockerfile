ARG DEBUG_IMAGE=tezos/tezos-debug
ARG DEBUG_IMAGE_VERSION=amd64_master

FROM ${DEBUG_IMAGE}:${DEBUG_IMAGE_VERSION}

WORKDIR /home/tezos

RUN mkdir -p scripts \
 && mkdir -p output

# environment variables
ENV NODE_ENV=production
ENV EXTERNAL_RESSOURCES=/usr/local/share/tezos/evm_kernel
ENV OUTPUT=/home/tezos/output

# install node dependencies
USER root
RUN apk --no-cache add nodejs=~18.20.1 \
    npm=~9.6.6 \
    parallel=~20230522-r0 \
    jq=~1.6
USER tezos

# copy scripts
COPY --chown=tezos:nogroup package.json .
COPY --chown=tezos:nogroup scripts scripts
COPY --chown=tezos:nogroup docker/command.sh .
RUN chmod +x command.sh \
 && chmod +x scripts/parabench.sh

# install scripts dependencies
RUN npm install --omit=optional

# launch benchmarking
ENTRYPOINT ["./command.sh"]
CMD ["-j", "10", "--", "--mode", "sequencer", "--multi-blueprint"]
