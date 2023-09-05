FROM tezos/tezos-debug:amd64_master

WORKDIR /home/tezos

RUN mkdir -p scripts
RUN mkdir -p output

# environment variables
ENV NODE_ENV=production
ENV EXTERNAL_RESSOURCES=/usr/local/share/tezos/evm_kernel
ENV OUTPUT=/home/tezos/output

# install node dependencies
USER root
RUN apk add nodejs npm
USER tezos

# copy scripts
COPY --chown=tezos:nogroup package.json .
COPY --chown=tezos:nogroup scripts scripts
COPY --chown=tezos:nogroup docker/command.sh .
RUN chmod +x command.sh

# install scripts dependencies
RUN npm install --omit=optional

# override the debug entrypoint
ENTRYPOINT [""]

# launch benchmarking
CMD ["./command.sh"]