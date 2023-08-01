# Prerequisite

This repository aims at gathering files necessary to run the
infrastructure for the DAL.

- `main.tf`: This file generates the docker registry associated to the
  project.

- `docker`: This directory provides docker images that are os and user
  dependent. This is to enable a setup where we can push already
  compiled binaries into the cluster via ssh.
  
  - `docker/push.sh <os> <ssh_public_key_file>`: This script builds a
    docker image for a given operating system and a public ssh key.
