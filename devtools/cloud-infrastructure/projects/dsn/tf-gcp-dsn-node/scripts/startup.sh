#!/bin/bash

#Install Docker

for pkg in docker.io docker-doc docker-compose podman-docker containerd runc; do sudo apt-get remove $pkg; done

# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/debian/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
# shellcheck disable=SC1091
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/debian \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" |
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin -y

# Configure privileges for current user

useradd -m dockeruser -G docker -s /bin/bash
chown -R dockeruser:dockeruser /home/dockeruser/
sudo -u dockeruser gcloud auth configure-docker europe-docker.pkg.dev --quiet
# shellcheck disable=SC2154
sudo -u dockeruser docker pull "${docker_registry}/${docker_image}"

# Authorize ssh login for dockeruser

sudo -u dockeruser mkdir /home/dockeruser/.ssh
# shellcheck disable=SC2154
sudo cp "/home/${ssh_user}/.ssh/authorized_keys" /home/dockeruser/.ssh/authorized_keys
sudo chown dockeruser /home/dockeruser/.ssh/authorized_keys
sudo -u dockeruser chmod 600 /home/dockeruser/.ssh/authorized_keys
