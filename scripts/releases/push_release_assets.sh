#!/usr/bin/env bash

set -ex

S3_BUCKET="site-prod.octez.tezos.com/releases"

#TODO: Add to docker image ?
sudo apk add aws-cli

# We use a file to list versions so that we can control what is actually displayed.
versions_list_filename="versions.json"

# export AWS_ACCESS_KEY_ID="${AWS_KEY_RELEASE_PUBLISH}"
# export AWS_SECRET_ACCESS_KEY="${AWS_SECRET_RELEASE_PUBLISH}"

if [ -z "${AWS_ACCESS_KEY_ID}" ] || [ -z "${AWS_SECRET_ACCESS_KEY}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

aws s3 ls "s3://${S3_BUCKET}/"

aws s3 cp s3://"${S3_BUCKET}"/"$versions_list_filename" "./versions.json"

major="22"
minor="1"
announcement="https://octez.tezos.com/docs/releases/version-22.html"
gitlab_release="octez-v22.1"

jq ". += [{\"major\":${major}, \"minor\":${minor}, \"latest\":true, \"announcement\":\"${announcement}\"}]" "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"

cat ./versions.json

aws s3 ls "s3://${S3_BUCKET}/"

aws s3 ls "s3://${S3_BUCKET}/octez-v22.0-rc3/binaries/arm64/"

wget -O "octez-arm.zip" "https://gitlab.com/tezos/tezos/-/jobs/10316771839/artifacts/download"
unzip "./octez-arm.zip"

wget -O "octez-x86.zip" "https://gitlab.com/tezos/tezos/-/jobs/10316771835/artifacts/download"
unzip "./octez-x86.zip"

# Upload binaries to S3 bucket
echo "Uploading binaries..."
aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/"
aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/"

# Create and push archives
tar -czf "${gitlab_release}.tar.gz" --transform 's|^\octez-binaries/x86_64/|octez/|' octez-binaries/x86_64/*
aws s3 cp "./${gitlab_release}.tar.gz" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/"
sha256sum "${gitlab_release}.tar.gz" >> "./x86_64_sha256sums.txt"
tar -czf "${gitlab_release}.tar.gz" --transform 's|^\octez-binaries/arm64/|octez/|' octez-binaries/arm64/*
sha256sum "${gitlab_release}.tar.gz" >> "./arm64_sha256sums.txt"
aws s3 cp "./${gitlab_release}.tar.gz" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/"

# Push checksums for x86_64 binaries
echo "Generating checksums for x86_64 binaries"
for binary in ./octez-binaries/x86_64/*; do
  filename=$(basename "$binary")
  [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./x86_64_sha256sums.txt"
done
aws s3 cp "./x86_64_sha256sums.txt" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/sha256sums.txt"

# Push checksums for arm64 binaries
echo "Generating checksums for arm64 binaries"
for binary in ./octez-binaries/arm64/*; do
  filename=$(basename "$binary")
  [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./arm64_sha256sums.txt"
done
aws s3 cp "./arm64_sha256sums.txt" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/sha256sums.txt"

aws s3 cp "./$versions_list_filename" "s3://${S3_BUCKET}/"

aws s3 ls "s3://${S3_BUCKET}/"
