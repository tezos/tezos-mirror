#!/usr/bin/env bash

set -e

cd "${CI_PROJECT_DIR}" || exit 1

if [ "${CI_COMMIT_REF_NAME}" == "master" ]; then
  git clone --depth 5 git@gitlab.com:"${CI_PROJECT_NAMESPACE}"/"${CI_PROJECT_NAMESPACE}".gitlab.io gitlab.io
  rsync --recursive --links --perms --delete --exclude=.doctrees --exclude={{main,alpha,zero}net,master}/index.html docs/_build/ gitlab.io/public/
  cd gitlab.io || exit 2

  if [ -z "$(git status -s)" ]; then
    echo "Nothing to commit!"
  else
    git add public
    git commit -m "Import doc of ${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}:${CI_COMMIT_SHA}"
    git push origin master

    # Install S3 CLI; credentials are set via the AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables in CI
    sudo apk add aws-cli
    # Update S3
    # https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3/sync.html
    aws s3 sync ./public s3://site-prod.octez.tezos.com/docs --delete
  fi
else
  echo "Skip pushing documentation. Only pushing for real master"
fi
