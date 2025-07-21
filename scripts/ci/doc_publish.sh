#!/usr/bin/env bash

set -e
set -x

cd "${CI_PROJECT_DIR}" || exit 1

if [ "${CI_COMMIT_REF_NAME}" == "master" ]; then
  # Install S3 CLI; credentials are set via the AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables in CI
  sudo apk add aws-cli
  # Update S3
  # https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3/sync.html
  aws configure set s3.max_concurrent_requests 500
  aws s3 sync docs/_build/ s3://site-prod.octez.tezos.com/docs --delete --only-show-errors
  aws cloudfront create-invalidation --distribution-id "${CLOUDFRONT_DISTRIBUTION_ID}" --paths "/*"

  # we publish to gitlab.io only file _redirects:
  git clone --depth 5 git@gitlab.com:"${CI_PROJECT_NAMESPACE}"/"${CI_PROJECT_NAMESPACE}".gitlab.io gitlab.io
  cp docs/_build/_redirects gitlab.io/public/
  cd gitlab.io || exit 2

  if [ -z "$(git status -s)" ]; then
    echo "Nothing to commit!"
  else
    git add public
    git commit -m "Import doc of ${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}:${CI_COMMIT_SHA}"
    git push origin master
  fi

else
  echo "Skip pushing documentation. Only pushing for real master"
fi
