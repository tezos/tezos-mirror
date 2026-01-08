<!--
Thank you for taking the time to contributing to the Tezos project!

Make sure to read our Contributing guide (https://tezos.gitlab.io/developer/contributing.html) and the Merge process description (https://tezos.gitlab.io/developer/merge_team.html). -->

# What

This MR focuses on debian packaging.


<!-- Explain what your MR does without going into details. -->

# Why

<!-- Explain the motivation for your work. -->

# How

<!-- Explain how your MR achieves what it says it does and why it is a good way. -->
<!-- Discuss possible side-effects and other solutions you have considered. -->

# Manually testing the MR

Here are the important aspects to consider while testing this MR, with spelling fixed and text clarified:

- Does it compile locally?
- Can it be installed locally?
- Can the octez-node and a baker be run locally using systemd?
- Does the package correctly upgrade from an already installed package?
- Does the package compile in a Docker container?
- Do all CI checks pass?
- Is the installation process user-friendly with clear questions and a good UX?
- Can the package be easily migrated from a Serokell Debian package?
- Does the MR introduce any regressions in lintian?

It's important to note that the CI cannot check all possible use cases,
particularly the interactive installation process. Therefore, it's crucial for
the reviewer to manually check in a Docker container or VM if the package can
be installed interactively and configured as a working Octez service using
systemd.

While the MR creator has already performed these manual steps, it's
important for the reviewer to independently repeat these tests. For reference,
reviewers should consult the following resources:

- Best practices: https://www.debian.org/doc/manuals/developers-reference/best-pkging-practices.html
- Lintian tags: https://web.mit.edu/~mkgray/stuff/ath/project/silk/root/afs/sipb/project/debathena/lintian/www/tags-all.html
- Debian policy: https://www.debian.org/doc/debian-policy/



## Local testing:
To test Debian packages locally, you need a Debian machine with the following
packages installed:

- `dpkg-buildpackage`
- `lintian`
- `debconf-utils`

Use the script scripts/packaging/build-deb-local.sh to build Debian packages
locally. It accepts two arguments: binaries and zcash to compile all binary
packages and all data packages, respectively. Use the --dev flag to run
lintian.

While testing the packages locally you should also test for lintian and make
sure we didn't introduce any regressions.

## CI packages testing

To test packages produced in the CI, refer to the official documentation:
[Tezos CI
Documentation](https://tezos.gitlab.io/introduction/howtoget.html#new-set-of-debian-packages)

The CI creates an apt repository for each MR. Replace `$CI_COMMIT_REF_NAME`
with the ref name related to this MR.

For distributions other than `debian/bookworm`, run a scheduled pipeline to
generate packages for this MR using:

```
pipeline-profiler/run_pipeline_type.sh schedule_extended_test
```

Then, to test the packages related to this MR you can simply export the env
variables as :

```
    export distribution=next/$CI_COMMIT_REF_NAME/debian
    export release=bookworm
```

and the follow all the steps in the detailed section dedicated to the installation of the debian packages: https://tezos.gitlab.io/introduction/services.html

We also have a dedicated page for the migration from the Serokell packages that should be considered : https://tezos.gitlab.io/introduction/serokell.html

## Test debian packages in isolation

To test Debian packages in a clean environment using Docker, install Docker (Docker Installation Guide https://docs.docker.com/engine/install/#supported-platforms ) and use the following Dockerfile:

```
FROM debian:bookworm

ARG CI_COMMIT_REF_NAME

RUN apt-get update && apt-get install -y gpg curl

RUN curl https://tezos-linux-repo.storage.googleapis.com/next/debian/octez.asc | gpg --dearmor -o /etc/apt/keyrings/octez.gpg

RUN echo "deb [signed-by=/etc/apt/keyrings/octez.gpg] https://tezos-linux-repo.storage.googleapis.com/next/${CI_COMMIT_REF_NAME}/debian bookworm main" > /etc/apt/sources.list.d/octez.list

RUN apt-get update
RUN apt-get install -y octez-node
```

You can build the Dockerfile using the command
```
docker build -f Dockerfile.test --build-arg CI_COMMIT_REF_NAME=< the ref name of this MR> .
```

<!--
remove to assign reviewers :
/ready
/assign @Killian-Delarue @romain.nl @NicVolanschi @AurelienMonteillet @onurb
/assign_reviewer @Killian-Delarue @romain.nl @NicVolanschi @AurelienMonteillet @onurb
-->

/milestone %"Octez  packages"
/draft

<!-- Describe how reviewers and approvers can test this MR. -->

# Checklist

- [ ] Document the interface of any function added or modified (see the [coding guidelines](https://tezos.gitlab.io/developer/guidelines.html))
- [ ] Document any change to the user interface, including configuration parameters (see [node configuration](https://tezos.gitlab.io/user/node-configuration.html))
- [ ] Provide automatic testing (see the [testing guide](https://tezos.gitlab.io/developer/testing.html)).
- [ ] For new features and bug fixes, add an item in the appropriate changelog (`docs/protocols/alpha.rst` for the protocol and the environment, `CHANGES.rst` at the root of the repository for everything else).
- [ ] Select suitable reviewers using the `Reviewers` field below.
- [ ] Select as `Assignee` the next person who should [take action on that MR](https://tezos.gitlab.io/developer/contributing.html#merge-request-assignees-field)
