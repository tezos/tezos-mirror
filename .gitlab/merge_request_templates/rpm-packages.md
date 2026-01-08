<!--
Thank you for taking the time to contributing to the Tezos project!

Make sure to read our Contributing guide (https://tezos.gitlab.io/developer/contributing.html) and the Merge process description (https://tezos.gitlab.io/developer/merge_team.html). -->

# What

This MR focuses on RPM packaging.


<!-- Explain what your MR does without going into details. -->

# Why

<!-- Explain the motivation for your work. -->

# How

<!-- Explain how your MR achieves what it says it does and why it is a good way. -->
<!-- Discuss possible side-effects and other solutions you have considered. -->

# Manually testing the MR

Here are the important aspects to consider while testing this MR, with spelling
fixed and text clarified:

- Does it compile locally?
- Can it be installed locally?
- Can the octez-node and a baker be run locally using systemd?
- Does the package correctly upgrade from an already installed package?
- Does the package compile in a Docker container?
- Do all CI checks pass?
- Is the installation process user-friendly ?

It's important to note that the CI cannot check all possible use cases,
particularly the interactive installation process. Therefore, it's crucial for
the reviewer to manually check in a Docker container or a VM if the package can
be installed interactively and configured as a working Octez service using
systemd.

While the MR creator has already performed these manual steps, it's important for the
reviewer in invited to independently repeat these tests. For reference,
reviewers should consult the following resources:

- Best practices: https://rpm-packaging-guide.github.io/
- Tips : https://jfearn.fedorapeople.org/en-US/RPM/4/html/RPM_Guide/ch-packaging-guidelines.html

## Local testing:

To test RPM packages locally, you need a machine compatible with the rpm
package manager with the following packages installed:

- `rpmbuild`

Use the script `scripts/packaging/build-rpm-local.sh` to build RPM packages
locally. It accepts two arguments: `binaries` and `zcash` to compile all binary
packages and all data packages, respectively.

## CI packages testing

The CI creates a dnf repository for each MR. These can be installed using `dnf`.

For RPM-based distributions other than `rockylinux/9.3` (e.g. Fedora), run a
scheduled pipeline to generate packages for this MR using:

```
./ci/run_pipeline/scheduled.sh
```

To install a rpm package, you need to configure the package manager to use the
correct dnf repository. Replace `$CI_COMMIT_REF_NAME` with the ref name related
to this MR.

```
dnf -y update
dnf -y install 'dnf-command(config-manager)'
dnf -y config-manager --add-repo "https://storage.googleapis.com/tezos-linux-repo/next/$CI_COMMIT_REF_NAME/rockylinux/dists/9.3/"

# only needed for rockylinux. not needed for fedora
dnf config-manager --set-enabled devel
rpm --import https://storage.googleapis.com/tezos-linux-repo/next/$CI_COMMIT_REF_NAME/rockylinux/octez.asc
```

Then we can use `dnf` to install the rpm packages and all their dependencies.

```
dnf -y update
dnf -y install octez-client octez-node octez-baker
rpm -v --info -q octez-client
rpm -v --verify octez-client
```

## Test rpm packages in isolation

To test RPM packages in a clean environment using Docker, install Docker
(Docker Installation Guide
https://docs.docker.com/engine/install/#supported-platforms ) and use the
following Dockerfile:

```
FROM rockylinux:9.3

ARG CI_COMMIT_REF_NAME

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN dnf -y update
RUN dnf -y install 'dnf-command(config-manager)'
RUN dnf -y config-manager --add-repo "https://storage.googleapis.com/tezos-linux-repo/next/${CI_COMMIT_REF_NAME}/rockylinux/dists/9.3/"
# only needed for rockylinux. not needed for fedora
RUN dnf config-manager --set-enabled devel

RUN dnf -y update
RUN rpm --import https://storage.googleapis.com/tezos-linux-repo/next/${CI_COMMIT_REF_NAME}/rockylinux/octez.asc
RUN dnf -y install octez-client octez-node octez-baker
RUN rpm -v --info -q octez-client
RUN rpm -v --verify octez-client

```

You can build the Dockerfile using the command
```
docker build -f Dockerfile.test --build-arg CI_COMMIT_REF_NAME=< the ref name of this MR> .
```

### Testing fedora packages.

On a merge pipeline we only generate (with a manual job) `rockylinux` packages.
If you want to test `fedora` packages, you must run a scheduled pipeline for this
branch using the script `./ci/run_pipeline/scheduled.sh`

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
