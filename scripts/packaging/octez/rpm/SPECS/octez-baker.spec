Name: octez-baker
Version: 0.0.1
Release: 1%{?dist}
Summary: Octez baking software for the Tezos blockchain.
License: MIT
Requires: octez-node curl systemd
Recommends: octez-signer
Requires(pre): octez-client
%description
 The Octez baker includes the necessary tools and functionalities for
 participating in the baking process, such as block creation, attestations, and
 protocol-specific consensus mechanisms.

 This package installs the baker, the accuser and the Octez node. For key
 management it is recommended to install a remote signer of a different host.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-baker-* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-accuser-* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/scripts/wait-for-node-up.sh %{buildroot}/usr/share/octez-baker/
%files
/usr/bin/octez-baker-*
/usr/bin/octez-accuser-*
/usr/share/octez-baker/wait-for-node-up.sh
