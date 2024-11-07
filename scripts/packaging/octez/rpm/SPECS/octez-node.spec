Name: octez-node
Version: 0.0.1
Release: 1%{?dist}
Summary: L1 Octez node for the Tezos network
License: MIT
Requires: shadow-utils logrotate octez-zcash-params systemd
Recommends: octez-client
Suggests: lz4 curl
%description
 This package serves as the core implementation for the Tezos blockchain node.
 It contains the fundamental components required for protocol execution,
 consensus, and network communication within the Tezos blockchain network

 This package installs the Octez node.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-node %{buildroot}/usr/bin/
%files
/usr/bin/octez-node
