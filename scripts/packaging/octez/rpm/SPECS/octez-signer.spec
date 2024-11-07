Name: octez-signer
Version: 0.0.1
Release: 1%{?dist}
Summary: Signing client for the Tezos blockchain.
License: MIT
Requires: octez-client
Recommends: systemd
Suggests: wireguard openssh-client
%description
 The Octez signer provides the essential tools and functionalities for securely
 signing transactions, blocks, and other protocol-related data within the Tezos
 network.

 This package installs the Octez remote signer. It's recommended to run the
 signer and the baker on different hosts and use a hardware ledger for key
 management.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-signer %{buildroot}/usr/bin/
%files
/usr/bin/octez-signer
