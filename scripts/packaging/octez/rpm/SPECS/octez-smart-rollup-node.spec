Name: octez-smart-rollup-node
Version: 0.0.1
Release: 1%{?dist}
Summary: Smart Rollup node for the Tezos blockchain.
License: MIT
Requires: octez-node systemd
Recommends: octez-client
%description
 The Octez Smart Rollup node enables rollup deployment, managing progress
 through commitment publication and refutation games. Similar to the Octez
 node, it offers an HTTP-accessible RPC interface.

 This package installs the Octez rollup node.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-smart-rollup-node %{buildroot}/usr/bin/
%files
/usr/bin/octez-smart-rollup-node
