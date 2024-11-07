Name: octez-dal-node
Version: 0.0.1
Release: 1%{?dist}
Summary: Octez distribution Availability Layer (DAL) node
License: MIT
Requires: octez-zcash-dal-params systemd
Suggests: octez-baker
%description
 The DAL node is responsible for temporarily storing data and providing it to
 bakers and Smart Rollups. Similar to the Octez node, it offers an
 HTTP-accessible RPC interface.

 This package installs the Octez DAL node.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-dal-node %{buildroot}/usr/bin/
%files
/usr/bin/octez-dal-node
