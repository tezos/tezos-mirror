Name: octez-smart-rollup-node
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
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
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-smart-rollup-node.1 %{buildroot}%{_mandir}/man1/octez-smart-rollup-node.1
gzip %{buildroot}%{_mandir}/man1/octez-smart-rollup-node.1
install -D -m 644 $HOME/rpmbuild/SPECS/octez-smart-rollup-node.service %{buildroot}/usr/lib/systemd/system/octez-smart-rollup-node.service
install -D -m 644  $HOME/rpmbuild/SPECS/octez-smart-rollup-node.default %{buildroot}/etc/default/octez-smart-rollup-node
%files
/usr/bin/octez-smart-rollup-node
%{_mandir}/man1/octez-smart-rollup-node.1*
/usr/lib/systemd/system/octez-smart-rollup-node.service
/etc/default/octez-smart-rollup-node
%postun
%post
%preun
