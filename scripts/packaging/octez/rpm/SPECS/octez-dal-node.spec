Name: octez-dal-node
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
Release: 1%{?dist}
Summary: Octez distribution Availability Layer (DAL) node
License: MIT
Requires: systemd
Suggests: octez-baker
%description
 The DAL node is responsible for temporarily storing data and providing it to
 bakers and Smart Rollups. Similar to the Octez node, it offers an
 HTTP-accessible RPC interface.

 This package installs the Octez DAL node.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-dal-node %{buildroot}/usr/bin/
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-dal-node.1 %{buildroot}%{_mandir}/man1/octez-dal-node.1
gzip %{buildroot}%{_mandir}/man1/octez-dal-node.1
install -D -m 644 $HOME/rpmbuild/SPECS/octez-dal-node.service %{buildroot}/usr/lib/systemd/system/octez-dal-node.service
install -D -m 644  $HOME/rpmbuild/SPECS/octez-dal-node.default %{buildroot}/etc/default/octez-dal-node
%files
/usr/bin/octez-dal-node
%{_mandir}/man1/octez-dal-node.1*
/usr/lib/systemd/system/octez-dal-node.service
/etc/default/octez-dal-node
%postun
%post
%preun
