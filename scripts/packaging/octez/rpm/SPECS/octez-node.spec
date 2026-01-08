Name: octez-node
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
Release: 1%{?dist}
Summary: L1 Octez node for the Tezos network
License: MIT
Requires: shadow-utils, logrotate, octez-zcash-params == 1.0.0, systemd, sudo
Recommends: octez-client
Suggests: lz4 curl sudo
%description
 This package serves as the core implementation for the Tezos blockchain node.
 It contains the fundamental components required for protocol execution,
 consensus, and network communication within the Tezos blockchain network

 This package installs the Octez node.
%install
mkdir -p %{buildroot}/usr/bin/
install -D -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-node %{buildroot}/usr/bin/
install -D -m 0644 $HOME/rpmbuild/SPECS/manpages/octez-node.1 %{buildroot}%{_mandir}/man1/octez-node.1
gzip %{buildroot}%{_mandir}/man1/octez-node.1
%files
/usr/bin/octez-node
%{_mandir}/man1/octez-node.1*
%postun
%post
%preun
