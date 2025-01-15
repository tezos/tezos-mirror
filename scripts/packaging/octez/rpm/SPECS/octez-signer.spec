Name: octez-signer
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
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
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-signer.1 %{buildroot}%{_mandir}/man1/octez-signer.1
gzip %{buildroot}%{_mandir}/man1/octez-signer.1
%files
/usr/bin/octez-signer
%{_mandir}/man1/octez-signer.1*
%postun
%post
%preun
