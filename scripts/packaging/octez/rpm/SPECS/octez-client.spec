Name: octez-client
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
Release: 1%{?dist}
Summary: Client for interacting with the Tezos blockchain
License: MIT
%description
 This package serves as the client-side interface for interacting with the
 Tezos blockchain. It includes command-line tools and functionalities for
 querying blockchain data, broadcasting transactions, and interacting with
 smart contracts on the Tezos network.

 This package installs the Octez client. For key management it is
 recommended to install a remote signer of a different host.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-client %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-admin-client %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-codec %{buildroot}/usr/bin/
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-client.1 %{buildroot}%{_mandir}/man1/octez-client.1
gzip %{buildroot}%{_mandir}/man1/octez-client.1
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-admin-client.1 %{buildroot}%{_mandir}/man1/octez-admin-client.1
gzip %{buildroot}%{_mandir}/man1/octez-admin-client.1
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-codec.1 %{buildroot}%{_mandir}/man1/octez-codec.1
gzip %{buildroot}%{_mandir}/man1/octez-codec.1
%files
/usr/bin/octez-client
/usr/bin/octez-admin-client
/usr/bin/octez-codec
%{_mandir}/man1/octez-client.1*
%{_mandir}/man1/octez-admin-client.1*
%{_mandir}/man1/octez-codec.1*
%postun
%post
%preun
