Name: octez-baker
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
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
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-baker-P* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-baker %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-accuser-P* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-accuser %{buildroot}/usr/bin/
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-baker.1 %{buildroot}%{_mandir}/man1/octez-baker.1
gzip %{buildroot}%{_mandir}/man1/octez-baker.1
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-accuser.1 %{buildroot}%{_mandir}/man1/octez-accuser.1
gzip %{buildroot}%{_mandir}/man1/octez-accuser.1
%files
/usr/bin/octez-baker-*
/usr/bin/octez-baker
/usr/bin/octez-accuser-*
/usr/bin/octez-accuser
%{_mandir}/man1/octez-baker.1*
%{_mandir}/man1/octez-accuser.1*
%postun
%post
%preun
