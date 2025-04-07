Name: octez-agnostic-baker
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
mkdir -p %{buildroot}/usr/share/octez-agnostic-baker
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-agnostic-baker %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-accuser-P* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/scripts/wait-for-node-up.sh %{buildroot}/usr/share/octez-agnostic-baker/
install -m 0755 $HOME/rpmbuild/SPECS/scripts/systemd-octez-agnostic-baker.sh %{buildroot}/usr/share/octez-agnostic-baker/
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-agnostic-baker.1 %{buildroot}%{_mandir}/man1/octez-agnostic-baker.1
gzip %{buildroot}%{_mandir}/man1/octez-agnostic-baker.1
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-accuser.1 %{buildroot}%{_mandir}/man1/octez-accuser.1
gzip %{buildroot}%{_mandir}/man1/octez-accuser.1
install -D -m 644 $HOME/rpmbuild/SPECS/octez-agnostic-baker.octez-accuser@.service %{buildroot}/usr/lib/systemd/system/octez-accuser@.service
install -D -m 644 $HOME/rpmbuild/SPECS/octez-agnostic-baker.service %{buildroot}/usr/lib/systemd/system/octez-agnostic-baker.service
install -D -m 644 $HOME/rpmbuild/SPECS/octez-agnostic-baker.octez-agnostic-baker-bin.service %{buildroot}/usr/lib/systemd/system/octez-agnostic-baker-bin.service
install -D -m 644  $HOME/rpmbuild/SPECS/octez-agnostic-baker.octez-accuser.default %{buildroot}/etc/default/octez-accuser
install -D -m 644  $HOME/rpmbuild/SPECS/octez-agnostic-baker.default %{buildroot}/etc/default/octez-baker
%files
/usr/bin/octez-agnostic-baker
/usr/bin/octez-accuser-*
/usr/share/octez-agnostic-baker/wait-for-node-up.sh
/usr/share/octez-agnostic-baker/systemd-octez-agnostic-baker.sh
%{_mandir}/man1/octez-agnostic-baker.1.gz
%{_mandir}/man1/octez-accuser.1*
/usr/lib/systemd/system/octez-accuser@.service
/usr/lib/systemd/system/octez-agnostic-baker.service
/usr/lib/systemd/system/octez-agnostic-baker-bin.service
/etc/default/octez-accuser
/etc/default/octez-baker
%postun
%post
%preun
