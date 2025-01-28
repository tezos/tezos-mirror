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
mkdir -p %{buildroot}/usr/share/octez-baker
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-baker-* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-accuser-* %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/scripts/wait-for-node-up.sh %{buildroot}/usr/share/octez-baker/
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-baker.1 %{buildroot}%{_mandir}/man1/octez-baker.1
gzip %{buildroot}%{_mandir}/man1/octez-baker.1
install -D -m 644 $HOME/rpmbuild/SPECS/manpages/octez-accuser.1 %{buildroot}%{_mandir}/man1/octez-accuser.1
gzip %{buildroot}%{_mandir}/man1/octez-accuser.1
install -D -m 644 $HOME/rpmbuild/SPECS/octez-baker.octez-accuser-active.service %{buildroot}/usr/lib/systemd/system/octez-accuser-active.service
install -D -m 644 $HOME/rpmbuild/SPECS/octez-baker.octez-accuser-next.service %{buildroot}/usr/lib/systemd/system/octez-accuser-next.service
install -D -m 644 $HOME/rpmbuild/SPECS/octez-baker.octez-baker-active.service %{buildroot}/usr/lib/systemd/system/octez-baker-active.service
install -D -m 644 $HOME/rpmbuild/SPECS/octez-baker.octez-baker-next.service %{buildroot}/usr/lib/systemd/system/octez-baker-next.service
install -D -m 644  $HOME/rpmbuild/SPECS/octez-baker.octez-accuser-active.default %{buildroot}/etc/default/octez-accuser-active
install -D -m 644  $HOME/rpmbuild/SPECS/octez-baker.octez-accuser-next.default %{buildroot}/etc/default/octez-accuser-next
install -D -m 644  $HOME/rpmbuild/SPECS/octez-baker.octez-baker-active.default %{buildroot}/etc/default/octez-baker-active
install -D -m 644  $HOME/rpmbuild/SPECS/octez-baker.octez-baker-next.default %{buildroot}/etc/default/octez-baker-next
%files
/usr/bin/octez-baker-*
/usr/bin/octez-accuser-*
/usr/share/octez-baker/wait-for-node-up.sh
%{_mandir}/man1/octez-baker.1*
%{_mandir}/man1/octez-accuser.1*
/usr/lib/systemd/system/octez-accuser-active.service
/usr/lib/systemd/system/octez-accuser-next.service
/usr/lib/systemd/system/octez-baker-active.service
/usr/lib/systemd/system/octez-baker-next.service
/etc/default/octez-accuser-active
/etc/default/octez-accuser-next
/etc/default/octez-baker-active
/etc/default/octez-baker-next
%postun
%post
%preun
