Name: octez-zcash-params
Version: %{version}
Release: 1%{?dist}
Summary: Octez zcash parameters
License: MIT
%description
 This package provides Zcash parameters necessary for the Octez node,
 covering cryptographic keys, zk-SNARKs, and protocol configurations.
%install
mkdir -p %{buildroot}/usr/share/zcash-params
install -m 0755 $HOME/rpmbuild/SPECS/zcash-params/* %{buildroot}/usr/share/zcash-params
%files
/usr/share/zcash-params*
%postun
%post
%preun
