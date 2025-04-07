Name: octez-zcash-dal-params
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
Release: 1%{?dist}
Summary: Octez zcash dal parameters
License: MIT
%description
 This package provides Zcash parameters necessary for the Octez DAL node.
%install
mkdir -p %{buildroot}/usr/share/dal_trusted_setup
install -m 0755 $HOME/rpmbuild/SPECS/dal-trusted-setup/* %{buildroot}/usr/share/dal_trusted_setup
%files
/usr/share/dal_trusted_setup*
%postun
%post
%preun
