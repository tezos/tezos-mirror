Name: octez-experimental
Version: %{version}
%if "%{epoch}" != ""
Epoch: %{epoch}
%endif
Release: 1%{?dist}
Summary: Octez experimental binaries
License: MIT
%description
 This package is a collection of debug and experimental binaries from the
 octez software suite.

 This package installs:
  * octez-snoop: is a tool for benchmarking and fitting statistical models
    which predict the performance of any piece of code of interest.
  * octez-smart-rollup-wasm-debugger: runs Smart Rollups in debug mode to
    make it easier to test and observe them.
  * octez-protocol-compiler: used by the node to compile new protocols on
    the fly, and that can be used for developing new protocols.
%install
mkdir -p %{buildroot}/usr/bin/
install -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-smart-rollup-wasm-debugger %{buildroot}/usr/bin/
%files
/usr/bin/octez-smart-rollup-wasm-debugger
%postun
%post
%preun
