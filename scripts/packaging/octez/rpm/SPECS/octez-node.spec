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
mkdir -p %{buildroot}/usr/share/octez-node/
install -D -m 0755 $HOME/rpmbuild/SPECS/binaries/octez-node %{buildroot}/usr/bin/
install -D -m 0755 $HOME/rpmbuild/SPECS/scripts/octez-node-prestart.sh %{buildroot}/usr/share/octez-node/
install -D -m 0755 $HOME/rpmbuild/SPECS/scripts/snapshot-import.sh %{buildroot}/usr/share/octez-node/
install -D -m 0755 "$HOME/rpmbuild/SPECS/scripts/wait-for-node-up.sh" %{buildroot}/usr/share/octez-node/
install -D -m 0644 $HOME/rpmbuild/SPECS/manpages/octez-node.1 %{buildroot}%{_mandir}/man1/octez-node.1
install -D -m 0644 $HOME/rpmbuild/SPECS/octez-node.default %{buildroot}/etc/default/octez-node
gzip %{buildroot}%{_mandir}/man1/octez-node.1
install -D -m 644 $HOME/rpmbuild/SPECS/octez-node.service %{buildroot}/usr/lib/systemd/system/octez-node.service
%files
/usr/bin/octez-node
%{_mandir}/man1/octez-node.1*
/usr/lib/systemd/system/octez-node.service
/usr/share/octez-node/*
%config /etc/default/octez-node
%postun

. /etc/default/octez-node

rm -Rf "$DATADIR"

userdel tezos
rm /etc/default/octez-node

%post
TEZOS_HOME=/var/tezos

. /etc/default/octez-node

# Work directory to store temporary files associated with this package
mkdir -p /run/octez-node

DATADIR="$TEZOS_HOME/.tezos-node"
NETWORK="mainnet"
HISTORY_MODE="rolling"

# Check if the tezos user exists, if not, create it
if ! id "tezos" > /dev/null 2>&1; then
  useradd --system --create-home \
    --home-dir "$TEZOS_HOME" --shell /bin/bash \
    --comment "admin user for octez" tezos
else
  # Setup data directory in case the tezos user already exists
  if [ ! -d "$TEZOS_HOME" ]; then
    mkdir -p "$TEZOS_HOME"
    chown tezos "$TEZOS_HOME"
  fi
fi

# Setup log directory
if [ ! -d /var/log/tezos ]; then
  mkdir -p /var/log/tezos
  chown tezos /var/log/tezos
fi

if [ ! -e $DATADIR/config.json ]; then
  su tezos -c "/usr/bin/octez-node config init \
        --data-dir=$DATADIR \
        --network=\"$NETWORK\" \
        --history-mode=\"$HISTORY_MODE\" \
        --net-addr=\"[::]:9732\" \
        --rpc-addr=\"127.0.0.1:8732\""
fi

%preun
