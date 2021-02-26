trap 'exit $?' ERR
set -x
# [install prerequisites]
dnf install -y dnf-plugins-core
# [install tezos]
dnf copr enable -y @Serokell/Tezos && dnf update -y
dnf install -y tezos-client
dnf install -y tezos-node
dnf install -y tezos-baker-007-PsDELPH1
dnf install -y tezos-endorser-007-PsDELPH1
dnf install -y tezos-accuser-007-PsDELPH1
