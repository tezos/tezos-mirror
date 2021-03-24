trap 'exit $?' ERR
set -x
# [install prerequisites]
dnf install -y dnf-plugins-core
# [install tezos]
dnf copr enable -y @Serokell/Tezos && dnf update -y
dnf install -y tezos-client
dnf install -y tezos-node
dnf install -y tezos-baker-008-PtEdo2Zk
dnf install -y tezos-endorser-008-PtEdo2Zk
dnf install -y tezos-accuser-008-PtEdo2Zk
