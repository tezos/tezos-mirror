trap 'exit $?' ERR
set -x
sudo apt-get update
export OPAMYES=true
export OPAMSOLVERTIMEOUT=1200
# [install ocaml compiler]
opam switch create for_tezos 4.10.2
eval $(opam env)
# [get system dependencies]
opam install depext
opam depext tezos
# [install tezos]
opam install -y tezos
