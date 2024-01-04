#!/usr/bin/env bash

DIR=$(dirname "$0")

PREFIX="last"

SUFFIX=crypto_data.csv

BIN=$DIR/yes-wallet/test/bench_signature_perf.exe
SRC=$DIR/yes-wallet/test/bench_signature_perf.ml

# Going into tezos root directory
cd "$DIR" || exit
cd ..

echo "#####################"
echo "benching true crypto."
echo "#####################"

echo "Shall we revert the yes-node patch ? dry run of reverse patch gives:"
patch -R --dry-run -p1 < scripts/yes-node.patch
echo 'answer "y" to apply the patch'
read -r revert
if [ "$revert" = "y" ]; then patch -R -p1 < scripts/yes-node.patch; fi

sed -i 's/let time = time ~yes_crypto:.*/let time = time ~yes_crypto:false/' "$SRC"

PERF_TARGET=perf_${PREFIX}_true_crypto.data
echo "# Running  perf utilty on the benchmark"
perf record --call-graph dwarf -- dune exec "$BIN"
mv perf.data $PERF_TARGET
echo "# Perf data are available in $PERF_TARGET"

echo "# Running benchmarking experiment three time:"
# Doing the experiment three time, to assess the reproducibility
dune exec "$BIN" > ${PREFIX}_true_${SUFFIX}
dune exec "$BIN" >> ${PREFIX}_true_${SUFFIX}
dune exec "$BIN" >> ${PREFIX}_true_${SUFFIX}
echo "# benchmark results recorded in ${PREFIX}_true_${SUFFIX} "

echo
echo "#####################"
echo "benching fake crypto."
echo "#####################"

patch -p1 < scripts/yes-node.patch
sed -i 's/let time = time ~yes_crypto:.*/let time = time ~yes_crypto:true/' "$SRC"

PERF_TARGET=perf_${PREFIX}_fake_crypto.data
echo "# Running  perf utilty on the benchmark"
perf record --call-graph dwarf -- dune exec "$BIN"
mv perf.data $PERF_TARGET
echo "# Perf data are available in $PERF_TARGET"

echo "# Running benchmarking experiment three time:"
dune exec "$BIN" > ${PREFIX}_fake_${SUFFIX}
dune exec "$BIN" >> ${PREFIX}_fake_${SUFFIX}
dune exec "$BIN" >> ${PREFIX}_fake_${SUFFIX}
echo "# benchmark results recorded in ${PREFIX}_fake_${SUFFIX} "
