#!/usr/bin/env bash

set -e

usage="Usage:

$ ./scripts/snapshot_alpha.sh <name>_<version_number>
Packs the current proto_alpha directory in a new
proto_<version_number>_<hash> directory with all the necessary
renamings.

<name> should be in lower case, e.g. 'stockholm'
<version_number> should be three digits, e.g. 022",

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
cd "$script_dir"/..

# e.g. stockholm_022
current=$1
# e.g. stockholm
label=$(echo $current | cut -d'_' -f1)
# e.g. 022
version=$(echo $current | cut -d'_' -f2)
# e.g. Stockholm
capitalized_label=$(tr '[:lower:]' '[:upper:]' <<< "${label:0:1}")${label:1}
# e.g. STOCKHOLM
upcased_label=$(tr '[:lower:]' '[:upper:]' <<< "${label}")
# e.g. Pt8PY9P47nYw7WgPqpr49JZX5iU511ZJ9UPrBKu1CuYtBsLy7q7 (set below)
long_hash=
# e.g. Pt8PY9P4 (set below)
short_hash=


if ! ( [[ "$label" =~ ^[a-z]+$ ]] && [[ "$version" =~ ^[0-9][0-9][0-9]$ ]] ); then
    echo "Wrong protocol version."
    echo "Name should be a lowercase alphabetic word."
    echo "Number should be a 3-digit number."
    echo
    echo "$usage"
    exit 1
fi

if [ -d src/proto_${version} ] ; then
    echo "Error: you should remove the directory 'src/proto_${version}'"
    exit 1
fi

if [ -d docs/${label} ] ; then
    echo "Error: you should remove the directory 'docs/${label}'"
    exit 1
fi

# create a temporary directory until the hash is known
# this is equivalent to `cp src/proto_alpha/ src/proto_${version}` but only for versioned files
echo "Copying src/proto_alpha to src/proto_${version}"
mkdir /tmp/tezos_proto_snapshot
git archive HEAD src/proto_alpha/ | tar -x -C /tmp/tezos_proto_snapshot
# remove the README because it is specific to Alpha
rm /tmp/tezos_proto_snapshot/src/proto_alpha/README.md
mv /tmp/tezos_proto_snapshot/src/proto_alpha src/proto_${version}
rm -rf /tmp/tezos_proto_snapshot

echo "Copying docs/alpha to docs/${label}"
mkdir /tmp/tezos_proto_doc_snapshot
git archive HEAD docs/alpha/ | tar -x -C /tmp/tezos_proto_doc_snapshot
mv /tmp/tezos_proto_doc_snapshot/docs/alpha docs/${label}
rm -rf /tmp/tezos_proto_doc_snapshot

echo "Copying tests_python/{contracts,tests}_alpha to tests_python/{contracts,tests}_${version}"
mkdir /tmp/tezos_proto_tests_python_snapshot
git archive HEAD tests_python/{contracts,tests}_alpha | tar -x -C /tmp/tezos_proto_tests_python_snapshot
mv /tmp/tezos_proto_tests_python_snapshot/tests_python/contracts_alpha tests_python/contracts_${version}
mv /tmp/tezos_proto_tests_python_snapshot/tests_python/tests_alpha tests_python/tests_${version}
rm -rf /tmp/tezos_proto_tests_python_snapshot

# set current version
echo "Setting current version in raw_context and proxy"
sed -i.old.old -e 's/let version_value = "alpha_current"/let version_value = "'${current}'"/' \
    src/proto_${version}/lib_protocol/raw_context.ml src/proto_${version}/lib_client/proxy.ml

long_hash=$(./tezos-protocol-compiler -hash-only src/proto_${version}/lib_protocol)
short_hash=$(echo $long_hash | head -c 8)

if [ -d src/proto_${version}_${short_hash} ] ; then
    echo "Error: you should remove the directory 'src/proto_${version}_${short_hash}'"
    exit 1
fi

mv src/proto_${version} src/proto_${version}_${short_hash}

# fix versioned links (in labels, references, and paths) in docs
echo "Fixing versioned links in docs"
cd docs/${label}
sed -i.old -e s/_alpha:/_${label}:/g \
       -e s,src/proto_alpha,src/proto_${version}_${short_hash},g \
       -e s/_alpha\>/_${label}\>/g \
       -e s/_alpha\`/_${label}\`/g \
       -e s/-alpha.html/-${label}.html/g \
    $(find . -name \*.rst)
cd ../..

# generate docs/protocols/042_jeanmichel.rst from docs/protocols/alpha.rst
echo "Copying+fixing docs/protocols/alpha.rst to docs/protocols/${version}_${label}.rst"
sed -e s/_alpha:/_${version}_${label}:/g \
    -e s,src/proto_alpha,src/proto_${version}_${short_hash},g \
    -e s/_alpha\>/_${version}\>/g \
    -e s/_alpha\`/_${version}\`/g \
    -e s/-alpha.html/-${version}.html/g \
    docs/protocols/alpha.rst > "docs/protocols/${version}_${label}.rst"

# add entries in the doc index
# copy from alpha rather from previous protocol because there may be newly added items
echo "Add entries in the doc index"
alpha_line='Alpha Development Protocol'
doc_index="docs/index.rst"
(
    set -e
    grep -B9999 -F "$alpha_line" "$doc_index" \
      | head -n-1
    grep -A9999 -F "$alpha_line" "$doc_index" \
      | grep -B9999 -F 'toctree' -m1 \
      | head -n-1 \
      | sed -e "s/Alpha Development/${label} ${capitalized_label}/g" \
            -e "s,alpha/,${label}/,g"
    grep -B9999 -F "$alpha_line" "$doc_index" \
      | tac \
      | grep -B9999 -F 'toctree' -m1 \
      | tac
    grep -A9999 -F "$alpha_line" "$doc_index" \
      | tail -n+2 \
      | awk '{
                if ($0 ~ PATTERN) {
                    x=$0
                    sub(PATTERN,REPLACEMENT)
                    print
                    print x
                } else {
                    print
                }
             }' PATTERN='protocols/alpha' REPLACEMENT="protocols/${version}_${label}"
) > "${doc_index}.tmp"
mv "${doc_index}.tmp" "$doc_index"


# fix paths and comments in python tests and regtest outputs
echo "Fixing python tests"
cd tests_python/tests_${version}
sed -i.old -e "s,tezos\.gitlab\.io/alpha/,tezos.gitlab.io/${version}_${label}/,g" \
           -e "s/proto_alpha/proto_${version}_${short_hash}/g" \
           -e "s/_alpha\b/_${version}/g" \
           -e "s/\balpha\b/${version}/g" \
    $(find . -name \*.py)
echo "Fixing python regtests ouputs"
cd _regtest_outputs
sed -i.old -e "s/_alpha\b/_${version}/g" *.out
cd ../..

echo "Fixing python protocol constants"

# update tests_python/tools/constants.py
crt_line='TEZOS_CRT = """'
constants_file='tools/constants.py'
(
    grep -B9999 -F "$crt_line" "$constants_file" \
      | head -n-1

    cat <<EOF
${upcased_label} = "$long_hash"
${upcased_label}_DAEMON = "${version}-${short_hash}"
${upcased_label}_FOLDER = "proto_${version}_${short_hash}"
${upcased_label}_PARAMETERS = get_parameters(${upcased_label}_FOLDER)

EOF
    grep -A9999 -F "$crt_line" "$constants_file" \
) > "${constants_file}.tmp"
mv "${constants_file}.tmp" $constants_file

# update pytests/tests_${current - 1}/protocol.py

prev_upcased_label=$(grep '^PREV_HASH = constants.*' "tests_alpha/protocol.py" | cut -d. -f2)
if [ -z "$prev_upcased_label" ]; then
    echo "Error: Could not read the label of the predecessor protocol in tests_alpha/protocol.py"
    exit 1
fi

sed -i.old -e "s/constants\.ALPHA/constants\.${upcased_label}/" "tests_${version}/protocol.py"
sed -i.old -e "s/constants\.${prev_upcased_label}/constants\.${upcased_label}/" "tests_alpha/protocol.py"
cd ../

# move daemons to a tmp directory to avoid editing lib_protocol
cd src/proto_${version}_${short_hash}
daemons=$(ls | grep -v lib_protocol)
mkdir tmp
mv $daemons tmp
cd tmp

# rename main_*.ml{,i} files of the binaries
for file in $(find . -name main_\*.ml -or -name main_\*.mli)
do
    mv "$file" $(echo "$file" | sed s/_alpha/_${version}_${short_hash}/g)
done


# rename .opam files
for file in $(find . -name \*.opam)
do
    mv "$file" $(echo "$file" | sed s/alpha/${version}-${short_hash}/g)
done

# fix content of dune and opam files
sed -i.old -e s/_alpha/_${version}_${short_hash}/g \
       -e s/-alpha/-${version}-${short_hash}/g \
    $(find . -name dune -or -name \*.opam)

mv $daemons ..
cd ..
rmdir tmp

cd lib_protocol

# replace fake hash with real hash, this file doesn't influence the hash
sed -i.old -e 's/"hash": "[^"]*",/"hash": "'$long_hash'",/' \
    TEZOS_PROTOCOL

sed -i.old -e s/protocol_alpha/protocol_${version}_${short_hash}/ \
           -e s/protocol-alpha/protocol-${version}-${short_hash}/ \
           -e s/protocol-functor-alpha/protocol-functor-${version}-${short_hash}/ \
    $(find . -type f)

sed -i.old -e s/-alpha/-${version}-${short_hash}/ \
           -e s/_alpha/_${version}_${short_hash}/ \
    $(find . -type f -name dune)

# replace fist the template call with underscore version,
# then the other occurrences with dash version
sed -i.old -e 's/"alpha"/"'${version}_${short_hash}'"/' \
           -e 's/alpha/'${version}-${short_hash}'/' \
    $(find . -name \*.opam)

for file in  $(find . -name \*.opam)
do
    mv "$file" $(echo "$file" | sed s/alpha/${version}-${short_hash}/g)
done

# add this protocol to the immutable list
printf \\n$long_hash >> ../../lib_protocol_compiler/final_protocol_versions

dune exec ../../lib_protocol_compiler/bin/replace.exe ../../lib_protocol_compiler/dune_protocol.template.v1 dune.inc ../../lib_protocol_compiler/final_protocol_versions ${version}_${short_hash}

cd ../../..

# remove files generated by sed
find . -name '*.old' -exec rm {} \;

if [ -z "$SILENCE_REMINDER" ]; then
  echo "Generated src/proto_${version}_${short_hash}. Don't forget to:"
  echo ""
  echo "  dune exec scripts/declare-new-protocol-unit-test/main.exe -- ${version} ${short_hash}"
fi
