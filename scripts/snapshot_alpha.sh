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

# set current version
echo "Setting current version in raw_context and proxy"
sed -i.old.old -e 's/let version_value = "alpha_current"/let version_value = "'${current}'"/' \
    src/proto_${version}/lib_protocol/raw_context.ml src/proto_${version}/lib_client/proxy.ml

long_hash=$(./octez-protocol-compiler -hash-only src/proto_${version}/lib_protocol)
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
       -e s,tezos-protocol-alpha/,tezos-protocol-${version}-${short_hash}/,g \
       -e s,raw_protocol_alpha/,raw_protocol_${version}_${short_hash}/,g \
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
      | sed -e "s/Alpha Development/${capitalized_label}/g" \
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

# Replace test invocation headers that mention proto_alpha
find src/proto_${version}_${short_hash} -type f -path \*/test/\*.ml \
     -exec sed -i "s@Invocation:\(.*\)/proto_alpha/\(.*\)@Invocation:\1/proto_${version}_${short_hash}/\2@" \{\} \;

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


mv $daemons ..
cd ..
rmdir tmp

cd lib_protocol

# replace fake hash with real hash, this file doesn't influence the hash
sed -i.old -e 's/"hash": "[^"]*",/"hash": "'$long_hash'",/' \
    TEZOS_PROTOCOL

# We use `--print0` and `xargs -0` instead of just passing the result
# of find to sed in order to support spaces in filenames.
find . -type f -print0 | xargs -0 \
  sed -i.old -e s/protocol_alpha/protocol_${version}_${short_hash}/ \
             -e s/protocol-alpha/protocol-${version}-${short_hash}/ \
             -e s/protocol-functor-alpha/protocol-functor-${version}-${short_hash}/
# add this protocol to the immutable list
printf \\n$long_hash >> ../../lib_protocol_compiler/final_protocol_versions

cd ../../..

# remove files generated by sed
find . -name '*.old' -exec rm {} \;

# automatically add the new protocol tag to alcotezt

temp_file=$(mktemp)
head -n -1 tezt/lib_alcotezt/alcotezt_utils.ml > "$temp_file"
echo "  | Some \"${version}_${short_hash}\" -> [\"${label}\"]" >> "$temp_file"
echo "  | Some _ -> assert false" >> "$temp_file"
mv "$temp_file" tezt/lib_alcotezt/alcotezt_utils.ml
