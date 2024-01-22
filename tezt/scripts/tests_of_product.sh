#!/bin/sh

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
tezt_dir="$(dirname "$script_dir")"
root_dir="$(dirname "$tezt_dir")"

if [ -n "${TRACE:-}" ]; then set -x; fi

testowners=${tezt_dir}/TESTOWNERS.json
if ! [ -f "$testowners" ]; then
    echo "Expected to find TESTOWNERS.json in ${tezt_dir}."
    exit 1
fi

products() {
    jq -r 'keys|join(", ")' < "$testowners"
}

usage() {
    cat <<EOF
Usage: $0 [ PRODUCT [PRODUCT ...] | --reverse TSL_EXPRESSION]

Prints the list of Tezt tests associated with PRODUCT(s) as defined in
'${testowners#"$root_dir"/}'. Tests are printed in the TSV format used by Tezt's
'--list-tsv', with an additional initial column containing the product
name.

If '--reverse TSL_EXPRESSION' is given, the tests corresponding to
TSL_EXPRESSION is printed with their associated product, or 'default'
if no product is associated to it.
EOF
    products=$(products)
    if [ -n "$products" ]; then
        echo
        echo "The defined set of products are: $products."
    fi

    exit 1
}

if [ "${1:-}" = "--help" ] || [ "$#" -lt 1 ] ; then
    usage
fi

if [ "${1:-}" = "--reverse" ]; then
    products_of_tests=1
    shift
else
    products_of_tests=0
fi

tezt() {
    if ! [ -f "${root_dir}"/_build/default/tezt/tests/main.exe ]; then
        ( cd "${root_dir}" && dune exec tezt/tests/main.exe -- "$@" )
    else
        "${root_dir}"/_build/default/tezt/tests/main.exe "$@"
    fi
}

tests_of_products() {
    while [ "$#" -gt 0 ]; do
        product="$1"
        shift

        if ! jq -e --arg product "$product" 'has($product)' < "$testowners" > /dev/null ; then
            echo "No product '$product' defined. Defined products: $(products)"
            exit 1
        fi

        # Translate selected tags and path_patterns to a Tezt TSL expression:
        f_tsl_expr=$(
            jq -r --arg product "$product" \
               '((.[$product].tags // []) + ((.[$product].path_patterns // [])|map("file =~ \"" + . + "\"")))|join(" || ")' \
               < "$testowners")
        # Prepend the product name to each row
        tezt --list-tsv "${f_tsl_expr}" | sed "s/^/${product}\t/"
    done
}

if [ $products_of_tests = "0" ]; then
    # Map products to tests
    tests_of_products "$@"
else
    tsl_expr="$*"
    # Map tests to products
    mapping=$(mktemp)
    products=$(jq -r 'keys|join(" ")' < "$testowners" )
    # shellcheck disable=SC2086
    tests_of_products $products > "$mapping"
    tezt --list-tsv "$tsl_expr" | grep -v 'qcheck random seed' | while read -r test_row; do
        title=$(echo "$test_row" | cut -f2)
        if ! grep -F "$title" "$mapping"; then
            printf "default\t%s\n" "$test_row"
        fi
    done
    rm "$mapping"
fi
