#!/bin/sh

echo "Installing hacl-wasm"
npm install hacl-wasm

echo "Running test"
# here we run directly a test in js compiled with jsoo
# instead of a mocha test
node test.bc.js
