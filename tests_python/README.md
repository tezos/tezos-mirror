Documentation: http://tezos.gitlab.io/developer/python_testing_framework.html

# Organization:

The Python Execution and Testing framework tests both the mainnet
protocol, the current protocol proposal and the protocol in development (alpha).

The framework provides helper functions for interacting with all of
the three protocols above.  mainnet and the proposed protocol. If a
new feature is needed to test the proposal, it should be added to the
framework.

For each tested protocol XXX, there are two protocol-specific directories:
 - contracts_XXX which contains michelson scripts for XXX
 - tests_XXX which contains pytests for XXX

# Snapshotting protocol tests

To snapshot a protocol, follow the following instructions
(replace the exported variables at the beginning):

```bash
export SNAPSHOT_PROTO=008
export SNAPSHOT_PROTONAME=EDO
export SNAPSHOT_protoname=edo

# snapshot the tests
cp -rv tests_alpha tests_${SNAPSHOT_PROTO}

# snapshot contracts
cp -rv contracts contracts_${SNAPSHOT_PROTO}

# update regression logs
sed -i '1 s@^tests_alpha/test_@tests_${SNAPSHOT_PROTO}/test_@' \
          tests_${SNAPSHOT_PROTO}/_regtest_outputs/*.out

# do manually:
# 1. add constants for SNAPSHOT_PROTO to tests_python/tools/constants.py, e.g. add the
#    constants {SNAPSHOT_PROTONAME}, {SNAPSHOT_PROTONAME}_DAEMON, {SNAPSHOT_PROTONAME}_PARAMETERS
# 2. in tests_python/tests_alpha/protocol.py, set PREV_HASH, PREV_DAEMON, and
#    PREV_PARAMETERS to match ${SNAPSHOT_PROTO}
# 3. update tests_python/tests_${SNAPSHOT_PROTO}/context/contract_paths.py
# 4. sanity check: `rgrep alpha` and other protocol names in the set of
#    snapshotted tests. remove absolute protocol references to relative ones
#    using the constants of tests_python/tests_alpha/protocol.py
# 5. update tests_python/README.md (this file) with a description of the new folders.

# Sanity checks

# 1. Run some tests per protocol:
poetry run pytest tests_*/test_contract_opcodes.py -k "test_arithmetic_overflow"
```
