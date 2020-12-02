Documentation: http://tezos.gitlab.io/developer/python_testing_framework.html

# Organization:

The Python Execution and Testing framework tests both the mainnet
protocol (delphi), the current protocol proposal (edo) and the
protocol in development (alpha).

The framework provides helper functions for interacting with all of
the three protocols above.  mainnet and the proposed protocol. If a
new feature is needed to test the proposal, it should be added to the
framework.

For each tested protocol (_007, _008, and _alpha), there are two
protocol-specific directories:
 - contracts{_007,_008,_alpha}
  - `contracts_007` contains michelson scripts for 007
  - `contracts_008` contains michelson scripts for 008
  - `contracts_alpha` contains michelson scripts for alpha
 - tests{_007,_008,_alpha}
  - `tests_007` contains pytests for 007
  - `tests_008` contains pytests for 008
  - `tests_alpha` contains pytests for alpha

# Snapshotting protocol tests

To snapshot a protocol, follow the following instructions:

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
# 5. remove multibranch test tests_${SNAPSHOT_PROTO}/multibranch/test_baker_endorser_mb.py if still
#    present. it is no longer maintained, and there cannot be two modules named multibranch.
# 6. update tests_python/README.md (this file) with a description of the new folders.
# 7. update .gitlab-ci.yml by running ./scripts/update_integration_tests.sh

# Sanity checks

# 1. Run some tests per protocol:
poetry run pytest tests_*/test_contract_opcodes.py -k "test_arithmetic_overflow"
```
