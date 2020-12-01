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
