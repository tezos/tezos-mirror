Documentation: http://tezos.gitlab.io/developer/python_testing_framework.html

Organization:

The Python Execution and Testing framework tests both the mainnet
protocol and the protocol proposal in development (alpha).

It does that with one framework that wraps the binaries of both
mainnet and the proposed protocol. If a new feature is needed to test
the proposal, it should be added to the framework.

There are two protocol specific directories:
 - contracts{_007,}
  - `contracts_007` contains michelson scripts for 007
  - `contracts_alpha` contains michelson scripts for alpha
 - tests{_007,}
  - `tests_007` contains pytest for 007
  - `tests_alpha` contains pytest for alpha
