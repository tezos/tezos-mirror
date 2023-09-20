# Contributed Software

This directory is meant to host the source code of tools which can be
useful for users and developers of Octez and the Tezos protocol but
which don't deserve as much care as the components which are critical
for the security of Tezos and Octez users.

Putting these non-critical pieces of code in a distinct directory
serves the following purposes:
- preventing accidental call of this code from the critical components;
- the mere presence of a tool in this directory indicates that it is
  considered non-critical;
- locating these tools under some directory eases the definition of
  approval rules lowering the quality requirements for non-critical
  code.

