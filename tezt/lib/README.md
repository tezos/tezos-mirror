# Tezt
<!-- Summary line: One sentence about this component. -->
Tezt is a test framework for OCaml covering unit/integration/regression tests.

## API Documentation
<!-- Link to the external API. -->
User documentation covering the API and its usage can be found at
<https://tezos.gitlab.io/api/odoc/_html/tezt/Tezt/index.html>.
This documentation is extracted from file `lib/tezt.ml` in this repository.

## Installation

Tezt is released as an Opam package. Thus, it can be simply installed
using the folowing command:

    opam install tezt

## Overview
<!--
- Describe the purpose of this component.
- Describe the interaction of the code in this directory with the other
  components. This includes dependencies on other components, for instance.
-->
Tezt provides an extensive but easy-to-use framework for testing,
including a command-line interface for running tests supporting
parallel execution, and flexible logs which facilitate locating and
reproducing failures. Tezt includes specific support for integration
tests, including running external and remote processes, and for
regression tests, including gathering and checking process outputs.

Tezt depends only on a few external libraries: `re` for regular
expressions, `lwt.unix` for concurrency, and `ezjsonm` for reading and
writing JSON values.

For running processes on remote machines, Tezt relies on the existence
of an `openssh` configuration on the different machines.

## Implementation Details
<!--
- Describe the file structure and the location of the main components.
- Other relevant implementation details (e.g., global invariants,
  implementation design rationale, etc.).
- Testing specifics, build-system specifics, etc. as needed.
-->

The components of Tezt are listed in file `lib/tezt.ml`, which defines
the whole external API of the Tezt library, and briefly describes the
purpose of each component.
