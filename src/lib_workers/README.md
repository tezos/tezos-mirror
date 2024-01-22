# Workers
<!-- Summary line: One sentence about this component. -->

The workers library: an event-driven task management abstraction somewhat akin
to actors.

## API Documentation
<!--
- Link to the external API.
-->

The API documentation is available online:
<https://tezos.gitlab.io/api/odoc/_html/octez-libs/tezos_workers.html>


## Overview
<!--
- Describe the purpose of this component.
- Describe the interaction of the code in this directory with the other
  components. This includes dependencies on other components, for instance.
-->

The workers library provides local event loops so that different components of
the Octez software can be somewhat isolated. Each workers maintains its own
queue of tasks, which allows for more control over scheduling than with the
global Lwt scheduler.


## Implementation Details
<!--
- Describe the file structure and the location of the main components.
- Other relevant implementation details (e.g., global invariants,
  implementation design rationale, etc.).
- Testing specifics, build-system specifics, etc. as needed.
-->

The library is made of a single compilation unit (`worker.ml`/`worker.mli`). The
bulk of it consists of types and signatures declaration, with a single functor
using them.
