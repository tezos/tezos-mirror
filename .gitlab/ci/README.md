# CI guideline

This document describes the operating principles of the source code around the CI

# Implementation of scripts linked to jobs

**Calling a script directly**

2 cases :

-   `Make` is not available in the base image
-   This is a legacy approach that is not recommended for new jobs

Special case for command `.venv/bin/activate` because it must be done in same shell as the core script, doing it from Makefile would be would be heavy and without any added value.

**Calling a `make` target that does everything directly**

Only up to 3 or 4 lines of code and no comments between lines.
`make` spins up a subshell for each script line so we use `; \` after each line to maintain the location reached by the previous `cd ${CI_PROJECT_DIR}` command.

**Calling a `make` target that itself calls a script**

It's the prefered approach.

**Having everything in the YAML**

SHOULD be the exception and MUST be explained in a comment.