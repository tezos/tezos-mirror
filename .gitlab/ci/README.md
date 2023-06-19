# CI guideline

This document describes the operating principles of the source code
around the CI.

# Implementation of job `script`s

These rules also apply to `before_script` and `after_script`.

## Inline scripts

I.e., having the `script` in the YAML definition of the job. Inline
scripts MUST only be used for small job definitions (less than 5
lines) that do not use any control flow.

## Calling out to a script

`script` definitions longer than 5 lines of code OR that use
control-flow MUST be placed in a separate shell script.

