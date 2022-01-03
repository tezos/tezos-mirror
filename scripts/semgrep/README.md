# Linting via semgrep

This directory includes rules for linting the OCaml source code of Octez.


## Usage

After [installing semgrep](https://semgrep.dev/docs/getting-started/) simply
execute

```
make check-ocaml-linting
```

This command will analyse all the relevant files and will purposefully ignore
files such as already hashed protocols.

Alternatively, if you want to test only the library you are currently working
on, say, `src/lib_validation`, you can execute

```
semgrep --metrics=off --error --lang ocaml --config scripts/semgrep/ src/lib_validation/
```

## CI

The CI also performs linting checks. Note that, to avoid issues related to
changes in `semgrep`, the CI uses a fixed version of the tool. If you need to
run the same version locally you need to install `docker` and then run

```
docker run --rm -v "${PWD}:/src" returntocorp/semgrep-agent:sha-932a84b --metrics=off --error -l ocaml -c scripts/semgrep/ --exclude "src/proto_[0-9]*/lib_protocol/*.ml*" src/
```

This command has the exact same output as the CI, no matter what updates have
been pushed to the upstream tool.


## Rules

This directory contains our own in-house rules.
