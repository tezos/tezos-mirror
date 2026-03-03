# REVM Evaluation

## Preliminary step

Fixtures define **state transition** tests.

1. Clone [evm-fixtures](https://github.com/functori/evm-fixtures).

2. **[OPTIONAL]** Setup a symbolic link from the repository to `etherlink/kernel_latest/revm_evaluation/fixtures/`. It should facilitate the process while working locally.

```
ln -s <evm-fixtures-path>/* etherlink/kernel_latest/revm_evaluation/fixtures
```

## Compile the binary

```
$ make -f etherlink.mk revm-evaluation-assessor
```

## Execute the assessor

The REVM evaluation assessor can be launch by simply running:

```
$ revm-evaluation-assessor --test-cases <evm-fixtures-path>
```

It is also possible to run tests specific to an evm version:

```
$ revm-evaluation-assessor --test-cases <evm-fixtures-path>/<evm-version>
```

In any case, this will create a report file named `revm_evaluation.logs`.
