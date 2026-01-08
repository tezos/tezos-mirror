# Fixtures

Fixtures define **state transition** tests.

# Get fixtures files

1. Clone [evm-fixtures](https://github.com/functori/evm-fixtures).

2. **[OPTIONAL]** Setup a symbolic link from the repository to `etherlink/kernel_latest/revm_evaluation/fixtures/`. It should facilitate the process while working locally.

```
ln -s <evm-fixtures-path>/* etherlink/kernel_latest/revm_evaluation/fixtures
```

## Update or add new fixture files

1. Clone [ethereum/execution-spec-tests](https://github.com/ethereum/execution-spec-tests)'s repository.

```
git clone https://github.com/ethereum/execution-spec-tests
```

2. Follow the instructions from the repository's [README](https://github.com/ethereum/execution-spec-tests/blob/main/README.md) to install all dependencies and generate all the needed fixture files.

As an example, a command to generate Osaka's fixtures should look like:

```
uv run fill -v tests/osaka/ --fork=Osaka -m state_test --clean
```

3. Clone [evm-fixtures](https://github.com/functori/evm-fixtures).

4. Once fixture generated, make a pull request on [evm-fixtures](https://github.com/functori/evm-fixtures).
