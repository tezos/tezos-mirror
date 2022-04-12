Compile with `make`.

Run with `dune exec git-gas-diff [file]`.

For instance, create a file from `git diff` and launch the tool with:
```
git diff HEAD^ HEAD > gas_diff
dune exec git-gas-diff gas_diff
```
if the top commit is a an update of the gas for regression tests.

See `bin/main.ml` for more details.
