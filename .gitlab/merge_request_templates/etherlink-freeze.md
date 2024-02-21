# What

The commit for the next ghostnet upgrade is: RELEASE_COMMIT

# Steps

## Freeze the kernel

Prepare the ghostnet kernel:
```
$ git checkout RELEASE_COMMIT
$ make -f etherlink.mk evm_kernel.wasm
$ cp evm_kernel.wasm etherlink/kernel_evm/kernel/tests/resources/ghostnet_evm_kernel.wasm
```

Reset the non regression test:
```
$ tezt -f evm_rollup.ml --title 'Alpha: Regression test for Ghostnet kernel' --reset-regressions
```

## Freeze the node

Copy dev into prod:
```
$ cd etherlink/bin_node
$ rm -rf lib_prod
$ cp -r lib_dev lib_prod
```

Replace the various dev mention:
```
$ find . -type f -exec sed -i 's/dev\"/prod\"/g' {} +
$ find . -type f -exec sed -i 's/\_dev/\_prod/g' {} +
```

Update manifest:
```
$ cd ../../
$ make -C manifest
```
At this point you might need to manually update the dependencies in `manifest.ml`. You simply have to copy the list of dependencies from dev to prod.

## Remove migration code if any

Any migration code should be removed and goes back to:
```
if STORAGE_VERSION == current_version + 1 {
  // MIGRATION CODE - START
  // MIGRATION CODE - END
  store_storage_version(host, STORAGE_VERSION)?;
  return Ok(MigrationStatus::Done);
}
```

## Test

Simply compile everything and run the tests!
```
$ make octez-evm-node && make -f etherlink.mk evm_kernel.wasm
$ tezt -f evm_sequencer.ml -f evm_rollup.ml -j 4 --keep-going
```

# Checklist

- [ ] Freeze the kernel and node changelogs and create the NEXT section.
- [ ] Freeze the kernel.
- [ ] Freeze evm-node.
- [ ] Remove migration code if any.
