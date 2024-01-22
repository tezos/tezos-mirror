# Scripts description

`apply_proto_patch.sh` and `apply_env_patch.sh` aims to ease porting
patches from one protocol/environment to another.

`apply_proto_patch.sh` takes a patch as first argument and the paths of all protocols
to which the patch should be applied.

`apply_env_patch.sh` takes a patch as first argument, the env number on which it
has been written, and then the environment's numbers to which the patch should
be applied.

# Patch description

`deactivate_baking_pow.patch` can be applied to a protocol to deactivate the
computation and verification of proof of work stamps in the block header.
It is useful to be able to bake faster large number of blocks but the patched
protocol will produce and validate blocks that are invalid for the actual
protocol.
Use only for local experiments.


# Examples:
```shell
./apply_proto_patch.sh  ../../scripts/yes-stresstest.patch ../../src/proto_018_Proxford  ../../src/proto_017_PtNairob

devtools/patchs/apply_proto_patch.sh  devtools/patchs/deactivate_baking_pow.patch src/proto_01[5-9]_*


devtools/patchs/apply_env_patch.sh devtools/protocol-print/add-hack-module.patch 3 9 10

```
