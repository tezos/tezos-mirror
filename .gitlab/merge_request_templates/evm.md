<!-- EVM merge request template. -->

# Context

<!--
Describe the feature this MR introduces or the bug that it fixes.

Refer to corresponding issues if applicable (writing "Related: `<issue number>` or "Fixes: `<issue number>`" accordingly).

Specify related merge requests, specifically dependencies. -->

# Manually testing the MR

You can always manually test merge request by playing with the debugger:
```
$ make -f kernels.mk evm_kernel.wasm
$ rlwrap ./octez-smart-rollup-wasm-debugger evm_kernel.wasm --inputs tezt/tests/evm_kernel_inputs.inputs.json
```

<!--
Describe how reviewers and approvers can test this MR. -->

# Checklist

- [ ] Document the interface of any function added or modified (see the [coding guidelines](https://tezos.gitlab.io/developer/guidelines.html))
- [ ] Document any change to the user interface, including configuration parameters (see [node configuration](https://tezos.gitlab.io/user/node-configuration.html))
- [ ] Provide automatic testing (see the [testing guide](https://tezos.gitlab.io/developer/testing.html)).
- [ ] For new features and bug fixes, add an item in the appropriate changelog (`docs/protocols/alpha.rst` for the protocol and the environment, `CHANGES.rst` at the root of the repository for everything else).
- [X] Select suitable reviewers using the `Reviewers` field below.
- [X] Select as `Assignee` the next person who should [take action on that MR](https://tezos.gitlab.io/developer/contributing.html#merge-request-assignees-field)

/assign me

/assign_reviewer @picdc @rodibozman @vch9

/labels ~kernel::EVM

/milestone %"Deploy Uniswap on EVM smart rollups"
