<!-- Etherlink sequencer merge request template. -->

# Context

# Manually testing the MR


# Checklist

- [ ] Document the interface of any function added or modified (see the [coding guidelines](https://tezos.gitlab.io/developer/guidelines.html))
- [ ] Document any change to the user interface, including configuration parameters (see [node configuration](https://tezos.gitlab.io/user/node-configuration.html))
- [ ] Provide automatic testing (see the [testing guide](https://tezos.gitlab.io/developer/testing.html)).
- [ ] For new features and bug fixes, add an item in the appropriate changelog (`docs/protocols/alpha.rst` for the protocol and the environment, `CHANGES.rst` at the root of the repository for everything else).
- [X] Select suitable reviewers using the `Reviewers` field below.
- [X] Select as `Assignee` the next person who should [take action on that MR](https://tezos.gitlab.io/developer/contributing.html#merge-request-assignees-field)

/assign @alocascio @lthms @sribaroud @vch9

/assign_reviewer @alocascio @lthms @sribaroud @vch9

/labels ~evm::sequencer

/milestone %"Etherlink: sequencers"
