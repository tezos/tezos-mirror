<!-- Etherlink sequencer merge request template. -->


# What

<!-- Explain what your MR does without going into details. -->

# Why

<!-- Explain the motivation for your work. -->

# How

<!-- Explain how your MR achieves what it says it does and why it is a good way. -->
<!-- Discuss possible side-effects and other solutions you have considered. -->

# Manually testing the MR

# Checklist

- [ ] Document the interface of any function added or modified (see the [coding guidelines](https://tezos.gitlab.io/developer/guidelines.html))
- [ ] Document any change to the user interface, including configuration parameters (see [node configuration](https://tezos.gitlab.io/user/node-configuration.html))
- [ ] Provide automatic testing (see the [testing guide](https://tezos.gitlab.io/developer/testing.html)).
- [ ] For new features and bug fixes, add an item in the appropriate changelog (`docs/protocols/alpha.rst` for the protocol and the environment, `CHANGES.rst` at the root of the repository for everything else).
- [X] Select suitable reviewers using the `Reviewers` field below.
- [X] Select as `Assignee` the next person who should [take action on that MR](https://tezos.gitlab.io/developer/contributing.html#merge-request-assignees-field)

/assign @lthms @sribaroud @vch9 @picdc

/assign_reviewer @lthms @sribaroud @vch9 @picdc

/labels ~evm::sequencer

/milestone %"Etherlink: sequencers"
