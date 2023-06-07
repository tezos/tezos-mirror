<!--
Smart rollup node template. -->

<!--
Thanks you for taking the time to contributing to the Tezos project!

Make sure to read our Contributing guide (https://tezos.gitlab.io/developer/contributing.html) and the Merge process description (https://tezos.gitlab.io/developer/merge_team.html). -->

# Context

<!--
Describe the feature this MR introduces or the bug that it fixes.

Refer to corresponding issues if applicable (writing "Related: `<issue number>` or "Fixes: `<issue number>`" accordingly).

Specify related merge requests, specifically dependencies. -->

# Manually testing the MR

<!--
Describe how reviewers and approvers can test this MR. -->

# Checklist

- [ ] Document the interface of any function added or modified (see the [coding guidelines](https://tezos.gitlab.io/developer/guidelines.html))
- [ ] Document any change to the user interface, including configuration parameters (see [node configuration](https://tezos.gitlab.io/user/node-configuration.html))
- [ ] Provide automatic testing (see the [testing guide](https://tezos.gitlab.io/developer/testing.html)).
- [ ] For new features and bug fixes, add an item in the appropriate changelog (`docs/protocols/alpha.rst` for the protocol and the environment, `CHANGES.rst` at the root of the repository for everything else).
- [X] Select suitable reviewers using the `Reviewers` field below.
- [ ] Select as `Assignee` the next person who should [take action on that MR](https://tezos.gitlab.io/developer/contributing.html#merge-request-assignees-field)

/assign me

/assign_reviewer @mebsout @lthms @sribaroud @vch9 @andrea.cerone

/labels ~scoru ~scoru::node

/milestone %"Smart Rollup Node and UX"
