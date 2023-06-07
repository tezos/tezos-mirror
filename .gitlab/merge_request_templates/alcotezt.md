<!--
Template for the Alcotezt porting MRs (https://gitlab.com/tezos/tezos/-/milestones/65) -->

<!--
Thanks you for taking the time to contributing to the Tezos project!

Make sure to read our Contributing guide (https://tezos.gitlab.io/developer/contributing.html) and the Merge process description (https://tezos.gitlab.io/developer/merge_team.html). -->

# Context

Related to: !6760 and #4741.

This MR applies the Alcotezt wrapper on the tests: `[TEST_FOLDERS]`

This wraps the tests in a Tezt compatibility layer. In short, this
means that the above tests are now executed through Tezt, enabling an
improved UI and automated load balancing. For more info on the
Alcotezt rationale, see !6760.

Practically, this entails that this MR will typically contain:
 - Modifications to `manifest/main.ml` for the tests in question to use
   the `tezt` instead of `test` registration function, and regenerate
   dune and opam files.
 - Extending the compatibility layer `tezt/lib_alcotezt`
 - Modifying the tests in question to better fit Alcotezt


<!--
Describe the feature this MR introduces or the bug that it fixes.

Refer to corresponding issues if applicable (writing "Related: `<issue number>` or "Fixes: `<issue number>`" accordingly).

Specify related merge requests, specifically dependencies. -->

# Manually testing the MR

<!--
Describe how reviewers and approvers can test this MR. -->

## Locally

Running the test:

```
dune exec [TEST_FOLDER]/main.exe -- --info
```

(passing `--info` prints individual Alcotest cases executed)

The tests in `[TEST_FOLDER]` no longer register a `runtest`
alias. Instead, a `runtezt` alias is registered:

```
dune build @[TEST_FOLDER]/runtezt
```

When registered in manifest with the `tezt` function, the test modules
are automatically linked with the main tezt entrypoint
`tezt/tests/main.exe`. This means that one can also run the tests with:

```
alias tezt='dune exec tezt/tests/main.exe --'
tezt --list --file [TEST_FILE]
```

## In the CI

As the tests are linked with `tezt/tests/main.exe`, they run in the CI
in the `tezt` jobs. The GitLab interface offers little 
to find individual test cases in the set of tezt jobs, but you can
download the Tezt records from a pipeline on this MR
(e.g. `[PIPELINE_ID]`) and check that they contain the test in
question:

```
PIPELINE=[PIPELINE_ID] dune exec ./tezt/records/update.exe
cat tezt/records/*.json | jq -c 'map(select(.file=="[TEST_FILE]"))[]'
```

# Checklist

- [ ] Document the interface of any function added or modified (see the [coding guidelines](https://tezos.gitlab.io/developer/guidelines.html))
- [ ] Document any change to the user interface, including configuration parameters (see [node configuration](https://tezos.gitlab.io/user/node-configuration.html))
- [ ] Provide automatic testing (see the [testing guide](https://tezos.gitlab.io/developer/testing.html)).
- [ ] For new features and bug fixes, add an item in the appropriate changelog (`docs/protocols/alpha.rst` for the protocol and the environment, `CHANGES.rst` at the root of the repository for everything else).
- [x] Select suitable reviewers using the `Reviewers` field below.
- [ ] Select as `Assignee` the next person who should [take action on that MR](https://tezos.gitlab.io/developer/contributing.html#merge-request-assignees-field)

/assign me

/assign_reviewer @abate @arvidnl

/label ~"test ⚒" ~tests::tezt

/milestone %"Unit tezts / Alcotezt"
