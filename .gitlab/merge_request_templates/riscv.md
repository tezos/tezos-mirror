<!-- 
    Link Linear issues using magic words. Examples of these are "Closes RV-XXX", "Part of RV-YYY"
    or "Relates to RV-ZZZ".
-->

# What

<!--
    Summarise the changes in this MR.
-->

# Why

<!-- 
    Explain why this MR is needed.
-->

# How

<!--
    Explain how the MR achieves its goal. If this is trivial, you may omit it.
-->

# Manually Testing

```
make -C src/riscv all
```

# Benchmarking

<!--
    Measure the impact on performance of this MR on your machine and the benchmark machine.
    Fill in the table below.
-->

|  | `master` | This MR | Improvement |
|--|----------|---------|-------------|
| $MyMachine | X.XXX TPS | X.XXX TPS | XX.XX% |
| Benchmark Machine | Y.YYY TPS | Y.YYY TPS | YY.YY% |

# Tasks for the Author

- [ ] Link all Linear issues related to this MR using magic words (e.g. part of, relates to, closes).
- [ ] Eliminate dead code and other spurious artefacts introduced in your changes.
- [ ] Document new public functions, methods and types.
- [ ] Make sure the documentation for updated functions, methods, and types is correct.
- [ ] Add tests for bugs that have been fixed.
- [ ] Put in reasonable effort to ensure that CI will pass.
- [ ] Benchmark performance and populate the table above if needed.
- [ ] Write commit messages to reflect the changes they're about.
- [ ] Self-review your changes to ensure they are high-quality.
- [ ] Complete all of the above before assigning this MR to reviewers.

/label ~riscv
/draft
/assign me

<!--
    Once the MR is ready, run the following GitLab commands.
-->

```
/assign @ole.kruger
/assign @victor-dumitrescu
/assign @felix.puscasu1
/assign @anastasia.courtney
/assign @emturner
/assign @santnr

/assign_reviewer @ole.kruger
/assign_reviewer @victor-dumitrescu
/assign_reviewer @felix.puscasu1
/assign_reviewer @anastasia.courtney
/assign_reviewer @emturner
/assign_reviewer @santnr

/unassign me
/unassign_reviewer me

/ready
```
