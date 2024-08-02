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
| $MyMachine | X.XXX TPS (1/XXX native) | X.XXX TPS (1/XXX native) | XX.XX% |
| Benchmark Machine | Y.YYY TPS (1/YYY native) | Y.YYY TPS (1/YYY native) | YY.YY% |

# Tasks for the Author

- [ ] All Linear issues that relate to this MR were linked using magic words (e.g. part of, relates to, closes)
- [ ] There is no dead code and other spurious artefacts in my changes
- [ ] All newly introduced public functions, methods and types are documented
- [ ] Documentation of changed functions, methods and types is up-to-date
- [ ] There are tests which cover bugs which have been fixed, if any
- [ ] Reasonable effort has been made towards making CI pass
- [ ] Performance has been benchmarked and populated in the table above
- [ ] All the above was completed before assigning this MR to reviewers

/label ~riscv
/draft
/assign me

<details>

<summary>Click to reveal GitLab Commands</summary>

<!--
    Once the MR is ready, run the following GitLab commands.
-->

```
/assign @ole.kruger
/assign @victor-dumitrescu
/assign @felix.puscasu1
/assign @anastasia.courtney
/assign @emturner

/assign_reviewer @ole.kruger
/assign_reviewer @victor-dumitrescu
/assign_reviewer @felix.puscasu1
/assign_reviewer @anastasia.courtney
/assign_reviewer @emturner

/unassign me
/unassign_reviewer me
```

</details>
