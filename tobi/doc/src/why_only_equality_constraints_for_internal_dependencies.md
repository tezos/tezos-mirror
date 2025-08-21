# Why only equality constraints for internal dependencies

Tobi [requires](what_Tobi_needs_from_Opam_files.md) that version constraints
on internal dependencies, i.e. dependencies that are other components
from the same repository, are equalities.

The main reason for this is that this allows Tobi's constraint solver
to be a simple topological sort, an algorithm that runs fast and is
relatively easy to implement. Allowing more complex constraints
can very quickly turn the problem into an NP-complete one.

If the need for a richer constraint language arises, we can reevaluate.
Tobi could for instance call an external solver.
Any reasonable solver should still be able to find a solution quickly
if we restrict the set of packages to internal components.
