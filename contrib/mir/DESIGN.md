### Design Rationale

#### Parser

The parser is generated using the Lalrpop parser generator library. We didn't
involve Micheline right now because it was not required within the current
scope.

There are some libraries that we could have used to parse Michelson, the most
complete being `airgap-it/tezos-rust-sdk`. However, we ended up not using it, since
we thought that the layout of its data structures did not match well with
our plan.

We thought about using this library for now (as it could work with the current
scope which is quite limited), with the possibility of switching to something
else later, but also decided against it, because we considered that it would be
too hard to switch away from such a basic component later in the project.

We used the Lalrpop library because we had some experience with it, and it seemed
to work well in our initial trials.
