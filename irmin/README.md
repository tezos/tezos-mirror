# Irmin Libraries

This directory contains forked libraries from
[Irmin](https://github.com/mirage/irmin) (v3.8) that are used in the
Octez suite.

As mentioned in [its own
README](https://github.com/mirage/irmin/blob/main/README.md), Irmin
itself is primarily developed and maintained by
[Tarides](https://tarides.com), with contributions from many
[contributors](https://github.com/mirage/irmin/graphs/contributors)
from various organizations.

Like Irmin, the code in this directory is under the
[ISC](https://github.com/mirage/irmin/blob/main/LICENSE.md) license.


## Structure

| Library | Local path | Upstream |
| -------- | --------- | -------- |
| `irmin`    | `./lib_irmin` | [`src/irmin`](https://github.com/mirage/irmin/tree/main/src/irmin) |
| `irmin_pack`    | `./lib_irmin_pack` | [`src/irmin-pack`](https://github.com/mirage/irmin/tree/main/src/irmin-pack) |
| `ppx_irmin`\*    | `./lib_ppx_irmin` | [`src/ppx_irmin`](https://github.com/mirage/irmin/tree/main/src/ppx_irmin) |

\*`ppx-irmin` is not directly used by Octez, but it is a dependency of `irmin` and `irmin-pack`.


## To-do before modifying the code

- Import Irmin's tests, and ideally benches.
- Add the `irmin` directory to `source_directories` in `lint.sh` to enable formatting checks.


## FAQ

#### Why is the `irmin` directory not inside the `vendors` one?

Libraries in `vendors` cannot be dependencies of released Octez
packages, because of a technical workaround that may no longer be
needed. See [this
thread](https://gitlab.com/tezos/tezos/-/merge_requests/10905#note_1694716679). If
the build of `vendors` is changed in the future to allow for such
libraries, it may be pertinent to move the `irmin` directory there.

#### Why are the `irmin` libraries in a separate `octez-internal-libs` package, instead of e.g. `octez-libs`?

The `irmin` libraries need to remain under the ISC license. Most Octez
packages such as `octez-libs` don't have this license. That's why the
`octez-internal-libs` package has been created with the ISC license.
