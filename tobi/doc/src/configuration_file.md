# Tobi: configuration file (`tobi/config`)

Tobi's configuration file is located at `tobi/config`.
It contains the list of components as well as the paths where the sources
of those components can be found.

## Component versions and commits

For a component to be defined in version V, it must be defined in `tobi/config` in commit V.

In particular, if you try to install a component without specifying its version,
this component must be defined in `tobi/config` in `HEAD`.
In other words, `tobi/config` from the working directory is ignored.
For your changes to take effect, you must commit them.

## Syntax

### Whitespace

Whitespace characters are spaces, form feed, carriage returns, line feeds, and tabs.

Whitespace is any sequence of whitespace characters.
The empty string in particular is whitespace.

### `KEY`

`KEY`s are any sequence of characters except `:`.
Whitespace around `KEY`s is ignored.

Examples:

- `a` is a `KEY`
- `this is a key` is a `KEY`
- `this/is/a/key` is a `KEY`
- `this is: not a key` is not a `KEY`
- ` some  key   ` is equivalent to `some  key`

### `VALUES`

`VALUES` are lists of individual values separated by `,`.
Individual values are any sequence of characters except `,`.
Whitespace around individual values is ignored.

Examples:

- `a` is a `VALUES` with one value: `a`
- `a,b,c:d` is a `VALUES` with three values: `a`, `b` and `c:d`
- `  some, values  ,with   , whitespace around ` is a `VALUES` with
  four values: `some`, `values`, `with` and `whitespace around`.

### Empty lines and comments

Lines composed only of whitespace are ignored.
Lines starting with whitespace followed by a `#` are comments and are also ignored.

Examples:

    # This line is a comment and is ignored
        #This line is also a comment
    # The following line is not a comment but it is ignored nonetheless

    # The following line is also a comment
                 #

### Other lines

Any other line must be of the form `KEY:VALUES` or `KEY`.

## Low-level semantics

From a low-level perspective, the configuration file is a mapping from *keys*
to lists of *values*. This mapping is obtained by iterating on lines as follows.

- Initially, the mapping is empty.
- Lines of the form `KEY:VALUES` cause `KEY` to be associated to `VALUES`.
  If `KEY` was already associated to some other list of values L,
  it is now associated to L concatenated with `VALUES` instead, in this order.
- Lines of the form `KEY` cause `KEY` to be present in the mapping.
  If it was not already present, it is now associated to the empty list.

See also [Why this configuration file format](why_this_configuration_file_format.md).

Example:

    a: u
    b: v
    a
    c
    a: w, x

Gives the following mapping:

- key `a` is associated to values `u`, `w` and `x`
- key `b` is associated to value `v`
- key `c` is associated to the empty list of values

## Component definitions

From a high-level perspective, the configuration file is a mapping from component name
to component paths. Each key in the key-values mapping defined in the previous section,
with the exception of `__pervasive`, is the name of a component;
and the associated list of values is the list of paths that are needed to build
this component.

`__pervasive` is a special key which defines a list of paths that are needed to build
all components. One can see `__pervasive` as a short-hand to avoid copy-pasting
a list of paths in the definition of all components, with one small difference:
Tobi may decide not to print those paths to avoid being too verbose.

Example:

    # Paths that are needed by all components
    __pervasive: dune, dune-project
    # Define component "tobi", located in "tobi/src1"
    tobi: tobi/src1
    # Define component "tibo", located in two directories
    tibo: tibo/src1, tibo/src2
    # Add another path that is needed by "tobi"
    tobi: tobi/src2
    # More paths that are needed by all components
    __pervasive: dune.inc
