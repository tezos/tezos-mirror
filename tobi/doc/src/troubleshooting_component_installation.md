# Troubleshooting component installation

## Installation fails

Read this if `tobi install COMPONENT` fails.

### Check which component failed to install

Check that `tobi install COMPONENT` failed while trying to
install `COMPONENT` and not one of its dependencies. Find the last line that looks like:

    Installing: XXX

in Tobi's output. If `XXX` is not `COMPONENT`, you can ask the authors of `XXX` to follow
this guide to fix `XXX`.

### Check that all your changes have been committed

`tobi install COMPONENT` is equivalent to `tobi install COMPONENT.HEAD`,
i.e. it uses the files from the current commit, not from the working directory.
So make sure all your changes have been committed.

### Check that it compiles without Tobi

Try to compile without Tobi.
For instance, for an OCaml component, you can try:

    dune build -p COMPONENT

### Check Tobi's configuration file

It may be the case that Tobi is not aware of a file that is needed to build `COMPONENT`.
To check this for a specific file with path `F`, first
find all the entries for your component in `tobi/config`:

    grep ^__pervasive: tobi/config
    grep ^COMPONENT: tobi/config

For each path `P` in the resulting list, check if `F` is present:

    git ls-path P | grep F

If this always returns an empty output,
[add `F` or one of its parent directory to `tobi/config`](how_to_declare_a_component.md).

### Check the Opam file

Check that all components that `COMPONENT` depends on are declared in the `depends`
section of `opam/COMPONENT.opam`.

## Component is not installed correctly

Read this if `tobi install COMPONENT` succeeds but building other components that depend
on `COMPONENT` fails because `COMPONENT` is not installed correctly.

### Check your environment

Make sure that the contents of `_opam` is visible by other components.
This is usually achieved by setting the `OPAM_SWITCH_PREFIX` environment variable to `_opam`,
which is itself a consequence of `eval $(opam env)`.

### Check the Opam directory

Check that the files that are needed by other components are in `_opam`.
Note that Tobi installs symbolic links to Tobi's cache.
Make sure that the links lead to files that actually exist in the cache as well.

### Check the Opam install file

If a file is missing from `_opam`, this is usually due to an issue with the Opam `.install`
file of your component.

Remove or move away file `COMPONENT.install` if it exists.
Open `opam/COMPONENT.opam` and find the `build` section.
Run those build instructions manually.
Ignore those that are conditioned by `{with-test}`.

Check that this produced a `COMPONENT.install` file.
If this is not the case, the build instructions need to be adapted.
OCaml components are usually built with `dune build -p`, which produces
the `.install` file automatically. But components written in other languages,
even partially, may require more work.

If the `.install` file was produced, check that it lists all the files
that need to be installed.

See also [What Tobi needs from Opam files](what_Tobi_needs_from_Opam_files.md).
