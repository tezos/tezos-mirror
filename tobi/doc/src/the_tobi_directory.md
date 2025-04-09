# The `_tobi` directory

The `_tobi` directory stores Tobi's state.
For now, it only contains a cache for component artifacts.

## Cache

`_tobi/cache` is Tobi's cache.
It contains one *component cache directory* per component, named after the component.
For instance, `_tobi/cache/tobi` is the component cache directory for
the component named `tobi` (i.e. Tobi itself, if you choose to install it).

### Component cache directories

Each component cache directory contains one *version cache directory* per version
for this component.
For instance, `_tobi/cache/tobi/3e447d7e3b83309ad0c7da4e0e5244b05e69ea05`
is the version cache directory for commit `3e447d7e3b83309ad0c7da4e0e5244b05e69ea05`
of the component named `tobi`.
In the cache, versions are always full commit hashes.

### Version cache directories

Each version cache directory contains at least a file named `COMPONENT.install`,
where `COMPONENT` is the name of the component.
For instance, `_tobi/cache/tobi/3e447d7e3b83309ad0c7da4e0e5244b05e69ea05/tobi.install`.
The rest of the files are the build artifacts.

### Opam install file

The `COMPONENT.install` file in version cache directories is the
[Opam install file](https://opam.ocaml.org/doc/Manual.html#lt-pkgname-gt-install)
that was produced by the build instructions of the component
(see [What Tobi needs from Opam files](what_Tobi_needs_from_Opam_files.md)).
It contains the list of the other files in the version cache directory,
as well as where to put them in `_opam`.

### Other files: build artifacts

Other files in version cache directories are build artifacts.
They can be in subdirectories such as `_build/install/default/lib`
or `_build/install/default/bin`. Their location relative to the version cache
directory must match the contents of the Opam install file.

Usually, build artifacts do not include intermediate compilation files.
This makes `_tobi` smaller than Dune's `_build` directory, for instance.

### Example

After installing Tobi with Tobi, the cache typically looks like this:

    _tobi/cache/tobi
    ├── 3e447d7e3b83309ad0c7da4e0e5244b05e69ea05
    │   ├── _build
    │   │   └── install
    │   │       └── default
    │   │           ├── bin
    │   │           │   └── tobi
    │   │           └── lib
    │   │               └── tobi
    │   │                   ├── dune-package
    │   │                   └── META
    │   └── tobi.install

with `tobi.install` containing:

    lib: [
      "_build/install/default/lib/tobi/META"
      "_build/install/default/lib/tobi/dune-package"
    ]
    bin: [
      "_build/install/default/bin/tobi"
    ]
