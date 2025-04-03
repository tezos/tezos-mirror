# How `tobi list --installed` works

The way `tobi list` works is rather obvious: it reads its configuration file
and iterates over the defined components. But what about the `--installed` flag?
How does Tobi detect whether a component is installed?

## Heuristic

To know whether `COMPONENT` is installed, Tobi tries to find a valid `COMMIT_HASH`
such that `_tobi/cache/COMPONENT/COMMIT_HASH/COMPONENT.install` exists.
If none can be found, Tobi assumes the component is not installed.

If `_tobi/cache/COMPONENT/COMMIT_HASH/COMPONENT.install` exists,
Tobi reads it. This is a list of files to install for this component.
Tobi takes the first one and checks whether the symbolic link exists
in the relevant location under `_opam`.
Tobi also checks whether the link is actually a link and whether
it leads to the expected location in `_tobi/cache`.
In particular it checks whether this location matches the expected `COMMIT_HASH`.

If everything checks out, Tobi assumes the component is installed, for version `COMMIT_HASH`.
If not, Tobi continues with another `COMMIT_HASH`.

## Caveats

If one deletes the cache, the symbolic links from `_opam` to `_tobi/cache` still exist.
This results in a component being installed, but this installation is broken
and `tobi list --installed` won't list the component as installed.

If one removes all symbolic links from `_opam` to `_tobi/cache/COMPONENT/COMMIT_HASH`
except the first one that is listed in `_tobi/cache/COMPONENT/COMMIT_HASH/COMPONENT.install`,
Tobi will believe that `COMPONENT` is installed, even though its installation is broken.

If some symbolic links in `_opam` lead to a commit hash while some others lead to another
commit hash, Tobi will believe one commit hash is installed, and will not notice the other,
even though the installation is broken.

Basically, this is `tobi list`, not `tobi check-install`.
In any case, `tobi reset` will fix broken installations,
and if the components are still in the cache, installing them back will be very quick.
