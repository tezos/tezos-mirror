# Why both Opam files and `tobi/config`

There are two places in which components are defined: their Opam files
in the `opam` directory, and `tobi/config`. Isn't it redundant?

## Opam files miss the paths

Tobi needs to know which paths are necessary to build a component.
See [What Tobi needs from Opam files](what_Tobi_needs_from_Opam_files.md).
This information is not present in Opam files.

The Opam file format seems rather flexible though.
It is probably possible to add the paths to the Opam files,
if only in comments.

## Different purposes

Tobi needs to know the list of components.
Tobi could assume that there is exactly one Opam file per component.
It could then just list the contents of the `opam` directory to get the list of components.

But while it is true that Tobi requires all components to have an Opam file,
it is not necessarily the case that each Opam file correspond to a component.
In other words, we may need to have Opam files that are not visible to Tobi.

Like for paths, it may be possible to have a custom field in each Opam file
stating whether the package is a component.
But this would be more difficult to implement and probably be less efficient
(see the next section).

## Efficiency

It is probably faster to read a single `tobi/config` file
than to list the contents of the `opam` directory,
especially since Tobi needs to do that through Git (for a given commit).
This claim was not checked though, and the actual impact is probably
negligible anyway.
