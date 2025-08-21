# How `tobi reset` works

Tobi reset simply browses the contents of `_opam` recursively.
Each time it encounters a symbolic link, it checks whether this link
leads inside `_tobi`. If it does, it removes it.

This means that `tobi reset` works even if `_tobi` was deleted.
After `tobi reset`, `_opam` is clean, at least concerning Tobi.
