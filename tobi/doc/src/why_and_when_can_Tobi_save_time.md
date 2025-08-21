# Why and when can Tobi save time

## How it saves time

Tobi saves build time by not recompiling installed components.
In particular, it does not recompile them even if their source changes.

## When is it useful

For Tobi to save time, you need the following scenario.

- You are working on component C.
- C depends on component D that takes time to compile.
- You regularly pull changes made to D.
- C does not need those changes made to D.

Under this scenario, you can install D with `tobi install`
and build C with `tobi build`. Without Tobi, after pulling changes to D,
recompiling C would recompile D as well, potentially wasting a lot of time
if C does not need those changes to D. But `tobi build` only recompiles D.

## Caveat

Sometimes when working on C with its dependency D installed,
you may pull changes to D that are actually needed for C.
In that case you need to reinstall D with `tobi install`.
The issue is that it may not be obvious when you need to do so.
You may get a compilation error that does not obviously tell you that C
needs to be reinstalled. Or worse, D may compile but may no longer work at runtime.

## Relationship with the vision for the monorepo

Part of [The vision for the monorepo](the_vision_for_the_monorepo.md) is to make
the favorable scenario above more likely.
With clearly separated components and clean, stable interfaces,
it is easier to avoid the case where changes to dependencies made by other teams
need to be compiled.

In particular, with modular packaging and modular releases,
it can make sense to have component C declare that
it depends on a particular version of its dependency D.
If all engineers use Tobi to build C,
then they build C with this old version of D.
Changes made to D thus do not impact C.
When the authors of C want to use a newer version of D,
they update the version constraint.
Now if you try to build C with the old version of D installed,
Tobi could notice and tell you before even trying to build.
(This particular check is not implemented yet.)
