Rewriting Micheline with ``tezos-micheline-rewriting``
======================================================

``tezos-micheline-rewriting`` allows to construct rewriting rules
on Micheline, enumerate matches and perform rewritings. It is
fairly generic and deserves a documentation of its own.
This library manipulates the following basic elements:

- Micheline terms
- Paths
- Patterns

Informally, the library allows to find patterns in terms: the result is
a list of paths, corresponding to the location of these patterns.
Rewriting is then performed by substituting a subterm at a given path
by another one. The library also provides facilities for manipulating
hash-consed terms and patterns, which is crucial in all non-trivial
applications.

Constructing Micheline terms
----------------------------

The type of Micheline terms has two parameters: the type of locations
and the type of primitives. We abstract away from the underlying
representation of terms via the  ``Micheline_sig.S`` module type:

.. code-block:: ocaml

  module type S = sig
    type label
    type head
    type node = (label, head) Micheline.node
    val default_label : label
    val int : Z.t -> node
    val string : string -> node
    val bytes : Bytes.t -> node
    val prim : head -> node list -> string list -> node
    val seq : node list -> node
    val label : node -> label
  end

We provide two functors for manipulating Micheline terms:

- ``Micheline_without_hash_consing.Make``
- ``Micheline_with_hash_consing.Make``

Both functors provide implementations of the ``Micheline_sig.S`` signature
(with distinct type constraints however). They both take as input some additional
structure on the type of primitives, as specified in the ``Signature.S`` module type:

.. code-block:: ocaml

  module type S = sig
    type t
    val compare : t -> t -> int
    val hash : t -> int
    val pp : Format.formatter -> t -> unit
  end

ie primitives must be totally ordered, hashable and pretty-printable.

``Micheline_without_hash_consing.Make`` has type

.. code-block:: ocaml

   functor (X : Signature.S) (Label : sig type t val default : t end) ->  Micheline_sig.S with type label = Label.t and type head = X.t

and provides a simple overlay over the Micheline constructors, setting the label to its default value.


``Micheline_with_hash_consing.Make`` has type

.. code-block:: ocaml

   functor (X : Signature.S) (P : sig val initial_size : int option end) -> Micheline_sig.S with type label = hcons_info and type head = X.t

We use the label slot to store hash-consing information on terms: the type ``hcons_info``
contains a unique id for the term and its hash. This implementation of ``Micheline_sig.S``
performs maximal sharing: it is important that terms are only constructed through
the interface provided by the module for the required invariants to hold true. A weak
hash table is used to store terms, its initial size is fixed by ``P.initial_size``.
One can use the unique id contained in ``hcons_info`` to construct a separate map
from the term to some other annotation.

Manipulating paths in Micheline terms
-------------------------------------

A Micheline term is either a primitive application, a sequence, or one of
three atoms (int, string or byte). The primitive application and sequence
constructors have variable arity (the take lists of subterms as arguments).
It follows that one can designate a subterm of a given term as a list of
integers, each integer denoting an index in the list of subterms of either
a primitive or sequence application.

We provide two implementations of path-manipulating modules: one without
hash-consing and one with. Both implement the following signature:

.. code-block:: ocaml

  module type S = sig
    type desc = private Root | At_index of int * t
    and t = private {tag : int; hash : int; rev_path_desc : desc}
    val compare : t -> t -> int
    val root : t
    val at_index : int -> t -> t
    val concat : above:t -> under:t -> t
    val to_string : t -> string
  end

We observe that paths, contrary to our intuition, are not directed from the root
to the subterm but rather from the subterm to the root. This allows
easy hash-consing and follows the usual way paths are constructed during the
pattern matching process. The two fundamental operations are ``root``,
corresponding to an empty path, and ``at_index i p``, corresponding
to the ith subterm of the term at path ``p``.

Pattern-matching Micheline
--------------------------

The ``Pattern`` module provides two functors implementing a
small pattern description language, as well as functions for enumerating
matches of a pattern in a given term. The signature is the following:

.. code-block:: ocaml

  module type S = sig
    type head
    type path
    type t
    type plist
    type node
    val pattern_matches : t -> node -> bool
    val all_matches : t -> node -> path list
    val focus_matches : t -> path list -> path list
    val int : (Z.t -> bool) option -> t
    val string : (string -> bool) option -> t
    val bytes : (Bytes.t -> bool) option -> t
    val prim : head -> plist -> t
    val prim_pred : (head -> bool) -> plist -> t
    val seq : plist -> t
    val any : t
    val focus : t -> t
    val list_any : plist
    val list_empty : plist
    val list_cons : t -> plist -> plist
    val ( @. ) : t -> plist -> plist
    val pp : Format.formatter -> t -> unit
    val uid : t -> int
  end

The comments describing all these constructs can be found in :src:`src/lib_benchmark/lib_micheline_rewriting/pattern.mli`.
It is worth describing a subset of these functions here:

- ``pattern_matches patt node`` returns true if and only if ``patt`` matches ``node``.
- ``all_matches patt node`` returns the list of all paths in of subterms of ``node`` matching ``patt``.
- ``focus patt`` constructs a focused subpattern. There can be several focused subpatterns but
  the cannot be nested.
- ``focus_matches patt matches`` converts a list of matches for ``patt`` into a list of matches
  for the focused subpatterns of ``patt``.

The focusing mechanism allows patterns to have a contextual part, corresponding to the subterm
matched by the whole pattern, and a "point of interest" in the context, corresponding to
a subterm of the subterm matched by the whole pattern. For instance, we can match on integers
that are directly under a particular primitive, etc.

The signatures of the non-hash-consing functor is as follows:

.. code-block:: ocaml

   Make : functor (X : Signature.S) (Micheline : Micheline_sig.S with type head = X.t) (Path : Path.S) -> S with type head = X.t and type path = Path.t and type node = Micheline.node

While the hash-consing implementation has the following slightly more complicated type:

.. code-block:: ocaml

  module Make_with_hash_consing : functor
      (X : Signature.S)
      (Micheline : Micheline_sig.S
                     with type head = X.t
                      and type label = Micheline_with_hash_consing.hcons_info)
      (Path : Path.S) -> sig
    include
      S
        with type head = X.t
         and type path = Path.t
         and type node = Micheline.node

    val all_matches_with_hash_consing : t -> node -> path list
  end

I.e the default implementation of match enumeration does not use hash-consing; one has
to use ``all_matches_with_hash_consing`` to do so.

Performing substitutions
------------------------

The ``Rewrite`` module provides facilities for performing substitutions. There is
only one implementation here (as all hash-consing is taken care of in previously
described modules). The module provides a functor taking implementations for
terms, paths and patterns and provides the following functions:

.. code-block:: ocaml

  module type S = sig
    type label
    type head
    type path
    type patt
    type node = (label, head) Micheline.node
    exception Rewrite_error of string * node option
    val get_subterm : term:node -> path:path -> node
    val subst : term:node -> path:path -> replacement:node -> node
    val pattern_matches : patt -> node -> bool
    val all_matches : patt -> node -> path list
  end

The key function here is ``subst`` which performs the substitution.
The implementation proceeds as one might expect, by recursive descent
on the term together with the specified path.

An example?
-----------

An example can be found in the ``test`` subdirectory. It consists in
a reimplementation of the migration of addresses towards pairs of
addresses and chain ids in multisignature contracts.
