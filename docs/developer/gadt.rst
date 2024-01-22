========================================
Generalized Algebraic Data Types (GADTs)
========================================

GADTs are a recent extension of OCaml, which is widely used in the Octez
code-base, especially in the protocol. They serve many important purposes, most
notably increasing type safety of the Michelson interpreter. This page reviews
some of the use cases of GADTs in Octez codebase and explains what role do they
play in the design.

For an explanation of the GADT concept itself, try one of the links below:

  - `Algebraic Data Types`_ (introduction to OCaml composite types)
  - `Generalized Algebraic Data Types`_ (OCaml manual)
  - `A simple expression evaluator`_ (example)


Ensuring type safety of the Michelson interpreter
=================================================

One of the most important use cases for GADTs in Octez is ensuring type safety
of the Michelson interpreter. Obviously, because Michelson scripts manage
possibly large amounts of funds, it is extremely important that the interpreter
is bug-free and reliable. The static type checking of Michelson expressions
assures that no operation will ever be executed on a value of another type than
the one this operation expects. Moreover, the Michelson type checking is
directly implemented as OCaml type checking, so that the correctness of the
latter entails the correctness of the former.

The (simplified) definition of Michelson expressions type is presented below
(see the :src:`src/proto_alpha/lib_protocol/script_typed_ir.ml` module for the
actual definition):

.. code-block:: ocaml

   type 't ty =
     | Unit_t : type_annot option -> unit ty
     | Int_t : type_annot option -> z num ty
     | String_t : type_annot option -> string ty
     | Mutez_t : type_annot option -> Tez.t ty
     | Bool_t : type_annot option -> bool ty
     | Pair_t :
       ('a ty * field_annot option * var_annot option)
       * ('b ty * field_annot option * var_annot option)
       * type_annot option
       -> ('a, 'b) pair ty
     | Union_t :
       ('a ty * field_annot option)
       * ('b ty * field_annot option)
       * type_annot option
       -> ('a, 'b) union ty

In the above type **values** of type ``ty`` represent **types** of Michelson
expressions. These types are ascribed to Michelson values of corresponding types
and then used to type check these expressions. However, these values are
expressed in terms of OCaml types of the underlying representations of these
expressions. This way the OCaml compiler itself can verify that the Michelson
interpreter only evaluates well-typed expressions. The ``'t`` type variable here
never appears on the right-hand side of the definition. Indeed, the GADT allows
``'t`` to be solely determined by the constructor used to create a value of type
``ty``. This way a Michelson type represented by a ``'t ty`` value is directly
linked to OCaml type ``'t``.

However, in case of compound types like pairs, unions and functions, ``'t``
depends on more than just the constructor. After all, these types are
polymorphic just like in OCaml. This is why the ``Pair_t`` constructor for
instance accepts (among other arguments) two values of the same type ``ty``, but
parameterized independently. These arguments determine types of the components
of the pair. Note that whatever types that will be, they will be included in the
final type of ``Pair_t``. This way concrete types "propagate" through variables
of polymorphic types so that the OCaml compiler can always know what types of
values it works with when evaluating a Michelson expression.

Also note that on the type level, constructor ``Union_t`` is very similar to
``Pair_t``. Indeed, the difference between these types lies in how their values
are constructed, but types themselves are very similar.

Building complex expressions
============================

A syntactically correct Michelson expression is not necessarily well-typed.
Hence a need presents itself to create a representation where all
sub-expressions are assigned their proper types so that it can be checked if
they match. Such an internal representation of an expression is usually called
an *abstract syntax tree* (or AST for short). This need for strongly-typed ASTs
is already fulfilled by the kind of type definitions above.

However, when one tries to gather a lot of typed expressions into a single data
structure, a problem immediately arises: with usual ADTs and the kind of GADTs
show above the type of such data structure must be parametrized by all type
variables appearing inside. On the other hand, when dealing with abstract
Michelson expressions, one would like them to be just of type ``expr`` (i.e.
monomorphic). Fortunately GADTs allow to omit type variables appearing inside,
thus making a type look monomorphic from user's perspective:

.. code-block:: ocaml

  type ex_ty = Ex_ty : 'a ty -> ex_ty

This type is defined in
:src:`src/proto_alpha/lib_protocol/script_ir_translator.mli` As one can see it's
just a wrapper around ``'a ty``. However, its function is to hide the ``'a``
variable from the user (and the compiler), by including it only on the
right-hand side. This way it's possible to form a list (or any data structure
for that matter) consisting of arbitrary type expressions. They don't have to
match. On the other hand, thanks to the construction of ``'t ty`` type, no type
information is really lost. Indeed, because of how ``'t ty`` is constructed, its
precise type can be retrieved by virtue of type inference. Since the compiler
knows that for instance ``Unit_t`` is **always** of type ``unit ty``, then in a
pattern matching branch on that constructor it will conclude that ``'a ty`` must
be indeed ``unit ty``.

While usual parametric polymorphism is often associated with the **universal
quantifier** (for all) in logic, this construction is associated with the
**existential quantifier** (hence the name ``ex_ty``). In type ``'a ty`` for all
possible types ``'a`` there exists a corresponding type ``'a ty`` (universal
quantification), whereas in type ``ex_ty`` there exists some ``'a`` which is
contained by its value, but it's unknown, which is it exactly (existential
quantification).

Type equality witness
=====================

Consider the following GADT:

.. code-block:: ocaml

   type (_, _) eq = Refl : ('t, 't) eq

This is the type equality witness. Note that the value constructor ``Refl`` does
not require any arguments, so it can be created at will just like ``()``.
Indeed, both its type parameters are phantom (i.e. unrelated to any components
of the actual value, but carrying some logical information about the type).
Nonetheless, a value of type ``('a, 'b) eq`` can be created if and only if
``'a`` and ``'b`` are the same type. If a value of such type is present in a
scope, the compiler will happily unify ``'a`` and ``'b``.

This type is defined in the protocol environment, in ``Equality_witness`` module
and used throughout the protocol to dynamically check for type equality of
values. It is not possible to write a general function checking equality of
types; it's only possible for particular examples. One such example can be found
in :src:`src/proto_alpha/lib_protocol/script_ir_translator.ml` in function
``merge_types``. It either produces a witness that two ``'t ty`` values are of
the same type or ``None`` if that's not the case.


.. _Algebraic Data Types: https://ocaml.org/docs/basic-data-types
.. _Generalized Algebraic Data Types: https://ocaml.org/manual/gadts.html
.. _A simple expression evaluator: https://blog.mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html
