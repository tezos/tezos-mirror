Profiler PPX
====================

The profiler PPX is an OCaml preprocessing tool allowing to use the ``Profiler``
functions in any part of the code. The PPX allows to choose at compile time if
we want to have the profiler enabled or not.

This guide is aimed at explaining how to use the PPX, not the ``Profiler``. For
explanations about the profiler, please look at this :doc:`page <./profiler_module>`.

*Both the PPX rewriter (our specific instance defined in* :package-api:`the
Ppx_profiler module <octez-libs/Ppx_profiler/index.html>`) *and the PPX, the
mechanism used to preprocess files in OCaml, are called PPX in this document.*

Why a Profiler PPX?
-------------------

After having created a profiler, you can create a wrapper around it in any file
with

.. code-block:: OCaml

   module Profiler = (val Profiler.wrap my_profiler)

That can be used like this

.. code-block:: OCaml

   Profiler.aggregate_f "advertise mempool" @@ fun () ->
   advertise pv_shell advertisable_mempool;

   let* _res =
     Profiler.aggregate_s "set mempool" @@ fun () ->
     set_mempool pv_shell our_mempool
   in ...

The issue with this direct approach is that it creates wrapper around functions
that may hinder their functioning. This is not wanted since the profiler is only
used by devs hence the use of a PPX that is controlled by an environment variable.

.. code-block:: OCaml

   TEZOS_PPX_PROFILER=<anything> make

Will preprocess the code before compiling (It should be noted that this is temporary and the content of this environment variable will be parsed and used in a near future to allow finer control over what PPX should be activated or not).

This will allow to preprocess

.. code-block:: OCaml

   () [@profiler.record "merge store"] ;

into

.. code-block:: OCaml

   Profiler.record "merge store" ;
   () ;

It should be noted that a ``Profiler`` module has to be available and has to
have the signature of :package-api:`the Profiler.GLOBAL_PROFILER module
<octez-libs/Tezos_base/Profiler/module-type-GLOBAL_PROFILER/index.html>` that
can be obtained with ``module Profiler = (val Profiler.wrap my_profiler)``.

Of course you can create any module with this signature but in case you didn't
name it ``Profiler`` (let's say you name it ``My_profiler``) you'll have to
declare your PPX attribute with a ``profiler_module`` field:

.. code-block:: OCaml

   () [@profiler.record {profiler_module = My_profiler} "merge store"] ;

This will be preprocessed into

.. code-block:: OCaml

   My_profiler.record "merge store" ;
   () ;


How to use this PPX?
--------------------

There are three types of functions in the Profiler library.

1. Inline functions
^^^^^^^^^^^^^^^^^^^

These functions are (for details about them, look at the :doc:`./profiler_module`
document)

- ``aggregate : ?verbosity:verbosity -> string -> unit``
- ``mark : ?verbosity:verbosity -> string list -> unit``
- ``record : ?verbosity:verbosity -> string -> unit``
- ``stamp : ?verbosity:verbosity -> string -> unit``
- ``stop : unit -> unit``
- ``reset_block_section: Block_hash.t -> unit`` (a utility function that calls
  ``stop`` and ``record`` for each new block profiled)

The PPX allows to replace

.. code-block:: OCaml

   Profiler.reset_block_section Block_repr.hash new_head;
   Profiler.record "merge store";
   ...

with

.. code-block:: OCaml

   ()
   [@profiler.reset_block_section Block_repr.hash new_head]
   [@profiler.record "merge store"] ;
   ...

You can also decompose it to be sure of the evaluation order:

.. code-block:: OCaml

   () [@profiler.reset_block_section Block_repr.hash new_head] ;
   () [@profiler.record "merge store"] ;
   ...

2. Wrapping functions
^^^^^^^^^^^^^^^^^^^^^

These functions are:

- ``aggregate_f : ?verbosity:verbosity -> string -> (unit -> 'a) -> 'a``
- ``aggregate_s : ?verbosity:verbosity -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t``
- ``record_f : ?verbosity:verbosity -> string -> (unit -> 'a) -> 'a``
- ``record_s : ?verbosity:verbosity -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t``
- ``span_f : ?verbosity:verbosity -> string list -> (unit -> 'a) -> 'a``
- ``span_s : ?verbosity:verbosity -> string list -> (unit -> 'a Lwt.t) -> 'a Lwt.t``

The PPX allows to replace

.. code-block:: OCaml

   (Profiler.record_f "read_test_line" @@ fun () -> read_test_line ())
   ...

with

.. code-block:: OCaml

   (read_test_line () [@profiler.record_f "read_test_line"])
   ...

3. Custom functions
^^^^^^^^^^^^^^^^^^^^^

You may want to declare a function that should only be used when the PPX is
active. In this case you don't want to compile nor call this function when the
PPX is disabled as it may create noise.

This PPX library provides a special construct:

- ``[profiler.custom function_application]``

With ``function_application ::= <fun_name> <args>``.

This construct will be preprocessed as ``fun_name args``.

Structure of an attribute
-------------------------

An attribute is a decoration attached to the syntax tree that allow the PPX to
preprocess some part of the AST when reading them. It is composed of two parts:

.. code-block:: OCaml

   [@attribute_id payload]

An attribute is attached to:

- ``@``: the closest node (expression, patterns, etc.),

  ``let a = "preprocess this" [@attr_id payload]``, the attribute is attached to
  ``"preprocess this"``
- ``@@``: the closest block (type declaration, class fields, etc.),

  ``let preprocess this = "and this" [@@attr_id payload]``, the attribute is
  attached to the whole value binding
- ``@@@``: *floating attributes are not used here*

The grammar for attributes can be found `in this page
<https://ocaml.org/manual/attributes.html>`_.

In the case of our PPX, the expected values are the following.

``attribute_id``
^^^^^^^^^^^^^^^^

Allows to know the kind of functions we want to use (like ``@profiler.mark`` or
``@profiler.record_s``) and to link our PPX to all the ``attribute_ids`` it can
handle. *The use of* ``profiler.`` *allows to make sure we don't have any conflict
with another PPX.*

``payload``
^^^^^^^^^^^

The payload is made of two parts, the first one being optional:

.. code-block:: OCaml

   payload ::= record? args

   record ::= { fields }

   fields ::= field ; fields | empty

   field ::=
     | level_of_detail = (Notice | Info | Debug)
     | profiler_module = module_ident

   args ::= <string> | <string list> | <function application> | ident | empty

As an example:

.. code-block:: OCaml

   f x [@profiler.aggregate_s {level_of_detail = Info} g y z] ;
   g x [@profiler.span_f {level_of_detail = Debug; profiler_module = Prof} "label"]
   ...

will be preprocessed as

.. code-block:: OCaml

   Profiler.aggregate_s ~verbosity:Info (g y z) @@ f x ;
   Prof.span_f ~verbosity:Debug "label" @@ g x
   ...

Adding functionalities
----------------------

To add a function that needs to be accepted by our PPX (let's say we want to add
``my_new_function`` that was recently added to the ``Profiler`` module) the
following files need to edited:

- ``src/lib_ppx_profiler/rewriter.ml``:

  * Add a ``my_new_function_constant`` to ``Constants``
  * Add this constant to ``Constants.constants``
  * Add ``My_new_function of content`` to ``Rewriter.t``
  * Add a ``my_new_function key location`` constructor with its accepted
    payloads (usually ``Key.Apply``, ``Key.Ident`` and ``Key.List`` or
    ``Key.String``)

- If this function needs to accept a new kind of payload (like an integer)
  you'll need to edit ``src/lib_ppx_profiler/key.ml`` and the
  ``extract_key_from_payload`` function in ``Rewriter`` (you can look at `the
  ppxlib documentation
  <https://ocaml-ppx.github.io/ppxlib/ppxlib/matching-code.html>`_)
- ``src/lib_ppx_profiler/expression.ml`` where you'll just need to add
  ``Rewriter.my_new_function`` to the ``rewrite`` function
