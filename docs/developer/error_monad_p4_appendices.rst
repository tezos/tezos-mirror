Part 4: Appendices
-------------------

These appendices are Part 4 of 4 of the :doc:`./error_monad` tutorial.

Legacy code
~~~~~~~~~~~

(This section will be removed once the whole code-base has been updated
to use the binding operators as recommended. In the meantime, you need
to learn some legacy constructs so you can read code that hasn’t been
upgraded yet. You should not use the operators introduced in this
section to write new code.)

The legacy code is written with infix bindings instead of ``let``-style
binding operators. The binding ``>>?`` for ``result`` and ``tzresult``,
``>>=`` for Lwt, and ``>>=?`` for Lwt-``result`` and Lwt-``tzresult``. A
full equivalence table follows.

+--------------------------------------+-------------------------------+
| Modern                               | Legacy                        |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Result_syntax in         |    e >>? fun x ->             |
|    let* x = e in                     |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Result_syntax in         |    e >|? fun x ->             |
|    let+ x = e in                     |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_syntax in            |    e >>= fun x ->             |
|    let* x = e in                     |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_syntax in            |    e >|= fun x ->             |
|    let+ x = e in                     |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    e >>=? fun x ->            |
|    let* x = e in                     |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    e >|=? fun x ->            |
|    let+ x = e in                     |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ``and*``, ``and+`` (any syntax       | No equivalent, uses           |
| module)                              | ``both_e``, ``both_p``, or    |
|                                      | ``both_ep``                   |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    (e >>= ok) >>=? fun x ->   |
|    let*! x = e in                    |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    e >>?= fun x ->            |
|    let*? x = e in                    |    e'                         |
|    e'                                |                               |
+--------------------------------------+-------------------------------+

In addition, instead of dedicated ``return`` and ``fail`` functions from
a given syntax module, the legacy code relied on global values.

+--------------------------------------+-------------------------------+
| Modern                               | Legacy                        |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Result_syntax in         |    ok x                       |
|    return x                          |                               |
+--------------------------------------+-------------------------------+
| ::                                   | No equivalent, uses           |
|                                      | ``Error e``                   |
|    let open Result_syntax in         |                               |
|    fail e                            |                               |
+--------------------------------------+-------------------------------+
| ::                                   | No equivalent, uses           |
|                                      | ``Lwt.return x``              |
|    let open Lwt_syntax in            |                               |
|    return x                          |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    return x                   |
|    return x                          |                               |
+--------------------------------------+-------------------------------+
| ::                                   | No equivalent, uses           |
|                                      | ``Lwt.return_error e``        |
|    let open Lwt_result_syntax in     |                               |
|    fail e                            |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Result_syntax in         |    ok x                       |
|    return x                          |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Result_syntax in         |    error e                    |
|    tzfail e                          |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    return x                   |
|    return x                          |                               |
+--------------------------------------+-------------------------------+
| ::                                   | ::                            |
|                                      |                               |
|    let open Lwt_result_syntax in     |    fail e                     |
|    tzfail e                          |                               |
+--------------------------------------+-------------------------------+

In addition to these syntactic differences, there are also usage
differences. You might encounter the following patterns which you should
not repeat:

-  Matching against a trace:

   ::

      match f () with
      | Ok .. -> ..
      | Error (Timeout :: _) -> ..
      | Error trace -> ..

   This is discouraged because the compiler is unable to warn you if the
   matching is affected by a change in the code. E.g., if you add
   context to an error in one place in the code, you may change the
   result of the matching somewhere else in the code.


In depth discussion: what even is a monad?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This tutorial has been pretty loose with the word monad. It has focused
on usage with very little explanations of fundamental concepts. It is
focused on the surface syntax instead of the underlying mathematical
model. This section goes into a bit more details about the general
principles of monads and such. It does not claim to even attempt to
give a complete description of monads, it simply gives more details than
the previous sections.

For coding purposes, a monad is a parametric type equipped with a set of
specific operators.

::

   type 'a t
   val bind : 'a t -> ('a -> 'b t) -> 'b t
   val return : 'a -> 'a t

The ``return`` operator injects a value into the monad and the ``bind``
operator continues within the monad.

The set of operators must also follow the monad laws. For example
``bind (return x) f`` must be equivalent to ``f x``.

Monads are used as a generic way to encode different abstractions within
a programming language: I/O, errors, collections, etc. For example, the
``option`` monad is defined as

::

   module OptionMonad = struct
     type 'a t = 'a option
     let bind x f = match x with | None -> None | Some x -> f x
     let return x = Some x
   end

And it is useful when dealing with queries that may have no answer. This
can be used as a lighter form of error management than the ``result``
monad.

Some programming languages also offer syntactic sugar for monads. This
is to avoid having to write ``bind`` within ``bind`` within ``bind``.
E.g., Haskell relies heavily on monads and has the dedicated
``do``-notation. In OCaml, you can use one of the following methods:

-  `binding operators <https://ocaml.org/manual/bindingops.html>`__
   (since OCaml 4.08.0)

   ::

      let add x y =
        let ( let* ) = OptionMonad.bind in
        let* x = int_of_string_opt x in
        let* y = int_of_string_opt y in
        Some (string_of_int (x + y))

-  `infix
   operators <https://ocaml.org/manual/lex.html#sss:lex-ops-symbols>`__

   ::

      let add x y =
        let ( >>= ) = OptionMonad.bind in
        int_of_string_opt x >>= fun x ->
        int_of_string_opt y >>= fun y ->
        Some (string_of_int (x + y))

   Note that mixing multiple infix operators is not always easy because
   of precedence and associativity.

-  partial application and infix ``@@``

   ::

      let add x y =
        OptionMonad.bind (int_of_string_opt x) @@ fun x ->
        OptionMonad.bind (int_of_string_opt y) @@ fun y ->
        Some (string_of_int (x + y))

   This is useful for the occasional application: you do not need to
   declare a dedicated operator nor open a dedicated syntax module.

Monads can have additional operators beside the required core. E.g., you
can add ``OptionMonad.join : 'a option option -> 'a option``.

In depth discussion: ``Error_monad``, ``src/lib_error_monad/``, ``Tezos_base__TzPervasives``, etc.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The different parts of the error monad (syntax modules, extended stdlib,
tracing primitives, etc.) are defined in separate files. Yet, they are
all available to you directly. This section explains where each part is
defined and how it reaches the scope of your code.

**From your code, working back to the definitions.**

In most of Octez, the ``Error_monad`` module is available. Specifically, it is
available in all the packages that depend on ``tezos-base``. This covers
everything except the protocols and a handful of low-level libraries.

In those part of Octez, the build files include
``-open Tezos_base__TzPervasives``.

The module ``Tezos_base__TzPervasives`` is defined by the compilation
unit ``src/lib_base/TzPervasives.ml``.

This compilation unit gathers multiple low-level modules together. Of
interest to us is ``include Tezos_error_monad.Error_monad`` (left
untouched in the ``mli``) and ``include Tezos_error_monad.TzLwtreslib``
(not present in the ``mli``, used to shadow the Stdlib modules ``List``,
``Option``, ``Result``, etc.).

The ``Error_monad`` module exports:

-  the ``error`` type along with the ``register_error_kind`` function,
-  the ``'a tzresult`` type,
-  the ``TzTrace`` module,
-  the ``Result_syntax`` and ``Lwt_result_syntax`` modules
   (from a different, more generic name),
- and exports a few more functions.

The rest of the ``tezos-error-monad`` package:

-  defines the ``'a trace`` type (in ``TzTrace.ml``), and
-  instantiates ``TzLwtreslib`` by applying ``Lwtreslib``\ ’s ``Traced``
   functor to ``TzTrace``.

The ``Lwtreslib`` module exports a ``Traced (T: TRACE)`` functor. This
functor takes a definition of traces and returns a group of modules
intended to shadow the Stdlib.

**From the underlying definitions, working all the way up to your
code.**

At the low-level is Lwtreslib.

-  ``src/lib_lwt_result_stdlib/bare/sigs``: defines interfaces for
   basic, non-traced syntax modules and Stdlib-replacement modules.
-  ``src/lib_lwt_result_stdlib/bare/structs``: defines implementations
   basic, non-traced syntax modules and Stdlib-replacement modules.
-  ``src/lib_lwt_result_stdlib/traced/sigs``: defines interfaces for
   traced syntax modules and Stdlib-replacement modules. These
   interfaces are built on top of the non-traced interfaces, mostly by
   addition and occasionally by shadowing.
-  ``src/lib_lwt_result_stdlib/traced/structs``: defines implementations
   for traced syntax modules and Stdlib-replacement modules. These
   implementations are built on top of the non-traced implementations,
   mostly by addition and occasionally by shadowing. These are defined
   as functors over some abstract tracing primitives.
-  ``src/lib_lwt_result_stdlib/lwtreslib.mli``: puts together the traced
   implementations into a single functor ``Traced`` that takes a trace
   definition and returns fully instantiated modules to shadow the
   Stdlib.

Above Lwtreslib is the Error monad.

-  ``src/lib_error_monad/TzTrace.ml``: defines the ``'a trace`` type
   along with the low-level trace-construction primitives.
-  ``src/lib_error_monad/TzLwtreslib.ml``: instantiates
   ``Lwtreslib.Traced`` with ``TzTrace``.
-  ``src/lib_error_monad/monad_extension_maker.ml``: provides a functor
   which, given a tracing module, provides some higher level functions
   for tracing as well as a few other functions.
-  ``src/lib_error_monad/core_maker.ml``: provides a functor which,
   given a name, provides an ``error`` type, a ``register_error_kind``
   function, and a few other related functions. This is a functor so we
   can instantiate it separately for the shell and for each of the
   protocols.
-  ``src/lib_error_monad/TzCore.ml``: instantiates the ``core_maker``
   functor for the shell.
-  ``src/lib_error_monad/error_monad.ml``: puts together all of the
   above into a single module.

Above the Error monad is lib-base:

-  ``src/lib_base/TzPervasives.ml``: exports the ``Error_monad`` module,
   includes the ``Error_monad`` module, exports each of the
   ``TzLwtreslib`` module.

In depth discussion: ``result`` as data and ``result`` as control-flow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that ``result`` (and similarly, ``tzresult``) is a data type.
Specifically

::

   type ('a, 'b) result =
   | Ok of 'a
   | Error of 'b

You can treat values of type ``result`` as data of that data-type. In
this case, you construct and match the values, you pass them around,
etc.

Note however that, in Octez, we also use the ``result`` type as a
control-flow mechanism. Specifically, in conjunction with the ``let*``
binding operator, the ``result`` type has a continue/abort meaning.

Within your code, you can go from one use to the other. E.g.,

::

   let xs =
     List.rev_map
       (fun x ->
         (* [result] as control-flow *)
         let open Result_syntax in
         let* .. = .. in
         let* .. = .. in
         return ..)
       ys
   in
   let successes xs =
     (* [result] as data *)
     List.length (List.rev_filter_ok xs)
   in
   ..

Using ``result`` as sometimes data and sometimes control-flow is the
main reason to bend the guidelines about which syntax module to
open. E.g., if your function returns ``(_, _) result Lwt.t`` but the
``result`` is data returned by the function rather than control-flow
used within the function, then you should open ``Lwt_syntax`` (rather
then ``Lwt_result_syntax``).

As a significant aside, note that in OCaml you can also use exceptions
for control-flow (with ``raise`` and ``try``-``with`` and
``match``-``with``-``exception``) and as data (the type ``exn`` is an
extensible variant data-type).

::

   (** [iter_no_raise f xs] applies [f] to all the elements of [xs]. If [f] raises
       an exception, the iteration continues and [f] is still applies to other
       elements. The function returns pairs of the exceptions raised by [f] along
       the elements of [xs] that triggered these exceptions. *)
   let iter_no_raise f xs =
     List.fold_left
       (fun excs x ->
         match f x with
         | exception exc -> exc :: excs
         | () -> excs)
       []
       xs

You can find uses of exception as data within the error monad itself.
First, the generic failure functions (``error_with``,
``error_with_exn``, ``failwith``, and ``fail_with_exn``) are just
wrapper around an ``error`` which carries an exception (as data).

Second, Lwtreslib provides helpers to catch exceptions. E.g.,
``Result.catch : (unit -> 'a) -> ('a, exn) result`` calls a function and
wraps any raised exception inside an ``Error`` constructor.

In depth discussion: pros and cons of ``result`` compared to other error management techniques
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Octez, we use ``result`` and the specialised ``tzresult``. For this
reason, this tutorial is focused on ``result``/``tzresult``. However,
there are other techniques for handling errors. This section compares
them briefly.

In general you should use ``result`` and ``tzresult`` but in some
specific cases you can deviate from that. The comparisons below may help
you decide.

**Exceptions**

In exception-based error handling, you raise an exception (via
``raise``) when an error occurs and you catch it (via ``try``-``with``)
to recover. Exceptions are fast because the OCaml compiler and runtime
provide the necessary mechanisms directly.

Whether a function can raise an exception or not cannot be determined by
its type. This means that it is easy to forget to recover from an
exception. An external library may change the set of exceptions that a
function raises and you need to update calls to this function, but the
type-checker cannot warn you about it. This places a heavy burden on the
developer who is responsible for checking the documentation of all the
functions they call.

Exception-raising functions should be documented as such using the
``@raise`` documentation keyword.

| Pros: performance is good, used widely in the larger ecosystem.
| Cons: you cannot rely on the type-checker to help you at all, you
  depend on the quality of the documentation of your external and
  internal dependencies.

Note that within the protocol, you should not use exceptions at all.

**tzresult**

With ``tzresult``, errors are carried by the ``Error`` constructor of a
``result``. In this way an ``'a tzresult`` represents the result of a
computation that normally returns an ``'a`` but may fail.

Because the type of errors is an abstract wrapper (``trace``) around an
extensible variant (``error``), you can only recover from these errors
in a generic way.

| Pros: the type of a function indicates if it can fail or not, you
  cannot forget to check for success/failure.
| Cons: you cannot check which error was raised, registration is heavy
  and complicated.

**result**

With ``result``, errors are carried by the ``Error`` constructor. Each
function defines its own type of errors.

| Pros: the type of a function indicates if and how it can fail, you
  cannot forget to check for success/failure, you can check the payload
  of failures.
| Cons: different errors from different functions cannot be used
  together (need conversions), ``and*`` is unusable.

**option**

With ``option``, errors are represented by the ``None`` constructor.
Errors are completely void of payload.

Because there are no payloads attached to an error, you should generally
treat the error directly at the call site. Otherwise you might lose
track of the origin of the failure. E.g., what was not found in the
following code fragment?

::

   match
     let open Option_syntax in
     let* z = find "zero" in
     let* o = find "one" in
     Some (z, o)
   with
   | None -> ..
   | Some (z, o) -> ..

| Pros: the type of a function indicates if it can fail, you cannot
  forget to check for success/failure.
| Cons: a single kind of errors means it cannot be very informative.

Option is a common enough strategy that the ``Option_syntax`` and
``Lwt_option_syntax`` modules are available in the Octez source.

**fallback**

Another approach to errors is to have a default or fallback value. In
that case, the function returns a default sensible value when it would
raise and exception or return an error. Alternatively, it can take this
fallback value as parameter.

::

   (** @raise Not_found if argument is [None] *)
   val get : 'a option -> 'a

   (** returns [default] if argument is [None] *)
   val value : default:'a -> 'a option -> 'a

| Pros: there is no error.
| Cons: doesn’t work for every function, works differently on different
  functions.
