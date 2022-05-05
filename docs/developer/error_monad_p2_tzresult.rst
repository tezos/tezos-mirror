Part 2: ``tzresult`` and Lwt-``tzresult``
------------------------------------------

This is Part 2 of 4 of the :doc:`./error_monad` tutorial.

``error``, ``trace``, and ``tzresult``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``error`` type is an `extensible variant
type <https://ocaml.org/manual/extensiblevariants.html>`__ defined in
the ``Error_monad`` module.

Each part of the Octez source code can extend it to include some errors
relevant to that part. E.g., the p2p layer adds
``type error += Connection_refused`` whilst the store layer adds
``type error += Snapshot_file_not_found of string``. More information on
errors is presented later.

The ``'error trace`` type is a data structure dedicated to holding
``error``\ s. It is exported by the ``Error_monad`` module. It is only
ever instantiated as ``error trace``. It can accumulate multiple errors,
which helps present information about the context in which an error
occurs. More information on traces is presented later.

The ``'a tzresult`` type is an alias for ``('a, error trace) result``.
It is used in Octez to represent the possibility of failure in a generic
way. Using ``tzresult`` is a trade-off. You should only use it in
situations where the pros outweigh the cons.

Pros:

- A generic error that is the same everywhere means that the
  binding operator ``let*`` can be used everywhere without having to
  convert errors to a common type.
- Traces allow you to easily add
  context to an existing error (see how later).

Cons:

- A generic wrapper around generic errors that cannot be
  meaningfully matched against (although the pattern exists in legacy
  code).
- Type information about errors is coarse-grained to the point of
  being meaningless (e.g., there is no exhaustiveness check when
  matching).
- Registration is syntactically heavy and requires care (see
  below).

In general, the type ``tzresult`` is mostly useful at a high-enough
level of abstraction and in the outermost interface of a module or even
package (i.e., when exposing errors to callers that do not have access
to internal implementation details).

Generic failing
~~~~~~~~~~~~~~~

The easiest way to return a ``tzresult`` is via functions provided by
the ``Error_monad``. These functions are located at the top-level of the
module, as such they are available, unqualified, everywhere in the code.
They do not even require the syntax modules to be open.

-  ``error_with`` is for formatting a string and wrapping it inside an
   ``error`` inside a ``trace`` inside an ``Error``. E.g.,

   ::

      let retry original_limit (f : unit -> unit tzresult) =
        let rec retry limit f
          if limit < 0 then
            error_with "retried %d times" original_limit
          else
            match f () with
            | Error _ -> retry (limit - 1) f
            | Ok () -> Ok ()
        in
        retry original_limit f

   You can use all the formatting percent-escapes from the `Format
   module <https://ocaml.org/api/Format.html>`__. However, you should
   generally keep the message on a single line so that it can be printed
   nicely in logs.

   Formally, ``error_with`` has type
   ``('a, Format.formatter, unit, 'b tzresult) format4 -> 'a``. This is
   somewhat inscrutable. Suffice to say: it is used with a format string
   and returns an ``Error``.

-  ``error_with_exn : exn -> 'a tzresult`` is for wrapping an exception
   inside an ``error`` inside a ``trace`` inside an ``Error``.

   ::

      let media_type_kind s =
        match s with
        | "json" | "bson" -> Ok `Json
        | "octet-stream" -> Ok `Binary
        | _ -> error_with_exn Not_found

-  ``failwith`` is for formatting a string and wrapping it inside an
   ``error`` inside a ``trace`` inside an ``Error`` inside a promise.
   E.g.,

   ::

      failwith "Cannot read file %s (ot %s)" file_name (Unix.error_message error)

   ``failwith`` is similar to ``error_with`` but for the combined
   Lwt-``tzresult`` monad. Again the type
   (``('a, Format.formatter, unit, 'b tzresult Lwt.t) format4 -> 'a``)
   is inscrutable, but again usage is as simple as a formatting.

-  ``fail_with_exn`` is for wrapping an exception inside an ``error``
   inside a ``trace`` inside an ``Error`` inside a promise. E.g.,

   ::

      fail_with_exn Not_found

   ``fail_with_exn`` is similar to ``error_with_exn`` but for the
   combined Lwt-``tzresult`` monad.

These functions are useful as a way to fail with generic errors that
carry a simple payload. The next section is about using custom errors
with more content.

.. _exercises-5:

Exercises
^^^^^^^^^

-  Write a function ``tzresult_of_option : 'a option -> 'a tzresult``
   which replaces ``None`` with a generic failure of your choosing.

-  Write a function ``catch : (unit -> 'a) -> 'a tzresult`` which wraps
   any exception raised during the evaluation of the function call.

Declaring and registering ``error``\ s
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On many occasions, ``error_with`` and friends (see above) are not
sufficient: you may want your error to carry more information than can
be conveyed in a simple string or a simple exception. When you do, you
need a custom error. You must first *declare* and then *register* an
``error``.

To declare a new error you simply extend the ``error`` type. Remember
that the ``error`` type is defined and exported to the rest of the code
by the ``Error_monad`` module. You extend it with the ``+=`` syntax:

::

   type error +=
     Invalid_configuration of { expected: string; got: string; line: int }

The registration function ``register_error_kind`` is also part of the
``Error_monad`` module. You *register* a new error by calling this
function.

::

   let () =
     register_error_kind
       `Temporary
       ~id:"invalid_configuration"
       ~title:"Invalid configuration"
       ~description:"The configuration is invalid."
       ~pp:(fun f (expected, got, line) ->
         Format.fprintf f
           "When parsing configuration expected %s but got %s (at line %d)"
           expected got line
       )
       Data_encoding.(
         obj3
           (req "expected" string)
           (req "got" string)
           (req "line" int31))
       (function
         | Invalid_configuration {expected; got; line} ->
           Some (expected, got, line)
         | _ -> None)
       (fun (expected, got, line) -> Invalid_configuration {expected; got; line})

Note that you **MUST** register the errors you declare. Failure to do so
can lead to serious issues.

The arguments for the ``register_error_kind`` function are as follows: -
category (:literal:`\`Temporary`): the category argument is meaningless
in the shell, just use :literal:`\`Temporary`.

-  ``id``: a short string containing only characters that do not need escaping
   (``[a-zA-Z0-9._-]``), must be unique across the whole program.
-  ``title``: a short human readable string.
-  ``description``: a longer human readable string.
-  ``pp``: a pretty-printing function carrying enough information for a full
   error message for the user. Note that the function does not receive the error,
   instead it receives the *projected payload of the error* (here a 3-tuple
   ``(expected, got, line)``.
-  encoding: an encoding for the projected payload of the error.
-  projection: a partial function that matches the specific error
   (out of all of them) and return its projected payload. This function always
   has the form
   ``function | <the error you are returning> -> Some <projected payload> | _ -> None``.
-  injection: a function that takes the projected payload and constructs
   the error out of it.

For errors that do not carry information (e.g.,
``type error += Size_limit_exceeded``), the projected payload of the
error is unit.

It is customary to either register the error immediately after the error
is declared or to register multiple errors immediately after declaring
them all. In some cases, the registration happens in a separate module.
Either way, registration of declared error is compulsory.

.. _exercises-6:

Exercises
^^^^^^^^^

-  Register the following error

   ::

      (** [Size_limit_exceeded {limit; current_size; attempted_insertion}] is used
          when an insertion into the global table of known blocks would cause the
          size of the table to exceed the limit. The field [limit] holds the
          maximum allowed size, the field [current_size] holds the current size of
          the table and [attempted_insertion] holds the size of the element that
          was passed to the insertion function. *)
      type error += Size_limit_exceeded {
        limit: int;
        current_size: int;
        attempted_insertion: int
      }

The ``Result_syntax``'s ``tz`` extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Remember that ``'a tzresult`` is a special case of ``('a, 'e) result``.
Specifically, a special case where ``'e`` is ``error trace``.
Consequently, you can handle ``tzresult`` values using the
``Result_syntax`` module.

The module ``Result_syntax`` exports a few functions dedicated to handling
``tzresult``. These functions were omitted from Part 1.

-  ``tzfail: 'e -> ('a, 'e trace) result``: the expression ``tzfail e``
   wraps ``e`` in a ``trace`` inside an ``Error``. When ``e`` is of type
   ``error`` as is the case throughout Octez, ``tzfail e`` is of type
   ``'a tzresult``.

-  ``and*``: a binding operator alias for ``tzboth`` (see below). You can
   use it with ``let*`` the same way you use ``and`` with ``let``.

   ::

      let apply_triple f (x, y, z) =
        let open Result_syntax in
        let* u = f x
        and* v = f y
        and* w = f z
        in
        return (u, v, w)

   When you use ``and*``, the bound results (``f x``, ``f y``, and
   ``f z``) are all evaluated fully, regardless of the success/failure
   of the others. The expression which follows the ``in``
   (``return ..``) is evaluated if all the bound results are successful.

-  ``tzboth : ('a, 'e trace) result -> ('b, 'e trace) result -> ('a * 'b, 'e trace) result``:
   the expression ``both a b`` is ``Ok`` if both ``a`` and ``b`` are
   ``Ok`` and ``Error`` otherwise`.

   Note that unlike ``both``, the type of errors
   (``error trace``) is the same on both the argument and return side of
   this function: the traces are combined automatically. This remark
   applies to the ``tzall`` and ``tzjoin`` (see below) as well.

   The stability of the return type is what allows this syntax module to
   include an ``and*`` binding operator.

-  ``tzall : ('a, 'e trace) result list -> ('a list, 'e trace) result``:
   the function ``tzall`` is a generalisation of ``tzboth`` from tuples to
   lists.

-  ``tzjoin : (unit, 'e trace) result list -> (unit, 'e trace) result``:
   the function ``tzjoin`` is a specialisation of ``tzall`` for list of
   unit-typed expressions (typically, for side-effects).

-  ``and+`` is a binding operator similar to ``and*`` but for use with
   ``let+`` rather than ``let*``.

.. _exercises-7:

Exercises
^^^^^^^^^

-  What is the difference between the two following functions?

   ::

      let twice f =
        let open Result_syntax in
        let* () = f () in
        let* () = f () in
        return_unit

   ::

      let twice f =
        let open Result_syntax in
        let* () = f ()
        and* () = f ()
        in
        return_unit

The ``Lwt_result_syntax``'s ``tz`` extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the same way ``result`` can be combined with Lwt, ``tzresult`` can
also be combined with Lwt. And in the same way that ``Result_syntax`` exports a
few ``tz``-specific extensions, ``Lwt_result_syntax`` exports a few Lwt+``tz``
specific extensions.

There are possibly too many parallels to keep track of, so the diagram
below might help.

::

   'a  -----------> ('a, 'e) result ------------> 'a tzresult
    |                        |                         |
    |                        |                         |
    V                        V                         V
   'a Lwt.t ------> ('a, 'e) result Lwt.t ------> 'a tzresult Lwt.t

Anyway, the ``Lwt_result_syntax`` module exports a few functions dedicated to
handling Lwt+``tzresult``. These functions were omitted from Part 1.

-  ``tzfail: 'e -> ('a, 'e trace) result Lwt.t``: the expression
   ``tzfail e`` wraps ``e`` in a ``trace`` inside an ``Error`` inside a
   promise. When ``e`` is of type ``error`` as is the case throughout
   Octez, ``tzfail e`` is of type ``'a tzresult Lwt.t``.

-  ``and*``: a binding operator alias for ``tzboth``. You can use it with
   ``let*`` the same way you use ``and`` with ``let``.

   ::

      let apply_triple f (x, y, z) =
        let open Lwt_result_syntax in
        let* u = f x
        and* v = f y
        and* w = f z
        in
        return (u, v, w)

   When you use ``and*``, the bound promises (``f x``, ``f y``, and
   ``f z``) are evaluated concurrently, and the expression which follows
   the ``in`` (``return   ..``) is evaluated once all the bound promises
   have all resolved but only if all of them resolve successfully.

   Note how this ``and*`` binding operator inherits the properties of
   both ``Lwt_syntax.( and* )`` and ``Result_syntax.( and* )``.
   Specifically, the promises are evaluated concurrently and the
   expression which follows the ``in`` is evaluated only if all the
   bound promises have successfully resolved. These two orthogonal
   properties are combined. This remark also applies to ``tzboth``,
   ``tzall``, ``tzjoin`` and ``and+`` below.

-  ``tzboth : ('a, 'e trace) result Lwt.t -> ('b, 'e trace) result Lwt.t -> ('a * 'b, 'e trace) result Lwt.t``:
   the expression ``tzboth p q`` is a promise that resolves once both
   ``p`` and ``q`` have resolved. It resolves to ``Ok`` if both ``p``
   and ``q`` do, and to ``Error`` otherwise`.

   Note that unlike ``Lwt_result_syntax.both``, the type of errors
   (``error trace``) is the same on both the argument and return side of
   this function: the trace are combined automatically. This remark
   applies to the ``tzall`` and ``tzjoin`` (see below) as well.

   The stability of the return type is what allows this syntax module to
   include an ``and*`` binding operator.

-  ``tzall : ('a, 'e trace) result Lwt.t list -> ('a list, 'e trace) result Lwt.t``:
   the function ``tzall`` is a generalisation of ``tzboth`` from tuples to
   lists.

-  ``join : (unit, 'e trace) result Lwt.t list -> (unit, 'e trace) result Lwt.t``:
   the function ``tzjoin`` is a specialisation of ``tzall`` for lists of
   unit-typed expressions (typically, for side-effects).

-  ``and+`` is a binding operator similar to ``and*`` but for use with
   ``let+`` rather than ``let*``.

.. _exercises-8:

Exercises
^^^^^^^^^

-  Rewrite this function to use the ``Lwt_result_syntax`` module and
   no other syntax module.

   ::

      let apply_tuple (f, g) (x, y) =
        let open Lwt_syntax in
        let* u = f x
        and* v = g y
        in
        let r = Result_syntax.tzboth u v in
        return r

-  Write the implementation for

   ::

      (** [map f [x1; x2; ..]] is [[y1; y2; ..]] where [y1] is the successful
          result of [f x1], [y2] is the successful result of [f x2], etc. If [f]
          fails on any of the inputs, returns an [Error] instead. Either way, all
          the calls to [f] on all the inputs are evaluated concurrently and all
          the calls to [f] have resolved before the whole promise resolves. *)
      val map : ('a -> 'b tzresult Lwt.t) -> 'a list -> 'b list tzresult

.. _lifting-1:

Lifting
~~~~~~~

When you are working with promises of ``tzresult`` (i.e., within
Lwt-``tzresult``), you may occasionally need to call functions that
return a simple promise (i.e., within Lwt-only) or a simple ``tzresult``
(i.e., within ``tzresult``-only).

Because ``tzresult`` is a special case of ``result``, you can use the same
operators ``let*!`` and ``let*?`` as presented in Part 1.

::

   let*! x = plain_lwt_function foo bar in
   let*? x = plain_result_function foo bar in
   ..


Tracing
~~~~~~~

Remember that a trace is a data structure specifically designed for
errors.

Traces have two roles:

-  As a programmer you benefit from the traces' ability to combine
   automatically. Indeed, this feature of traces makes the ``and*`` binding
   operators possible which can simplify some tasks such as concurrent
   evaluation of multiple ``tzresult`` promises.

-  For the user, traces combine multiple errors, allowing for high-level
   errors (e.g., ``Cannot_bootstrap_node``) to be paired with low-level
   errors (e.g., ``Unix_error EADDRINUSE``). When used correctly, this
   can help create more informative error messages which, in turn, can
   help debugging.

Tracing primitives are declared in the ``Error_monad`` module. As such,
they are available almost everywhere in the code. They should be used
whenever an error passes from one component (say the p2p layer, or the
storage layer) into another (say the shell).

-  ``record_trace err r`` leaves ``r`` untouched if it evaluates to
   ``Ok``. Otherwise, it adds the error ``err`` to the trace carried in
   ``Error``.

   ::

      let check_hashes head block operation =
        let open Result_syntax in
        let* () =
          record_trace (Invalid_hash { kind: "head"; hash: head}) @@
          check_hash chain
        in
        let* () =
          record_trace (Invalid_hash { kind: "block"; hash: block}) @@
          check_hash block
        in
        let* () =
          record_trace (Invalid_hash { kind: "operation"; hash: operation}) @@
          check_hash operation
        in
        return_unit

   In this example a failure from any of the calls to ``check_hash``
   will be given context that helps with understanding the source of the
   error.

-  ``record_trace_eval`` is a lazy version of ``record_trace`` in that
   the error added to the trace is only evaluated if needed. More
   formally ``record_trace_eval make_err r`` leaves ``r`` untouched if
   it evaluates to ``Ok``. Otherwise it calls ``make_err`` and adds the
   returned error onto the trace.

   You should use the strict ``record_trace`` version when the error you
   are adding to the trace is an immediate value (e.g., ``Overflow``) or
   a constructor with immediate values (e.g., ``Invalid_file name``).
   You should use the lazy ``record_trace_eval`` version when the error
   you are adding to the trace requires computation to generate (e.g.,
   if it requires formatting or querying).

-  ``trace`` is the Lwt-aware variant of ``record_trace``. More
   formally, ``trace err p`` leaves ``p`` untouched if it resolves
   successfully, otherwise it adds the error ``err`` to the trace
   carried by the unsuccessful resolve.

   ::

      let get_data_and_gossip_it () =
        let open Lwt_result_syntax in
        let* data =
          trace Cannot_get_random_data_from_storage @@
          Storage.get_random_data ()
        in
        let* number_of_peers =
          trace Cannot_gossip_data @@
          P2p.gossip data
        in
        return (data, number_of_peers)

   In this example, low-level storage errors are given more context by
   the ``Cannot_get_random_data_from_storage`` error. Similarly,
   low-level p2p errors are given more context by the
   ``Cannot_gossip_data`` error. This is important because both the
   storage and the p2p layer may suffer from similar system issues (such
   as file-descriptor exhaustion).

-  ``trace_eval`` is the lazy version of ``trace``, or, equivalently,
   the Lwt-aware version of ``record_trace_eval``.

   You should use the strict ``trace`` version when the error you are
   adding is immediate or a constructor with immediate values. You
   should use the lazy ``trace_eval`` version when the error you are
   adding requires computation to generate.

Do not hesitate to use the tracing primitives. Too much context is
better than too little context. Think of ``trace`` (and variants) as a
way to document Octez. Specifically, as making the error messages of
Octez more informative.

.. _meta-commentary-1:

META COMMENTARY
~~~~~~~~~~~~~~~

The previous sections had a practical focus: how to handle errors? how
to mix different syntaxes? how?! By contrast, the following sections are
in-depth discussions of advanced or tangential topics which you should
feel free to skim or even to skip.

| You should take this opportunity to take a break.
| Come back in a few minutes.
