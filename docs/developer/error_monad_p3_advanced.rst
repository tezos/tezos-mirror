Part 3: Advanced topics
------------------------

This is Part 3 of 4 of the :doc:`./error_monad` tutorial.

Working with standard data-structures: Lwtreslib
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Handling values within the Lwt, ``result``, and Lwt-``result`` monads is
so common in Octez that you also have access to an extension of the
Stdlib dedicated to these monads: the ``Lwtreslib`` library.

The ``tezos-lwt-result-stdlib`` package exports an ``Lwtreslib`` module
which is made available, through ``tezos-error-monad`` and
``tezos-base``, to the whole of the codebase. Specifically, within the
codebase of Octez the following modules of OCaml’s ``Stdlib`` are
shadowed by Lwtreslib’s:

-  ``List``,
-  ``Result``,
-  ``Option``,
-  ``Seq``,
-  ``Map``,
-  ``Set``, and
-  ``Hashtbl``.

In all those modules, the underlying data structures are compatible with
those of the ``Stdlib`` and thus with the rest of the OCaml ecosystem.
However, the primitives in these modules are extended to support Lwt,
``result`` and the combination of the two. Specifically, for each
function that traverses the data structure, the module also contains
variants that perform the same traversal within each of the monad. E.g.,
for ``List.map``

::

     (* vanilla map *)
     val map : ('a -> 'b) -> 'a list -> 'b list

     (* [result]-aware map: stops at the first error *)
     val map_e : ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

     (* sequential Lwt map: treats each element after the previous one *)
     val map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

     (* sequential Lwt-[result] map:
        - treats each element after the previous one
        - stops at the first error *)
     val map_es :
       ('a -> ('b, 'trace) result Lwt.t) ->
       'a list ->
       ('b list, 'trace) result Lwt.t

     (* concurrent Lwt map: treats all the elements concurrently *)
     val map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

     (* concurrent Lwt-[result] map:
        - treats all the elements concurrently
        - treats the whole list no matter the success/errors *)
     val map_ep :
       ('a -> ('b, 'trace) result Lwt.t) ->
       'a list ->
       ('b list, 'trace list) result Lwt.t

Check out `the online documentation of
Lwtreslib <../api/odoc/_html/tezos-lwt-result-stdlib/Tezos_lwt_result_stdlib/Lwtreslib/index.html>`__
for a description of the semantic and naming convention.

In addition to shadowing existing modules, ``Lwtreslib`` also exports
new modules:

-  ``Seq_e`` for ``result``-aware variant of ``Seq``,
-  ``Seq_s`` for Lwt-aware variant of ``Seq``,
-  ``Seq_es`` for Lwt-``result``-aware variant of ``Seq``, and
-  ``WithExceptions`` for unsafe accesses such as ``Option.get``.

Whenever you need to traverse a standard data structure with some
``result`` or Lwt or Lwt-``result`` function, ``Lwtreslib`` should have
that function ready for you. **You should never fold over a data
structure with a promise or result accumulator.** E.g., you should
do

::

   List.rev_map_es fetch keys

and you shouldn’t do

::

   let open Lwt_result_syntax in
   List.fold_left
     (fun resources key ->
       let* resources = resources in
       let* resource = fetch key in
       return (resource :: resources))
     (return [])
     keys

If you do not find the traversal function you need, do not hesitate to
contribute to Lwtreslib.

Working with traces and errors and such directly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Occasionally, you may need more interaction with traces than the
primitives presented thus far.

For error reporting or debugging purpose, you may need to show traces to
users. You can do so with the following values.

-  ``pp_print_trace``: a ``%a``-`compatible
   formatter <https://ocaml.org/api/Format.html>`__. Note that the trace
   formatting is unspecified and subject to change. Also be aware that
   it generally prints the trace over multiple lines.
-  ``pp_print_top_error_of_trace``: a ``%a``-`compatible
   formatter <https://ocaml.org/api/Format.html>`__ that only shows the
   most recent error in the trace (or one of the most recent errors if
   there are several). This is useful to get shorter error messages.
   Most often used for the declaration of logging events in
   ``Internal_event.Simple``.
-  ``trace_encoding``: an encoding for traces. Useful to combine into
   encoding of data structures that contain traces. Most often used for
   the declaration of logging events in ``Internal_event.Simple``.

If you are working with non-standard data structures and if you need to
define monad-aware traversors for these data structures, you may need to
build some traces by hand. You can do so with the following values.

-  ``TzTrace.make : 'e -> 'e trace`` useful to convert an ``error`` into
   an ``error   trace``. By extension, this is useful to convert an
   ``('a, error) result`` into an ``'a tzresult``.

-  ``TzTrace.cons : 'e -> 'e trace -> 'e trace`` is the low-level
   combinators that builds-up traces. In most cases, you’ll want to use
   ``trace`` or ``record_trace`` instead, but you might need it when you
   are defining a low-level traversal function for some data structure.

   ::

      let iter_with_bounded_errors bound f xs =
        (* we rely on syntax for Lwt, we handle results by hand *)
        let open Lwt_syntax in
        let rec aux_all_ok = function
          | [] -> return_ok ()
          | x :: xs ->
            let* r = f x in
            match r with
            | Ok () -> aux_all_ok xs
            | Error e -> aux_some_error 1 (TzTrace.make e) xs
        and aux_some_error num_errors trace xs =
          if num_errors > bound then
            return_error (TzTrace.cons (Exceeded_error_limit bound) trace)
          else
            match xs with
            | [] -> return_ok ()
            | x :: xs ->
              let* r = f x in
              match r with
              | Ok () -> aux_some_error num_errors trace xs
              | Error e -> aux_some_error (num_errors + 1) (TzTrace.cons e trace) xs
        in
        aux_all_ok xs

-  ``TzTrace.conp : 'e trace -> 'e trace -> 'e trace`` is the parallel
   composition of two traces. Unlike ``cons``, the traces composed by
   ``conp`` are not organised hierarchically. The errors are presented
   as having happened side-by-side.

   Note that currently there is little difference between cons and conp
   traces. But the difference will be more marked in the future.

   You should use ``conp`` (rather than ``cons``) when you are gathering
   errors and traces from two or more concurrent processes.



.. _error_monad_within_protocol:

Working within the protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are working on the protocol, things are slightly different for
you. This is because the protocol has a restricted access to external
resources and libraries. You can find more details in :doc:`the dedicated
documentation <../shell/protocol_environment>`.
This section focuses on the error-monad within the protocol.

The protocol environment libraries evolve at a slightly different pace
than the underlying library. You need to check the ``mli`` files within
``src/lib_protocol_environment/sigs/``.

Note that unlike in the shell, the traces in the protocol are already
abstract. As a result there is no matching of traces (and thus errors)
within the protocol: you can match ``Ok`` and ``Error``, but not the
payload of the ``Error``. This part of the legacy code has already been
removed.

The main difference between the protocol and the shell is that the
``category`` parameter of the ``register_error_kind`` function is
meaningful. You must pass a category which is appropriate for the error
you are registering:

-  ``Branch``: is for branch-specific failures, i.e., failures that
   happen in the current branch (of the chain) but maybe wouldn’t happen in a
   different branch. E.g., a reference to an unknown block is invalid, but it
   might become valid once the head block has changed. This category is
   then used by the shell to retry after the branch changes.

-  ``Temporary``: is for transient failures, i.e., failures that happen
   but may not always happen. This category is used by the shell to
   retry at some later time.

-  ``Permanent``: is for irremediable failures, i.e., failures that
   happen and will always happen whatever the context. E.g.,
   `originating a
   contract <https://tezos.gitlab.io/alpha/glossary.html?highlight=originate#origination>`__
   that does not type-check is a permanent error. This is used by the
   shell to mark the data as invalid.

-  ``Outdated``: is for failures that happen when some data is too old.

Another thing to consider is that errors from the protocol can reach the
shell. However, because the ``error`` type of the protocol is distinct
from that of the shell, the protocol errors are wrapped inside a shell
error constructor.

This has no impact within the protocol (where shell errors don’t exist)
nor within the shell (where protocol errors are automatically wrapped
inside a shell error). However, it can have an impact in the spaces in
between. Most typically, this matters in the unit-tests of the protocol
(``src/proto_alpha/lib_protocol/test/unit/``) where you call some
protocol functions directly. In this case, you need to wrap the errors
yourself, using the wrapping functions provided by the environment:
``Environment.wrap_tzresult``, ``Environment.wrap_tztrace``, and
``Environment.wrap_tzerror``.

Working below the error-monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are working on some low-level libraries (e.g.,
``src/lib_stdlib``) or the external dependencies (e.g.,
``data-encoding``) you don’t have access to the error monad at all.

In this case, you can still use the ``result`` type but you need to
define your own ``let*`` binding operator:
``let ( let* ) = Result.bind``.

You can also use Lwt which provides its own `Lwt.Syntax
module <https://github.com/ocsigen/lwt/blob/5.4.2/src/core/lwt.mli#L1505>`__.

Finally, the `Lwt_result
module <https://github.com/ocsigen/lwt/blob/5.4.2/src/core/lwt_result.mli>`__
(provided as part of Lwt) can help you deal with result-Lwt
combinations, including via its `Lwt_result.Syntax
module <https://github.com/ocsigen/lwt/blob/5.4.2/src/core/lwt_result.mli#L78>`__.

Working with external libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This tutorial covers error-management techniques in Octez. However, from
within Octez, you may need to call external libraries for cryptography
or RPCs or data-encoding or what have you.

The first thing you do is to carefully read the documentation of the
external library you are using. You should check the overview
documentation with a look out for comments on error management.

Then, you also need to read the documentation of each function that you
are calling. This documentation may explain how errors are handled: does
the function return a ``result``? does it raise and exception? is it
unspecified?

If the function you are calling may raise exceptions, you should catch
these exceptions. You can either do so at the level of the call itself
or, if you are calling multiple functions that can all raise similar
exceptions, around a whole block of calls.

When you catch an exception, the most common thing to do is to translate
it or wrap it into a ``result`` or a ``tzresult``.

::

   try
     let v1 = Data_encoding.Json.destruct e1 j1 in
     let v2 = Data_encoding.Json.destruct e2 j2 in
     Ok (v1, v2)
   with
     | exc -> Error (Cannot_destruct_json_value exc)

Note that if you are calling an Lwt function, you have to use ``Lwt.catch`` or
``Lwt.try_bind`` rather than ``try``-``with``.

::

   Lwt.catch
     (fun () ->
       let open Lwt_syntax in
       let* () = Lwt_unix.mkdir d1 perm in
       let* () = Lwt_unix.mkdir d2 perm in
       Lwt_result_syntax.return_unit)
     (function
       | exc -> Lwt_result_syntax.fail (Cannot_destruct_json_value exc))

The error monad provides :package-api:`several helpers functions for catching exceptions
<octez-libs/Tezos_error_monad/Error_monad/index.html#catching-exceptions>`.

::

   val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a tzresult

If the function you are calling may raise exceptions only under
well-defined conditions on the parameters, then you can also check those
conditions yourself and ignore the exceptions. When doing so, please add
a comment to explain it.

::

   let get_or_defaults low_default high_default array offset =
     if offset < 0 then
       low_default
     else if offset >= Array.length array then
       high_default
     else
       (* This cannot raise because of checks on offset above *)
       Array.get array offset

If the function may fail with ``result``, you can map the error directly
or simply continue with it. If it may fail with ``option``, you can
translate ``None`` into an appropriate error.

::

   match find k kvs with
   | None -> Error "cannot find key"
   | Some v -> Ok v

If the function’s documentation specifies some pre-conditions but
doesn’t explain what happens if those aren’t met, then you must check
those pre-conditions.
