Part 1: ``result``, Lwt, and Lwt-``result``
--------------------------------------------

This is Part 1 of 4 of the :doc:`./error_monad` tutorial.

The ``result`` type
~~~~~~~~~~~~~~~~~~~

The type ``result`` is part of the standard library of OCaml. It is
defined as

::

   type ('a, 'b) result =
   | Ok of 'a
   | Error of 'b

(See `reference
manual <https://ocaml.org/api/Stdlib.html#1_Resulttype>`__.)

The constructors of the ``result`` type have meaning: ``Ok`` is for
normal, successful cases that carry a value that is somewhat expected.
``Error`` is for abnormal, failure cases that carry information about
what went wrong. E.g.,

::

   let get a index =
     if index < 0 then
       Error "negative index in array access"
     else if index >= Array.length a then
       Error "index beyond length in array access"
     else
       Ok a.(index)

You can see the ``result`` type as an opinionated ``either``: a
left-or-right sum type where the left and right side have distinct
roles. Or you can see the ``result`` type as a buffed-up ``option``
type: a type that either carries a value or doesn’t (but in this case it
carries an error instead).

Exercises
^^^^^^^^^

-  Write the implementation for

   ::

      (** [to_option r] is [Some x] if [r] is [Ok x]. Otherwise it is [None]. *)
      val to_option : ('a, unit) result -> 'a option

-  Write the implementation for

   ::

      (** [to_option ?log r] is [Some x] if [r] is [Ok x]. Otherwise it calls
          [log e] and returns [None] if [r] is [Error e]. By default [log] is
          [ignore]. *)
      val to_option : ?log:('e -> unit) -> ('a, 'e) result -> 'a option

-  Write the implementation for

   ::

      (** [catch f] is [Ok (f ())] except if [f] raises an exception [exc] in
          which case it is [Error exc]. *)
      val catch : (unit -> 'a) -> ('a, exn) result

The binding operator
~~~~~~~~~~~~~~~~~~~~

Working directly with the ``result`` type can quickly become cumbersome.
Consider, for example the following code.

::

   (** [compose3 f g h x] is [f (g (h x))] except that it handles errors. *)
   let compose3 f g h x
     match h x with
     | Error e -> Error e
     | Ok y ->
       match g y with
       | Error e -> Error e
       | Ok z ->
         f z

The nested ``match``-``with`` constructs lead to further and further
indentation. The ``Error`` cases are all identical and simply add noise.

To circumvent this, in Octez we use a `binding
operator <https://ocaml.org/manual/bindingops.html>`__: a user-defined
``let``-binding. Specifically, you can open the ``Result_syntax`` module
which includes the binding operator ``let*`` dedicated to ``result``.

::

   (** [compose3 f g h x] is [f (g (h x))] except that it handles errors. *)
   let compose3 f g h x
     let open Result_syntax in (* adds [let*] in scope *)
     let* y = h x in
     let* z = g y in
     f z

An expression ``let* x = e in e'`` is equivalent to
``Result.bind e (fun x -> e')`` and also to
``match e with | Error err -> Error err | Ok x -> e'``. In all of these forms,
the expression ``e'`` is evaluated if and only if the expression ``e`` is
successful (i.e., evaluates to ``Ok``).

.. _exercises-1:

Exercises
^^^^^^^^^

-  Rewrite the following code without ``match``-``with``

   ::

      let apply2 (f, g) (x, y) =
        match f x with
        | Error e -> Error e
        | Ok u ->
          match g y with
          | Error e -> Error e
          | Ok v -> Ok (u, v)

   Did you remember to open the syntax module?

-  Write the implementation for

   ::

      (** [map f [x1; x2; x3; ..]] is a list [[y1; y2; y3; ..]] where [y1 = f x1],
          [y2 = f x2], etc. except if [f] is fails on one of the inputs, in which
          case it is an [Error] carrying the same error as [f]'s. *)
      val map : ('a -> ('b, 'err) result) -> 'a list -> ('a list, 'err) result

   Note that this exercise is for learning only. You won’t need to write
   this function in Octez. Indeed, a helper function which does exactly
   that is provided in the extended standard library of Octez.

Aside: the ``Error_monad`` module is opened everywhere
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Octez, the ``Error_monad`` module provides types and values for error
management. It is part of the ``tezos-error-monad`` package. And it is
opened in most of the source code. Apart from some specific libraries
(discussed later), the content of this module is already in scope.

The ``Error_monad`` module contains the ``Result_syntax`` module. This
is why you can use ``let open Result_syntax in`` directly in your own
functions.

The ``Error_monad`` module contains other modules and functions and
types which you will learn about later.

Recovering from errors
~~~~~~~~~~~~~~~~~~~~~~

When given a value of type ``result``, you can inspect its content to
determine if it is an ``Ok`` or an ``Error``. You can use this feature
to recover from the failures.

::

   match e with
   | Ok x -> do_something x
   | Error e -> do_something_else e

Recovering can mean different things depending on the task that failed
and the error with which it failed. Sometimes you just want to retry,
sometimes you want to retry with a different input, sometimes you want
to propagate the error, sometimes you want to log the error and continue
as if it hadn’t happened, etc.

::

   let rec write data =
     match write_to_disk data with
     | Ok () -> ()
     | Error EAGAIN -> write data (* again: try again *)
     | Error ENOSPC -> Stdlib.exit 1 (* no space left on device: escalate to exit *)

There is a correspondence between a ``match``-``with`` around a
``result`` and a ``try``-``with``. Both are for recovering from
failures. However, in Octez, you will mostly use a ``match``-``with``
around a ``result``, because we favour ``result`` over exceptions. You
may use the ``try``-``with`` construct when interfacing with an external
library which uses exceptions.

There are several ways to use the ``match``-``with`` recovery with the
binding operator from ``Result_syntax``. Depending on the size of the
expression you are recovering from, one may be more readable than the
other. Choose accordingly.

You can simply place the expression directly inside the
``match``-``with``.

::

   match
     let* x = get_horizontal point in
     let* y = get_vertical point in
     Ok (x, y)
   with
   | Ok (x, y) -> ..
   | Error e -> ..

Alternatively, if the expression grows too much in size or in
complexity, you can move the expression inside a vanilla
``let``-binding: ``let r = .. in match r with ..``.

Alternatively, if the expression grows even more, or if the expression
may be re-used in other parts of the code, you may move the expression
inside a vanilla function which you can call inside the ``match``-``with``.

You can also use the functions from `the standard library’s Result
module <https://ocaml.org/api/Result.html>`__. Note however, that some
of these functions are shadowed in the extended library of Octez, which
you will learn more about later.

Mixing different kinds of errors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Occasionally, you may have to call a function which returns a value of
type, say, ``(_, unit) result`` and one, say, ``(_, string) result``. In
this case, you cannot simply bind the two expressions as is.
Specifically, the type checker will complain. You can see the constraint
you would be breaking in the type of ``let*`` where the two error types
are the same (``'e``):

::

   val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

When you need to mix those function, you have to either handle the
errors of each independently (see the section above about recovering from
errors) or you need to convert the errors so they have the same type.
You should use ``Result.map_error`` to do that.

::

   let* cfg =
     Result.map_error (fun () -> "Error whilst reading configuration")
     @@ read_configuration ()
   in
   ..

The ``Result_syntax`` module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You have already learned about the ``let*`` binding operator from the
``Result_syntax``. But there are other values you can use from this
module.

The following functions form the core of the ``Result_syntax`` module.

-  ``let*``: a binding operator to continue with the value in the ``Ok``
   constructor or interrupt with the error in the ``Error`` constructor.

   ``let* x = e in e'`` is equivalent to
   ``match e with Error err -> Error err | Ok x -> e'``.

   (See above for examples and more details.)

-  ``return : 'a -> ('a, 'e) result``: the expression ``return x`` is
   equivalent to ``Ok x``. The function is included for consistency with
   other syntax modules presented later. You can use either form.

-  ``fail : 'e -> ('a, 'e) result``: the expression ``fail e`` is
   equivalent to ``Error e``. The function is included for consistency
   with other syntax modules presented later. You can use either form.

The following functions offer additional, less often used
functionalities.

-  ``both : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e list) result``:
   the expression ``both e1 e2`` is ``Ok`` if both expressions
   evaluate to ``Ok`` and ``Error`` otherwise.

   Note that the expression ``both e1 e2`` is different from the
   expression ``let* x = e1 in let* y = e2 in return (x, y)``. In the
   former (``both``) version, both ``e1`` and ``e2`` are evaluated,
   regardless of success/failure of ``e1`` and ``e2``. In the latter
   (``let*``-``let*``) version, ``e2`` is evaluated if and only if
   ``e1`` is successful.

   This distinction is important when the expressions ``e1`` and ``e2``
   have side effects: ``both (f ()) (g ())``.

-  ``all : ('a, 'e) result list -> ('a list, 'e list) result``: the
   function ``all`` is a generalisation of ``both`` from tuples to
   lists.

   Note that, as a generalisation of ``both``, in a call to the function
   ``all``, all the elements of the list are evaluated, regardless of
   success/failure of the elements: ``all (List.map f xs)``.

-  ``join : (unit, 'e) result list -> (unit, 'e list) result``: the
   function ``join`` is a specialisation of ``all`` for lists of
   unit-typed expressions (typically, for side-effects).

   Note that, as a specialisation of ``all``, in a call to the function
   ``join``, all the elements of the list are evaluated, regardless of
   success/failure of the elements: ``join (List.map f xs)``.

The following functions do not provide new functionalities but they are
useful for small performance gains or for shorter syntax.

-  ``return_unit`` is equivalent to ``return ()`` but it avoids one
   allocation. This is important in parts of the code that are
   performance critical, and it is a good habit to take otherwise.

   | ``return_nil`` is equivalent to ``return []`` but it avoids one
     allocation.
   | ``return_true`` is equivalent to ``return true`` but it avoids one
     allocation.
   | ``return_false`` is equivalent to ``return false`` but it avoids
     one allocation.

   | ``return_none`` is equivalent to ``return None`` but it avoids one
     allocation.
   | ``return_some x`` is equivalent to ``return (Some x)`` and it is
     provided for completeness with ``return_none``.

-  ``let+`` is a binding operator similar to ``let*`` but when the
   expression which follows the ``in`` returns a non-result value. In
   other words, ``let+ x = e in e'`` is equivalent to
   ``let* x = e in return (e')``.

   The ``let+`` is purely for syntactic conciseness (compared to the ``*``
   variant), use it if it makes
   your code more readable.

Lwt
~~~

In Octez, I/O and concurrency are handled using the Lwt library. With
Lwt you use *promises* to handle I/O and concurrency. You can think of
promises as data structures that are empty until they *resolve*, at
which point they hold a value.

A promise for a value of type ``'a`` has type ``'a Lwt.t``. The function
``Lwt.bind : 'a t -> ('a -> 'b t) -> 'b t`` waits for the promise to
resolve (i.e., to carry a value of type ``'a``) before applying the
provided function. The expression ``bind p f`` is a promise which
resolves only once the promise ``p`` has resolved *and then* the promise
returned by ``f`` has resolved too.

If you are not familiar with Lwt, you should check out `the official manual
<https://ocsigen.org/lwt/latest/manual/manual>`__ and `this
introduction <https://raphael-proust.gitlab.io/code/lwt-part-1.html>`__
before continuing. This is important. Do it.

Unlike is mentioned in those separate resources on Lwt, in Octez, we
do not in general use the success/failure mechanism of Lwt. Instead, we
mostly rely on ``result`` (as mentioned above).

Thus, in the rest of this tutorial we only consider the subset of Lwt
without failures. In practice, you might need to take care of exceptions
in some cases, but this is discussed in the later, more advanced parts of the
tutorial.

The ``Lwt_syntax`` module
~~~~~~~~~~~~~~~~~~~~~~~~~

In Octez, because Lwt is pervasive, you need to bind promises often. To
make it easier, you can use the ``Lwt_syntax`` module. The
``Lwt_syntax`` module is made available everywhere by the
``Error_monad`` module. The ``Lwt_syntax`` module is similar to the
``Result_syntax`` module but for the Lwt monad (more about monads
later).

-  ``let*``: a binding operator to wait for the promise to resolve
   before continuing.

   ``let* x = e in e'`` is a promise that resolves after ``e`` has
   resolved to a given value and then ``e'`` has resolved with that
   value carried by ``x``.

   Note that ``Lwt_syntax`` and ``Result_syntax`` (see above) both use
   ``let*`` for their main binding operator. Consequently, the specific
   meaning of ``let*`` depends on which module is open. This extends to
   other syntax modules introduced later in this tutorial.

   (What if you need to use both Lwt and ``result``? Which syntax module
   should you use? You will learn about that in the next section!)

-  ``return : 'a -> 'a Lwt.t``: the expression ``return x`` is equivalent to
   ``Lwt.return x``. It is a promise that is already resolved with the value of
   ``x``.

-  ``and*``: a binding operator alias for ``both`` (see below). You can
   use it with ``let*`` the same way you use ``and`` with ``let``.

   ::

      let apply_triple f (x, y, z) =
        let open Lwt_syntax in
        let* u = f x
        and* v = f y
        and* w = f z
        in
        return (u, v, w)

   When you use ``and*``, the bound promises (``f x``, ``f y``, and
   ``f z``) are evaluated concurrently, and the expression which follows
   the ``in`` (``return ..``) is evaluated once all the bound promises
   have all resolved.

The following functions offer additional, less often used
functionalities.

-  ``both: 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t``: the expression
   ``both p q`` is a promise that resolves only once both promises
   ``p`` and ``q`` (which make progress concurrently) have resolved.

   In practice, you will most likely use ``and*`` instead of both.

-  ``all: 'a Lwt.t list -> 'a list Lwt.t``: the function ``all`` is a
   generalisation of ``both`` from tuples to lists.

   Note that, as a generalisation of ``both``, in a call to the function
   ``all``, all the promises in the provided list make progress towards
   resolution concurrently.

-  ``join : unit Lwt.t list -> unit Lwt.t``: the function ``join`` is a
   specialisation of ``all`` to lists of units (i.e., side-effects).

The following functions do not provide new functionalities but they are
useful for small performance gains or for shorter syntax.

-  ``return_unit`` is equivalent to ``return ()`` but it avoids one
   allocation. This is important in parts of the code that are
   performance critical, and it is a good habit to take otherwise.

   | ``return_nil`` is equivalent to ``return []`` but it avoids one
     allocation.
   | ``return_true`` is equivalent to ``return true`` but it avoids one
     allocation.
   | ``return_false`` is equivalent to ``return false`` but it avoids
     one allocation.

   | ``return_none`` is equivalent to ``return None`` but it avoids one
     allocation.
   | ``return_some x`` is equivalent to ``return (Some x)`` and it is
     provided for completeness.

   | ``return_ok x`` is equivalent to ``return (Ok x)`` and it is
     provided for completeness.
   | ``return_error x`` is equivalent to ``return (Error x)`` and it is
     provided for completeness.

-  ``let+`` and ``and+`` are binding operators similar to ``let*`` and
   ``and*`` but when the expression which follows the ``in`` returns a
   non-promise value. In other words, ``let+ x = e1 and+ y = e2 in e`` is
   equivalent to ``let* x = e1 and* y = e2 in return e``.

   The ``let+`` and ``and+`` are purely for syntactic conciseness (compared to
   the ``*`` variants), use them if it makes your code more readable.

Promises of results: Lwt and ``result`` together
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Octez, we have functions that perform I/O and also may fail. In this
case, the function returns a promise of a ``result``. This is the topic
of this section.

Note that Lwt and ``result`` are orthogonal concerns. On the one hand,
Lwt is for concurrency, for automatically scheduling code around I/O,
for making progress on different parts of the program side-by-side. On
the other hand, ``result`` is for aborting computations, for handling
success/failures. It is because Lwt and ``result`` are orthogonal that
we can use them together.

::

   'a  --------------> ('a, 'e) result
    |                           |
    |                           |
    V                           V
   'a Lwt.t ---------> ('a, 'e) result Lwt.t

When we combine Lwt and ``result`` for control-flow purpose we combine
both of the orthogonal behaviours. We can achieve this combined
behaviour “by hand”. However, doing so requires mixing
``Lwt_syntax.( let* )`` and regular ``match``-``with``:

::

   let apply2 (f, g) (x, y) =
     let open Lwt_syntax in
     let* r = f x in
     match r with
     | Error e -> return (Error e)
     | Ok u ->
       let* r = g y in
       match r with
       | Error e -> return (Error e)
       | Ok v -> return (Ok (u, v))

This is interesting to consider because it shows the two orthogonal
features of control-flow separately: wait for the promise to resolve,
and check for errors. However, in practice, this becomes cumbersome even
faster than when working with plain ``result`` values.

To make this easier, in Octez we use a binding operator. Specifically,
you can open the ``Lwt_result_syntax`` (instead of the other syntax
modules) which includes a binding operator dedicated to promises of
``result``.

::

   let apply2 (f, g) (x, y) =
     let open Lwt_result_syntax in
     let* u = f x in
     let* v = g y in
     return (u, v)

When a promise resolves to ``Ok`` we say that it resolves successfully.
When it resolves to ``Error`` we say that it resolves unsuccessfully or
that it fails.

.. _exercises-2:

Exercises
^^^^^^^^^

-  Rewrite the following code without ``match``-``with``

   ::

      let compose3 f g h x
        let open Lwt_syntax in
        let* r = h x in
        match r with
        | Error e -> return (Error e)
        | Ok y ->
          let* s = g y in
          match s with
          | Error e -> return (Error e)
          | Ok z ->
            f z

   Did you remember to change the opened syntax module?

The ``Lwt_result_syntax`` module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Octez provides the ``Lwt_result_syntax`` module to help handle promises
of results.

-  ``let*``: a binding operator to wait for the promise to resolve
   before continuing with the value in the ``Ok`` constructor or
   interrupting with the error in the ``Error`` constructor.

   Note how the ``let*`` binding operator combines the behaviour of
   ``Lwt_syntax.( let* )`` and ``Result_syntax.( let* )``. Also note
   that the different ``let*``\ s are differentiated by context;
   specifically by what syntax module has been opened.

-  ``return: 'a -> ('a, 'e) result Lwt.t``: the expression ``return x``
   is a promise already successfully resolved to ``x``. More formally,
   ``return x`` is equivalent to
   ``Lwt_syntax.return (Result_syntax.return x)``.

-  ``fail: 'e -> ('a, 'e) result Lwt.t``: the expression ``fail e`` is a
   promise already unsuccessfully resolved with the error ``e``. More
   formally, ``fail e`` is equivalent to
   ``Lwt_syntax.return (Result_syntax.fail e)``.

The following functions offer additional, less often used
functionalities.

-  ``both : ('a, 'e) result Lwt.t -> ('b, 'e) result Lwt.t -> ('a * 'b, 'e list) result Lwt.t``:
   the expression ``both p1 p2`` is a promise that resolves
   successfully if both ``p1`` and ``p2`` resolve successfully. It
   resolves unsuccessfully if either ``p1`` or ``p2`` resolve
   unsuccessfully.

   Note that in the expression ``both p1 p2``, both promises ``p1`` and
   ``p2`` are evaluated concurrently. Moreover, the returned promise
   only resolves once both promises have resolved, even if one resolves
   unsuccessfully.

   Note that this syntax module does not offer ``and*`` as a binding
   operator alias for ``both``. This is because, as with
   ``Result_syntax``, the type for errors in ``both`` is not stable (it
   is ``'e`` on the argument side and ``'e   list`` on the return side).
   This hinders practical uses of ``and*``.

-  ``all : ('a, 'e) result Lwt.t list -> ('a list, 'e list) result Lwt.t``:
   the function ``all`` is a generalisation of ``both`` from tuples to
   lists.

   Note that, as a generalisation of ``both``, in a call to the function
   ``all``, all the promises in the provided list make progress towards
   resolution concurrently and continue to evaluate until resolution
   regardless of successes and failures.

-  ``join : (unit, 'e) result Lwt.t list -> (unit, 'e list) result Lwt.t``:
   the function ``join`` is a specialisation of ``all`` for lists of
   unit-type expressions (typically, for side-effects).

The following functions do not provide new functionalities but they are
useful for small performance gains or for shorter syntax.

-  ``return_unit`` is equivalent to ``return ()`` but it avoids one
   allocation. This is important in parts of the code that are
   performance critical, and it is a good habit to take otherwise.

   | ``return_nil`` is equivalent to ``return []`` but it avoids one
     allocation.
   | ``return_true`` is equivalent to ``return true`` but it avoids one
     allocation.
   | ``return_false`` is equivalent to ``return false`` but it avoids
     one allocation.

   | ``return_none`` is equivalent to ``return None`` but it avoids one
     allocation.
   | ``return_some x`` is equivalent to ``return (Some x)`` and it is
     provided for completeness.

   Note that, like with ``Result_syntax``, this syntax module does not
   provide ``return_ok`` and ``return_error``. This is to avoid nested
   ``result`` types. If you need to nest ``result``\ s you can do so by
   hand.

-  ``let+`` is a binding operator similar to ``let*`` but when the
   expression which follows the ``in`` returns a non-promise value. In
   other words, ``let+ x = e in e'`` is equivalent to
   ``let* x = e in return (e')``.

   The ``let+`` is purely for syntactic conciseness (compared to the ``*``
   variant), use it if it makes your code more readable.

.. _exercises-3:

Exercises
^^^^^^^^^

-  Write the implementation for

   ::

      (** [map f [x1; x2; ..]] is a promise for a list [[y1; y2; .. ]] where [y1]
          is the value that [f x1] successfully resolves to, etc. except if [f]
          resolves unsuccessfully on one of the input in which case it also
          resolves unsuccessfully with the same error as [f]. *)
      map : ('a -> ('b, 'e) result Lwt.t) -> 'a list -> ('b list, 'e) result Lwt.t

   How does your code compare to the one in the ``result``-only variant
   of this exercise?

   Note that this exercise is for learning only. You won’t need to write
   this function in Octez. Indeed, a helper function which does exactly
   that is provided in the extended standard library of Octez.

-  Make your ``map`` function tail-recursive.

-  What type error is triggered by the following code?

   ::

      open Lwt_result_syntax ;;
      let ( and* ) = both ;;
      let _ =
        let* x = return 0 and* y = return 1 in
        let* z = return 2 in
        return (x + y + z) ;;

-  Rewrite the following function without ``match``-``with``

   ::

      let compose3 f g h x =
        let open Lwt_syntax in
        let* y_result = f x in
        match y_result with
        | Error e -> return (Error e)
        | Ok y ->
          let* z_result = g y in
          match z_result with
          | Error e -> return (Error e)
          | Ok z ->
            h z

Converting errors of promises
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Remember that, with ``Result_syntax``, you cannot mix different types of
errors in a single sequence of ``let*``. This also applies to
``Lwt_result_syntax``. Indeed, the type checker will prevent you from
doing so.

You can use the same ``Result.map_error`` function as for plain
``result``\ s. But when you are working with promises of ``result``, the
syntactic cost of doing so is high:

::

   let open Lwt_result_syntax in
   let* config =
     let open Lwt_syntax in
     let* config_result = read_configuration () in
     Lwt.return (Result.map_error (fun () -> ..) config_result)
   in
   ..

To avoid this syntactic weight, the ``Lwt_result_module`` provides a
dedicated function:

::

   lwt_map_error : ('e -> 'f) -> ('a, 'e) result Lwt.t -> ('a, 'f) result Lwt.t

Lifting
~~~~~~~

Occasionally, whilst you are working with promises of ``result`` (i.e.,
working with values of the type ``(_, _) result Lwt.t``), you will need
to call a function that returns a simple promise (a promise that cannot
fail, a promise of a value that’s not a ``result``, i.e., a value of
type ``_ Lwt.t``) or a simple result (an immediate value of a
``result``, i.e., a value of type ``(_, _) result``). This is common
enough that the module ``Lwt_result_syntax`` provides helpers dedicated
to this.

From ``result``-only into Lwt-``result``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The module ``Lwt_result_syntax`` includes the binding operator ``let*?``. It is
dedicated to binding Result-only expressions.

::

   let*? x = check foo bar in (* Result-only: checking doesn't yield *)
   ..


.. sidebar:: Mnemonic

   The ``let*?`` binding operator uses the question mark (``?``) to represent
   the uncertainty of the ``result``. Is it a success? Is it a failure?


From Lwt-only into Lwt-``result``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The module ``Lwt_result_syntax`` includes the binding operator ``let*!``. It is
dedicated to binding Lwt-only expressions.

::

   let*! x = Events.emit foo bar in (* Lwt-only: logs can't fail *)
   ..


.. sidebar:: Mnemonic

   The ``let*!`` binding operator uses the exclamation mark (``!``) to represent
   the impossibility of errors: Thou shall not fail!


Wait! There is too much! What module am I supposed to open locally and what operators should I use?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are feeling overwhelmed by the different syntax modules, here are
some simple guidelines.

-  If your function returns ``(_, _) result Lwt.t`` values, then you
   start the function with ``let open Lwt_result_syntax in``. Within the
   function you use

   -  ``let`` for vanilla expressions,
   -  ``let*`` for Lwt-``result`` expressions,
   -  ``let*!`` for Lwt-only expressions,
   -  ``let*?`` for ``result``-only expressions.

   And you end your function with a call to ``return``.

-  If your function returns ``(_, _) result`` values, then you start the
   function with ``let open Result_syntax in``. Within the function you
   use

   -  ``let`` for vanilla expressions,
   -  ``let*`` for ``result`` expressions,

   And you end your function with a call to ``return``.

-  If your function returns ``_ Lwt.t`` values, then you start the
   function with ``let open Lwt_syntax in``.

   -  ``let`` for vanilla expressions,
   -  ``let*`` for Lwt expressions,

   And you end your function with a call to ``return``.

These are rules of thumb and there are exceptions to them. Still, they
should cover most of your uses and, as such, they are a good starting
point.

What’s an error?
~~~~~~~~~~~~~~~~

So far, this tutorial has considered errors in an abstract way. Most of
the types carried by the ``Error`` constructors have been parameters
(``'e``). This is a common pattern for higher-order functions that
compose multiple ``result`` and Lwt-``result`` functions together. But,
in practice, not every function is a higher-order abstract combinator
and you sometimes need to choose a concrete error. This section explores
common choices.

**A dedicated algebraic data type**

Often, a dedicated algebraic data type is appropriate. A sum type
represents the different kinds of failures that might occur. E.g.,
``type hashing_error = Not_enough_data | Invalid_escape_sequence``. A
product type (typically a record) carries multiple bits of information
about a failure. E.g.,
``type parsing_error = { source: string; location: int; token: token; }``

This approach works best when a set of functions (say, all the functions
of a module) have similar ways to fail. Indeed, when that is the case,
you can simply define the error type once and all calls to these
functions can match on that error type if need be.

E.g., `binary encoding and decoding errors in
data-encoding <https://nomadic-labs.gitlab.io/data-encoding/data-encoding/Data_encoding/Binary/index.html>`__.

**Polymorphic variants**

In some cases, the different functions of a module may each fail with
different subsets of a common set of errors. In such a case, you can use
`polymorphic variants <https://ocaml.org/manual/polyvariant.html>`__ to
represent errors. E.g.,

::

   val connect_to_peer:
     address -> (connection, [ `timeout | `connection_refused ]) result Lwt.t
   val send_message:
     connection -> signing_key -> string ->
       (unit, [ `timeout | `connection_closed ]) result Lwt.t
   val read_message:
     connection ->
       (string, [ `timeout | `unknown_signing_key | `invalid_signature | `connection_closed ]) result Lwt.t
   val close_connection: connection -> (unit, [ `unread_messages of string list ]) result

The benefit of this approach is that the caller can compose the
different functions together easily and match only on the union of
errors that may actually happen. The type checker keeps track of the
variants that can reach any program point.

::

   let handshake conn =
     let open Lwt_result_syntax in
     let* () = send_message conn "ping" in
     let* m = read_message conn in
     if m = "pong" then
       return ()
     else
       `unrecognised_message m

   let handshake conn =
     let open Lwt_syntax in
     let* r = handshake conn in
     match r with
     | Ok () -> return_unit
     | Error (`unknown_signing_key | `invalid_signature) ->
       (* we ignore unread messages if the peer had signature issues *)
       let _ = close_connection conn in
       return_unit
     | Error (`timeout | `connection_closed) ->
       match close_connection with
       | Ok () -> return_unit
       | Error (`unread_messages msgs) ->
         let* () = log_unread_messages msgs in
         return_unit

**A human-readable string**

In some cases, there is nothing to be done about an error but to inform
the user. In this case, the error may just as well be the message.

It is important to note that these messages are not generally meant to
be matched against. Indeed, such messages may not be stable and even if
they are, they probably don’t carry precise enough information to be
acted upon.

You should only use ``string`` as an error type when the error is not
recoverable and you should not try to recover from ``string`` errors (or
more precisely, your recovery should not depend on the content of the
string).

**An abstract type**

If the error is not meant to be recovered from, it is sometimes ok to use an
abstract type. This is generally useful at the interface of a module,
specifically when the functions within the module are meant to inspect
the errors and possibly attempt recovery, but the callers outside of the
modules are not.

If you do use an abstract type for errors, you should also provide a
pretty-printing function.

**A wrapper around one of the above**

Sometimes you want to add context or information to an error. E.g.,

::

   type 'a with_debug_info = {
     payload: 'a;
     timestamp: Time.System.t;
     position: string * int * int * int;
   }

   let with_debug_info ~position f =
     match f () with
     | Ok _ as ok -> ok
     | Error e -> Error { payload = e; timestamp = Time.System.now (); position }

This specific example can be useful for debugging, but other wrappers
can be useful in other contexts.

**Mixing error types**

It is difficult to work with different types of errors within the same
function. This most commonly happens if you are calling functions from
different libraries, which use different types of errors.

This is difficult because the errors on both sides of the binding
operator are the same.

::

   val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

The error monad provides some support to deal with multiple types of
errors at once. But this support is limited. It is not generally an
issue because error-mixing is somewhat rare: it tends to happen at the
boundary between different levels of abstractions.

If you encounter one of these situations, you will need to convert all
the errors to a common type.

::

   type error = Configuration_error | Command_line_error

   let* config =
     match Config.parse_file config_file with
     | Ok _ as ok -> ok
     | Error Config.File_not_found -> Ok Config.default
     | Error Config.Invalid_file -> Error Configuration_error
   in
   let* cli_parameters =
     match Cli.parse_parameters () with
     | Ok _ as ok -> ok
     | Error Cli.Invalid_parameter -> Error Command_line_error
   in
   ..

You can also use the ``Result.map_error`` and ``lwt_map_error``
functions introduced in previous sections.

Wait! It was supposed to be “one single uniform way of dealing with errors”! What is this?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The error management in Octez is a unified way (syntax modules
with regular, predictable interfaces) of handling different types of
errors.

The variety of errors is a boon in that it lets you use whatever is the
most appropriate for the part of the code that you are working on.
However, the variety of errors is also a curse in that stitching
together functions which return different errors requires boilerplate
conversion functions.

That’s where the global ``error`` type comes in: a unified type for
errors. And that’s for the next section to introduce.

META COMMENTARY
~~~~~~~~~~~~~~~

The previous sections are not Octez-specific. True, the syntax modules
are defined within the Octez source tree, but they could be released
separately (and they will be) or they could easily be replicated in a
separate project.

The next sections are Octez-specific. They introduce types and values
that are used within the whole of Octez.

| You should take this opportunity to take a break.
| Come back in a few minutes.
