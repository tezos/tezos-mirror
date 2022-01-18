Time measurement PPX
====================

The time measurement PPX is an OCaml preprocessing tool that intends to
embed generated benchmark tooling into specific pieces of OCaml code.

It is able to measure the time spent in the execution of annotated OCaml
expressions and to log these measurements when desired. Since it uses
``Tezos_event_logging`` for the logging part, this PPX can easily be used
together with ``Tezt`` framework to perform the benchmarking of specific
parts of Tezos node.

**This PPX is only intended to be used for tests. As the current runtime
implementation performs memory allocation, an unwise usage could mess with
the garbage collector or blow up your memory.**

**PLEASE, MAKE SURE THAT IT IS NOT ACTIVATED WHEN COMPILING CODE FOR
PRODUCTION.**

Getting started
---------------

Suppose we want to measure the performance of some specific parts of
the following OCaml function inside the module ``lib_my_module``:

.. code-block:: OCaml

    let my_function () =
      let a = f () in
      let b = g () in
      h () >>= fun c ->
      foo a b c

Suppose also that module ``lib_my_module`` contains the following dune file:

.. code-block::

    (library
      (name tezos_my_module)
      (libraries lwt)
      (flags (:standard -open Lwt)))

We can measure the execution time of ``f ()`` and ``g ()`` and log them by
adding the following OCaml attributes:

.. code-block:: OCaml

    let my_function () =
      let a = f () [@time.duration f_time] in
      let b = g () [@time.duration g_time] in
      h () >>= fun c ->
      foo a b c [@time.flush]

``[@time.duration]`` will be used to measure the time of ``f ()`` and ``g ()``
expressions execution and to name them respectively ``f_time`` and
``g_time``. ``[@time.flush]`` will then be used to log these measurements.

When the preprocessing will occur, the code will be transformed as follows:

.. code-block:: OCaml

    let my_function () =
      let a = Tezos_time_measurement_runtime.Default.Time_measurement.duration
        ("f_time", [])
        (fun () -> f ())
      in
      let b = Tezos_time_measurement_runtime.Default.Time_measurement.duration
        ("g_time", [])
        (fun () -> g ())
      in
      h () >>= fun c ->
      foo a b c >>= fun __flush__id__0 ->
      Tezos_time_measurement_runtime.Default.Time_measurement.flush () >|= fun () ->
      __flush__id__0

Woah! What a mess... Let's see what this means.

The first thing that can be noted is that our annotated expression ``f ()`` has been
wrapped in a closure ``fun () -> f ()`` to delay the execution of ``f ()``.
The resulting thunk is passed to the function ``Time_measurement.duration`` from
the module ``Tezos_time_measurement_runtime.Default`` along with the argument
``("f_time", [])``.

``Time_measurement.duration`` measures the current time before and after executing
the given thunk in order to compute the span between the two timestamps. The resulting
measurement is then bufferized in memory and, at last, the function evaluates in the
result of the thunk so that we can respect the invariants of the initial program.
Hence, the result of the expression ``f ()`` is well bounded to the identifier ``a``
as expected.

The couple ``("f_time", [])`` given as argument represents a key that is bound to the
time measurement in order to discriminate it later. This key is computed from the payload
that was given inside the attribute ``[@time.duration f_time]``.
``"f_time"`` is the label of the key, an identifier that represents the name of the
measurement.
``[]`` is a list of strings that contains some additional metadata. It can be useful
to distinguish several measurements registered with the same label, for example if
the expression is evaluated in a loop. In our case, the list is empty because no
metadata was provided in the attribute's payload.

The same logic applies for ``g ()``. We measure the time at the limits of its execution,
we bufferize the resulting span along with the key ``("g_time", [])``, and we then
bind the result of the expression to the identifier ``b``.

At the end of the treatment, the ``Lwt.t`` promise ``foo a b c`` is bound to a fresh
identifier ``__flush__id__0``. This permit to memoize its result while calling the
``Time_measurement.flush`` function from the module ``Tezos_time_measurement_runtime.Default``.
This function logs all the time measurements that where previously bufferized
and also removes them from memory.
The flushing promise is then bounded again to return ``__flush__id__0`` value
to preserve the program invariants as well.

That's great, but since OCaml attributes are ignored by default,
we still need to update the dune stanza of ``lib_my_module`` so that
it can take effect:

.. code-block:: OCaml

    (library
      (name tezos_my_module)
      (libraries lwt)
      (flags (:standard -open Lwt))
      (instrumentation (backend tezos-time-measurement)))

This update adds the ``tezos-time-measurement`` instrumentation backend, which,
if set using ``--instrument-with tezos-time-measurement`` on ``dune build``
command line, will preprocess our OCaml code using the PPX.

This is useful to prevent our code from embedding benchmarking tooling in
production by mistake: If no backend is specified for the compilation, added
attributes will just be ignored by the OCaml compiler and that's it!

We can now compile our ready-to-benchmark code:::

    dune build --instrument-with tezos-time-measurement

We can then run the executable:::

    ./my_program.exe

When ``my_function`` is executed, an event named ``"time_measurements.v0"``
is displayed on standard output. For example:

.. code-block::

    Aug 23 17:52:58.593 - benchmarking: time measurements:
    Aug 23 17:52:58.593 - benchmarking:   [(f_time, 0.000177); (g_time, 0.005658)]

Compatible OCaml Attributes
---------------------------

The PPX provides the handling of three attributes:

- ``[@time.duration <label> (<metadata>)]`` is used to measure the time of
  OCaml expressions execution.
  The ``<label>`` inside the payload will be used to tag the measured time.
  The ``<metadata>`` is an OCaml expression that can be added optionally
  and should evaluate to a list of ``string``\s. It can be given to add
  additional contextual information to the measurement and it can permit
  to discriminate it from other measurements registered with the same label.

  Be careful, annotating ``Lwt.t`` values with this attribute may
  not give consistent time measurements since it will only measure
  the time spent to return the corresponding promise.

- ``[@time.duration_lwt <label> (<metadata>)]`` does the same as
  ``[@time.duration]`` except that it must annotate an expression evaluating
  in a ``Lwt.t`` value. The measured time will then be the time spent by the
  promise to be fulfilled.

- ``[@time.timestamp_pre <label> (<metadata>)]`` is used to measure the current
  timestamp before the annotated expression is evaluated. The measurement
  will be tagged with the given ``<label>`` and optional ``<metadata>`` like
  with ``[@time.duration]``.

- ``[@time.duration]`` is used to log each time measurements that were registered
  using ``[@time.duration]``, ``[@time.duration_lwt]`` or ``[@time.timestamp_pre]``.
  Since logging will be done using ``tezos-event-log`` library, it must be done
  inside a ``Lwt.t`` monad. So, this attribute must be placed on an expression
  evaluating in a ``Lwt.t`` value in order to compile.

Instrumenting the tezos-node executable
---------------------------------------

A helper has been added in the ``Makefile``, so you just need to run the following
command to instrument the node during the compilation:::

    ./make enable-time-measurement
