The Profiler module
===================

Octez offers a profiler module that is better suited than external tools like
``perf`` for the monadic programming model of Lwt and for generating traces, as
it offers more control and is able to handle the elusive nature of Lwt.

Example of use
^^^^^^^^^^^^^^

This step-by-step guide shows how a profiler is created, plugged and used in
Octez (based on the ``lib_shell`` profiling):

Example file
^^^^^^^^^^^^

We'll start with this simple file:

.. code-block:: OCaml

   let read_int ic =
     let rec aux acc =
       match input_char ic with
       | ' ' | '\n' -> acc
       | c -> aux ((10 * acc) + (Char.code c - 48))
     in
     aux 0

   let read_test_int () =
     let ic = open_in sample in
     let max = ref 0 in
     try
       while true do
         let e = read_int ic in
         if e > !max then max := e
       done
     with End_of_file ->
       close_in ic ;
       Format.eprintf "%d@." !max

   let read_test_line () =
     let ic = open_in sample in
     let max = ref 0 in
     try
       while true do
         input_line ic |> String.split_on_char ' '
         |> List.iter (fun e ->
                let e = int_of_string e in
                if e > !max then max := e)
       done
     with End_of_file ->
       close_in ic ;
       Format.eprintf "%d@." !max

   let () =
     read_test_int () ;
     read_test_line () ;
     read_test_scanf ()

We have three functions that we would want to profile to see which one is faster
and to see what's taking the longest time in each.

Create a new profiler
^^^^^^^^^^^^^^^^^^^^^

Start by creating a unique profiler:

.. code-block:: OCaml

   let read_profiler = unplugged ()

You can see it as an API to the profiling machinery that isn't able to do
anything useful for now. Why is that? Because you need to attach it to a
``backend``.

A ``backend`` is defined in two steps:

- Select a ``Driver`` (like "this driver writes text files in an unix
  filesystem")
- Define a specific ``instance`` of a ``Driver`` (like "this driver will write in
  this file with this level of detail")

Octez already provides two ``Drivers``:

.. code-block:: OCaml

   val auto_write_as_txt_to_file : (string * Profiler.verbosity) Profiler.driver

   val auto_write_as_json_to_file : (string * Profiler.verbosity) Profiler.driver

These ``Drivers`` are specifically crafted to write text or JSON files in an Unix
filesystem. As you can see, they expect two 'arguments', a ``string`` (where to
write) and a ``Profiler.verbosity`` (the maximum level of detail/verbosity
expected from the profiler).

.. code-block:: OCaml

   type verbosity = Notice | Info | Debug

For this tutorial, we will use ``verbosity = Info``, but developers are
encouraged to use their preferred option.

To choose the verbosity of the profiler at runtime, the ``PROFILING``
environment variable is used. It follows the same pattern as the ``TEZOS_LOG``
environment variable (see :doc:`../user/logging`).

Starting a node with
``PROFILING='shell_profiling->Notice;mempool_profiling->Debug'`` will set the
maximum verbosity for these two profilers and execute the rest of the profilers
with no maximal verbosity.

We can now easily create an instance for a ``Driver``:

.. code-block:: OCaml

   let read_instance =
     Tezos_profiler.Profiler.instance
       Tezos_profiler_backends.Simple_profiler.auto_write_as_txt_to_file
       ("read_profiling.txt", Profiler.Info)

We just need one last thing. We have a ``read_profiler`` and a ``read_instance`` that
writes in ``read_profiling.txt`` but they are not connected. That's where the
following function needs to be used:

.. code-block:: OCaml

   val plug : profiler -> instance -> unit

So we just need to

.. code-block:: OCaml

   Profiler.plug read_profiler read_instance

And voilà!, when we'll call functions attached to ``read_profiler`` the reports
will be properly written in ``read_profiling.txt`` (It should be noted here that
a profiler can be plugged to multiple instances allowing to write infos in
different files or with different format).

Since it would be a little bit annoying to call each functions by giving it
``read_profiler`` as a parameter, the ``Profiler`` module offers a convenient
function that creates a module allowing to call all the ``read_profiler`` functions
without providing it:

.. code-block:: OCaml

   val wrap : profiler -> (module GLOBAL_PROFILER)

This will give access to the functions in :package-api:`the Profiler.GLOBAL_PROFILER module <octez-libs/Tezos_profiler/Profiler/module-type-GLOBAL_PROFILER/index.html>`.

Use the profiler
^^^^^^^^^^^^^^^^
We can now wrap our profiler to create a module that we will use to profile our
code.

.. code-block:: OCaml

   module Read_profiler = (val Profiler.wrap read_profiler)

Since ``Read_profiler`` is already plugged to ``read_instance``, calling
``Read_profiler`` functions will work as expected.

We can now start monitoring our code. We can start with a simple change:

.. code-block:: OCaml

   let () =
     Read_profiler.plug instance ;
     (Read_profiler.record_f ~verbosity:Info ("read_test_line", []) @@ fun () -> read_test_line ()) ;
     (Read_profiler.record_f ~verbosity:Info ("read_test_int", []) @@ fun () -> read_test_int ()) ;
     Read_profiler.record_f ~verbosity:Info ("read_test_scanf", []) @@ fun () -> read_test_scanf ()

Note: the ``Read_profiler`` functions take a ``(id, metadata) : string * metadata`` as their
first parameter:
- ``id`` is used to create sections and represent the part being profiled by the function
- ``metadata`` is used by backends that need specific informations (like Prometheus)

Looking at the result gives us:

.. code-block:: OCaml

   2024-09-18T09:46:46.376-00:00 read_test_line .... 1           42.707ms 100% +0.002ms
   2024-09-18T09:46:46.419-00:00 read_test_int ..... 1          106.481ms 100% +42.865ms
   2024-09-18T09:46:46.525-00:00 read_test_scanf ... 1          122.623ms 100% +149.439ms

Now that we know that the profiler outputs correctly to our chosen file, let's
monitor our functions more precisely:

.. code-block:: OCaml

   let profiler = Read_profiler.unplugged ()

   module Read_profiler = (val Profiler.wrap profiler)

   let instance =
     Profiler.instance Tezos_profiler_backends.Simple_profiler.auto_write_as_txt_to_file
       ("/tmp/test_profiler.txt", Profiler.Info)

   let read_int ic =
     let rec aux acc =
       match input_char ic with
       | ' ' | '\n' -> acc
       | c -> aux ((10 * acc) + (Char.code c - 48))
     in
     aux 0

   let read_test_int () =
     Read_profiler.record_f  ~verbosity:Info ("read_test_int", []) @@ fun () ->
     let ic = Read_profiler.aggregate_f ~verbosity:Info ("open_in", []) @@ fun () -> open_in sample in
     let max = ref 0 in
     try
       while true do
         Read_profiler.aggregate_f ~verbosity:Info ("read_int", []) @@ fun () ->
         read_int ic |> fun e -> if e > !max then max := e
       done
     with End_of_file ->
       Read_profiler.aggregate_f ~verbosity:Info ("close_in", []) @@ fun () ->
       close_in ic ;
       Format.eprintf "%d@." !max

   let read_test_line () =
     Read_profiler.record_f ~verbosity:Info ("read_test_line", []) @@ fun () ->
     let ic = Read_profiler.aggregate_f ~verbosity:Info ("open_in", []) @@ fun () -> open_in sample in
     let max = ref 0 in
     try
       while true do
         Read_profiler.span_f ~verbosity:Info (["input_line"], []) @@ fun () ->
         input_line ic |> String.split_on_char ' '
         |> List.iter (fun e ->
                let e = int_of_string e in
                if e > !max then max := e)
       done
     with End_of_file ->
       Read_profiler.aggregate_f ~verbosity:Info ("close_in", []) @@ fun () ->
       close_in ic ;
       Format.eprintf "%d@." !max

   let read_test_scanf () =
     Read_profiler.record_f ~verbosity:Info ("read_test_scanf", []) @@ fun () ->
     let ic =
       Read_profiler.aggregate_f ~verbosity:Info ("open_in", []) @@ fun () -> Scanf.Scanning.open_in sample
     in
     let max = ref 0 in
     try
       while true do
         Read_profiler.mark ~verbosity:Info (["Scanf.bscanf"], []) ;
         Scanf.bscanf ic "%d " (fun i -> i) |> fun e -> if e > !max then max := e
       done
     with End_of_file ->
       Read_profiler.aggregate_f ~verbosity:Info ("close_in", []) @@ fun () ->
       Scanf.Scanning.close_in ic ;
       Format.eprintf "%d@." !max

   let () =
     Read_profiler.plug instance ;
     read_test_line () ;
     read_test_int () ;
     read_test_scanf ()

You should obtain something like this:

.. code-block::

   2024-09-18T09:19:13.555-00:00
   read_test_line ... 1           44.079ms 101% +0.002ms
     close_in ....... 1            0.049ms 101%
     input_line ..... 1002        42.992ms 100%
     open_in ........ 1            0.013ms 109%
   2024-09-18T09:19:13.599-00:00
   read_test_int .... 1         1660.119ms 100% +44.247ms
     close_in ....... 1            0.048ms  99%
     open_in ........ 1            0.035ms 100%
     read_int ....... 1003003    807.536ms 101%
   2024-09-18T09:19:15.259-00:00
   read_test_scanf .. 1          300.168ms  99% +1s704.432ms
     Scanf.bscanf ... 1003003
     close_in ....... 1            0.063ms 102%
     open_in ........ 1            0.036ms 100%

The execution time of ``read_int`` seems off. Replacing the following lines:

.. code-block:: OCaml

   Read_profiler.aggregate_f ~verbosity:Info ("read_int", []) @@ fun () ->
   read_int ic |> fun e -> if e > !max then max := e

By:

.. code-block:: OCaml

   Read_profiler.mark ~verbosity:Info (["read_int"], []) ;
   read_int ic |> fun e -> if e > !max then max := e

Gives a completely different result:

.. code-block::

   2024-09-18T09:25:23.516-00:00
   read_test_line ... 1           44.440ms 100% +0.001ms
     close_in ....... 1            0.081ms 100%
     input_line ..... 1002        43.287ms 100%
     open_in ........ 1            0.014ms 102%
   2024-09-18T09:25:23.560-00:00
   read_test_int .... 1          267.466ms 100% +44.609ms
     close_in ....... 1            0.046ms 103%
     open_in ........ 1            0.008ms 102%
     read_int ....... 1003003
   2024-09-18T09:25:23.828-00:00
   read_test_scanf .. 1          289.068ms 100% +312.139ms
     Scanf.bscanf ... 1003003
     close_in ....... 1            0.055ms 103%
     open_in ........ 1            0.037ms  98%

This is expected because ``aggregate``-like and ``record``-like functions will call
``Unix.gettimeofday`` for each occurrence. Here we're calling it ``1003003``
times and losing a lot of time. Out of the 1660ms spent in ``read_int``, almost
900ms were spent computing ``Unix.gettimeofday``. You can either choose to keep
these slowdowns while making sure you know where they happen and why they
happen or you can choose simpler functions like ``mark`` that just count a
number of occurrences.

As you can see, though, monitoring your code with the ``Profiler`` can lead to
extreme slowdowns. The first solution is to call ``Profiler.plug`` only when
needed. Since your ``profiler`` is just an API, calling its functions has little
to no impact. The other solution is to use the ``PPX`` specially crafted for the ``Profiler``.

.. toctree::
   :maxdepth: 2
   :caption: PPX Profiler

   ppx_profiler
