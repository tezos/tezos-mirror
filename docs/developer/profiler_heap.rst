Memory profiling the OCaml heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The profiler offers specific support for displaying the memory footprint of the
OCaml heap. This is how you can use it

Using Memtrace
==============

- Install an OCaml switch that support ``statmemprof``:
  until the ``4.14.2`` or after the ``5.3``

- Install ``memtrace`` and ``memtrace_viewer``.

- Add the following line at the beginning of the program you want to profile
  ``Memtrace.trace_if_requested ();``

- Recompile your program

- Add the following environment variable to your executable command:
  ``MEMTRACE=trace.ctf ./my_executable``

- ``trace.ctf`` will be filled with data from the execution of the program and
  can be analysed with ``memtrace-viewer trace.ctf``.

- Open your favorite web browser and go to ``localhost:8080``


Using Statmemprof-emacs
=======================

This section is legacy and might not work any more with recent OCaml versions.

- Install an OCaml switch with the ``statmemprof`` patch:
  ``4.04.2+statistical-memprof`` or ``4.06.0+statistical-memprof``

- Install ``statmemprof-emacs``.

- Enable loading ``statmemprof`` into the node.

  Add the ``statmemprof-emacs`` package as a dependency to the main package, and
  add ``let () = Statmemprof_emacs.start 1E-4 30 5`` to the ``node_main.ml`` file.

  Arguments:

  * ``sampling_rate`` is the sampling rate of the profiler. Good value: ``1e-4``.
  * ``callstack_size`` is the size of the fragment of the call stack which is
    captured for each sampled allocation.
  * ``min_sample_print`` is the minimum number of samples under which the
    location of an allocation is not displayed.

- Load sturgeon into emacs, by adding this to your ``.emacs``:

::

   (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
     (when (and opam-share (file-directory-p opam-share))
     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

   (require 'sturgeon)

- Launch the node then connect to it with sturgeon.

  If the process is launched with pid ``1234`` then

  ::

     M-x sturgeon-connect octez-nodememprof.1234.sturgeon

  (tab-completion works for finding the socket name)
