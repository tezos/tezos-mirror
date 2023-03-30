Profiling the Octez node
========================

Memory profiling the OCaml heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Install an OCaml switch with the ``statmemprof`` patch:

  ``4.04.2+statistical-memprof`` or ``4.06.0+statistical-memprof``

- Install ``statmemprof-emacs``.

- Enable loading ``statmemprof`` into the node.

  Add the ``statmemprof-emacs`` package as a dependency to the main package, and add
  ``let () = Statmemprof_emacs.start 1E-4 30 5`` to the ``node_main.ml`` file.

  Arguments:

  - ``sampling_rate`` is the sampling rate of the profiler. Good value: ``1e-4``.
  - ``callstack_size`` is the size of the fragment of the call stack which is captured for each sampled allocation.
  - ``min_sample_print`` is the minimum number of samples under which the location of an allocation is not displayed.

- Load sturgeon into emacs, by adding this to your ``.emacs``:

::

    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
     (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

    (require 'sturgeon)

- Launch the node then connect to it with sturgeon.

  If the process is launched with pid ``1234`` then

::

    M-x sturgeon-connect
    octez-nodememprof.1234.sturgeon

  (tab-completion works for finding the socket name)

Memory profiling the C heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Install ``valgrind`` and ``massif-visualizer``

::

    valgrind --tool=massif octez-node run ...

- Stop with ``Ctrl-C`` then display with

::

    massif-visualizer massif.out.pid


Performance profiling
~~~~~~~~~~~~~~~~~~~~~

- Install ``perf`` (the ``linux-perf`` package for debian).

  If the package does not exist for your current kernel, a previous
  version can be used. Substitute the ``perf`` command to ``perf_4.9``
  if your kernel is 4.9).

- Either:

   - Run the node, find the pid.

     Attach ``perf`` with ``perf record -p pid -F 99 --call-stack dwarf``.

     Then stop capturing with ``Ctrl-C``. This can represent a lot of
     data. Don't do that for too long. If this is too much you can remove
     the ``--call-stack dwarf`` to get something more manageable, but
     interpreting the information can be harder.

   - Let ``perf`` run ``octez-node``: ``perf record -g -F 99 --call-graph=dwarf -- ./octez-node run ...``

     This will write ``perf.data`` after having stopped the node with ``Ctrl-C``.

  In both cases, the ``-F`` argument specifies the frequency of sampling of data (in hertz).
  If too much data is generated, use a smaller value. If data is not precise
  enough, try using a higher value.

- display the result with ``perf report``, or use a more advanced
  visualizer (recommended). Such visualizers include:

   - `flamegraph <https://github.com/brendangregg/FlameGraph>`_: command-line
     tool for generating flamegraphs
     (`example <https://gitlab.com/tezos/tezos/uploads/f8f8cece73da52b54fd9c79364e656e1/flame.svg>`__ for octez-node)
   - `gprof2dot <https://github.com/jrfonseca/gprof2dot>`_: command-line
     tool for generating callgraphs
     (`example <https://gitlab.com/tezos/tezos/uploads/8640f489ad8002271fe41bbd0c34dfdc/callgraph.svg>`__ for octez-node)
   - `hotspot <https://github.com/KDAB/hotspot>`_: a GUI for the ``perf`` tool
