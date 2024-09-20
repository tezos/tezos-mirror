Performance profiling
~~~~~~~~~~~~~~~~~~~~~

If you are interested to know how much time is spent in different functions in
your program, this is how to proceed.

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

  - Let ``perf`` run ``octez-node``: ``perf record -g -F 99 --call-graph=dwarf
    -- ./octez-node run ...``

    This will write the output in file ``perf.data`` after having stopped the
    node with ``Ctrl-C``.

  In both cases, the ``-F`` argument specifies the frequency of sampling of data
  (in hertz).

  If too much data is generated, use a smaller value. If data is not precise
  enough, try using a higher value.

- display the result with ``perf report``, or use a more advanced
  visualizer (recommended). Such visualizers include:

  * `flamegraph <https://github.com/brendangregg/FlameGraph>`_: command-line
    tool for generating flamegraphs (`example
    <https://gitlab.com/tezos/tezos/uploads/f8f8cece73da52b54fd9c79364e656e1/flame.svg>`__
    for octez-node)
  * `gprof2dot <https://github.com/jrfonseca/gprof2dot>`_: command-line tool for
    generating callgraphs (`example
    <https://gitlab.com/tezos/tezos/uploads/8640f489ad8002271fe41bbd0c34dfdc/callgraph.svg>`__
    for octez-node)
  * `hotspot <https://github.com/KDAB/hotspot>`_: a GUI for the ``perf`` tool
