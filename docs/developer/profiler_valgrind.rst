Memory profiling the C heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Install ``valgrind`` and ``massif-visualizer``

::

    valgrind --tool=massif octez-node run ...

- Stop with ``Ctrl-C`` then display with

::

    massif-visualizer massif.out.pid
