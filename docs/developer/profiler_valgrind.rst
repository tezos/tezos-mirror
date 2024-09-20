Memory profiling the C heap
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several language-independent tools are available for displaying the memory usage
of the heap in any compiled program.

- Install ``valgrind`` and ``massif-visualizer``

::

   valgrind --tool=massif octez-node run ...

- Stop with ``Ctrl-C`` then display with

::

   massif-visualizer massif.out.pid
