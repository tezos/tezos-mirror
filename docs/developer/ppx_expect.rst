Ppx_expect
==========

`Ppx_expect :ref:<ppx_expect_section>` is a framework for writing tests for OCaml
code printing textual output.

There is pretty comprehensive documentation about ``Inline expectation tests`` and the ``ppx_expect`` tool for writing them:
 - `Dune documentation about inline expectation tests <https://dune.readthedocs.io/en/stable/tests.html#inline-expectation-tests>`_.
 - `Ppx_expect README <https://github.com/janestreet/ppx_expect>`_.

Here, we will just cover enough to get started using them inside the Tezos codebase.

How to run tests
----------------

If you want to run tests and see whether they succeed, build the standard runtest alias ::

    dune runtest src/lib_stdlib
    # or dune build @@src/lib_stdlib/runtest

The result would look like this ::

    File "src/lib_stdlib/bloomer.ml", line 97, characters 0-817: random_read_writes (0.006 sec)
    File "src/lib_stdlib/bloomer.ml", line 123, characters 0-1339: peek and poke work with bits = [1 .. Sys.int_size - 7] (0.012 sec)
    File "src/lib_stdlib/bloomer.ml", line 163, characters 0-739: sequential_read_writes (0.021 sec)
    File "src/lib_stdlib/bloomer.ml", line 182, characters 0-705: read_over_write (0.001 sec)
    File "src/lib_stdlib/bloomer.ml", line 282, characters 0-835: consistent_add_mem_countdown (0.275 sec)
    File "src/lib_stdlib/bloomer.ml", line 309, characters 0-1531: consistent_add_countdown_count (2.079 sec)
    File "src/lib_stdlib/bloomer.ml", line 456, characters 4-137 (11.966 sec)
    File "src/lib_stdlib/bloomer.ml", line 461, characters 4-1695: <<match Sys.getenv_opt "BLOOMER_TEST_GNUPLOT_PA[...]>> (0.000 sec)

How to add tests
----------------

Ppx_expect is based on ``ppx_inline_test`` machinery. That is, it
collects tests automatically for you.

To add a new test, make sure you have the following in your library stanza in the dune file::

    (inline_tests)
    (preprocess (pps ppx_expect))

In the manifest, just add the following argument to the library containing expect tests::

    ~inline_tests:ppx_expect

Adding new tests is then just a matter of adding ``let%expect_test`` at toplevel::

    let%expect_test "optional name" =
      print_endline "hello world";
      [%expect {||}]

Running test with the example above will fail and show you a diff between your source and the corrected one::

     |let%expect_test "optional name" =
     |  print_endline "hello world";
    -|  [%expect {||}]
    +|  [%expect {| hello world |}]

If you agree with the diff, just ask dune to promote the source and you're done::

    dune runtest --auto-promote

Where to put the tests
----------------------

Expect tests can live next to the implementation or in a different library dedicated to tests
(e.g. if you don't want to polute your source/binary or if your want to only test the exposed api).

Intergration with Lwt
---------------------

Ppx_expect can be used in combination with Lwt, see the
`README <https://github.com/janestreet/ppx_expect/blob/master/README.org#lwt>`_.
This integration has not been tested on the Tezos codebase yet, hence some work will be
needed to a have specific support for the codebase.
