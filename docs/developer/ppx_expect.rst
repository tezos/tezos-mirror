Ppx_expect
==========

:ref:`Ppx_expect<ppx_expect_section>` is a framework for writing tests for OCaml
code printing textual output.

There is pretty comprehensive documentation about ``Inline expectation tests`` and the ``ppx_expect`` tool for writing them:
 - `Dune documentation about inline expectation tests <https://dune.readthedocs.io/en/stable/tests.html#inline-expectation-tests>`_.
 - `Ppx_expect README <https://github.com/janestreet/ppx_expect>`_.

Here, we will just cover enough to get started using them inside the Octez codebase.

How to run tests
----------------

If you want to run tests and see whether they succeed, build the standard ``runtest`` alias::

    dune runtest src/lib_stdlib
    # or dune build @@src/lib_stdlib/runtest

The result would look like this::

    File "src/lib_stdlib/bloomer.ml", line 97, characters 0-817: random_read_writes (0.006 sec)
    File "src/lib_stdlib/bloomer.ml", line 123, characters 0-1339: peek and poke work with bits = [1 .. Sys.int_size - 7] (0.012 sec)
    File "src/lib_stdlib/bloomer.ml", line 163, characters 0-739: sequential_read_writes (0.021 sec)
    File "src/lib_stdlib/bloomer.ml", line 182, characters 0-705: read_over_write (0.001 sec)
    File "src/lib_stdlib/bloomer.ml", line 282, characters 0-835: consistent_add_mem_countdown (0.275 sec)
    File "src/lib_stdlib/bloomer.ml", line 309, characters 0-1531: consistent_add_countdown_count (2.079 sec)
    File "src/lib_stdlib/bloomer.ml", line 456, characters 4-137 (11.966 sec)
    File "src/lib_stdlib/bloomer.ml", line 461, characters 4-1695: <<match Sys.getenv_opt "BLOOMER_TEST_GNUPLOT_PA[...]>> (0.000 sec)

A test can fail because it generates an output different from what is expected or because it raises an exception.
Failures are reported as a diff in both cases.

Different output::

    File "src/proto_alpha/lib_client/limit.ml", line 1, characters 0-0:
    ------ src/proto_alpha/lib_client/limit.ml
    ++++++ src/proto_alpha/lib_client/limit.ml.corrected
    File "src/proto_alpha/lib_client/limit.ml", line 57, characters 0-1:
     |      if eq x y then Result.return_some x
     |      else error_with "Limit.join: error (%s)" where
     |
     |let%expect_test "join" =
     |  let pp_print_err fmt = function
     |    | Result.Error _ -> Format.pp_print_string fmt "error"
     |    | Ok x ->
     |        Format.(
     |          pp_print_option
     |            ~none:(fun fmt () -> pp_print_string fmt "None")
     |            pp_print_bool)
     |          fmt
     |          x
     |  in
     |  let print x = Format.fprintf Format.std_formatter "%a" pp_print_err x in
     |  print (join ~where:__LOC__ Bool.equal (Some true) (Some true)) ;
    -|  [%expect {| false |}] ;
    +|  [%expect {| true |}] ;
     |  print (join ~where:__LOC__ Bool.equal None None) ;
     |  [%expect {| None |}] ;
     |  print (join ~where:__LOC__ Bool.equal None (Some true)) ;
     |  [%expect {| true |}] ;
     |  print (join ~where:__LOC__ Bool.equal (Some true) None) ;
     |  [%expect {| true |}] ;
     |  print (join ~where:__LOC__ Bool.equal (Some true) (Some false)) ;
     |  [%expect {| error |}]
     |
     |let get ~when_unknown = function
     |  | None -> error_with "Limit.get: %s" when_unknown
     |  | Some x -> ok x
     |
     |let%expect_test "get" =
     |  let pp_print_err fmt = function
     |    | Result.Error _ -> Format.fprintf fmt "error"

Exception raised::

    File "src/lib_stdlib/bloomer.ml", line 1, characters 0-0:
    ------ src/lib_stdlib/bloomer.ml
    ++++++ src/lib_stdlib/bloomer.ml.corrected
    File "src/lib_stdlib/bloomer.ml", line 309, characters 0-1:
     |    let bloomer = create ~hash ~index_bits ~hashes ~countdown_bits in
     |    let rec init n acc =
     |      if n = 0 then acc
     |      else
     |        let x = Random.int (1 lsl 29) in
     |        add bloomer x ;
     |        assert (mem bloomer x) ;
     |        init (n - 1) (x :: acc)
     |    in
     |    let all = init 1000 [] in
     |    for _ = 0 to (1 lsl countdown_bits) - 2 do
     |      List.iter (fun x -> assert (mem bloomer x)) all ;
     |      countdown bloomer
     |    done ;
     |    List.iter (fun x -> assert (not (mem bloomer x))) all
     |  done
    +|[@@expect.uncaught_exn {|
    +|  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
    +|     This is strongly discouraged as backtraces are fragile.
    +|     Please change this test to not include a backtrace. *)
    +|
    +|  "Assert_failure src/lib_stdlib/bloomer.ml:287:4"
    +|  Raised at Tezos_stdlib__Bloomer.(fun) in file "src/lib_stdlib/bloomer.ml", line 287, characters 4-18
    +|  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]


How to add tests
----------------

Ppx_expect is based on ``ppx_inline_test`` machinery. That is, it
collects tests automatically for you.

To add a new test, make sure you have the following in your library stanza in the dune file::

    (inline_tests)
    (preprocess (pps ppx_expect))

In the manifest, just add the following argument to the library containing expect tests::

    ~inline_tests:ppx_expect

Adding new tests is then just a matter of adding ``let%expect_test`` at top level::

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
(e.g. if you don't want to polute your source/binary or if your want to only test the exposed API).

Integration with Lwt
---------------------

Ppx_expect can be used in combination with Lwt, see the
`package description <https://ocaml.org/p/ppx_expect/latest>`_.
This integration has not been tested on the Octez codebase yet, hence some work will be
needed to a have specific support for the codebase.
