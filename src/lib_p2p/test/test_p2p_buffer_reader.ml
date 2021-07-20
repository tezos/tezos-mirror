(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    P2P
    Invocation:   dune build @src/lib_p2p/test/runtest_p2p_buffer_reader
    Subject:      Tests [P2p_buffer_reader]
*)

let test_mk_buffer_1 () =
  Lib_test.Assert.assert_true
    "[mk_buffer ?len:-1] fails"
    (Result.is_error @@ P2p_buffer_reader.mk_buffer ~len:(-1) @@ Bytes.create 8)

let test_mk_buffer_2 () =
  Lib_test.Assert.assert_true
    "[mk_buffer ?pos:-1] fails"
    (Result.is_error @@ P2p_buffer_reader.mk_buffer ~pos:(-1) @@ Bytes.create 8)

let test_mk_buffer_3 () =
  Lib_test.Assert.assert_true
    "[mk_buffer ?len ?pos buf] with (len + pos > (Bytes.length buf)) fails"
    (Result.is_error
    @@ P2p_buffer_reader.mk_buffer ~pos:14 ~len:3
    @@ Bytes.create 16)

let test_mk_buffer_4 () =
  Lib_test.Assert.assert_true
    "[mk_buffer Bytes.empty] succeeds"
    (Result.is_ok @@ P2p_buffer_reader.mk_buffer Bytes.empty)

let test_mk_buffer_5 () =
  let len = 16 in
  Lib_test.Assert.assert_true
    "[mk_buffer ?len=max ?pos:0] succeeds"
    (Result.is_ok @@ P2p_buffer_reader.mk_buffer ~len ~pos:0 @@ Bytes.create len)

let test_mk_buffer_safe () =
  (* We don't need to use QCheck, because the input data we need
     is [0..some_not_too_big_int[, so we can just test all the ints
     until the chosen bound, which we set here to 128. The bound should
     be small, because a buffer of the given size is allocated. *)
  let lengths = Stdlib.List.init 128 Fun.id in
  List.iter
    (fun buf_len ->
      let safe_buffer =
        Bytes.create buf_len |> P2p_buffer_reader.mk_buffer_safe
      in
      let (pos, len, buf) =
        P2p_buffer_reader.Internal_for_tests.destruct_buffer safe_buffer
      in
      Lib_test.Assert.assert_true
        "Result.is_ok mk_buffer"
        (P2p_buffer_reader.mk_buffer ~pos ~len buf |> Result.is_ok))
    lengths

let cancel_and_assert_success canceler =
  Lwt_canceler.cancel canceler >|= function
  | Ok _ -> ()
  | Error _ -> assert false

let easy_mk_readable ?maxlength ?fresh_buf_size ?size () =
  let read_buffer = Circular_buffer.create ?maxlength ?fresh_buf_size () in
  let read_queue = Lwt_pipe.create ?size () in
  P2p_buffer_reader.mk_readable ~read_buffer ~read_queue

let test_read_waits_for_data ?maxlength ?fresh_buf_size ?size () =
  let readable = easy_mk_readable ?maxlength ?fresh_buf_size ?size () in
  let buffer = P2p_buffer_reader.mk_buffer_safe @@ Bytes.create 16 in
  let cancelled = ref false in
  let canceler = Lwt_canceler.create () in
  Lwt_canceler.on_cancel canceler (fun () ->
      cancelled := true ;
      Lwt.return_unit) ;
  let schedule () : _ Lwt.t =
    Lwt_unix.sleep 0.3 >>= fun () -> cancel_and_assert_success canceler
  in
  Lwt_main.run
    (Lwt.async schedule ;
     P2p_buffer_reader.read ~canceler readable buffer >>= fun res ->
     assert (Result.is_error res) ;
     Lwt.return_unit) ;
  assert (!cancelled = true)

let () =
  let mk_buffer_tests =
    [
      test_mk_buffer_1;
      test_mk_buffer_2;
      test_mk_buffer_3;
      test_mk_buffer_4;
      test_mk_buffer_5;
    ]
  in
  Alcotest.run
    ~argv:[|""|]
    "tezos-p2p"
    [
      ( "p2p.reader.mk_buffer",
        Stdlib.List.combine (1 -- List.length mk_buffer_tests) mk_buffer_tests
        |> List.map (fun (i, test) ->
               Alcotest.test_case (Printf.sprintf "mk_buffer_%d" i) `Quick test)
      );
      ( "p2p.reader.mk_safe_buffer",
        [Alcotest.test_case "mk_safe_buffer" `Quick test_mk_buffer_safe] );
      ( "p2p.reader.read",
        [Alcotest.test_case "read" `Quick test_read_waits_for_data] );
    ]
