(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    Invocation:   dune exec src/lib_p2p/test/test_p2p_buffer_reader.exe
    Subject:      Tests [P2p_buffer_reader]
*)

open Qcheck2_helpers
open Lwt.Syntax

let test_mk_buffer_negative_len =
  Alcotest.test_case "Negative length is rejected" `Quick @@ fun () ->
  Assert.assert_true
    "[mk_buffer ?length_to_copy:-1] fails"
    (Result.is_error
    @@ P2p_buffer_reader.mk_buffer ~length_to_copy:(-1)
    @@ Bytes.create 8)

let test_mk_buffer_negative_pos =
  Alcotest.test_case "Negative position is rejected" `Quick @@ fun () ->
  Assert.assert_true
    "[mk_buffer ?pos:-1] fails"
    (Result.is_error @@ P2p_buffer_reader.mk_buffer ~pos:(-1) @@ Bytes.create 8)

let test_mk_buffer_out_of_bound =
  Alcotest.test_case "Invalid length+position is rejected" `Quick @@ fun () ->
  Assert.assert_true
    "[mk_buffer ?length_to_copy ?pos buf] with (length_to_copy + pos > \
     (Bytes.length buf)) fails"
    (Result.is_error
    @@ P2p_buffer_reader.mk_buffer ~pos:14 ~length_to_copy:3
    @@ Bytes.create 16)

let test_mk_buffer_default =
  Alcotest.test_case "Default values are accepted" `Quick @@ fun () ->
  Assert.assert_true
    "[mk_buffer Bytes.empty] succeeds"
    (Result.is_ok @@ P2p_buffer_reader.mk_buffer Bytes.empty)

let test_mk_buffer_regular =
  Alcotest.test_case "Extreme valid values are accepted" `Quick @@ fun () ->
  let length_to_copy = 16 in
  Assert.assert_true
    "[mk_buffer ?length_to_copy=max ?pos:0] succeeds"
    (Result.is_ok
    @@ P2p_buffer_reader.mk_buffer ~length_to_copy ~pos:0
    @@ Bytes.create length_to_copy)

let test_mk_buffer_safe =
  Alcotest.test_case "mk_buffer_safe makes valid buffers" `Quick @@ fun () ->
  (* We don't need to use QCheck2, because the input data we need
     is [0..some_not_too_big_int[, so we can just test all the ints
     until the chosen bound, which we set here to 128. The bound should
     be small, because a buffer of the given size is allocated. *)
  let lengths = Stdlib.List.init 128 Fun.id in
  List.iter
    (fun buf_len ->
      let safe_buffer =
        Bytes.create buf_len |> P2p_buffer_reader.mk_buffer_safe
      in
      let pos, length_to_copy, buf =
        P2p_buffer_reader.Internal_for_tests.destruct_buffer safe_buffer
      in
      Alcotest.(check int "pos is always 0") 0 pos ;
      Alcotest.(check int "length_to_copy is always the buffer length")
        buf_len
        length_to_copy ;
      Assert.assert_true
        "A destructed mk_buffer_safe buffer correctly reconstructs"
        (P2p_buffer_reader.mk_buffer ~pos ~length_to_copy buf |> Result.is_ok))
    lengths

let cancel_and_assert_success canceler =
  let open Lwt_syntax in
  let* r = Lwt_canceler.cancel canceler in
  match r with Ok _ -> return_unit | Error _ -> assert false

let easy_mk_readable ?maxlength ?fresh_buf_size ?size () =
  let read_buffer = Circular_buffer.create ?maxlength ?fresh_buf_size () in
  let read_queue = Lwt_pipe.Maybe_bounded.create ?bound:size () in
  P2p_buffer_reader.mk_readable ~read_buffer ~read_queue

let test_read_waits_for_data =
  let open Lwt_syntax in
  Alcotest.test_case "read hangs if there is no data to read" `Quick
  @@ fun () ->
  let readable = easy_mk_readable () in
  let buffer = P2p_buffer_reader.mk_buffer_safe @@ Bytes.create 16 in
  let cancelled = ref false in
  let canceler = Lwt_canceler.create () in
  Lwt_canceler.on_cancel canceler (fun () ->
      cancelled := true ;
      return_unit) ;
  let schedule () : _ Lwt.t =
    let* () = Lwt_unix.sleep 0.3 in
    cancel_and_assert_success canceler
  in
  Lwt_main.run
    (Lwt.dont_wait schedule (fun exn ->
         Alcotest.failf "Schedule execution failed: %s" (Printexc.to_string exn)) ;
     let* res = P2p_buffer_reader.read ~canceler readable buffer in
     assert (Result.is_error res) ;
     return_unit) ;
  assert (!cancelled = true)

let test_read_less_than_length : QCheck2.Test.t =
  QCheck2.(
    Test.make
      ~name:
        "read result (number of read bytes) is lower than or equal to \
         buffer.len"
      Gen.(pair (0 -- 2048) string))
  @@ fun (buf_size, data_to_write) ->
  Lwt_main.run
  @@
  let read_buffer = Circular_buffer.create () in
  let read_queue = Lwt_pipe.Maybe_bounded.create () in
  let readable = P2p_buffer_reader.mk_readable ~read_buffer ~read_queue in
  let buffer = Bytes.create buf_size in
  let reader_buffer = P2p_buffer_reader.mk_buffer_safe buffer in
  (* Wrap in a timeout in case reading hangs - which can happen if the
     test is not well written or if a bug is introduced. Better safe than sorry. *)
  Lwt_unix.with_timeout 1. @@ fun () ->
  let* data_result =
    Circular_buffer.write
      ~maxlen:(String.length data_to_write)
      ~fill_using:(fun bytes offset maxlen ->
        Bytes.blit_string data_to_write 0 bytes offset maxlen ;
        Lwt.return_ok maxlen)
      read_buffer
  in
  let* () =
    match data_result with
    | Ok data -> Lwt_pipe.Maybe_bounded.push read_queue (Ok data)
    | Error _ -> Alcotest.failf "Faild to read from circular buffer"
  in
  let+ res = P2p_buffer_reader.read readable reader_buffer in
  qcheck_eq'
    ~expected:true
    ~actual:
      (Result.fold
         ~ok:(fun nb_read_bytes -> nb_read_bytes <= buf_size)
         ~error:(fun _ -> false)
         res)
    ()

(** - Write a sequence of bytes of length [l1 + l2 + l3] into a [readable]
    - Read a first part in a buffer of length [l1]
    - Read a second part in another buffer of length [l2]
    - Check that both buffers are correct (i.e. partial reading worked, no data was lost)
*)
let test_read_partial : QCheck2.Test.t =
  let open QCheck2 in
  let buffer_size_gen = Gen.(0 -- 2048)
  and print = QCheck2.Print.(quad int int int (Format.sprintf "%S")) in
  (* The data to write (bytes) must be at least the sum of the 3 sizes, as this will be the [maxlen]*)
  let buffer_sizes_and_data_gen =
    let open QCheck2.Gen in
    let* buf1_size = buffer_size_gen
    and* buf2_size = buffer_size_gen
    and* additional_size = buffer_size_gen in
    let+ data_to_write =
      string_size (pure (buf1_size + buf2_size + additional_size))
    in
    (buf1_size, buf2_size, additional_size, data_to_write)
  in
  let qcheck_eq_result_tztrace ~expected ~actual =
    qcheck_eq'
      ~pp:
        Format.(
          pp_print_result ~ok:pp_print_int ~error:Error_monad.pp_print_trace)
      ~expected
      ~actual
      ()
  in
  let qcheck_eq_bytes ~expected ~actual =
    qcheck_eq'
      ~eq:Bytes.equal
      ~pp:(fun ppf bytes -> Format.fprintf ppf "%S" (Bytes.to_string bytes))
      ~expected:(Bytes.of_string expected)
      ~actual
      ()
  in
  Test.make
    ~name:
      "When reading less data than available, [read] reads partially and reads \
       the remaining data on second [read]"
    ~print
    buffer_sizes_and_data_gen
  @@ fun (buf1_size, buf2_size, additional_size, data_to_write) ->
  (* If both [buf2_size] and [additional_size] are [0], then the second [read] call
     hangs forever as it waits for data to arrive in the circular buffer, even if it's
     useless. *)
  assume (buf2_size + additional_size > 0) ;
  Lwt_main.run
  @@
  let read_buffer = Circular_buffer.create () in
  let read_queue = Lwt_pipe.Maybe_bounded.create () in
  let readable = P2p_buffer_reader.mk_readable ~read_buffer ~read_queue in
  let buffer1 = Bytes.create buf1_size in
  let buffer2 = Bytes.create buf2_size in
  let reader_buffer1 = P2p_buffer_reader.mk_buffer_safe buffer1 in
  let reader_buffer2 = P2p_buffer_reader.mk_buffer_safe buffer2 in
  (* Wrap in a timeout in case reading hangs - which can happen if the
     test is not well written or if a bug is introduced. Better safe than sorry. *)
  Lwt_unix.with_timeout 1. @@ fun () ->
  (* Write data into the [readable] *)
  let* data_result =
    Circular_buffer.write
      ~maxlen:(buf1_size + buf2_size + additional_size)
      ~fill_using:(fun bytes offset maxlen ->
        Bytes.blit_string data_to_write 0 bytes offset maxlen ;
        Lwt.return_ok maxlen)
      read_buffer
  in
  let* () =
    match data_result with
    | Ok data -> Lwt_pipe.Maybe_bounded.push read_queue (Ok data)
    | Error _ -> Alcotest.failf "Faild to read from circular buffer"
  in
  (* [read] a first segment, which leaves partial data in the [readable] *)
  let* res1 = P2p_buffer_reader.read readable reader_buffer1 in
  (* [read] a second segment, which should start on the remaining partial data of the [readable] *)
  let+ res2 = P2p_buffer_reader.read readable reader_buffer2 in
  (* Check the buffers were correctly written *)
  let _ = qcheck_eq_result_tztrace ~expected:(Ok buf1_size) ~actual:res1 in
  let _ = qcheck_eq_result_tztrace ~expected:(Ok buf2_size) ~actual:res2 in
  let _ =
    qcheck_eq_bytes
      ~expected:(String.sub data_to_write 0 buf1_size)
      ~actual:buffer1
  in
  qcheck_eq_bytes
    ~expected:(String.sub data_to_write buf1_size buf2_size)
    ~actual:buffer2

let test_read_full_basic =
  QCheck2.(Test.make ~name:"read_full fills the buffer" Gen.string)
  @@ fun data_to_write ->
  Lwt_main.run
  @@
  let read_buffer = Circular_buffer.create () in
  let read_queue = Lwt_pipe.Maybe_bounded.create () in
  let readable = P2p_buffer_reader.mk_readable ~read_buffer ~read_queue in
  let buffer = Bytes.create (String.length data_to_write) in
  let reader_buffer = P2p_buffer_reader.mk_buffer_safe buffer in
  (* Wrap in a timeout in case reading hangs - which can happen if the
     test is not well written or if a bug is introduced. Better safe than sorry. *)
  Lwt_unix.with_timeout 1. @@ fun () ->
  let* data_result =
    Circular_buffer.write
      ~maxlen:(String.length data_to_write)
      ~fill_using:(fun bytes offset maxlen ->
        Bytes.blit_string data_to_write 0 bytes offset maxlen ;
        Lwt.return_ok maxlen)
      read_buffer
  in
  let* () =
    match data_result with
    | Ok data -> Lwt_pipe.Maybe_bounded.push read_queue (Ok data)
    | Error _ -> Alcotest.failf "Faild to read from circular buffer"
  in
  let+ res = P2p_buffer_reader.read_full readable reader_buffer in
  let _ = qcheck_eq' ~expected:true ~actual:(Result.is_ok res) () in
  qcheck_eq' ~expected:data_to_write ~actual:(Bytes.to_string buffer) ()

let test_read_full_waits =
  QCheck2.(
    Test.make
      ~name:
        "read_full fills the buffer and waits for enough data to be available \
         if needed"
      Gen.(small_list string))
  @@ fun data_to_write_list ->
  let total_size =
    List.fold_left (fun size s -> size + String.length s) 0 data_to_write_list
  in
  Lwt_main.run
  @@
  let read_buffer = Circular_buffer.create () in
  let read_queue = Lwt_pipe.Maybe_bounded.create () in
  let readable = P2p_buffer_reader.mk_readable ~read_buffer ~read_queue in
  let buffer = Bytes.create total_size in
  let reader_buffer = P2p_buffer_reader.mk_buffer_safe buffer in
  (* Wrap in a timeout in case reading hangs - which can happen if the
     test is not well written or if a bug is introduced. Better safe than sorry. *)
  Lwt_unix.with_timeout 10. @@ fun () ->
  let+ res = P2p_buffer_reader.read_full readable reader_buffer
  and+ () =
    (* Sleep a bit, to ensure [read_full] starts before there are available data in the [readable] *)
    let* () = Lwt_unix.sleep 0.01 in
    List.iter_s
      (fun data_to_write ->
        let* data_result =
          Circular_buffer.write
            ~maxlen:(String.length data_to_write)
            ~fill_using:(fun bytes offset maxlen ->
              Bytes.blit_string data_to_write 0 bytes offset maxlen ;
              Lwt.return_ok maxlen)
            read_buffer
        in
        let* () =
          match data_result with
          | Ok data -> Lwt_pipe.Maybe_bounded.push read_queue (Ok data)
          | Error _ -> Alcotest.failf "Faild to read from circular buffer"
        in
        (* Note: this cooperation point is necessary to allow [read_full] to read some data from the circular buffer before writing again; otherwise it can deadlock if [total_size > Circular_buffer.default_size] (one can also set the Circular_buffer size to [total_size] but putting a cooperation point is more representative of real-world scenarios). *)
        Lwt.pause ())
      data_to_write_list
  in
  let _ = qcheck_eq' ~expected:true ~actual:(Result.is_ok res) () in
  qcheck_eq'
    ~expected:(String.concat "" data_to_write_list)
    ~actual:(Bytes.to_string buffer)
    ()

let () =
  Alcotest.run
    ~__FILE__
    "P2p_buffer_reader"
    [
      ( "mk_buffer",
        [
          test_mk_buffer_negative_len;
          test_mk_buffer_negative_pos;
          test_mk_buffer_out_of_bound;
          test_mk_buffer_default;
          test_mk_buffer_regular;
        ] );
      ("mk_buffer_safe", [test_mk_buffer_safe]);
      ( "read",
        test_read_waits_for_data
        :: qcheck_wrap [test_read_less_than_length; test_read_partial] );
      ("read_full", qcheck_wrap [test_read_full_basic; test_read_full_waits]);
    ]

let () = Tezt.Test.run ()
