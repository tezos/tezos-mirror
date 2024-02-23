(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

open Protocol
open Tztest

(** Testing
    -------
    Component:    Raw_level_repr
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/unit/main.exe \
                  -- --file test_raw_level_repr.ml
    Dependencies: --
    Subject:      To test the modules (including the top-level)
                  in raw_level_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)

module Test_raw_level_repr = struct
  (* NOTE: Avoid assertions against too many functions from Raw_level_repr. For instance,
      Raw_level_repr contains a [compare] function, but while [Assert]'ing, convert them to
      int32 (or any convenient OCaml value) and compare instead of using [Raw_level_repr]'s compare *)

  (** Testing [encoding], int32 underneath, by applying it with Data_encoding *)
  let test_encoding () =
    let open Lwt_result_syntax in
    let encoding = Raw_level_repr.encoding in
    let bytes = Bytes.make 4 '0' in
    Bytes.set_int32_ne bytes 0 0l ;
    let* v =
      match Data_encoding.Binary.of_bytes encoding bytes with
      | Ok x -> return x
      | Error e ->
          failwith
            "Data_encoding.Binary.read shouldn't have failed with \
             Raw_level_repr.encoding: %a"
            Data_encoding.Binary.pp_read_error
            e
    in
    let* () =
      Assert.equal_int ~loc:__LOC__ (Int32.to_int (Raw_level_repr.to_int32 v)) 0
    in
    Bytes.set_int32_ne bytes 0 (-1l) ;
    Data_encoding.Binary.of_bytes encoding bytes |> function
    | Error _ -> return_unit
    | Ok x ->
        failwith
          "Data_encoding.Binary.read shouldn't have succeeded with %a"
          Raw_level_repr.pp
          x

  (* TODO rpc_arg. RPC_arg needs to be unit tested separately. Preferably, with a functor *)
  (* let rpc_arg () = () *)

  (** int32 interop tests *)
  let test_int32_interop () =
    let open Lwt_result_wrap_syntax in
    let int32v = 100l in
    let*?@ raw_level = Raw_level_repr.of_int32 int32v in
    let* () =
      Assert.equal_int32 ~loc:__LOC__ (Raw_level_repr.to_int32 raw_level) int32v
    in
    let int32v = -1l in
    let* () =
      match Raw_level_repr.of_int32 int32v with
      | Ok _ -> failwith "Negative int32s should not be coerced into raw_level"
      | Error _ -> return_unit
    in
    try
      let (_ : Raw_level_repr.t) = Raw_level_repr.of_int32_exn int32v in
      failwith "Negative int32s should not be coerced into raw_level"
    with Invalid_argument _ -> return_unit

  (** Asserting [root]'s runtime value. Expected to be [0l] *)
  let test_root () =
    let root = Raw_level_repr.root in
    Assert.equal_int32 ~loc:__LOC__ (root |> Raw_level_repr.to_int32) 0l

  (** Asserting [succ] which is expected to return successor levels *)
  let test_succ () =
    let open Lwt_result_syntax in
    let next_raw_level = Raw_level_repr.succ Raw_level_repr.root in
    let* () =
      Assert.equal_int32
        ~loc:__LOC__
        (next_raw_level |> Raw_level_repr.to_int32)
        1l
    in
    let arbitrary_next_raw_level =
      Raw_level_repr.succ (Raw_level_repr.of_int32_exn 99l)
    in
    Assert.equal_int32
      ~loc:__LOC__
      (arbitrary_next_raw_level |> Raw_level_repr.to_int32)
      100l

  (** Asserting [pred] which is expected to return predecessor levels *)
  let test_pred () =
    let open Lwt_result_syntax in
    let* () =
      match Raw_level_repr.pred (Raw_level_repr.of_int32_exn 1l) with
      | Some previous_raw_level ->
          Assert.equal_int32
            ~loc:__LOC__
            (previous_raw_level |> Raw_level_repr.to_int32)
            0l
      | None ->
          failwith
            "Raw_level_repr.pred should have successfully returned 0l as the \
             predecessor of 1l"
    in
    Raw_level_repr.pred Raw_level_repr.root |> function
    | Some _ ->
        failwith
          "Raw_level_repr.pred should have returned None when asked for \
           predecessor of [root]"
    | None -> return_unit

  let test_skip_succ () =
    let int32_limit = 0x7FFFFFFFl in
    let overflown_next_raw_level =
      Raw_level_repr.succ (Raw_level_repr.of_int32_exn int32_limit)
    in
    if Int32.compare (Raw_level_repr.to_int32 overflown_next_raw_level) 0l >= 0
    then return_unit
    else
      failwith
        "succ of 0x7FFFFFFFl %a was expected to be non-negative"
        Assert.Int32.pp
        (overflown_next_raw_level |> Raw_level_repr.to_int32)
end

let tests =
  [
    tztest
      "Raw_level_repr.encoding: checks if encoding is int32 as expected"
      `Quick
      Test_raw_level_repr.test_encoding;
    tztest
      "Raw_level_repr.root: check if value is 0l"
      `Quick
      Test_raw_level_repr.test_root;
    tztest
      "Raw_level_repr.succ: basic assertions"
      `Quick
      Test_raw_level_repr.test_succ;
    tztest
      "Raw_level_repr.pred: basic assertions"
      `Quick
      Test_raw_level_repr.test_pred;
    tztest
      "Raw_level_repr: int32 interop"
      `Quick
      Test_raw_level_repr.test_int32_interop;
  ]

let skipped_tests =
  [
    tztest
      "Raw_level_repr.succ: overflow"
      `Quick
      Test_raw_level_repr.test_skip_succ;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("Raw level", tests)]
  |> Lwt_main.run
