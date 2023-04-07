(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:  Protocol (quantities)
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/unit/main.exe \
                  -- --file test_qty.ml
    Subject:    On tez quantities.
*)

open Protocol

let known_ok_tez_literals =
  [
    (0L, "0");
    (10L, "0.00001");
    (100L, "0.0001");
    (1_000L, "0.001");
    (10_000L, "0.01");
    (100_000L, "0.1");
    (1_000_000L, "1");
    (10_000_000L, "10");
    (100_000_000L, "100");
    (1_000_000_000L, "1000");
    (10_000_000_000L, "10000");
    (100_000_000_000L, "100000");
    (1_000_000_000_000L, "1000000");
    (1_000_000_000_001L, "1000000.000001");
    (1_000_000_000_010L, "1000000.00001");
    (1_000_000_000_100L, "1000000.0001");
    (1_000_000_001_000L, "1000000.001");
    (1_000_000_010_000L, "1000000.01");
    (1_000_000_100_000L, "1000000.1");
    (123_123_123_123_123_123L, "123123123123.123123");
    (999_999_999_999_999_999L, "999999999999.999999");
  ]

let known_bad_tez_literals =
  [
    "10000.";
    "100,.";
    "100,";
    "1,0000";
    "0.0000,1";
    "0.00,1";
    "0,1";
    "HAHA";
    "0.000,000,1";
    "0.0000000";
    "9,999,999,999,999.999,999";
  ]

let fail expected given msg =
  Format.kasprintf
    Stdlib.failwith
    "@[%s@ expected: %s@ got: %s@]"
    msg
    expected
    given

let fail_msg fmt = Format.kasprintf (fail "" "") fmt

let default_printer _ = ""

(** Literals which are supposed to be parsed correctly. *)
let test_known_tez_literals () =
  List.iter
    (fun (v, s) ->
      let vv = Tez_repr.of_mutez v in
      let vs = Tez_repr.of_string s in
      let vs' =
        Tez_repr.of_string (String.concat "" (String.split_on_char ',' s))
      in
      let vv =
        match vv with None -> fail_msg "could not unopt %Ld" v | Some vv -> vv
      in
      let vs =
        match vs with None -> fail_msg "could not unopt %s" s | Some vs -> vs
      in
      let vs' =
        match vs' with
        | None -> fail_msg "could not unopt %s" s
        | Some vs' -> vs'
      in
      assert (vv = vs) ;
      assert (vv = vs') ;
      assert (Tez_repr.to_string vv = s))
    known_ok_tez_literals ;
  List.iter
    (fun s ->
      let vs = Tez_repr.of_string s in
      assert (vs = None))
    known_bad_tez_literals ;
  return_unit

(** Randomly generated tez value which is printed into a string then
    parsed again for their equality. *)
let test_random_tez_literals () =
  for _ = 0 to 100_000 do
    let v = Random.int64 12L in
    let vv = Tez_repr.of_mutez v in
    let vv =
      match vv with None -> fail_msg "could not unopt %Ld" v | Some vv -> vv
    in
    let s = Tez_repr.to_string vv in
    let vs = Tez_repr.of_string s in
    let s' = String.concat "" (String.split_on_char ',' s) in
    let vs' = Tez_repr.of_string s' in
    assert (vs <> None) ;
    assert (vs' <> None) ;
    (match vs with
    | None -> assert false
    | Some vs ->
        let rev = Tez_repr.to_mutez vs in
        assert (v = rev)) ;
    match vs' with
    | None -> assert false
    | Some vs' ->
        let rev = Tez_repr.to_mutez vs' in
        assert (v = rev)
  done ;
  return_unit

let tests =
  [
    ("tez-literals", fun _ -> test_known_tez_literals ());
    ("rnd-tez-literals", fun _ -> test_random_tez_literals ());
  ]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      f () >|= function
      | Ok () -> ()
      | Error error ->
          Format.kasprintf Stdlib.failwith "%a" pp_print_trace error)

let tests = List.map wrap tests

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("qty", tests)] |> Lwt_main.run
