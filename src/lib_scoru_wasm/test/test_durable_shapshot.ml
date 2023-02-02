(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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
    Component:    Lib_scoru_wasm durable
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^Durable snapshot$"
    Subject:      Tests for the tezos-scoru-wasm durable snapshotting
*)

open Tztest
open Encodings_util
open Durable_snapshot_util

let run_scenario (scenario : Paired_durable.t -> 'a Lwt.t) : 'a Lwt.t =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let* durable = Tree_encoding_runner.decode Paired_durable.encoding tree in
  let* result = scenario durable in
  let* final_state_eq = compare_durable_storages (fst durable) (snd durable) in
  match final_state_eq with
  | Error (snapshot_str, current_str) ->
      Assert.fail
        ~loc:__LOC__
        ~msg:"Final durable states diverged"
        Fmt.string
        snapshot_str
        current_str
  | _ -> Lwt.return result

(* Actual tests *)

let assert_exception exected_ex run =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let+ _ = run () in
      assert false)
    (fun caught_exn ->
      match caught_exn with
      | e when e = exected_ex -> Lwt.return_unit
      | x -> raise x)

let test_several_operations () =
  run_scenario @@ fun durable ->
  let open Lwt_syntax in
  let key1 = Paired_durable.key_of_string_exn "/durable/value/to/write1" in
  let key2 = Paired_durable.key_of_string_exn "/durable/value/to/write2" in
  let* durable = Paired_durable.write_value_exn durable key1 0L "hello" in
  let* durable = Paired_durable.write_value_exn durable key2 0L "world" in
  let* res_hello = Paired_durable.read_value_exn durable key1 0L 5L in
  Assert.String.equal ~loc:__LOC__ "hello" res_hello ;
  let* res_still_hello = Paired_durable.read_value_exn durable key1 0L 10L in
  Assert.String.equal ~loc:__LOC__ "hello" res_still_hello ;
  let key_prefix = Paired_durable.key_of_string_exn "/durable/value" in
  let* durable = Paired_durable.delete durable key_prefix in
  let* () =
    assert_exception Tezos_scoru_wasm_durable_snapshot.Durable.Value_not_found
    @@ fun () -> Paired_durable.read_value_exn durable key1 0L 5L
  in
  return_ok_unit

let tests =
  [tztest "Do several operations on durable" `Quick test_several_operations]
