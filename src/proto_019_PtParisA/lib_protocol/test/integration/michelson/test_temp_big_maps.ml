(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (temporary big maps)
    Invocation: dune exec src/proto_019_PtParisA/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_temp_big_maps.ml
    Subject:    On temporary big maps.
*)

open Protocol

let to_raw_context (b : Block.t) =
  let open Lwt_result_wrap_syntax in
  let+@ ctxt =
    Raw_context.prepare
      b.context
      ~level:b.header.shell.level
      ~predecessor_timestamp:b.header.shell.timestamp
      ~timestamp:b.header.shell.timestamp
      ~adaptive_issuance_enable:false
  in
  ctxt

let check_no_dangling_temp_big_map b =
  let open Lwt_result_syntax in
  let* ctxt = to_raw_context b in
  let*! () =
    Storage.Big_map.fold ctxt ~init:() ~order:`Sorted ~f:(fun id () ->
        assert (not (Lazy_storage_kind.Big_map.Id.is_temp id)) ;
        Lwt.return_unit)
  in
  let*! () =
    Storage.Big_map.fold ctxt ~init:() ~order:`Undefined ~f:(fun id () ->
        assert (not (Lazy_storage_kind.Big_map.Id.is_temp id)) ;
        Lwt.return_unit)
  in
  return_unit

let call_the_contract b ~baker ~src contract param_left param_right =
  let open Lwt_result_syntax in
  let fee = Alpha_context.Tez.one in
  let amount = Alpha_context.Tez.zero in
  let param = Printf.sprintf "Pair (%s) %s" param_left param_right in
  let parameters = Alpha_context.Script.lazy_expr (Expr.from_string param) in
  let* operation = Op.transaction ~fee (B b) src contract amount ~parameters in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) b
  in
  let* incr = Incremental.add_operation incr operation in
  Incremental.finalize_block incr

let path = project_root // Filename.dirname __FILE__

(** Originates the contract at contracts/temp_big_maps.tz and calls it with
    the pair [(param_left, param_right)].
    An action (originating, storing, passing, passing twice) is done on a big
    map (either fresh, passed, or stored).
    All combinations are exercised.
*)
let test_temp_big_maps_contract param_left param_right () =
  let open Lwt_result_syntax in
  let* b, baker, src, _src2 = Contract_helpers.init () in
  let* contract, b =
    Contract_helpers.originate_contract
      (path // "contracts/temp_big_maps.tz")
      "{}"
      src
      b
      baker
  in
  let* () = check_no_dangling_temp_big_map b in
  let* b = call_the_contract b ~baker ~src contract param_left param_right in
  check_no_dangling_temp_big_map b

let param_left_values = ["Left True"; "Left False"; "Right {}"]

let param_right_values = ["-1"; "0"; "1"; "2"]

let tests =
  List.flatten
    (List.map
       (fun param_left ->
         List.map
           (fun param_right ->
             Tztest.tztest
               (Printf.sprintf "temp_big_maps(%s, %s)" param_left param_right)
               `Quick
               (test_temp_big_maps_contract param_left param_right))
           param_right_values)
       param_left_values)

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("temp big maps", tests)]
  |> Lwt_main.run
