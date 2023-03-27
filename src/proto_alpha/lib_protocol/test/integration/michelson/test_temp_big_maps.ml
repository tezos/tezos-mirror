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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/main.exe
    Subject:    On temporary big maps.
*)

open Protocol

let to_raw_context (b : Block.t) =
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
  >|= Environment.wrap_tzresult

let check_no_dangling_temp_big_map b =
  to_raw_context b >>=? fun ctxt ->
  Storage.Big_map.fold ctxt ~init:() ~order:`Sorted ~f:(fun id () ->
      assert (not (Lazy_storage_kind.Big_map.Id.is_temp id)) ;
      Lwt.return_unit)
  >>= fun () ->
  Storage.Big_map.fold ctxt ~init:() ~order:`Undefined ~f:(fun id () ->
      assert (not (Lazy_storage_kind.Big_map.Id.is_temp id)) ;
      Lwt.return_unit)
  >>= fun () -> return_unit

let call_the_contract b ~baker ~src contract param_left param_right =
  let fee = Alpha_context.Tez.one in
  let amount = Alpha_context.Tez.zero in
  let param = Printf.sprintf "Pair (%s) %s" param_left param_right in
  let parameters = Alpha_context.Script.lazy_expr (Expr.from_string param) in
  Op.transaction ~fee (B b) src contract amount ~parameters
  >>=? fun operation ->
  Incremental.begin_construction ~policy:Block.(By_account baker) b
  >>=? fun incr ->
  Incremental.add_operation incr operation >>=? fun incr ->
  Incremental.finalize_block incr

let path = project_root // Filename.dirname __FILE__

(** Originates the contract at contracts/temp_big_maps.tz and calls it with
    the pair [(param_left, param_right)].
    An action (originating, storing, passing, passing twice) is done on a big
    map (either fresh, passed, or stored).
    All combinations are exercised.
*)
let test_temp_big_maps_contract param_left param_right () =
  Contract_helpers.init () >>=? fun (b, baker, src, _src2) ->
  Contract_helpers.originate_contract
    (path // "contracts/temp_big_maps.tz")
    "{}"
    src
    b
    baker
  >>=? fun (contract, b) ->
  check_no_dangling_temp_big_map b >>=? fun () ->
  call_the_contract b ~baker ~src contract param_left param_right >>=? fun b ->
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
