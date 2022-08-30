(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context

type error += Zk_rollup_feature_disabled | Zk_rollup_negative_nb_ops

let () =
  let description = "ZK rollups will be enabled in a future proposal." in
  register_error_kind
    `Permanent
    ~id:"operation.zk_rollup_disabled"
    ~title:"ZK rollups are disabled"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Zk_rollup_feature_disabled -> Some () | _ -> None)
    (fun () -> Zk_rollup_feature_disabled) ;
  let description = "The value of [nb_ops] should never be negative." in
  register_error_kind
    `Permanent
    ~id:"operation.zk_rollup_negative_nb_ops"
    ~title:"ZK rollups negative number of operations"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Zk_rollup_negative_nb_ops -> Some () | _ -> None)
    (fun () -> Zk_rollup_negative_nb_ops)

let assert_feature_enabled ctxt =
  error_unless (Constants.zk_rollup_enable ctxt) Zk_rollup_feature_disabled

let originate ~ctxt_before_op ~ctxt ~public_parameters ~circuits_info
    ~init_state ~nb_ops =
  let open Lwt_result_syntax in
  let*? () = assert_feature_enabled ctxt in
  let*? () = error_when Compare.Int.(nb_ops < 0) Zk_rollup_negative_nb_ops in
  let+ ctxt, originated_zk_rollup, size =
    Zk_rollup.originate
      ctxt
      {
        public_parameters;
        state_length = Array.length init_state;
        circuits_info;
        nb_ops;
      }
      ~init_state
  in
  let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
  let result =
    Apply_results.Zk_rollup_origination_result
      {
        balance_updates = [];
        originated_zk_rollup;
        (* TODO https://gitlab.com/tezos/tezos/-/issues/3544
           Carbonate ZKRU operations *)
        consumed_gas;
        size;
      }
  in
  (ctxt, result, [])
