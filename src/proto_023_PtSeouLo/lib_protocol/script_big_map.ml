(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
(* Copyright (c) 2022 Marigold <team@marigold.dev>                           *)
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

open Script_typed_ir
open Script_ir_translator

let empty key_type value_type =
  Big_map
    {
      id = None;
      diff = {map = Big_map_overlay.empty; size = 0};
      key_type;
      value_type;
    }

let mem ctxt key (Big_map {id; diff; key_type; _}) =
  let open Lwt_result_syntax in
  let* key_hash, ctxt = hash_comparable_data ctxt key_type key in
  match (Big_map_overlay.find key_hash diff.map, id) with
  | None, None -> return (false, ctxt)
  | None, Some id ->
      let+ ctxt, res = Alpha_context.Big_map.mem ctxt id key_hash in
      (res, ctxt)
  | Some (_, None), _ -> return (false, ctxt)
  | Some (_, Some _), _ -> return (true, ctxt)

let get_by_hash ctxt key (Big_map {id; diff; value_type; _}) =
  let open Lwt_result_syntax in
  match (Big_map_overlay.find key diff.map, id) with
  | Some (_, x), _ -> return (x, ctxt)
  | None, None -> return (None, ctxt)
  | None, Some id -> (
      let* ctxt, value_opt = Alpha_context.Big_map.get_opt ctxt id key in
      match value_opt with
      | None -> return (None, ctxt)
      | Some value ->
          let+ x, ctxt =
            parse_data
              ctxt
              ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
              ~allow_forged_tickets:true
              ~allow_forged_lazy_storage_id:true
              value_type
              (Micheline.root value)
          in
          (Some x, ctxt))

let get ctxt key (Big_map {key_type; _} as map) =
  let open Lwt_result_syntax in
  let* key_hash, ctxt = hash_comparable_data ctxt key_type key in
  get_by_hash ctxt key_hash map

let update_by_hash key_hash key value (Big_map map) =
  let contains = Big_map_overlay.mem key_hash map.diff.map in
  Big_map
    {
      map with
      diff =
        {
          map = Big_map_overlay.add key_hash (key, value) map.diff.map;
          size = (if contains then map.diff.size else map.diff.size + 1);
        };
    }

let update ctxt key value (Big_map {key_type; _} as map) =
  let open Lwt_result_syntax in
  let* key_hash, ctxt = hash_comparable_data ctxt key_type key in
  let map = update_by_hash key_hash key value map in
  return (map, ctxt)

let get_and_update ctxt key value (Big_map {key_type; _} as map) =
  let open Lwt_result_syntax in
  let* key_hash, ctxt = hash_comparable_data ctxt key_type key in
  let new_map = update_by_hash key_hash key value map in
  let* old_value, ctxt = get_by_hash ctxt key_hash map in
  return ((old_value, new_map), ctxt)
