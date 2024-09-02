(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
open Alpha_context

let make_big_map block ~source ~key_type ~value_type key_values =
  let open Lwt_result_wrap_syntax in
  let key_type = Expr.from_string key_type in
  let value_type = Expr.from_string value_type in
  let* operation, originated =
    Op.contract_origination_hash (B block) source ~script:Op.dummy_script
  in
  let* block = Block.bake ~operation block in
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let*@ ctxt, big_map_id = Big_map.fresh ~temporary:false ctxt in
  let* updates, ctxt =
    List.fold_left_es
      (fun (kvs, ctxt) (key, value) ->
        let key_hash =
          match
            Data_encoding.Binary.to_bytes_opt Script_repr.expr_encoding key
          with
          | Some bytes -> Script_expr_hash.hash_bytes [bytes]
          | None -> assert false
        in
        return ({Big_map.key; key_hash; value = Some value} :: kvs, ctxt))
      ([], ctxt)
      key_values
  in
  let*@ ctxt =
    Contract.update_script_storage
      ctxt
      originated
      key_type
      (Some
         [
           Lazy_storage.make
             Lazy_storage.Kind.Big_map
             big_map_id
             (Update
                {
                  init = Lazy_storage.Alloc Big_map.{key_type; value_type};
                  updates;
                });
         ])
  in
  return (big_map_id, ctxt)
