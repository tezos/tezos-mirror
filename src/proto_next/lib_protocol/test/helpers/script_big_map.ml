(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
(*                                                                           *)
(*****************************************************************************)

let update k v m ctxt = Protocol.Script_big_map.update ctxt k v m

let of_list key_ty ty xs ctxt =
  List.fold_left_es
    (fun (bm, ctxt) (k, v) -> update k (Some v) bm ctxt)
    (Protocol.Script_big_map.empty key_ty ty, ctxt)
    xs
