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

type 'error t = {
  applied : (Tezos_crypto.Operation_hash.t * Operation.t) list;
  refused : (Operation.t * 'error list) Tezos_crypto.Operation_hash.Map.t;
  outdated : (Operation.t * 'error list) Tezos_crypto.Operation_hash.Map.t;
  branch_refused :
    (Operation.t * 'error list) Tezos_crypto.Operation_hash.Map.t;
  branch_delayed : (Operation.t * 'error list) Tezos_crypto.Operation_hash.Map.t;
}

let empty =
  {
    applied = [];
    refused = Tezos_crypto.Operation_hash.Map.empty;
    outdated = Tezos_crypto.Operation_hash.Map.empty;
    branch_refused = Tezos_crypto.Operation_hash.Map.empty;
    branch_delayed = Tezos_crypto.Operation_hash.Map.empty;
  }

let encoding error_encoding =
  let open Data_encoding in
  let operation_encoding =
    merge_objs
      (obj1 (req "hash" Tezos_crypto.Operation_hash.encoding))
      (dynamic_size Operation.encoding)
  in
  let refused_encoding =
    merge_objs
      (obj1 (req "hash" Tezos_crypto.Operation_hash.encoding))
      (merge_objs
         (dynamic_size Operation.encoding)
         (obj1 (req "error" error_encoding)))
  in
  let build_list map = Tezos_crypto.Operation_hash.Map.bindings map in
  let build_map list =
    List.fold_right
      (fun (k, e) m -> Tezos_crypto.Operation_hash.Map.add k e m)
      list
      Tezos_crypto.Operation_hash.Map.empty
  in
  conv
    (fun {applied; refused; outdated; branch_refused; branch_delayed} ->
      ( applied,
        build_list refused,
        build_list outdated,
        build_list branch_refused,
        build_list branch_delayed ))
    (fun (applied, refused, outdated, branch_refused, branch_delayed) ->
      let refused = build_map refused in
      let outdated = build_map outdated in
      let branch_refused = build_map branch_refused in
      let branch_delayed = build_map branch_delayed in
      {applied; refused; outdated; branch_refused; branch_delayed})
    (obj5
       (req "applied" (list operation_encoding))
       (req "refused" (list refused_encoding))
       (req "outdated" (list refused_encoding))
       (req "branch_refused" (list refused_encoding))
       (req "branch_delayed" (list refused_encoding)))
