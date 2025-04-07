(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type _ t =
  | Manager_info : {
      source : Alpha_context.public_key_hash option;
      fee : Tez.t Limit.t;
      gas_limit : Gas.Arith.integral Limit.t;
      storage_limit : Z.t Limit.t;
      counter : Manager_counter.t option;
      operation : 'kind manager_operation;
    }
      -> 'kind t

type packed = Annotated_manager_operation : 'kind t -> packed

type _ annotated_list =
  | Single_manager : 'kind t -> 'kind annotated_list
  | Cons_manager :
      'kind t * 'rest annotated_list
      -> ('kind * 'rest) annotated_list

type packed_annotated_list =
  | Manager_list : 'kind annotated_list -> packed_annotated_list

let rec manager_to_list = function
  | Manager_list (Single_manager o) -> [Annotated_manager_operation o]
  | Manager_list (Cons_manager (o, os)) ->
      Annotated_manager_operation o :: manager_to_list (Manager_list os)

let rec manager_of_list = function
  | [] -> assert false
  | [Annotated_manager_operation o] -> Manager_list (Single_manager o)
  | Annotated_manager_operation o :: os ->
      let (Manager_list os) = manager_of_list os in
      Manager_list (Cons_manager (o, os))

let join_fee fee operation =
  let open Result_syntax in
  let (Manager_info c) = operation in
  let* fee = Limit.join ~where:__LOC__ Tez.equal fee c.fee in
  return (Manager_info {c with fee})

let set_fee fee (Manager_info c) = Manager_info {c with fee}

let join_gas_limit gas_limit operation =
  let open Result_syntax in
  let (Manager_info c) = operation in
  let* gas_limit =
    Limit.join ~where:__LOC__ Gas.Arith.equal gas_limit c.gas_limit
  in
  return (Manager_info {c with gas_limit})

let set_gas_limit gas_limit (Manager_info c) = Manager_info {c with gas_limit}

let join_storage_limit storage_limit (Manager_info c) =
  let open Result_syntax in
  let* storage_limit =
    Limit.join ~where:__LOC__ Z.equal storage_limit c.storage_limit
  in
  return (Manager_info {c with storage_limit})

let set_storage_limit storage_limit (Manager_info c) =
  Manager_info {c with storage_limit}

let set_counter counter (Manager_info c) =
  match c.counter with
  | Some _ -> error_with "set_counter_annot: already set"
  | None -> Ok (Manager_info {c with counter = Some counter})

let set_source source (Manager_info c) =
  match c.source with
  | Some _ -> error_with "set_source_annot: already set"
  | None -> Ok (Manager_info {c with source = Some source})

let manager_from_annotated operation =
  let open Result_syntax in
  let (Manager_info {source; fee; gas_limit; storage_limit; counter; operation})
      =
    operation
  in
  let* fee = Limit.get ~when_unknown:"unknown fee" fee in
  let* gas_limit = Limit.get ~when_unknown:"unknown gas limit" gas_limit in
  let* storage_limit =
    Limit.get ~when_unknown:"unknown storage limit" storage_limit
  in
  let* source =
    Option.fold
      ~some:return
      ~none:(error_with "manager_from_annotated: source not set")
      source
  in
  let* counter =
    Option.fold
      ~some:return
      ~none:(error_with "manager_from_annotated: counter not set")
      counter
  in
  return
  @@ Manager_operation
       {source; fee; counter; gas_limit; storage_limit; operation}

let rec manager_list_from_annotated :
    type kind. kind annotated_list -> kind Kind.manager contents_list tzresult =
  let open Result_syntax in
  function
  | Single_manager operation ->
      let* op = manager_from_annotated operation in
      return (Single op)
  | Cons_manager (operation, rest) ->
      let* rest = manager_list_from_annotated rest in
      let* op = manager_from_annotated operation in
      return (Cons (op, rest))
