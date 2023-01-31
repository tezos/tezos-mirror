(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type config = {max_operations : int; max_total_bytes : int}

let default_max_operations = 10_000

let default_max_total_bytes = 10_000_000

let default_config =
  {
    max_operations = default_max_operations;
    max_total_bytes = default_max_total_bytes;
  }

let config_encoding : config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {max_operations; max_total_bytes} -> (max_operations, max_total_bytes))
    (fun (max_operations, max_total_bytes) -> {max_operations; max_total_bytes})
    (obj2
       (dft "max_operations" uint16 default_config.max_operations)
       (dft "max_total_bytes" (uint_like_n ()) default_config.max_total_bytes))

open Shell_operation

module type T = sig
  type state

  val empty : state

  type protocol_operation

  val add_operation :
    state ->
    config ->
    protocol_operation operation ->
    (state * Operation_hash.t list) option

  val remove_operation : state -> Operation_hash.t -> state
end

module Make (Proto : Tezos_protocol_environment.PROTOCOL) :
  T with type protocol_operation = Proto.operation = struct
  type protocol_operation = Proto.operation

  type operation = protocol_operation Shell_operation.operation

  let compare_ops op1 op2 =
    Proto.compare_operations (op1.hash, op1.protocol) (op2.hash, op2.protocol)

  module Opset = Set.Make (struct
    type t = operation

    let compare = compare_ops
  end)

  (** Internal overview of all the valid operations present in the mempool.
      
      Structural invariants:
      - [opset] and [ophmap] contain the same operations.
      - [minop] is the minimum of [opset] (or [None] when [opset] is empty).
      - [cardinal] is the cardinal of [opset].
      - [total_bytes] is the sum of the byte sizes of all elements in [opset].

      Bound invariants:
      - [cardinal <= config.max_operations]
      - [total_bytes <= config.max_total_bytes] *)
  type state = {
    opset : Opset.t;
        (** Ordered set of valid operations in the mempool. Note that the
            operations are ordered by the protocol's [compare_operations]
            function, NOT by the size of their bytes. *)
    ophmap : operation Operation_hash.Map.t;
        (** Contain the same elements as [opset], indexed by their hash. *)
    minop : operation option;
        (** The smallest operation in [opset] according to the protocol's
            [compare_operations] function (not necessarily the one with the
            least bytes). This is [None] if and only if [opset] is empty. *)
    cardinal : int;  (** The number of operations in [opset]. *)
    total_bytes : int;
        (** The sum of the sizes in bytes of all the operations in [opset]. *)
  }

  let empty =
    {
      opset = Opset.empty;
      ophmap = Operation_hash.Map.empty;
      minop = None;
      cardinal = 0;
      total_bytes = 0;
    }

  (* Precondition: [op] is present in the [state]. *)
  let remove_present state op =
    let opset = Opset.remove op state.opset in
    let minop =
      match state.minop with
      | None -> None (* This is impossible since [op] was in the [state]. *)
      | Some minop ->
          if compare_ops op minop <= 0 then
            (* The removed [op] was the minimum. *)
            Opset.min_elt opset
          else state.minop
    in
    {
      opset;
      ophmap = Operation_hash.Map.remove op.hash state.ophmap;
      minop;
      cardinal = state.cardinal - 1;
      total_bytes = state.total_bytes - op.size;
    }

  (* Remove [oph] if it is in the [state], otherwise do nothing. *)
  let remove_operation state oph =
    match Operation_hash.Map.find oph state.ophmap with
    | Some op -> remove_present state op
    | None -> state

  let check_bound_invariants state config =
    state.cardinal <= config.max_operations
    && state.total_bytes <= config.max_total_bytes

  (* Remove the minimal operation until the bound invariants are restored.
     Return the updated state and the list of removed operation hashes. *)
  let enforce_bound_invariants state config =
    let rec aux state removed =
      if check_bound_invariants state config then (state, removed)
      else
        (* Invariants are broken: remove the minimal operation. *)
        match state.minop with
        | None ->
            (* Should not happen: the empty set cannot break the invariants. *)
            (state, removed)
        | Some minop -> aux (remove_present state minop) (minop.hash :: removed)
    in
    aux state []

  (* Precondition: [op] is valid (otherwise calling
     [Proto.compare_operations] on it may return an error). *)
  let add_operation state config op =
    if Operation_hash.Map.mem op.hash state.ophmap then Some (state, [])
    else
      let state =
        {
          opset = Opset.add op state.opset;
          ophmap = Operation_hash.Map.add op.hash op state.ophmap;
          minop =
            (match state.minop with
            | None -> Some op
            | Some minop ->
                if compare_ops op minop < 0 then Some op else state.minop);
          cardinal = state.cardinal + 1;
          total_bytes = state.total_bytes + op.size;
        }
      in
      let state, removed = enforce_bound_invariants state config in
      if List.mem ~equal:Operation_hash.equal op.hash removed then
        (* If the new operation needs to be immediately removed in
           order to maintain the mempool bound invariants, then it
           should actually be rejected. *)
        None
      else Some (state, removed)
end
