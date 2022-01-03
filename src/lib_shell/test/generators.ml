(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Prevalidator_classification

let add_if_not_present classification oph op t =
  Prevalidator_classification.(
    if not (is_in_mempool oph t) then add classification oph op t)

let string_gen = QCheck.Gen.string ?gen:None

let block_hash_gen : Block_hash.t QCheck.Gen.t =
  let open QCheck.Gen in
  let+ key = opt (string_size (0 -- 64))
  and+ path = list_size (0 -- 100) string_gen in
  Block_hash.hash_string ?key path

(** A generator of operations.
    - [string_gen] is the generator for protocol bytes. By default, it is
      {!string_gen} above. This default is fine for cases where having
      valid proto bytes doesn't matter (for example for {!Prevalidator_classification}).
    - [block_hash_t] is an optional generator for the branch.
      If omitted {!block_hash_gen} is used. *)
let operation_gen ?(string_gen = string_gen) ?block_hash_t () :
    Operation.t QCheck.Gen.t =
  let open QCheck.Gen in
  let prod_block_hash_gen = Option.value ~default:block_hash_gen block_hash_t in
  let+ branch = prod_block_hash_gen
  and+ proto = string_gen >|= Bytes.of_string in
  Operation.{shell = {branch}; proto}

(** Like {!operation_gen} with a hash. *)
let operation_with_hash_gen ?string_gen ?block_hash_t () :
    (Operation_hash.t * Operation.t) QCheck.Gen.t =
  let open QCheck.Gen in
  let+ op = operation_gen ?string_gen ?block_hash_t () in
  let hash = Operation.hash op in
  (hash, op)

(** A generator of maps of operations and their hashes. Parameters are:
    - [string_gen] is an optional generator for the protocol bytes.
    - [?block_hash_t] is an optional generator for the branch of operations.

    Because it returns a map,
    this generator guarantees that all returned operations are distinct
    (because their hashes differ). *)
let op_map_gen ?string_gen ?block_hash_t () :
    Operation.t Operation_hash.Map.t QCheck.Gen.t =
  let open QCheck.Gen in
  let+ ops = small_list (operation_gen ?string_gen ?block_hash_t ()) in
  (* Op_map.of_seq eliminates duplicate keys (if any) *)
  List.map (fun op -> (Operation.hash op, op)) ops
  |> List.to_seq |> Operation_hash.Map.of_seq

(** A generator like {!op_map_gen} but which guarantees the size
    of the returned maps: they are exactly of size [n]. We need
    a custom function (as opposed to using a QCheck function for lists
    of fixed lengths) because we *need* to return maps, because we need
    the properties that all operations hashes are different. *)
let op_map_gen_n ?string_gen ?block_hash_t ~(n : int) :
    Operation.t Operation_hash.Map.t QCheck.Gen.t =
  let open QCheck.Gen in
  let map_take_n n m =
    Operation_hash.Map.bindings m
    |> List.take_n n |> List.to_seq |> Operation_hash.Map.of_seq
  in
  let merge _oph old _new = Some old in
  let rec go (ops : Operation.t Operation_hash.Map.t) =
    if Operation_hash.Map.cardinal ops >= n then
      (* Done *)
      return (map_take_n n ops)
    else
      (* Not enough operations yet, generate more *)
      let* new_ops = op_map_gen ?string_gen ?block_hash_t () in
      go (Operation_hash.Map.union merge ops new_ops)
  in
  go Operation_hash.Map.empty

(** Do we need richer errors? If so, how to generate those? *)
let classification_gen : classification QCheck.Gen.t =
  QCheck.Gen.oneofa
    [|
      `Applied;
      `Prechecked;
      `Branch_delayed [];
      `Branch_refused [];
      `Refused [];
      `Outdated [];
    |]

let unrefused_classification_gen : classification QCheck.Gen.t =
  QCheck.Gen.oneofa
    [|`Applied; `Prechecked; `Branch_delayed []; `Branch_refused []|]

let parameters_gen : parameters QCheck.Gen.t =
  let open QCheck.Gen in
  let+ map_size_limit = 1 -- 100 in
  let on_discarded_operation _ = () in
  {map_size_limit; on_discarded_operation}

let t_gen ?(can_be_full = true) () : t QCheck.Gen.t =
  let open QCheck.Gen in
  let* parameters = parameters_gen in
  let+ inputs =
    let limit = parameters.map_size_limit - if can_be_full then 0 else 1 in
    list_size
      (0 -- limit)
      (pair classification_gen (operation_with_hash_gen ()))
  in
  let t = Prevalidator_classification.create parameters in
  List.iter
    (fun (classification, (operation_hash, operation)) ->
      add_if_not_present classification operation_hash operation t)
    inputs ;
  t

(* With probability 1/2, we take an operation hash already present in the
   classification. This operation is taken uniformly among the
   different classes. *)
let with_t_operation_gen : t -> (Operation_hash.t * Operation.t) QCheck.Gen.t =
  let module Classification = Prevalidator_classification in
  let open QCheck.Gen in
  fun t ->
    let to_ops map =
      Operation_hash.Map.bindings map
      |> List.map (fun (oph, (op, _)) -> (oph, op))
    in
    (* If map is empty, it cannot be used as a generator *)
    let freq_of_map map = if Operation_hash.Map.is_empty map then 0 else 1 in
    (* If list is empty, it cannot be used as a generator *)
    let freq_of_list = function [] -> 0 | _ -> 1 in
    (* If map is not empty, take one of its elements *)
    let freq_and_gen_of_map map = (freq_of_map map, oneofl (to_ops map)) in
    (* If list is not empty, take one of its elements *)
    let freq_and_gen_of_list list = (freq_of_list list, oneofl list) in
    (* We use max to ensure the ponderation is strictly greater than 0. *)
    let freq_fresh t =
      max
        1
        (freq_of_list t.applied_rev + freq_of_map t.prechecked
        + freq_of_map (Classification.map t.branch_refused)
        + freq_of_map (Classification.map t.branch_delayed)
        + freq_of_map (Classification.map t.refused)
        + freq_of_map (Classification.map t.outdated))
    in
    frequency
      [
        freq_and_gen_of_list t.applied_rev;
        freq_and_gen_of_list (Operation_hash.Map.bindings t.prechecked);
        freq_and_gen_of_map (Classification.map t.branch_refused);
        freq_and_gen_of_map (Classification.map t.branch_delayed);
        freq_and_gen_of_map (Classification.map t.refused);
        freq_and_gen_of_map (Classification.map t.outdated);
        (freq_fresh t, operation_with_hash_gen ());
      ]

let t_with_operation_gen ?can_be_full () :
    (t * (Operation_hash.t * Operation.t)) QCheck.Gen.t =
  let open QCheck.Gen in
  t_gen ?can_be_full () >>= fun t -> pair (return t) (with_t_operation_gen t)
