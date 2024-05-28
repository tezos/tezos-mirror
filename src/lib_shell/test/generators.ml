(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
open Shell_operation

let add_if_not_present classification op t =
  if Option.is_none (is_in_mempool op.hash t) then add classification op t

(** A generator for the protocol bytes of an operation. *)
let operation_proto_gen = QCheck2.Gen.small_string ?gen:None

(** A generator for the protocol bytes of an operation, when the protocol
    being used has [type operation_data = unit]. *)
let operation_mock_proto_gen =
  let open QCheck2.Gen in
  (* 9/10 generates the right size (empty), 1/10 generates too long. *)
  let* len_gen = frequencya [|(9, return 0); (1, 0 -- 31)|] in
  string_size ?gen:None len_gen

let key_gen = QCheck2.Gen.(opt (string_size (0 -- 64)))

let path_gen = QCheck2.Gen.(list_size (0 -- 10) (small_string ?gen:None))

let block_hash_gen : Block_hash.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ key = key_gen and+ path = path_gen in
  Block_hash.hash_string ?key path

(** A generator of operations.
    - [proto_gen] is the generator for protocol bytes. By default, it is
      {!proto_gen} above. This default is fine for cases where having
      valid proto bytes doesn't matter (for example for {!Prevalidator_classification}).
    - [block_hash_t] is an optional generator for the branch.
      If omitted {!block_hash_gen} is used.

    This function should be renamed to [raw_operation_gen] at some point,
    because it returns {!Operation.t} (the [op] prefix is for functions
    returning {!operation} values). *)
let operation_gen ?(proto_gen = operation_proto_gen) ?block_hash_t () :
    Operation.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let prod_block_hash_gen = Option.value ~default:block_hash_gen block_hash_t in
  let* branch = prod_block_hash_gen in
  let+ proto = proto_gen in
  let proto = Bytes.of_string proto in
  Operation.{shell = {branch}; proto}

(** Like {!operation_gen} with a hash. *)
let raw_operation_with_hash_gen ?proto_gen ?block_hash_t () :
    (Operation_hash.t * Operation.t) QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ op = operation_gen ?proto_gen ?block_hash_t () in
  let hash = Operation.hash op in
  (hash, op)

let q_in_0_1 () =
  let open QCheck2.Gen in
  let* q = Qcheck2_helpers.int64_range_gen 1L Int64.max_int in
  let+ p = Qcheck2_helpers.int64_range_gen 0L q in
  Q.make (Z.of_int64 p) (Z.of_int64 q)

let priority_gen () : Prevalidator_pending_operations.priority QCheck2.Gen.t =
  let open QCheck2.Gen in
  let open Prevalidator_pending_operations in
  let* top_prio_value = oneofl [`High; `Medium; `Low] in
  match top_prio_value with
  | `High -> pure High
  | `Medium -> pure Medium
  | `Low ->
      let+ weights = small_list (q_in_0_1 ()) in
      Low weights

let status_gen () : Prevalidator_pending_operations.status QCheck2.Gen.t =
  let open QCheck2.Gen in
  oneofl [Prevalidator_pending_operations.Fresh; Reclassified]

(** [operation_with_hash_gen ?proto_gen ?block_hash_t ()] is a generator
    for parsable operations, i.e. values of type {!operation}.

    In production, this type guarantees that the underlying operation
    has been successfully parsed. This is NOT the case with this generator.
    This is a weakness of this function that can only be solved by
    clearly making the difference between proto-dependent tests and
    proto-independent tests.

    By default, [?proto_gen] is [operation_proto_gen] which
    generates random bytes, making generated operations unparsable generally
    speaking. One can make sure that this generator generates
    parsable operations by assuming a protocol and using a custom [proto_gen].
    As an example this is the case when using
    {!Tezos_protocol_environment.Internal_for_tests.Environment_protocol_T_test.Mock_all_unit}
    as the protocol and specifying [proto_gen] to be [string_size (return 0)]
    i.e. to have both [operation_data = unit] and strings generated for
    [operation_data] always empty. *)
let operation_with_hash_gen ?proto_gen ?block_hash_t () :
    unit operation QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* signature_checked = bool in
  let+ oph, op = raw_operation_with_hash_gen ?proto_gen ?block_hash_t () in
  Internal_for_tests.make_operation ~signature_checked oph op ()

let operation_with_hash_and_status_and_priority_gen ?proto_gen ?block_hash_t ()
    :
    (unit operation * Prevalidator_pending_operations.status_and_priority)
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* op = operation_with_hash_gen ?proto_gen ?block_hash_t () in
  let* priority = priority_gen () in
  let* status = status_gen () in
  return (op, Prevalidator_pending_operations.{status; priority})

let raw_op_map_gen ?proto_gen ?block_hash_t () :
    Operation.t Operation_hash.Map.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ ops =
    small_list (raw_operation_with_hash_gen ?proto_gen ?block_hash_t ())
  in
  List.to_seq ops |> Operation_hash.Map.of_seq

(** A generator of maps of operations and their hashes. Parameters are:
    - [?proto_gen] is an optional generator for the protocol bytes.
    - [?block_hash_t] is an optional generator for the branch of operations.

    Because it returns a map,
    this generator guarantees that all returned operations are distinct
    (because their hashes differ). *)
let op_map_gen ?proto_gen ?block_hash_t () :
    unit operation Operation_hash.Map.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ ops = small_list (operation_with_hash_gen ?proto_gen ?block_hash_t ()) in
  List.to_seq ops
  |> Seq.map (fun op -> (op.hash, op))
  |> Operation_hash.Map.of_seq

(** A generator like {!raw_op_map_gen} but which guarantees the size
    of the returned maps: they are exactly of size [n]. We need
    a custom function (as opposed to using a QCheck2 function for lists
    of fixed lengths) because we *need* to return maps, because we need
    the properties that all operations hashes are different. *)
let raw_op_map_gen_n ?proto_gen ?block_hash_t (n : int) :
    Operation.t Operation_hash.Map.t QCheck2.Gen.t =
  let open QCheck2.Gen in
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
      let* new_ops = raw_op_map_gen ?proto_gen ?block_hash_t () in
      go (Operation_hash.Map.union merge ops new_ops)
  in
  go Operation_hash.Map.empty

(** A generator like {!op_map_gen} but which guarantees the size
    of the returned maps: they are exactly of size [n]. We need
    a custom function (as opposed to using a QCheck2 function for lists
    of fixed lengths) because we *need* to return maps, because we need
    the properties that all operations hashes are different. *)
let op_map_gen_n ?proto_gen ?block_hash_t (n : int) :
    unit operation Operation_hash.Map.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let map_take_n n m =
    Operation_hash.Map.bindings m
    |> List.take_n n |> List.to_seq |> Operation_hash.Map.of_seq
  in
  let merge _oph old _new = Some old in
  let rec go (ops : unit operation Operation_hash.Map.t) =
    if Operation_hash.Map.cardinal ops >= n then
      (* Done *)
      return (map_take_n n ops)
    else
      (* Not enough operations yet, generate more *)
      let* new_ops = op_map_gen ?proto_gen ?block_hash_t () in
      go (Operation_hash.Map.union merge ops new_ops)
  in
  go Operation_hash.Map.empty

(** Do we need richer errors? If so, how to generate those? *)
let classification_gen : classification QCheck2.Gen.t =
  QCheck2.Gen.oneofa
    [|
      `Validated;
      `Branch_delayed [];
      `Branch_refused [];
      `Refused [];
      `Outdated [];
    |]

let unrefused_classification_gen : classification QCheck2.Gen.t =
  QCheck2.Gen.oneofa [|`Validated; `Branch_delayed []; `Branch_refused []|]

let parameters_gen : parameters QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ map_size_limit = 1 -- 30 in
  let on_discarded_operation _ = () in
  {map_size_limit; on_discarded_operation}

let t_gen_ ~can_be_full : unit t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* parameters = parameters_gen in
  let limit = parameters.map_size_limit - if can_be_full then 0 else 1 in
  let* size = 0 -- limit in
  let+ inputs =
    list_repeat size (pair classification_gen (operation_with_hash_gen ()))
  in
  let t = Prevalidator_classification.create parameters in
  List.iter
    (fun (classification, op) -> add_if_not_present classification op t)
    inputs ;
  t

let t_gen = t_gen_ ~can_be_full:true

(* With probability 1/2, we take an operation hash already present in the
   classification. This operation is taken uniformly among the
   different classes. *)
let with_t_operation_gen : unit t -> unit operation QCheck2.Gen.t =
  let module Classification = Prevalidator_classification in
  let open QCheck2 in
  fun t ->
    let to_ops map =
      Operation_hash.Map.bindings map |> List.map (fun (_oph, (op, _)) -> op)
    in
    (* If map is empty, it cannot be used as a generator *)
    let freq_of_map map = if Operation_hash.Map.is_empty map then 0 else 1 in
    (* If list is empty, it cannot be used as a generator *)
    let freq_of_list = function [] -> 0 | _ -> 1 in
    (* If map is not empty, take one of its elements *)
    let freq_and_gen_of_map map =
      let b = freq_of_map map in
      if b = 1 then [(1, Gen.oneofl (to_ops map))] else []
    in
    (* If list is not empty, take one of its elements *)
    let freq_and_gen_of_list list =
      let b = freq_of_list list in
      if b = 1 then [(1, Gen.oneofl list)] else []
    in
    (* We use max to ensure the ponderation is strictly greater than 0. *)
    let freq_fresh t =
      max
        1
        (freq_of_map (Sized_map.to_map t.validated)
        + freq_of_map (Classification.map t.branch_refused)
        + freq_of_map (Classification.map t.branch_delayed)
        + freq_of_map (Classification.map t.refused)
        + freq_of_map (Classification.map t.outdated))
    in
    freq_and_gen_of_list (List.map snd (Sized_map.bindings t.validated))
    @ freq_and_gen_of_map (Classification.map t.branch_refused)
    @ freq_and_gen_of_map (Classification.map t.branch_delayed)
    @ freq_and_gen_of_map (Classification.map t.refused)
    @ freq_and_gen_of_map (Classification.map t.outdated)
    @ [(freq_fresh t, operation_with_hash_gen ())]
    |> Gen.frequency

let t_with_operation_gen_ ~can_be_full : (unit t * unit operation) QCheck2.Gen.t
    =
  let open QCheck2.Gen in
  let* t = t_gen_ ~can_be_full in
  pair (return t) (with_t_operation_gen t)

let t_with_operation_gen = t_with_operation_gen_ ~can_be_full:true

let t_with_operation_gen__cant_be_full =
  t_with_operation_gen_ ~can_be_full:false

let operation_hash_gen =
  let open QCheck2.Gen in
  let+ key = key_gen and+ path = path_gen in
  Operation_hash.hash_string ?key path

(** Generate an operation hash that is not present as a key in the given map. *)
let rec fresh_oph_gen ophmap =
  let open QCheck2.Gen in
  let* oph = operation_hash_gen in
  if Operation_hash.Map.mem oph ophmap then fresh_oph_gen ophmap else return oph
