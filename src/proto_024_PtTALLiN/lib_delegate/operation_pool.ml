(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Should we use a better ordering ? *)

type 'collection t = {
  consensus : 'collection;
  votes : 'collection;
  anonymous : 'collection;
  managers : 'collection;
}

let compare_op op1 op2 =
  try Stdlib.compare op1 op2
  with _ ->
    (* FIXME some operations (e.g. tx_rollup_rejection) pack
       functional values which could raise an exception. In this
       specific case, we default to comparing their hashes. *)
    Operation_hash.compare
      (Alpha_context.Operation.hash_packed op1)
      (Alpha_context.Operation.hash_packed op2)

module Prioritized_operation = struct
  (* Higher priority operations will be included first *)
  type t = Prioritized of int * packed_operation | Low of packed_operation

  let extern ?(priority = 1) op = Prioritized (priority, op)

  let node op = Low op

  let packed = function Prioritized (_, op) | Low op -> op

  let compare_priority t1 t2 =
    match (t1, t2) with
    | Prioritized _, Low _ -> 1
    | Low _, Prioritized _ -> -1
    | Low _, Low _ -> 0
    | Prioritized (p0, _), Prioritized (p1, _) -> Compare.Int.compare p0 p1

  let compare a b =
    let c = compare_priority a b in
    if c <> 0 then c else compare_op (packed a) (packed b)
end

module Operation_set = Set.Make (struct
  type t = packed_operation

  let compare = compare_op
end)

module Prioritized_operation_set = struct
  include Set.Make (struct
    type t = Prioritized_operation.t

    let compare = Prioritized_operation.compare
  end)

  let operations set = elements set |> List.map Prioritized_operation.packed
end

(* TODO refine this: unpack operations *)
type pool = Operation_set.t t

(* TODO refine this: unpack operations *)
type ordered_pool = packed_operation list t

let ordered_pool_encoding =
  let open Data_encoding in
  conv
    (fun {consensus; votes; anonymous; managers} ->
      (consensus, votes, anonymous, managers))
    (fun (consensus, votes, anonymous, managers) ->
      {consensus; votes; anonymous; managers})
    (obj4
       (req "ordered_consensus" (list (dynamic_size Operation.encoding)))
       (req "ordered_votes" (list (dynamic_size Operation.encoding)))
       (req "ordered_anonymouns" (list (dynamic_size Operation.encoding)))
       (req "ordered_managers" (list (dynamic_size Operation.encoding))))

type payload = {
  votes_payload : packed_operation list;
  anonymous_payload : packed_operation list;
  managers_payload : packed_operation list;
}

let empty_payload =
  {votes_payload = []; anonymous_payload = []; managers_payload = []}

let payload_encoding =
  let open Data_encoding in
  conv
    (fun {votes_payload; anonymous_payload; managers_payload} ->
      (votes_payload, anonymous_payload, managers_payload))
    (fun (votes_payload, anonymous_payload, managers_payload) ->
      {votes_payload; anonymous_payload; managers_payload})
    (obj3
       (req "votes_payload" (list (dynamic_size Operation.encoding)))
       (req "anonymous_payload" (list (dynamic_size Operation.encoding)))
       (req "managers_payload" (list (dynamic_size Operation.encoding))))

let pp_payload fmt {votes_payload; anonymous_payload; managers_payload} =
  Format.fprintf
    fmt
    "[votes: %d, anonymous: %d, managers: %d]"
    (List.length votes_payload)
    (List.length anonymous_payload)
    (List.length managers_payload)

let empty =
  {
    consensus = Operation_set.empty;
    votes = Operation_set.empty;
    anonymous = Operation_set.empty;
    managers = Operation_set.empty;
  }

let empty_ordered = {consensus = []; votes = []; anonymous = []; managers = []}

let pp_pool fmt {consensus; votes; anonymous; managers} =
  Format.fprintf
    fmt
    "[consensus: %d, votes: %d, anonymous: %d, managers: %d]"
    (Operation_set.cardinal consensus)
    (Operation_set.cardinal votes)
    (Operation_set.cardinal anonymous)
    (Operation_set.cardinal managers)

let pp_ordered_pool fmt {consensus; votes; anonymous; managers} =
  Format.fprintf
    fmt
    "[consensus: %d, votes: %d, anonymous: %d, managers: %d]"
    (List.length consensus)
    (List.length votes)
    (List.length anonymous)
    (List.length managers)

let classify op =
  (* Hypothesis: acceptable passes on an ill-formed operation returns
     None. *)
  let pass = Main.acceptable_pass op in
  match pass with
  | None -> `Bad
  | Some pass ->
      let open Operation_repr in
      if pass = consensus_pass then
        `Consensus (* TODO filter outdated consensus ops ? *)
      else if pass = voting_pass then `Votes
      else if pass = anonymous_pass then `Anonymous
      else if pass = manager_pass then `Managers
      else `Bad

let add_operation_to_pool add classify pool operation =
  match classify operation with
  | `Consensus ->
      let consensus = add operation pool.consensus in
      {pool with consensus}
  | `Votes ->
      let votes = add operation pool.votes in
      {pool with votes}
  | `Anonymous ->
      let anonymous = add operation pool.anonymous in
      {pool with anonymous}
  | `Managers ->
      let managers = add operation pool.managers in
      {pool with managers}
  | `Bad -> pool

let add_operation = add_operation_to_pool Operation_set.add classify

let add_operations pool ops = List.fold_left add_operation pool ops

type consensus_filter = {
  level : int32;
  round : Round.t;
  payload_hash : Block_payload_hash.t;
  branch : Block_hash.t;
}

(** From a pool of operations [operation_pool], the function filters
    out the attestations that are different from the [current_level],
    the [current_round] or the optional [current_block_payload_hash],
    as well as preattestations. *)
let filter_with_relevant_consensus_ops ~aggregate_attestation_feature_flag
    ~(attestation_filter : consensus_filter)
    ~(preattestation_filter : consensus_filter option) operation_set =
  Operation_set.filter
    (fun {shell = {branch}; protocol_data} ->
      match (protocol_data, preattestation_filter) with
      (* 1a. Remove preattestations. *)
      | Operation_data {contents = Single (Preattestation _); _}, None -> false
      (* 1b. Filter preattestations. *)
      | ( Operation_data
            {
              contents =
                Single (Preattestation {level; round; block_payload_hash; _});
              _;
            },
          Some
            {
              level = level';
              round = round';
              payload_hash = block_payload_hash';
              branch = branch';
            } ) ->
          Compare.Int32.(Raw_level.to_int32 level = level')
          && Round.(round = round')
          && Block_payload_hash.(block_payload_hash = block_payload_hash')
          && ((not aggregate_attestation_feature_flag)
             || Block_hash.(branch = branch'))
      (* 2. Filter attestations. *)
      | ( Operation_data
            {
              contents =
                Single
                  (Attestation
                     {
                       consensus_content = {level; round; block_payload_hash; _};
                       dal_content = _;
                     });
              _;
            },
          _ ) ->
          Compare.Int32.(Raw_level.to_int32 level = attestation_filter.level)
          && Round.(round = attestation_filter.round)
          && Block_payload_hash.(
               block_payload_hash = attestation_filter.payload_hash
               && ((not aggregate_attestation_feature_flag)
                  || Block_hash.(branch = attestation_filter.branch)))
      (* 3. Preserve all non-consensus operations. *)
      | _ -> true)
    operation_set

let unpack_preattestation packed_preattestation =
  let {shell; protocol_data = Operation_data data} = packed_preattestation in
  match data with
  | {contents = Single (Preattestation _); _} ->
      Some ({shell; protocol_data = data} : Kind.preattestation Operation.t)
  | _ -> None

let unpack_attestation packed_attestation =
  let {shell; protocol_data = Operation_data data} = packed_attestation in
  match data with
  | {contents = Single (Attestation _); _} ->
      Some ({shell; protocol_data = data} : Kind.attestation Operation.t)
  | _ -> None

let filter_preattestations ops =
  List.filter_map
    (function
      | {
          shell = {branch};
          protocol_data =
            Operation_data
              ({contents = Single (Preattestation _); _} as content);
          _;
        } ->
          Some
            ({shell = {branch}; protocol_data = content}
              : Kind.preattestation operation)
      | _ -> None)
    ops

let filter_attestations ops =
  List.filter_map
    (function
      | {
          shell = {branch};
          protocol_data =
            Operation_data ({contents = Single (Attestation _); _} as content);
          _;
        } ->
          Some
            ({shell = {branch}; protocol_data = content}
              : Kind.attestation operation)
      | _ -> None)
    ops

let ordered_to_list_list {consensus; votes; anonymous; managers} =
  [consensus; votes; anonymous; managers]

let ordered_of_list_list = function
  | [consensus; votes; anonymous; managers] ->
      Some {consensus; votes; anonymous; managers}
  | _ -> None

let payload_of_ordered_pool {votes; anonymous; managers; _} =
  {
    votes_payload = votes;
    anonymous_payload = anonymous;
    managers_payload = managers;
  }

let ordered_pool_of_payload ~consensus_operations
    {votes_payload; anonymous_payload; managers_payload} =
  {
    consensus = consensus_operations;
    votes = votes_payload;
    anonymous = anonymous_payload;
    managers = managers_payload;
  }

let extract_operations_of_list_list = function
  | [consensus; votes_payload; anonymous_payload; managers_payload] ->
      let preattestations, attestations =
        List.fold_left
          (fun ( (preattestations : packed_operation list),
                 (attestations : packed_operation list) )
               packed_op
             ->
            let (Operation_data protocol_data) = packed_op.protocol_data in
            match protocol_data.contents with
            | Single (Preattestation _) ->
                (packed_op :: preattestations, attestations)
            | Single (Preattestations_aggregate _) ->
                (packed_op :: preattestations, attestations)
            | Single (Attestation _) ->
                (preattestations, packed_op :: attestations)
            | Single (Attestations_aggregate _) ->
                (preattestations, packed_op :: attestations)
            | _ ->
                (* unreachable *)
                (preattestations, attestations))
          ([], [])
          consensus
        (* N.b. the order doesn't matter *)
      in
      let preattestations =
        if preattestations = [] then None else Some preattestations
      in
      let payload = {votes_payload; anonymous_payload; managers_payload} in
      Some (preattestations, attestations, payload)
  | _ -> None

let filter_pool p {consensus; votes; anonymous; managers} =
  {
    consensus = Operation_set.filter p consensus;
    votes = Operation_set.filter p votes;
    anonymous = Operation_set.filter p anonymous;
    managers = Operation_set.filter p managers;
  }

module Prioritized = struct
  type nonrec t = Prioritized_operation_set.t t

  let of_operation_set (operation_set : Operation_set.t) =
    Operation_set.fold
      (fun elt set ->
        Prioritized_operation_set.add (Prioritized_operation.node elt) set)
      operation_set
      Prioritized_operation_set.empty

  let of_pool (pool : pool) : t =
    {
      consensus = of_operation_set pool.consensus;
      votes = of_operation_set pool.votes;
      anonymous = of_operation_set pool.anonymous;
      managers = of_operation_set pool.managers;
    }

  let add_operation =
    add_operation_to_pool Prioritized_operation_set.add (fun op ->
        classify (Prioritized_operation.packed op))

  let add_external_operation pool priority operation =
    add_operation pool (Prioritized_operation.extern ~priority operation)

  let add_operations prioritized_pool operations =
    List.fold_left add_operation prioritized_pool operations

  (* [merge_external_operations] considers that the list of operation
     represents an ordererd list of operation with the head having the highest
     prioritiy.
  *)
  let merge_external_operations pool
      (external_operations : packed_operation list) =
    List.fold_left_i
      (fun i pool op -> add_external_operation pool (-i) op)
      pool
      external_operations

  let filter p {consensus; votes; anonymous; managers} =
    let filter =
      Prioritized_operation_set.filter (fun pop ->
          p (Prioritized_operation.packed pop))
    in
    {
      consensus = filter consensus;
      votes = filter votes;
      anonymous = filter anonymous;
      managers = filter managers;
    }
end
