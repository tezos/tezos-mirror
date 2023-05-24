(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
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

module Int_set = Set.Make (Compare.Int)

(*

   Gas levels maintenance
   =======================

   The context maintains two levels of gas, one corresponds to the gas
   available for the current operation while the other is the gas
   available for the current block. Both levels are maintained
   independently: [consume_gas] only decreases the operation level,
   and block level should be updated with [consume_gas_limit_in_block].

   A layered context
   =================

   Updating the context [remaining_operation_gas] is a critical routine
   called very frequently by the operations performed by the protocol.
   On the contrary, other fields are less frequently updated.

   In a previous version of the context datatype definition, all
   the fields were represented at the toplevel. To update the remaining
   gas, we had to copy ~25 fields (that is 200 bytes).

   With the following layered representation, we only have to
   copy 2 fields (16 bytes) during [remaining_operation_gas] update.
   This has a significant impact on the Michelson runtime efficiency.

   Here are the fields on the [back] of the context:

 *)

type consensus_pk = {
  delegate : Signature.Public_key_hash.t;
  consensus_pk : Signature.Public_key.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

let consensus_pk_encoding =
  let open Data_encoding in
  conv
    (fun {delegate; consensus_pk; consensus_pkh} ->
      if Signature.Public_key_hash.equal consensus_pkh delegate then
        (consensus_pk, None)
      else (consensus_pk, Some delegate))
    (fun (consensus_pk, delegate) ->
      let consensus_pkh = Signature.Public_key.hash consensus_pk in
      let delegate =
        match delegate with None -> consensus_pkh | Some del -> del
      in
      {delegate; consensus_pk; consensus_pkh})
    (obj2
       (req "consensus_pk" Signature.Public_key.encoding)
       (opt "delegate" Signature.Public_key_hash.encoding))

module Raw_consensus = struct
  (** Consensus operations are indexed by their [initial slots]. Given
      a delegate, the [initial slot] is the lowest slot assigned to
      this delegate. *)

  type t = {
    current_endorsement_power : int;
        (** Number of endorsement slots recorded for the current block. *)
    allowed_endorsements : (consensus_pk * int) Slot_repr.Map.t option;
        (** Endorsements rights for the current block. Only an endorsement
            for the lowest slot in the block can be recorded. The map
            associates to each initial slot the [pkh] associated to this
            slot with its power. This is [None] only in mempool mode. *)
    allowed_preendorsements : (consensus_pk * int) Slot_repr.Map.t option;
        (** Preendorsements rights for the current block. Only a preendorsement
            for the lowest slot in the block can be recorded. The map
            associates to each initial slot the [pkh] associated to this
            slot with its power. This is [None] only in mempool mode, or in
            application mode when there is no locked round (so the block
            cannot contain any preendorsements). *)
    endorsements_seen : Slot_repr.Set.t;
        (** Record the endorsements already seen. Only initial slots are indexed. *)
    preendorsements_seen : Slot_repr.Set.t;
        (** Record the preendorsements already seen. Only initial slots
            are indexed. *)
    locked_round_evidence : (Round_repr.t * int) option;
        (** Record the preendorsement power for a locked round. *)
    preendorsements_quorum_round : Round_repr.t option;
        (** in block construction mode, record the round of preendorsements
            included in a block. *)
    endorsement_branch : (Block_hash.t * Block_payload_hash.t) option;
  }

  (** Invariant:

      - [slot \in endorsements_seen => Int_map.mem slot allowed_endorsements]

      - [slot \in preendorsements_seen => Int_map.mem slot allowed_preendorsements]

      - [ |endorsements_seen| > 0 => |included endorsements| > 0]

  *)

  let empty : t =
    {
      current_endorsement_power = 0;
      allowed_endorsements = Some Slot_repr.Map.empty;
      allowed_preendorsements = Some Slot_repr.Map.empty;
      endorsements_seen = Slot_repr.Set.empty;
      preendorsements_seen = Slot_repr.Set.empty;
      locked_round_evidence = None;
      preendorsements_quorum_round = None;
      endorsement_branch = None;
    }

  type error += Double_inclusion_of_consensus_operation

  let () =
    register_error_kind
      `Branch
      ~id:"operation.double_inclusion_of_consensus_operation"
      ~title:"Double inclusion of consensus operation"
      ~description:"double inclusion of consensus operation"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Double inclusion of consensus operation")
      Data_encoding.empty
      (function
        | Double_inclusion_of_consensus_operation -> Some () | _ -> None)
      (fun () -> Double_inclusion_of_consensus_operation)

  let record_endorsement t ~initial_slot ~power =
    error_when
      (Slot_repr.Set.mem initial_slot t.endorsements_seen)
      Double_inclusion_of_consensus_operation
    >|? fun () ->
    {
      t with
      current_endorsement_power = t.current_endorsement_power + power;
      endorsements_seen = Slot_repr.Set.add initial_slot t.endorsements_seen;
    }

  let record_preendorsement ~initial_slot ~power round t =
    error_when
      (Slot_repr.Set.mem initial_slot t.preendorsements_seen)
      Double_inclusion_of_consensus_operation
    >|? fun () ->
    let locked_round_evidence =
      match t.locked_round_evidence with
      | None -> Some (round, power)
      | Some (_stored_round, evidences) ->
          (* In mempool mode, round and stored_round can be different.
             It doesn't matter in that case since quorum certificates
             are not used in mempool.
             For other cases [Apply.check_round] verifies it. *)
          Some (round, evidences + power)
    in
    {
      t with
      locked_round_evidence;
      preendorsements_seen =
        Slot_repr.Set.add initial_slot t.preendorsements_seen;
    }

  let set_preendorsements_quorum_round round t =
    match t.preendorsements_quorum_round with
    | Some round' ->
        (* If the rounds are different, an error should have already
           been raised. *)
        assert (Round_repr.equal round round') ;
        t
    | None -> {t with preendorsements_quorum_round = Some round}

  let initialize_with_endorsements_and_preendorsements ~allowed_endorsements
      ~allowed_preendorsements t =
    {t with allowed_endorsements; allowed_preendorsements}

  let locked_round_evidence t = t.locked_round_evidence

  let endorsement_branch t = t.endorsement_branch

  let set_endorsement_branch t endorsement_branch =
    {t with endorsement_branch = Some endorsement_branch}
end

type dal_committee = {
  pkh_to_shards :
    (Dal_attestation_repr.shard_index * int) Signature.Public_key_hash.Map.t;
  shard_to_pkh : Signature.Public_key_hash.t Dal_attestation_repr.Shard_map.t;
}

let empty_dal_committee =
  {
    pkh_to_shards = Signature.Public_key_hash.Map.empty;
    shard_to_pkh = Dal_attestation_repr.Shard_map.empty;
  }

type back = {
  context : Context.t;
  constants : Constants_parametric_repr.t;
  round_durations : Round_repr.Durations.t;
  cycle_eras : Level_repr.cycle_eras;
  level : Level_repr.t;
  predecessor_timestamp : Time.t;
  timestamp : Time.t;
  fees : Tez_repr.t;
  origination_nonce : Origination_nonce.t option;
  temporary_lazy_storage_ids : Lazy_storage_kind.Temp_ids.t;
  internal_nonce : int;
  internal_nonces_used : Int_set.t;
  remaining_block_gas : Gas_limit_repr.Arith.fp;
  unlimited_operation_gas : bool;
  consensus : Raw_consensus.t;
  non_consensus_operations_rev : Operation_hash.t list;
  dictator_proposal_seen : bool;
  sampler_state : (Seed_repr.seed * consensus_pk Sampler.t) Cycle_repr.Map.t;
  stake_distribution_for_current_cycle :
    Stake_repr.t Signature.Public_key_hash.Map.t option;
  sc_rollup_current_messages : Sc_rollup_inbox_merkelized_payload_hashes_repr.t;
  dal_slot_fee_market : Dal_slot_repr.Slot_market.t;
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3105

     We associate to a slot header some fees. This enable the use
     of a fee market for slot publication. However, this is not
     resilient from the game theory point of view. Probably we can find
     better incentives here. In any case, because we want the following
     invariant:

         - For each level and for each slot there is at most one slot
     header.

         - We need to provide an incentive to avoid byzantines to post
     dummy slot headers. *)
  dal_attestation_slot_accountability : Dal_attestation_repr.Accountability.t;
  dal_committee : dal_committee;
}

(*

   The context is simply a record with two fields which
   limits the cost of updating the [remaining_operation_gas].

*)
type t = {remaining_operation_gas : Gas_limit_repr.Arith.fp; back : back}

type root = t

(*

   Context fields accessors
   ========================

   To have the context related code more robust to evolutions,
   we introduce accessors to get and to update the context
   components.

*)
let[@inline] context ctxt = ctxt.back.context

let[@inline] current_level ctxt = ctxt.back.level

let[@inline] predecessor_timestamp ctxt = ctxt.back.predecessor_timestamp

let[@inline] current_timestamp ctxt = ctxt.back.timestamp

let[@inline] round_durations ctxt = ctxt.back.round_durations

let[@inline] cycle_eras ctxt = ctxt.back.cycle_eras

let[@inline] constants ctxt = ctxt.back.constants

let[@inline] tx_rollup ctxt = ctxt.back.constants.tx_rollup

let[@inline] sc_rollup ctxt = ctxt.back.constants.sc_rollup

let[@inline] zk_rollup ctxt = ctxt.back.constants.zk_rollup

let[@inline] recover ctxt = ctxt.back.context

let[@inline] fees ctxt = ctxt.back.fees

let[@inline] origination_nonce ctxt = ctxt.back.origination_nonce

let[@inline] internal_nonce ctxt = ctxt.back.internal_nonce

let[@inline] internal_nonces_used ctxt = ctxt.back.internal_nonces_used

let[@inline] remaining_block_gas ctxt = ctxt.back.remaining_block_gas

let[@inline] unlimited_operation_gas ctxt = ctxt.back.unlimited_operation_gas

let[@inline] temporary_lazy_storage_ids ctxt =
  ctxt.back.temporary_lazy_storage_ids

let[@inline] remaining_operation_gas ctxt = ctxt.remaining_operation_gas

let[@inline] non_consensus_operations_rev ctxt =
  ctxt.back.non_consensus_operations_rev

let[@inline] dictator_proposal_seen ctxt = ctxt.back.dictator_proposal_seen

let[@inline] sampler_state ctxt = ctxt.back.sampler_state

let[@inline] update_back ctxt back = {ctxt with back}

let[@inline] update_remaining_block_gas ctxt remaining_block_gas =
  update_back ctxt {ctxt.back with remaining_block_gas}

let[@inline] update_remaining_operation_gas ctxt remaining_operation_gas =
  {ctxt with remaining_operation_gas}

let[@inline] update_unlimited_operation_gas ctxt unlimited_operation_gas =
  update_back ctxt {ctxt.back with unlimited_operation_gas}

let[@inline] update_context ctxt context =
  update_back ctxt {ctxt.back with context}

let[@inline] update_constants ctxt constants =
  update_back ctxt {ctxt.back with constants}

let[@inline] update_origination_nonce ctxt origination_nonce =
  update_back ctxt {ctxt.back with origination_nonce}

let[@inline] update_internal_nonce ctxt internal_nonce =
  update_back ctxt {ctxt.back with internal_nonce}

let[@inline] update_internal_nonces_used ctxt internal_nonces_used =
  update_back ctxt {ctxt.back with internal_nonces_used}

let[@inline] update_fees ctxt fees = update_back ctxt {ctxt.back with fees}

let[@inline] update_temporary_lazy_storage_ids ctxt temporary_lazy_storage_ids =
  update_back ctxt {ctxt.back with temporary_lazy_storage_ids}

let[@inline] update_non_consensus_operations_rev ctxt
    non_consensus_operations_rev =
  update_back ctxt {ctxt.back with non_consensus_operations_rev}

let[@inline] update_dictator_proposal_seen ctxt dictator_proposal_seen =
  update_back ctxt {ctxt.back with dictator_proposal_seen}

let[@inline] update_sampler_state ctxt sampler_state =
  update_back ctxt {ctxt.back with sampler_state}

type error += Too_many_internal_operations (* `Permanent *)

type error += Block_quota_exceeded (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

type error += Stake_distribution_not_set (* `Branch *)

type error += Sampler_already_set of Cycle_repr.t (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"too_many_internal_operations"
    ~title:"Too many internal operations"
    ~description:
      "A transaction exceeded the hard limit of internal operations it can emit"
    empty
    (function Too_many_internal_operations -> Some () | _ -> None)
    (fun () -> Too_many_internal_operations) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.operation"
    ~title:"Gas quota exceeded for the operation"
    ~description:
      "A script or one of its callee took more time than the operation said it \
       would"
    empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Temporary
    ~id:"gas_exhausted.block"
    ~title:"Gas quota exceeded for the block"
    ~description:
      "The sum of gas consumed by all the operations in the block exceeds the \
       hard gas limit per block"
    empty
    (function Block_quota_exceeded -> Some () | _ -> None)
    (fun () -> Block_quota_exceeded) ;
  register_error_kind
    `Permanent
    ~id:"delegate.stake_distribution_not_set"
    ~title:"Stake distribution not set"
    ~description:"The stake distribution for the current cycle is not set."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The stake distribution for the current cycle is not set.")
    empty
    (function Stake_distribution_not_set -> Some () | _ -> None)
    (fun () -> Stake_distribution_not_set) ;
  register_error_kind
    `Permanent
    ~id:"sampler_already_set"
    ~title:"Sampler already set"
    ~description:
      "Internal error: Raw_context.set_sampler_for_cycle was called twice for \
       a given cycle"
    ~pp:(fun ppf c ->
      Format.fprintf
        ppf
        "Internal error: sampler already set for cycle %a."
        Cycle_repr.pp
        c)
    (obj1 (req "cycle" Cycle_repr.encoding))
    (function Sampler_already_set c -> Some c | _ -> None)
    (fun c -> Sampler_already_set c)

let fresh_internal_nonce ctxt =
  if Compare.Int.(internal_nonce ctxt >= 65_535) then
    error Too_many_internal_operations
  else
    ok
      (update_internal_nonce ctxt (internal_nonce ctxt + 1), internal_nonce ctxt)

let reset_internal_nonce ctxt =
  let ctxt = update_internal_nonce ctxt 0 in
  update_internal_nonces_used ctxt Int_set.empty

let record_internal_nonce ctxt k =
  update_internal_nonces_used ctxt (Int_set.add k (internal_nonces_used ctxt))

let internal_nonce_already_recorded ctxt k =
  Int_set.mem k (internal_nonces_used ctxt)

let get_collected_fees ctxt = fees ctxt

let credit_collected_fees_only_call_from_token ctxt fees' =
  let previous = get_collected_fees ctxt in
  Tez_repr.(previous +? fees') >|? fun fees -> update_fees ctxt fees

let spend_collected_fees_only_call_from_token ctxt fees' =
  let previous = get_collected_fees ctxt in
  Tez_repr.(previous -? fees') >|? fun fees -> update_fees ctxt fees

type error += Undefined_operation_nonce (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"undefined_operation_nonce"
    ~title:"Ill timed access to the origination nonce"
    ~description:
      "An origination was attempted out of the scope of a manager operation"
    empty
    (function Undefined_operation_nonce -> Some () | _ -> None)
    (fun () -> Undefined_operation_nonce)

let init_origination_nonce ctxt operation_hash =
  let origination_nonce = Some (Origination_nonce.initial operation_hash) in
  update_origination_nonce ctxt origination_nonce

let increment_origination_nonce ctxt =
  match origination_nonce ctxt with
  | None -> error Undefined_operation_nonce
  | Some cur_origination_nonce ->
      let origination_nonce =
        Some (Origination_nonce.incr cur_origination_nonce)
      in
      let ctxt = update_origination_nonce ctxt origination_nonce in
      ok (ctxt, cur_origination_nonce)

let get_origination_nonce ctxt =
  match origination_nonce ctxt with
  | None -> error Undefined_operation_nonce
  | Some origination_nonce -> ok origination_nonce

let unset_origination_nonce ctxt = update_origination_nonce ctxt None

let gas_level ctxt =
  let open Gas_limit_repr in
  if unlimited_operation_gas ctxt then Unaccounted
  else Limited {remaining = remaining_operation_gas ctxt}

let block_gas_level = remaining_block_gas

let consume_gas_limit_in_block ctxt gas_limit =
  let open Gas_limit_repr in
  check_gas_limit
    ~hard_gas_limit_per_operation:(constants ctxt).hard_gas_limit_per_operation
    ~gas_limit
  >>? fun () ->
  let block_gas = block_gas_level ctxt in
  let limit = Arith.fp gas_limit in
  if Arith.(limit > block_gas) then error Block_quota_exceeded
  else
    let level = Arith.sub (block_gas_level ctxt) limit in
    let ctxt = update_remaining_block_gas ctxt level in
    Ok ctxt

let set_gas_limit ctxt (remaining : 'a Gas_limit_repr.Arith.t) =
  let open Gas_limit_repr in
  let remaining_operation_gas = Arith.fp remaining in
  let ctxt = update_unlimited_operation_gas ctxt false in
  {ctxt with remaining_operation_gas}

let set_gas_unlimited ctxt = update_unlimited_operation_gas ctxt true

let consume_gas ctxt cost =
  match Gas_limit_repr.raw_consume (remaining_operation_gas ctxt) cost with
  | Some gas_counter -> Ok (update_remaining_operation_gas ctxt gas_counter)
  | None ->
      if unlimited_operation_gas ctxt then ok ctxt
      else error Operation_quota_exceeded

let check_enough_gas ctxt cost =
  consume_gas ctxt cost >>? fun (_ : t) -> Result.return_unit

let gas_consumed ~since ~until =
  match (gas_level since, gas_level until) with
  | Limited {remaining = before}, Limited {remaining = after} ->
      Gas_limit_repr.Arith.sub before after
  | _, _ -> Gas_limit_repr.Arith.zero

type missing_key_kind = Get | Set | Del | Copy

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

let storage_error_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Incompatible_protocol_version"
        (obj1 (req "incompatible_protocol_version" @@ string Plain))
        (function Incompatible_protocol_version arg -> Some arg | _ -> None)
        (fun arg -> Incompatible_protocol_version arg);
      case
        (Tag 1)
        ~title:"Missing_key"
        (obj2
           (req "missing_key" (list @@ string Plain))
           (req
              "function"
              (string_enum
                 [("get", Get); ("set", Set); ("del", Del); ("copy", Copy)])))
        (function Missing_key (key, f) -> Some (key, f) | _ -> None)
        (fun (key, f) -> Missing_key (key, f));
      case
        (Tag 2)
        ~title:"Existing_key"
        (obj1 (req "existing_key" (list @@ string Plain)))
        (function Existing_key key -> Some key | _ -> None)
        (fun key -> Existing_key key);
      case
        (Tag 3)
        ~title:"Corrupted_data"
        (obj1 (req "corrupted_data" (list @@ string Plain)))
        (function Corrupted_data key -> Some key | _ -> None)
        (fun key -> Corrupted_data key);
    ]

let pp_storage_error ppf = function
  | Incompatible_protocol_version version ->
      Format.fprintf
        ppf
        "Found a context with an unexpected version '%s'."
        version
  | Missing_key (key, Get) ->
      Format.fprintf ppf "Missing key '%s'." (String.concat "/" key)
  | Missing_key (key, Set) ->
      Format.fprintf
        ppf
        "Cannot set undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, Del) ->
      Format.fprintf
        ppf
        "Cannot delete undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, Copy) ->
      Format.fprintf
        ppf
        "Cannot copy undefined key '%s'."
        (String.concat "/" key)
  | Existing_key key ->
      Format.fprintf
        ppf
        "Cannot initialize defined key '%s'."
        (String.concat "/" key)
  | Corrupted_data key ->
      Format.fprintf
        ppf
        "Failed to parse the data at '%s'."
        (String.concat "/" key)

type error += Storage_error of storage_error

let () =
  register_error_kind
    `Permanent
    ~id:"context.storage_error"
    ~title:"Storage error (fatal internal error)"
    ~description:
      "An error that should never happen unless something has been deleted or \
       corrupted in the database."
    ~pp:(fun ppf err ->
      Format.fprintf ppf "@[<v 2>Storage error:@ %a@]" pp_storage_error err)
    storage_error_encoding
    (function Storage_error err -> Some err | _ -> None)
    (fun err -> Storage_error err)

let storage_error err = error (Storage_error err)

(* Initialization *********************************************************)

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]

(* This value is set by the snapshot_alpha.sh script, don't change it. *)
let version_value = "alpha_current"

let version = "v1"

let protocol_migration_internal_message =
  Sc_rollup_inbox_message_repr.Protocol_migration version_value

let protocol_migration_serialized_message =
  match
    Sc_rollup_inbox_message_repr.serialize
      (Internal protocol_migration_internal_message)
  with
  | Ok msg -> msg
  | Error trace ->
      Format.kasprintf
        failwith
        "%s: Could not serialize protocol message : %a"
        __LOC__
        pp_trace
        trace

let cycle_eras_key = [version; "cycle_eras"]

let constants_key = [version; "constants"]

let protocol_param_key = ["protocol_parameters"]

let get_cycle_eras ctxt =
  Context.find ctxt cycle_eras_key >|= function
  | None -> storage_error (Missing_key (cycle_eras_key, Get))
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt Level_repr.cycle_eras_encoding bytes
      with
      | None -> storage_error (Corrupted_data cycle_eras_key)
      | Some cycle_eras -> ok cycle_eras)

let set_cycle_eras ctxt cycle_eras =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Level_repr.cycle_eras_encoding cycle_eras
  in
  Context.add ctxt cycle_eras_key bytes >|= ok

type error += Failed_to_parse_parameter of bytes

type error += Failed_to_decode_parameter of Data_encoding.json * string

let () =
  register_error_kind
    `Temporary
    ~id:"context.failed_to_parse_parameter"
    ~title:"Failed to parse parameter"
    ~description:"The protocol parameters are not valid JSON."
    ~pp:(fun ppf bytes ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot parse the protocol parameter:@ %s@]"
        (Bytes.to_string bytes))
    Data_encoding.(obj1 (req "contents" @@ bytes Hex))
    (function Failed_to_parse_parameter data -> Some data | _ -> None)
    (fun data -> Failed_to_parse_parameter data) ;
  register_error_kind
    `Temporary
    ~id:"context.failed_to_decode_parameter"
    ~title:"Failed to decode parameter"
    ~description:"Unexpected JSON object."
    ~pp:(fun ppf (json, msg) ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot decode the protocol parameter:@ %s@ %a@]"
        msg
        Data_encoding.Json.pp
        json)
    Data_encoding.(obj2 (req "contents" json) (req "error" @@ string Plain))
    (function
      | Failed_to_decode_parameter (json, msg) -> Some (json, msg) | _ -> None)
    (fun (json, msg) -> Failed_to_decode_parameter (json, msg))

let get_proto_param ctxt =
  Context.find ctxt protocol_param_key >>= function
  | None -> failwith "Missing protocol parameters."
  | Some bytes -> (
      match Data_encoding.Binary.of_bytes_opt Data_encoding.json bytes with
      | None -> tzfail (Failed_to_parse_parameter bytes)
      | Some json -> (
          Context.remove ctxt protocol_param_key >|= fun ctxt ->
          match Data_encoding.Json.destruct Parameters_repr.encoding json with
          | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
              Format.kasprintf
                failwith
                "Invalid protocol_parameters: %a %a"
                (fun ppf -> Data_encoding.Json.print_error ppf)
                exn
                Data_encoding.Json.pp
                json
          | param ->
              Parameters_repr.check_params param >>? fun () -> ok (param, ctxt))
      )

let add_constants ctxt constants =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Constants_parametric_repr.encoding
      constants
  in
  Context.add ctxt constants_key bytes

let get_constants ctxt =
  Context.find ctxt constants_key >|= function
  | None -> failwith "Internal error: cannot read constants in context."
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Constants_parametric_repr.encoding
          bytes
      with
      | None -> failwith "Internal error: cannot parse constants in context."
      | Some constants -> ok constants)

let patch_constants ctxt f =
  let constants = f (constants ctxt) in
  add_constants (context ctxt) constants >|= fun context ->
  let ctxt = update_context ctxt context in
  update_constants ctxt constants

let check_inited ctxt =
  Context.find ctxt version_key >|= function
  | None -> failwith "Internal error: un-initialized context."
  | Some bytes ->
      let s = Bytes.to_string bytes in
      if Compare.String.(s = version_value) then Result.return_unit
      else storage_error (Incompatible_protocol_version s)

let check_cycle_eras (cycle_eras : Level_repr.cycle_eras)
    (constants : Constants_parametric_repr.t) =
  let current_era = Level_repr.current_era cycle_eras in
  assert (
    Compare.Int32.(current_era.blocks_per_cycle = constants.blocks_per_cycle)) ;
  assert (
    Compare.Int32.(
      current_era.blocks_per_commitment = constants.blocks_per_commitment))

let prepare ~level ~predecessor_timestamp ~timestamp ctxt =
  Raw_level_repr.of_int32 level >>?= fun level ->
  check_inited ctxt >>=? fun () ->
  get_constants ctxt >>=? fun constants ->
  Round_repr.Durations.create
    ~first_round_duration:constants.minimal_block_delay
    ~delay_increment_per_round:constants.delay_increment_per_round
  >>?= fun round_durations ->
  get_cycle_eras ctxt >|=? fun cycle_eras ->
  check_cycle_eras cycle_eras constants ;
  let level = Level_repr.level_from_raw ~cycle_eras level in
  let sc_rollup_current_messages =
    Sc_rollup_inbox_repr.init_witness_no_history
  in
  {
    remaining_operation_gas = Gas_limit_repr.Arith.zero;
    back =
      {
        context = ctxt;
        constants;
        level;
        predecessor_timestamp;
        timestamp;
        round_durations;
        cycle_eras;
        fees = Tez_repr.zero;
        origination_nonce = None;
        temporary_lazy_storage_ids = Lazy_storage_kind.Temp_ids.init;
        internal_nonce = 0;
        internal_nonces_used = Int_set.empty;
        remaining_block_gas =
          Gas_limit_repr.Arith.fp
            constants.Constants_parametric_repr.hard_gas_limit_per_block;
        unlimited_operation_gas = true;
        consensus = Raw_consensus.empty;
        non_consensus_operations_rev = [];
        dictator_proposal_seen = false;
        sampler_state = Cycle_repr.Map.empty;
        stake_distribution_for_current_cycle = None;
        sc_rollup_current_messages;
        dal_slot_fee_market =
          Dal_slot_repr.Slot_market.init
            ~length:constants.Constants_parametric_repr.dal.number_of_slots;
        dal_attestation_slot_accountability =
          Dal_attestation_repr.Accountability.init
            ~length:constants.Constants_parametric_repr.dal.number_of_slots;
        dal_committee = empty_dal_committee;
      };
  }

type previous_protocol = Genesis of Parameters_repr.t | Nairobi_017

let check_and_update_protocol_version ctxt =
  (Context.find ctxt version_key >>= function
   | None ->
       failwith "Internal error: un-initialized context in check_first_block."
   | Some bytes ->
       let s = Bytes.to_string bytes in
       if Compare.String.(s = version_value) then
         failwith "Internal error: previously initialized context."
       else if Compare.String.(s = "genesis") then
         get_proto_param ctxt >|=? fun (param, ctxt) -> (Genesis param, ctxt)
       else if Compare.String.(s = "nairobi_017") then return (Nairobi_017, ctxt)
       else Lwt.return @@ storage_error (Incompatible_protocol_version s))
  >>=? fun (previous_proto, ctxt) ->
  Context.add ctxt version_key (Bytes.of_string version_value) >|= fun ctxt ->
  ok (previous_proto, ctxt)

(* only for the migration *)
let[@warning "-32"] get_previous_protocol_constants ctxt =
  Context.find ctxt constants_key >>= function
  | None ->
      failwith
        "Internal error: cannot read previous protocol constants in context."
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Constants_parametric_previous_repr.encoding
          bytes
      with
      | None ->
          failwith
            "Internal error: cannot parse previous protocol constants in \
             context."
      | Some constants -> Lwt.return constants)

(* You should ensure that if the type `Constants_parametric_repr.t` is
   different from `Constants_parametric_previous_repr.t` or the value of these
   constants is modified, is changed from the previous protocol, then
   you `propagate` these constants to the new protocol by writing them
   onto the context via the function `add_constants` or
   `patch_constants`.

   This migration can be achieved also implicitly by modifying the
   encoding directly in a way which is compatible with the previous
   protocol. However, by doing so, you do not change the value of
   these constants inside the context. *)
let prepare_first_block ~level ~timestamp ctxt =
  check_and_update_protocol_version ctxt >>=? fun (previous_proto, ctxt) ->
  (match previous_proto with
  | Genesis param ->
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      let cycle_era =
        {
          Level_repr.first_level;
          first_cycle = Cycle_repr.root;
          blocks_per_cycle = param.constants.blocks_per_cycle;
          blocks_per_commitment = param.constants.blocks_per_commitment;
        }
      in
      Level_repr.create_cycle_eras [cycle_era] >>?= fun cycle_eras ->
      set_cycle_eras ctxt cycle_eras >>=? fun ctxt ->
      add_constants ctxt param.constants >|= ok
  | Nairobi_017 ->
      get_previous_protocol_constants ctxt >>= fun c ->
      let tx_rollup =
        Constants_parametric_repr.
          {
            enable = c.tx_rollup.enable;
            origination_size = c.tx_rollup.origination_size;
            hard_size_limit_per_inbox = c.tx_rollup.hard_size_limit_per_inbox;
            hard_size_limit_per_message =
              c.tx_rollup.hard_size_limit_per_message;
            max_withdrawals_per_batch = c.tx_rollup.max_withdrawals_per_batch;
            max_ticket_payload_size = c.tx_rollup.max_ticket_payload_size;
            commitment_bond = c.tx_rollup.commitment_bond;
            finality_period = c.tx_rollup.finality_period;
            withdraw_period = c.tx_rollup.withdraw_period;
            max_inboxes_count = c.tx_rollup.max_inboxes_count;
            max_messages_per_inbox = c.tx_rollup.max_messages_per_inbox;
            max_commitments_count = c.tx_rollup.max_commitments_count;
            cost_per_byte_ema_factor = c.tx_rollup.cost_per_byte_ema_factor;
            rejection_max_proof_size = c.tx_rollup.rejection_max_proof_size;
            sunset_level = c.tx_rollup.sunset_level;
          }
      in
      let cryptobox_parameters =
        {
          Dal.page_size = c.dal.cryptobox_parameters.page_size;
          number_of_shards = c.dal.cryptobox_parameters.number_of_shards;
          slot_size = c.dal.cryptobox_parameters.slot_size;
          redundancy_factor = c.dal.cryptobox_parameters.redundancy_factor;
        }
      in
      let dal =
        Constants_parametric_repr.
          {
            feature_enable = c.dal.feature_enable;
            number_of_slots = c.dal.number_of_slots;
            attestation_lag = c.dal.attestation_lag;
            attestation_threshold = c.dal.attestation_threshold;
            blocks_per_epoch = c.dal.blocks_per_epoch;
            cryptobox_parameters;
          }
      in
      let sc_rollup =
        Constants_parametric_repr.
          {
            enable = c.sc_rollup.enable;
            arith_pvm_enable = c.sc_rollup.arith_pvm_enable;
            origination_size = c.sc_rollup.origination_size;
            challenge_window_in_blocks = c.sc_rollup.challenge_window_in_blocks;
            stake_amount = c.sc_rollup.stake_amount;
            commitment_period_in_blocks =
              c.sc_rollup.commitment_period_in_blocks;
            max_lookahead_in_blocks = c.sc_rollup.max_lookahead_in_blocks;
            max_active_outbox_levels = c.sc_rollup.max_active_outbox_levels;
            max_outbox_messages_per_level =
              c.sc_rollup.max_outbox_messages_per_level;
            number_of_sections_in_dissection =
              c.sc_rollup.number_of_sections_in_dissection;
            timeout_period_in_blocks = c.sc_rollup.timeout_period_in_blocks;
            max_number_of_stored_cemented_commitments =
              c.sc_rollup.max_number_of_stored_cemented_commitments;
            max_number_of_parallel_games =
              c.sc_rollup.max_number_of_parallel_games;
          }
      in
      let zk_rollup =
        Constants_parametric_repr.
          {
            enable = c.zk_rollup.enable;
            origination_size = c.zk_rollup.origination_size;
            min_pending_to_process = c.zk_rollup.min_pending_to_process;
          }
      in

      let adaptive_inflation =
        Constants_parametric_repr.
          {
            enable = false;
            staking_over_baking_limit = 5;
            max_costaking_baker_count = 5;
          }
      in

      let reward_weights =
        let c_gen =
          Constants_repr.Generated.generate
            ~consensus_committee_size:c.consensus_committee_size
        in
        c_gen.reward_weights
      in

      let percentage_of_frozen_deposits_slashed_per_double_endorsement =
        100
        * c.ratio_of_frozen_deposits_slashed_per_double_endorsement.numerator
        / c.ratio_of_frozen_deposits_slashed_per_double_endorsement.denominator
      in
      let percentage_of_frozen_deposits_slashed_per_double_baking =
        let double_baking_punishment_times_100 =
          Int64.mul 100L (Tez_repr.to_mutez c.double_baking_punishment)
        in
        let percentage_rounded_down =
          Int64.div
            double_baking_punishment_times_100
            (Tez_repr.to_mutez c.minimal_stake)
        in
        1 + Int64.to_int percentage_rounded_down
      in
      let constants =
        Constants_parametric_repr.
          {
            preserved_cycles = c.preserved_cycles;
            blocks_per_cycle = c.blocks_per_cycle;
            blocks_per_commitment = c.blocks_per_commitment;
            nonce_revelation_threshold = c.nonce_revelation_threshold;
            blocks_per_stake_snapshot = c.blocks_per_stake_snapshot;
            cycles_per_voting_period = c.cycles_per_voting_period;
            hard_gas_limit_per_operation = c.hard_gas_limit_per_operation;
            hard_gas_limit_per_block = c.hard_gas_limit_per_block;
            proof_of_work_threshold = c.proof_of_work_threshold;
            minimal_stake = c.minimal_stake;
            vdf_difficulty = c.vdf_difficulty;
            origination_size = c.origination_size;
            max_operations_time_to_live = c.max_operations_time_to_live;
            reward_weights;
            cost_per_byte = c.cost_per_byte;
            hard_storage_limit_per_operation =
              c.hard_storage_limit_per_operation;
            quorum_min = c.quorum_min;
            quorum_max = c.quorum_max;
            min_proposal_quorum = c.min_proposal_quorum;
            liquidity_baking_toggle_ema_threshold =
              c.liquidity_baking_toggle_ema_threshold;
            minimal_block_delay = c.minimal_block_delay;
            delay_increment_per_round = c.delay_increment_per_round;
            consensus_committee_size = c.consensus_committee_size;
            consensus_threshold = c.consensus_threshold;
            minimal_participation_ratio = c.minimal_participation_ratio;
            max_slashing_period = c.max_slashing_period;
            frozen_deposits_percentage = c.frozen_deposits_percentage;
            percentage_of_frozen_deposits_slashed_per_double_baking;
            percentage_of_frozen_deposits_slashed_per_double_endorsement;
            (* The `testnet_dictator` should absolutely be None on mainnet *)
            testnet_dictator = c.testnet_dictator;
            initial_seed = c.initial_seed;
            cache_script_size = c.cache_script_size;
            cache_stake_distribution_cycles = c.cache_stake_distribution_cycles;
            cache_sampler_state_cycles = c.cache_sampler_state_cycles;
            tx_rollup;
            dal;
            sc_rollup;
            zk_rollup;
            adaptive_inflation;
          }
      in
      add_constants ctxt constants >>= fun ctxt -> return ctxt)
  >>=? fun ctxt ->
  prepare ctxt ~level ~predecessor_timestamp:timestamp ~timestamp
  >|=? fun ctxt -> (previous_proto, ctxt)

let activate ctxt h = Updater.activate (context ctxt) h >|= update_context ctxt

(* Generic context ********************************************************)

type key = string list

type value = bytes

type tree = Context.tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

let mem ctxt k = Context.mem (context ctxt) k

let mem_tree ctxt k = Context.mem_tree (context ctxt) k

let get ctxt k =
  Context.find (context ctxt) k >|= function
  | None -> storage_error (Missing_key (k, Get))
  | Some v -> ok v

let get_tree ctxt k =
  Context.find_tree (context ctxt) k >|= function
  | None -> storage_error (Missing_key (k, Get))
  | Some v -> ok v

let find ctxt k = Context.find (context ctxt) k

let find_tree ctxt k = Context.find_tree (context ctxt) k

let add ctxt k v = Context.add (context ctxt) k v >|= update_context ctxt

let add_tree ctxt k v =
  Context.add_tree (context ctxt) k v >|= update_context ctxt

let init ctxt k v =
  Context.mem (context ctxt) k >>= function
  | true -> Lwt.return @@ storage_error (Existing_key k)
  | _ ->
      Context.add (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let init_tree ctxt k v : _ tzresult Lwt.t =
  Context.mem_tree (context ctxt) k >>= function
  | true -> Lwt.return @@ storage_error (Existing_key k)
  | _ ->
      Context.add_tree (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let update ctxt k v =
  Context.mem (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
      Context.add (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let update_tree ctxt k v =
  Context.mem_tree (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
      Context.add_tree (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

(* Verify that the key is present before deleting *)
let remove_existing ctxt k =
  Context.mem (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
      Context.remove (context ctxt) k >|= fun context ->
      ok (update_context ctxt context)

(* Verify that the key is present before deleting *)
let remove_existing_tree ctxt k =
  Context.mem_tree (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
      Context.remove (context ctxt) k >|= fun context ->
      ok (update_context ctxt context)

(* Do not verify before deleting *)
let remove ctxt k = Context.remove (context ctxt) k >|= update_context ctxt

let add_or_remove ctxt k = function
  | None -> remove ctxt k
  | Some v -> add ctxt k v

let add_or_remove_tree ctxt k = function
  | None -> remove ctxt k
  | Some v -> add_tree ctxt k v

let list ctxt ?offset ?length k = Context.list (context ctxt) ?offset ?length k

let fold ?depth ctxt k ~order ~init ~f =
  Context.fold ?depth (context ctxt) k ~order ~init ~f

let config ctxt = Context.config (context ctxt)

module Proof = Context.Proof

let length ctxt key = Context.length (context ctxt) key

module Tree :
  Raw_context_intf.TREE
    with type t := t
     and type key := key
     and type value := value
     and type tree := tree = struct
  include Context.Tree

  let empty ctxt = Context.Tree.empty (context ctxt)

  let get t k =
    find t k >|= function
    | None -> storage_error (Missing_key (k, Get))
    | Some v -> ok v

  let get_tree t k =
    find_tree t k >|= function
    | None -> storage_error (Missing_key (k, Get))
    | Some v -> ok v

  let init t k v =
    mem t k >>= function
    | true -> Lwt.return @@ storage_error (Existing_key k)
    | _ -> add t k v >|= ok

  let init_tree t k v =
    mem_tree t k >>= function
    | true -> Lwt.return @@ storage_error (Existing_key k)
    | _ -> add_tree t k v >|= ok

  let update t k v =
    mem t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ -> add t k v >|= ok

  let update_tree t k v =
    mem_tree t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ -> add_tree t k v >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing t k =
    mem t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ -> remove t k >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing_tree t k =
    mem_tree t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ -> remove t k >|= ok

  let add_or_remove t k = function None -> remove t k | Some v -> add t k v

  let add_or_remove_tree t k = function
    | None -> remove t k
    | Some v -> add_tree t k v
end

let verify_tree_proof proof f = Context.verify_tree_proof proof f

let verify_stream_proof proof f = Context.verify_stream_proof proof f

let equal_config = Context.equal_config

let project x = x

let absolute_key _ k = k

let description = Storage_description.create ()

let fold_map_temporary_lazy_storage_ids ctxt f =
  f (temporary_lazy_storage_ids ctxt) |> fun (temporary_lazy_storage_ids, x) ->
  (update_temporary_lazy_storage_ids ctxt temporary_lazy_storage_ids, x)

let map_temporary_lazy_storage_ids_s ctxt f =
  f (temporary_lazy_storage_ids ctxt)
  >|= fun (ctxt, temporary_lazy_storage_ids) ->
  update_temporary_lazy_storage_ids ctxt temporary_lazy_storage_ids

module Cache = struct
  type key = Context.Cache.key

  type value = Context.Cache.value = ..

  let key_of_identifier = Context.Cache.key_of_identifier

  let identifier_of_key = Context.Cache.identifier_of_key

  let pp fmt ctxt = Context.Cache.pp fmt (context ctxt)

  let find c k = Context.Cache.find (context c) k

  let set_cache_layout c layout =
    Context.Cache.set_cache_layout (context c) layout >>= fun ctxt ->
    Lwt.return (update_context c ctxt)

  let update c k v = Context.Cache.update (context c) k v |> update_context c

  let sync c cache_nonce =
    Context.Cache.sync (context c) ~cache_nonce >>= fun ctxt ->
    Lwt.return (update_context c ctxt)

  let clear c = Context.Cache.clear (context c) |> update_context c

  let list_keys c ~cache_index =
    Context.Cache.list_keys (context c) ~cache_index

  let key_rank c key = Context.Cache.key_rank (context c) key

  let cache_size_limit c ~cache_index =
    Context.Cache.cache_size_limit (context c) ~cache_index

  let cache_size c ~cache_index =
    Context.Cache.cache_size (context c) ~cache_index

  let future_cache_expectation c ~time_in_blocks =
    Context.Cache.future_cache_expectation (context c) ~time_in_blocks
    |> update_context c
end

let record_non_consensus_operation_hash ctxt operation_hash =
  update_non_consensus_operations_rev
    ctxt
    (operation_hash :: non_consensus_operations_rev ctxt)

let non_consensus_operations ctxt = List.rev (non_consensus_operations_rev ctxt)

let record_dictator_proposal_seen ctxt = update_dictator_proposal_seen ctxt true

let dictator_proposal_seen ctxt = dictator_proposal_seen ctxt

let init_sampler_for_cycle ctxt cycle seed state =
  let map = sampler_state ctxt in
  if Cycle_repr.Map.mem cycle map then error (Sampler_already_set cycle)
  else
    let map = Cycle_repr.Map.add cycle (seed, state) map in
    let ctxt = update_sampler_state ctxt map in
    ok ctxt

let sampler_for_cycle ~read ctxt cycle =
  let map = sampler_state ctxt in
  match Cycle_repr.Map.find cycle map with
  | Some (seed, state) -> return (ctxt, seed, state)
  | None ->
      read ctxt >>=? fun (seed, state) ->
      let map = Cycle_repr.Map.add cycle (seed, state) map in
      let ctxt = update_sampler_state ctxt map in
      return (ctxt, seed, state)

let stake_distribution_for_current_cycle ctxt =
  match ctxt.back.stake_distribution_for_current_cycle with
  | None -> error Stake_distribution_not_set
  | Some s -> ok s

let init_stake_distribution_for_current_cycle ctxt
    stake_distribution_for_current_cycle =
  update_back
    ctxt
    {
      ctxt.back with
      stake_distribution_for_current_cycle =
        Some stake_distribution_for_current_cycle;
    }

module Internal_for_tests = struct
  let add_level ctxt l =
    let new_level = Level_repr.Internal_for_tests.add_level ctxt.back.level l in
    let new_back = {ctxt.back with level = new_level} in
    {ctxt with back = new_back}

  let add_cycles ctxt l =
    let blocks_per_cycle = Int32.to_int (constants ctxt).blocks_per_cycle in
    let new_level =
      Level_repr.Internal_for_tests.add_cycles
        ~blocks_per_cycle
        ctxt.back.level
        l
    in
    let new_back = {ctxt.back with level = new_level} in
    {ctxt with back = new_back}
end

module type CONSENSUS = sig
  type t

  type 'value slot_map

  type slot_set

  type slot

  type round

  type consensus_pk

  val allowed_endorsements : t -> (consensus_pk * int) slot_map option

  val allowed_preendorsements : t -> (consensus_pk * int) slot_map option

  type error += Slot_map_not_found of {loc : string}

  val current_endorsement_power : t -> int

  val initialize_consensus_operation :
    t ->
    allowed_endorsements:(consensus_pk * int) slot_map option ->
    allowed_preendorsements:(consensus_pk * int) slot_map option ->
    t

  val record_endorsement : t -> initial_slot:slot -> power:int -> t tzresult

  val record_preendorsement :
    t -> initial_slot:slot -> power:int -> round -> t tzresult

  val endorsements_seen : t -> slot_set

  val get_preendorsements_quorum_round : t -> round option

  val set_preendorsements_quorum_round : t -> round -> t

  val locked_round_evidence : t -> (round * int) option

  val set_endorsement_branch : t -> Block_hash.t * Block_payload_hash.t -> t

  val endorsement_branch : t -> (Block_hash.t * Block_payload_hash.t) option
end

module Consensus :
  CONSENSUS
    with type t := t
     and type slot := Slot_repr.t
     and type 'a slot_map := 'a Slot_repr.Map.t
     and type slot_set := Slot_repr.Set.t
     and type round := Round_repr.t
     and type consensus_pk := consensus_pk = struct
  let[@inline] allowed_endorsements ctxt =
    ctxt.back.consensus.allowed_endorsements

  let[@inline] allowed_preendorsements ctxt =
    ctxt.back.consensus.allowed_preendorsements

  let[@inline] current_endorsement_power ctxt =
    ctxt.back.consensus.current_endorsement_power

  let[@inline] get_preendorsements_quorum_round ctxt =
    ctxt.back.consensus.preendorsements_quorum_round

  let[@inline] locked_round_evidence ctxt =
    Raw_consensus.locked_round_evidence ctxt.back.consensus

  let[@inline] update_consensus_with ctxt f =
    {ctxt with back = {ctxt.back with consensus = f ctxt.back.consensus}}

  let[@inline] update_consensus_with_tzresult ctxt f =
    f ctxt.back.consensus >|? fun consensus ->
    {ctxt with back = {ctxt.back with consensus}}

  let[@inline] initialize_consensus_operation ctxt ~allowed_endorsements
      ~allowed_preendorsements =
    update_consensus_with
      ctxt
      (Raw_consensus.initialize_with_endorsements_and_preendorsements
         ~allowed_endorsements
         ~allowed_preendorsements)

  let[@inline] record_preendorsement ctxt ~initial_slot ~power round =
    update_consensus_with_tzresult
      ctxt
      (Raw_consensus.record_preendorsement ~initial_slot ~power round)

  let[@inline] record_endorsement ctxt ~initial_slot ~power =
    update_consensus_with_tzresult
      ctxt
      (Raw_consensus.record_endorsement ~initial_slot ~power)

  let[@inline] endorsements_seen ctxt = ctxt.back.consensus.endorsements_seen

  let[@inline] set_preendorsements_quorum_round ctxt round =
    update_consensus_with
      ctxt
      (Raw_consensus.set_preendorsements_quorum_round round)

  let[@inline] endorsement_branch ctxt =
    Raw_consensus.endorsement_branch ctxt.back.consensus

  let[@inline] set_endorsement_branch ctxt branch =
    update_consensus_with ctxt (fun ctxt ->
        Raw_consensus.set_endorsement_branch ctxt branch)

  type error += Slot_map_not_found of {loc : string}

  let () =
    register_error_kind
      `Permanent
      ~id:"raw_context.consensus.slot_map_not_found"
      ~title:"Slot map not found"
      ~description:"Pre-computed map by first slot not found."
      Data_encoding.(obj1 (req "loc" (string Plain)))
      (function Slot_map_not_found {loc} -> Some loc | _ -> None)
      (fun loc -> Slot_map_not_found {loc})
end

(*
   To optimize message insertion in smart contract rollup inboxes, we
   maintain the sequence of current messages of each rollup used in
   the block in a in-memory map.
*)
module Sc_rollup_in_memory_inbox = struct
  let current_messages ctxt = ctxt.back.sc_rollup_current_messages

  let set_current_messages ctxt witness =
    {ctxt with back = {ctxt.back with sc_rollup_current_messages = witness}}
end

module Dal = struct
  type cryptobox = Dal.t

  let make ctxt =
    let open Result_syntax in
    let Constants_parametric_repr.{dal = {cryptobox_parameters; _}; _} =
      ctxt.back.constants
    in
    match Dal.make cryptobox_parameters with
    | Ok cryptobox -> return cryptobox
    | Error (`Fail explanation) ->
        error (Dal_errors_repr.Dal_cryptobox_error {explanation})

  let number_of_slots ctxt = ctxt.back.constants.dal.number_of_slots

  let record_attested_shards ctxt attestation shards =
    let dal_attestation_slot_accountability =
      Dal_attestation_repr.Accountability.record_attested_shards
        ctxt.back.dal_attestation_slot_accountability
        attestation
        shards
    in
    {ctxt with back = {ctxt.back with dal_attestation_slot_accountability}}

  let register_slot_header ctxt slot_header =
    match
      Dal_slot_repr.Slot_market.register
        ctxt.back.dal_slot_fee_market
        slot_header
    with
    | None ->
        let length =
          Dal_slot_repr.Slot_market.length ctxt.back.dal_slot_fee_market
        in
        error
          (Dal_errors_repr.Dal_register_invalid_slot_header
             {length; slot_header})
    | Some (dal_slot_fee_market, updated) ->
        if not updated then
          error
            (Dal_errors_repr.Dal_publish_slot_header_duplicate {slot_header})
        else ok {ctxt with back = {ctxt.back with dal_slot_fee_market}}

  let candidates ctxt =
    Dal_slot_repr.Slot_market.candidates ctxt.back.dal_slot_fee_market

  let is_slot_index_attested ctxt =
    let threshold =
      ctxt.back.constants.Constants_parametric_repr.dal.attestation_threshold
    in
    let number_of_shards =
      ctxt.back.constants.Constants_parametric_repr.dal.cryptobox_parameters
        .number_of_shards
    in
    Dal_attestation_repr.Accountability.is_slot_attested
      ctxt.back.dal_attestation_slot_accountability
      ~threshold
      ~number_of_shards

  type committee = dal_committee = {
    pkh_to_shards :
      (Dal_attestation_repr.shard_index * int) Signature.Public_key_hash.Map.t;
    shard_to_pkh : Signature.Public_key_hash.t Dal_attestation_repr.Shard_map.t;
  }

  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3110

     A committee is selected by the callback function
     [pkh_from_tenderbake_slot]. We use a callback because of circular
     dependencies. It is not clear whether it will be the final choice
     for the DAL committee. The current solution is a bit hackish but
     should work. If we decide to differ from the Tenderbake
     committee, one could just draw a new committee.

     The problem with drawing a new committee is that it is not
     guaranteed that everyone in the DAL committee will be in the
     Tenderbake committee. Consequently, either we decide to have a
     new consensus operation which does not count for Tenderbake,
     and/or we take into account for the model of DAL that at every
     level, a percentage of DAL attestations cannot be received. *)
  let compute_committee ctxt pkh_from_tenderbake_slot =
    let Constants_parametric_repr.
          {
            dal = {cryptobox_parameters = {number_of_shards; _}; _};
            consensus_committee_size;
            _;
          } =
      ctxt.back.constants
    in
    (* We first draw a committee by drawing slots from the Tenderbake
       committee. To have a compact representation of slots, we can
       sort the Tenderbake slots by [pkh], so that a committee is
       actually only an interval. This is done by recomputing a
       committee from the first one. *)
    let update_committee committee pkh ~slot_index ~power =
      {
        pkh_to_shards =
          Signature.Public_key_hash.Map.update
            pkh
            (function
              | None -> Some (slot_index, power)
              | Some (initial_shard_index, old_power) ->
                  Some (initial_shard_index, old_power + power))
            committee.pkh_to_shards;
        shard_to_pkh =
          List.fold_left
            (fun shard_to_pkh slot ->
              Dal_attestation_repr.Shard_map.add slot pkh shard_to_pkh)
            committee.shard_to_pkh
            Misc.(slot_index --> (slot_index + (power - 1)));
      }
    in
    let rec compute_power index committee =
      if Compare.Int.(index < 0) then return committee
      else
        let shard_index = index mod consensus_committee_size in
        Slot_repr.of_int shard_index >>?= fun slot ->
        pkh_from_tenderbake_slot slot >>=? fun (_ctxt, pkh) ->
        (* The [Slot_repr] module is related to the Tenderbake committee. *)
        let slot_index = Slot_repr.to_int slot in
        (* An optimisation could be to return only [pkh_to_shards] map
           because the second one is not used. This can be done later
           on, if it is a good optimisation. *)
        let committee = update_committee committee pkh ~slot_index ~power:1 in
        compute_power (index - 1) committee
    in
    (* This committee is an intermediate to compute the final DAL
       committee. This one only projects the Tenderbake committee into
       the DAL committee. The next one reorders the slots so that they
       are grouped by public key hash. *)
    compute_power (number_of_shards - 1) empty_dal_committee
    >>=? fun unordered_committee ->
    let dal_committee =
      Signature.Public_key_hash.Map.fold
        (fun pkh (_, power) (total_power, committee) ->
          let committee =
            update_committee committee pkh ~slot_index:total_power ~power
          in
          let new_total_power = total_power + power in
          (new_total_power, committee))
        unordered_committee.pkh_to_shards
        (0, empty_dal_committee)
      |> snd
    in
    return dal_committee

  let init_committee ctxt committee =
    {ctxt with back = {ctxt.back with dal_committee = committee}}

  let shards_of_attestor ctxt ~attestor:pkh =
    let rec make acc (initial_shard_index, power) =
      if Compare.Int.(power <= 0) then List.rev acc
      else make (initial_shard_index :: acc) (initial_shard_index + 1, power - 1)
    in
    Signature.Public_key_hash.Map.find_opt
      pkh
      ctxt.back.dal_committee.pkh_to_shards
    |> Option.map (fun pre_shards -> make [] pre_shards)
end

(* The type for relative context accesses instead from the root. In order for
   the carbonated storage functions to consume the gas, this has gas infomation
*)
type local_context = {
  tree : tree;
  path : key;
  remaining_operation_gas : Gas_limit_repr.Arith.fp;
  unlimited_operation_gas : bool;
}

let with_local_context ctxt key f =
  (find_tree ctxt key >|= function None -> Tree.empty ctxt | Some tree -> tree)
  >>= fun tree ->
  let local_ctxt =
    {
      tree;
      path = key;
      remaining_operation_gas = remaining_operation_gas ctxt;
      unlimited_operation_gas = unlimited_operation_gas ctxt;
    }
  in
  f local_ctxt >>=? fun (local_ctxt, res) ->
  add_tree ctxt key local_ctxt.tree >|= fun ctxt ->
  update_remaining_operation_gas ctxt local_ctxt.remaining_operation_gas
  |> fun ctxt ->
  update_unlimited_operation_gas ctxt local_ctxt.unlimited_operation_gas
  |> fun ctxt -> ok (ctxt, res)

module Local_context : sig
  include
    Raw_context_intf.VIEW
      with type t = local_context
       and type key := key
       and type value := value
       and type tree := tree

  val consume_gas :
    local_context -> Gas_limit_repr.cost -> local_context tzresult

  val absolute_key : local_context -> key -> key
end = struct
  type t = local_context

  let consume_gas local cost =
    match Gas_limit_repr.raw_consume local.remaining_operation_gas cost with
    | Some gas_counter -> Ok {local with remaining_operation_gas = gas_counter}
    | None ->
        if local.unlimited_operation_gas then ok local
        else error Operation_quota_exceeded

  let tree local = local.tree

  let update_root_tree local tree = {local with tree}

  let absolute_key local key = local.path @ key

  let find local = Tree.find (tree local)

  let find_tree local = Tree.find_tree (tree local)

  let mem local = Tree.mem (tree local)

  let mem_tree local = Tree.mem_tree (tree local)

  let get local = Tree.get (tree local)

  let get_tree local = Tree.get_tree (tree local)

  let update local key b =
    Tree.update (tree local) key b >|=? update_root_tree local

  let update_tree local key b =
    Tree.update_tree (tree local) key b >|=? update_root_tree local

  let init local key b =
    Tree.init (tree local) key b >|=? update_root_tree local

  let init_tree local key t =
    Tree.init_tree (tree local) key t >|=? update_root_tree local

  let add local i b = Tree.add (tree local) i b >|= update_root_tree local

  let add_tree local i t =
    Tree.add_tree (tree local) i t >|= update_root_tree local

  let remove local i = Tree.remove (tree local) i >|= update_root_tree local

  let remove_existing local key =
    Tree.remove_existing (tree local) key >|=? update_root_tree local

  let remove_existing_tree local key =
    Tree.remove_existing_tree (tree local) key >|=? update_root_tree local

  let add_or_remove local key vopt =
    Tree.add_or_remove (tree local) key vopt >|= update_root_tree local

  let add_or_remove_tree local key topt =
    Tree.add_or_remove_tree (tree local) key topt >|= update_root_tree local

  let fold ?depth local key ~order ~init ~f =
    Tree.fold ?depth (tree local) key ~order ~init ~f

  let list local ?offset ?length key =
    Tree.list (tree local) ?offset ?length key

  let config local = Tree.config (tree local)

  let length local i = Tree.length (tree local) i
end
