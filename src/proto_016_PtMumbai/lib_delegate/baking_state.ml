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
open Protocol_client_context

(** A consensus key (aka, a validator) is identified by its alias name, its
    public key, its public key hash, and its secret key. *)
type consensus_key = {
  alias : string option;
  public_key : Signature.Public_key.t;
  public_key_hash : Signature.Public_key_hash.t;
  secret_key_uri : Client_keys.sk_uri;
}

let consensus_key_encoding =
  let open Data_encoding in
  conv
    (fun {alias; public_key; public_key_hash; secret_key_uri} ->
      ( alias,
        public_key,
        public_key_hash,
        Uri.to_string (secret_key_uri :> Uri.t) ))
    (fun (alias, public_key, public_key_hash, secret_key_uri) ->
      {
        alias;
        public_key;
        public_key_hash;
        secret_key_uri =
          (match Client_keys.make_sk_uri (Uri.of_string secret_key_uri) with
          | Ok sk -> sk
          | Error e -> Format.kasprintf Stdlib.failwith "%a" pp_print_trace e);
      })
    (obj4
       (req "alias" (option string))
       (req "public_key" Signature.Public_key.encoding)
       (req "public_key_hash" Signature.Public_key_hash.encoding)
       (req "secret_key_uri" string))

let pp_consensus_key fmt {alias; public_key_hash; _} =
  match alias with
  | None -> Format.fprintf fmt "%a" Signature.Public_key_hash.pp public_key_hash
  | Some alias ->
      Format.fprintf
        fmt
        "%s (%a)"
        alias
        Signature.Public_key_hash.pp
        public_key_hash

type consensus_key_and_delegate = consensus_key * Signature.Public_key_hash.t

let consensus_key_and_delegate_encoding =
  let open Data_encoding in
  merge_objs
    consensus_key_encoding
    (obj1 (req "delegate" Signature.Public_key_hash.encoding))

let pp_consensus_key_and_delegate fmt (consensus_key, delegate) =
  if Signature.Public_key_hash.equal consensus_key.public_key_hash delegate then
    pp_consensus_key fmt consensus_key
  else
    Format.fprintf
      fmt
      "%a@,on behalf of %a"
      pp_consensus_key
      consensus_key
      Signature.Public_key_hash.pp
      delegate

type validation_mode = Node | Local of Abstract_context_index.t

type prequorum = {
  level : int32;
  round : Round.t;
  block_payload_hash : Block_payload_hash.t;
  preendorsements : Kind.preendorsement operation list;
}

type block_info = {
  hash : Block_hash.t;
  shell : Block_header.shell_header;
  payload_hash : Block_payload_hash.t;
  payload_round : Round.t;
  round : Round.t;
  prequorum : prequorum option;
  quorum : Kind.endorsement operation list;
  payload : Operation_pool.payload;
}

type cache = {
  known_timestamps : Timestamp.time Baking_cache.Timestamp_of_round_cache.t;
  round_timestamps :
    (Timestamp.time * Round.t * consensus_key_and_delegate)
    Baking_cache.Round_timestamp_interval_cache.t;
}

type global_state = {
  (* client context *)
  cctxt : Protocol_client_context.full;
  (* chain id *)
  chain_id : Chain_id.t;
  (* baker configuration *)
  config : Baking_configuration.t;
  (* protocol constants *)
  constants : Constants.t;
  (* round durations *)
  round_durations : Round.round_durations;
  (* worker that monitor and aggregates new operations *)
  operation_worker : Operation_worker.t;
  (* the validation mode used by the baker*)
  validation_mode : validation_mode;
  (* the delegates on behalf of which the baker is running *)
  delegates : consensus_key list;
  cache : cache;
}

let prequorum_encoding =
  let open Data_encoding in
  conv
    (fun {level; round; block_payload_hash; preendorsements} ->
      (level, round, block_payload_hash, List.map Operation.pack preendorsements))
    (fun (level, round, block_payload_hash, preendorsements) ->
      {
        level;
        round;
        block_payload_hash;
        preendorsements =
          List.filter_map Operation_pool.unpack_preendorsement preendorsements;
      })
    (obj4
       (req "level" int32)
       (req "round" Round.encoding)
       (req "block_payload_hash" Block_payload_hash.encoding)
       (req "preendorsements" (list (dynamic_size Operation.encoding))))

let block_info_encoding =
  let open Data_encoding in
  conv
    (fun {
           hash;
           shell;
           payload_hash;
           payload_round;
           round;
           prequorum;
           quorum;
           payload;
         } ->
      ( hash,
        shell,
        payload_hash,
        payload_round,
        round,
        prequorum,
        List.map Operation.pack quorum,
        payload ))
    (fun ( hash,
           shell,
           payload_hash,
           payload_round,
           round,
           prequorum,
           quorum,
           payload ) ->
      {
        hash;
        shell;
        payload_hash;
        payload_round;
        round;
        prequorum;
        quorum = List.filter_map Operation_pool.unpack_endorsement quorum;
        payload;
      })
    (obj8
       (req "hash" Block_hash.encoding)
       (req "shell" Block_header.shell_header_encoding)
       (req "payload_hash" Block_payload_hash.encoding)
       (req "payload_round" Round.encoding)
       (req "round" Round.encoding)
       (req "prequorum" (option prequorum_encoding))
       (req "quorum" (list (dynamic_size Operation.encoding)))
       (req "payload" Operation_pool.payload_encoding))

let round_of_shell_header shell_header =
  Environment.wrap_tzresult
  @@ Fitness.from_raw shell_header.Tezos_base.Block_header.fitness
  >>? fun fitness -> ok (Fitness.round fitness)

module SlotMap : Map.S with type key = Slot.t = Map.Make (Slot)

(** An endorsing slot consists of the public key hash of a delegate, a
    list of slots (i.e., a list of position indexes in the slot map, in
    other words the list of rounds when it will be the proposer), and
    its endorsing power. *)
type endorsing_slot = {first_slot : Slot.t; endorsing_power : int}

(* FIXME: determine if the slot map should contain all slots or just
   the first one *)
(* We also use the delegate slots as proposal slots *)
(* TODO: make sure that this is correct *)
type delegate_slots = {
  (* be careful not to duplicate endorsing slots with different slots
     keys: always use the first slot in the slots list *)
  own_delegate_slots : (consensus_key_and_delegate * endorsing_slot) SlotMap.t;
  all_delegate_slots : endorsing_slot SlotMap.t;
  all_slots_by_round : Slot.t array;
}

type proposal = {block : block_info; predecessor : block_info}

let proposal_encoding =
  let open Data_encoding in
  conv
    (fun {block; predecessor} -> (block, predecessor))
    (fun (block, predecessor) -> {block; predecessor})
    (obj2
       (req "block" block_info_encoding)
       (req "predecessor" block_info_encoding))

let is_first_block_in_protocol {block; predecessor; _} =
  Compare.Int.(block.shell.proto_level <> predecessor.shell.proto_level)

type locked_round = {payload_hash : Block_payload_hash.t; round : Round.t}

let locked_round_encoding =
  let open Data_encoding in
  conv
    (fun {payload_hash; round} -> (payload_hash, round))
    (fun (payload_hash, round) -> {payload_hash; round})
    (obj2
       (req "payload_hash" Block_payload_hash.encoding)
       (req "round" Round.encoding))

type endorsable_payload = {proposal : proposal; prequorum : prequorum}

let endorsable_payload_encoding =
  let open Data_encoding in
  conv
    (fun {proposal; prequorum} -> (proposal, prequorum))
    (fun (proposal, prequorum) -> {proposal; prequorum})
    (obj2
       (req "proposal" proposal_encoding)
       (req "prequorum" prequorum_encoding))

type elected_block = {
  proposal : proposal;
  endorsement_qc : Kind.endorsement Operation.t list;
}

(* Updated only when we receive a block at a different level.

   N.B. it may be our own: implying that we should not update unless
   we already baked a block *)
type level_state = {
  current_level : int32;
  latest_proposal : proposal;
  is_latest_proposal_applied : bool;
  delayed_prequorum :
    (Operation_worker.candidate * Kind.preendorsement operation list) option;
  injected_preendorsements : packed_operation list option;
  (* Last proposal received where we injected an endorsement (thus we
     have seen 2f+1 preendorsements) *)
  locked_round : locked_round option;
  (* Latest payload where we've seen a proposal reach 2f+1 preendorsements *)
  endorsable_payload : endorsable_payload option;
  (* Block for which we've seen 2f+1 endorsements and that we may bake onto *)
  elected_block : elected_block option;
  delegate_slots : delegate_slots;
  next_level_delegate_slots : delegate_slots;
  next_level_proposed_round : Round.t option;
}

type phase =
  | Idle
  | Awaiting_preendorsements
  | Awaiting_application
  | Awaiting_endorsements

let phase_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Idle"
        (Tag 0)
        (constant "Idle")
        (function Idle -> Some () | _ -> None)
        (fun () -> Idle);
      case
        ~title:"Awaiting_preendorsements"
        (Tag 1)
        (constant "Awaiting_preendorsements")
        (function Awaiting_preendorsements -> Some () | _ -> None)
        (fun () -> Awaiting_preendorsements);
      case
        ~title:"Awaiting_application"
        (Tag 2)
        (constant "Awaiting_application")
        (function Awaiting_application -> Some () | _ -> None)
        (fun () -> Awaiting_application);
      case
        ~title:"Awaiting_endorsements"
        (Tag 3)
        (constant "Awaiting_endorsements")
        (function Awaiting_endorsements -> Some () | _ -> None)
        (fun () -> Awaiting_endorsements);
    ]

type round_state = {current_round : Round.t; current_phase : phase}

type state = {
  global_state : global_state;
  level_state : level_state;
  round_state : round_state;
}

type t = state

let update_current_phase state new_phase =
  {state with round_state = {state.round_state with current_phase = new_phase}}

type timeout_kind =
  | End_of_round of {ending_round : Round.t}
  | Time_to_bake_next_level of {at_round : Round.t}

let timeout_kind_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"End_of_round"
        (obj2
           (req "kind" (constant "End_of_round"))
           (req "round" Round.encoding))
        (function
          | End_of_round {ending_round} -> Some ((), ending_round) | _ -> None)
        (fun ((), ending_round) -> End_of_round {ending_round});
      case
        (Tag 1)
        ~title:"Time_to_bake_next_level"
        (obj2
           (req "kind" (constant "Time_to_bake_next_level"))
           (req "round" Round.encoding))
        (function
          | Time_to_bake_next_level {at_round} -> Some ((), at_round)
          | _ -> None)
        (fun ((), at_round) -> Time_to_bake_next_level {at_round});
    ]

type event =
  | New_valid_proposal of proposal
  | New_head_proposal of proposal
  | Prequorum_reached of
      Operation_worker.candidate * Kind.preendorsement operation list
  | Quorum_reached of
      Operation_worker.candidate * Kind.endorsement operation list
  | Timeout of timeout_kind

let event_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"New_valid_proposal"
        (tup2 (constant "New_valid_proposal") proposal_encoding)
        (function New_valid_proposal p -> Some ((), p) | _ -> None)
        (fun ((), p) -> New_valid_proposal p);
      case
        (Tag 1)
        ~title:"New_head_proposal"
        (tup2 (constant "New_head_proposal") proposal_encoding)
        (function New_head_proposal p -> Some ((), p) | _ -> None)
        (fun ((), p) -> New_head_proposal p);
      case
        (Tag 2)
        ~title:"Prequorum_reached"
        (tup3
           (constant "Prequorum_reached")
           Operation_worker.candidate_encoding
           (Data_encoding.list (dynamic_size Operation.encoding)))
        (function
          | Prequorum_reached (candidate, ops) ->
              Some ((), candidate, List.map Operation.pack ops)
          | _ -> None)
        (fun ((), candidate, ops) ->
          Prequorum_reached
            (candidate, Operation_pool.filter_preendorsements ops));
      case
        (Tag 3)
        ~title:"Quorum_reached"
        (tup3
           (constant "Quorum_reached")
           Operation_worker.candidate_encoding
           (Data_encoding.list (dynamic_size Operation.encoding)))
        (function
          | Quorum_reached (candidate, ops) ->
              Some ((), candidate, List.map Operation.pack ops)
          | _ -> None)
        (fun ((), candidate, ops) ->
          Quorum_reached (candidate, Operation_pool.filter_endorsements ops));
      case
        (Tag 4)
        ~title:"Timeout"
        (tup2 (constant "Timeout") timeout_kind_encoding)
        (function Timeout tk -> Some ((), tk) | _ -> None)
        (fun ((), tk) -> Timeout tk);
    ]

(* Disk state *)

module Events = struct
  include Internal_event.Simple

  let section = [Protocol.name; "baker"; "disk"]

  let incompatible_stored_state =
    declare_0
      ~section
      ~name:"incompatible_stored_state"
      ~level:Warning
      ~msg:"found an outdated or corrupted baking state: discarding it"
      ()
end

type state_data = {
  level_data : int32;
  locked_round_data : locked_round option;
  endorsable_payload_data : endorsable_payload option;
}

let state_data_encoding =
  let open Data_encoding in
  conv
    (fun {level_data; locked_round_data; endorsable_payload_data} ->
      (level_data, locked_round_data, endorsable_payload_data))
    (fun (level_data, locked_round_data, endorsable_payload_data) ->
      {level_data; locked_round_data; endorsable_payload_data})
    (obj3
       (req "level" int32)
       (req "locked_round" (option locked_round_encoding))
       (req "endorsable_payload" (option endorsable_payload_encoding)))

let record_state (state : state) =
  let cctxt = state.global_state.cctxt in
  let location =
    Baking_files.resolve_location ~chain_id:state.global_state.chain_id `State
  in
  let filename =
    Filename.Infix.(cctxt#get_base_dir // Baking_files.filename location)
  in
  protect @@ fun () ->
  cctxt#with_lock @@ fun () ->
  let level_data = state.level_state.current_level in
  let locked_round_data = state.level_state.locked_round in
  let endorsable_payload_data = state.level_state.endorsable_payload in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      state_data_encoding
      {level_data; locked_round_data; endorsable_payload_data}
  in
  let filename_tmp = filename ^ "_tmp" in
  Lwt_io.with_file
    ~flags:[Unix.O_CREAT; O_WRONLY; O_TRUNC; O_CLOEXEC; O_SYNC]
    ~mode:Output
    filename_tmp
    (fun channel ->
      Lwt_io.write_from_exactly channel bytes 0 (Bytes.length bytes))
  >>= fun () ->
  Lwt_unix.rename filename_tmp filename >>= fun () -> return_unit

type error += Broken_locked_values_invariant

let () =
  register_error_kind
    `Permanent
    ~id:"Baking_state.broken_locked_values_invariant"
    ~title:"Broken locked values invariant"
    ~description:
      "The expected consistency invariant on locked values does not hold"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The expected consistency invariant on locked values does not hold")
    Data_encoding.unit
    (function Broken_locked_values_invariant -> Some () | _ -> None)
    (fun () -> Broken_locked_values_invariant)

let may_record_new_state ~previous_state ~new_state =
  let {
    current_level = previous_current_level;
    locked_round = previous_locked_round;
    endorsable_payload = previous_endorsable_payload;
    _;
  } =
    previous_state.level_state
  in
  let {
    current_level = new_current_level;
    locked_round = new_locked_round;
    endorsable_payload = new_endorsable_payload;
    _;
  } =
    new_state.level_state
  in
  let is_new_state_consistent =
    Compare.Int32.(new_current_level > previous_current_level)
    || new_current_level = previous_current_level
       &&
       if Compare.Int32.(new_current_level = previous_current_level) then
         let is_new_locked_round_consistent =
           match (new_locked_round, previous_locked_round) with
           | None, None -> true
           | Some _, None -> true
           | None, Some _ -> false
           | Some new_locked_round, Some previous_locked_round ->
               Round.(new_locked_round.round >= previous_locked_round.round)
         in
         let is_new_endorsable_payload_consistent =
           match (new_endorsable_payload, previous_endorsable_payload) with
           | None, None -> true
           | Some _, None -> true
           | None, Some _ -> false
           | Some new_endorsable_payload, Some previous_endorsable_payload ->
               Round.(
                 new_endorsable_payload.proposal.block.round
                 >= previous_endorsable_payload.proposal.block.round)
         in
         is_new_locked_round_consistent && is_new_endorsable_payload_consistent
       else true
  in
  fail_unless is_new_state_consistent Broken_locked_values_invariant
  >>=? fun () ->
  let has_not_changed =
    previous_state.level_state.current_level
    == new_state.level_state.current_level
    && previous_state.level_state.locked_round
       == new_state.level_state.locked_round
    && previous_state.level_state.endorsable_payload
       == new_state.level_state.endorsable_payload
  in
  if has_not_changed then return_unit else record_state new_state

let load_endorsable_data cctxt location =
  protect (fun () ->
      let filename =
        Filename.Infix.(cctxt#get_base_dir // Baking_files.filename location)
      in
      Lwt_unix.file_exists filename >>= function
      | false -> return_none
      | true ->
          Lwt_io.with_file
            ~flags:[Unix.O_EXCL; O_RDONLY; O_CLOEXEC]
            ~mode:Input
            filename
            (fun channel ->
              Lwt_io.read channel >>= fun str ->
              match
                Data_encoding.Binary.of_string_opt state_data_encoding str
              with
              | Some state_data -> return_some state_data
              | None ->
                  (* The stored state format is incompatible: discard it. *)
                  Events.(emit incompatible_stored_state ()) >>= fun () ->
                  return_none))

let may_load_endorsable_data state =
  let cctxt = state.global_state.cctxt in
  let chain_id = state.global_state.chain_id in
  let location = Baking_files.resolve_location ~chain_id `State in
  protect ~on_error:(fun _ -> return state) @@ fun () ->
  cctxt#with_lock @@ fun () ->
  load_endorsable_data cctxt location >>=? function
  | None -> return state
  | Some {level_data; locked_round_data; endorsable_payload_data} ->
      if Compare.Int32.(state.level_state.current_level = level_data) then
        let loaded_level_state =
          {
            state.level_state with
            locked_round = locked_round_data;
            endorsable_payload = endorsable_payload_data;
          }
        in
        return {state with level_state = loaded_level_state}
      else return state

(* Helpers *)

module DelegateSet = struct
  include Set.Make (struct
    type t = consensus_key

    let compare {public_key_hash = pkh; _} {public_key_hash = pkh'; _} =
      Signature.Public_key_hash.compare pkh pkh'
  end)

  let find_pkh pkh s =
    let exception Found of elt in
    try
      iter
        (fun ({public_key_hash; _} as delegate) ->
          if Signature.Public_key_hash.equal pkh public_key_hash then
            raise (Found delegate)
          else ())
        s ;
      None
    with Found d -> Some d
end

let cache_size_limit = 100

let compute_delegate_slots (cctxt : Protocol_client_context.full)
    ?(block = `Head 0) ~level ~chain delegates =
  let own_delegates = DelegateSet.of_list delegates in
  Environment.wrap_tzresult (Raw_level.of_int32 level) >>?= fun level ->
  Plugin.RPC.Validators.get cctxt (chain, block) ~levels:[level]
  >>=? fun endorsing_rights ->
  let own_delegate_slots, all_delegate_slots =
    List.fold_left
      (fun (own_map, all_map) slot ->
        let {Plugin.RPC.Validators.consensus_key; delegate; slots; _} = slot in
        let endorsing_slot =
          {
            endorsing_power = List.length slots;
            first_slot = Stdlib.List.hd slots;
          }
        in
        let all_map =
          List.fold_left
            (fun all_map slot -> SlotMap.add slot endorsing_slot all_map)
            all_map
            slots
        in
        let own_map =
          match DelegateSet.find_pkh consensus_key own_delegates with
          | Some consensus_key ->
              List.fold_left
                (fun own_map slot ->
                  SlotMap.add
                    slot
                    ((consensus_key, delegate), endorsing_slot)
                    own_map)
                own_map
                slots
          | None -> own_map
        in
        (own_map, all_map))
      (SlotMap.empty, SlotMap.empty)
      endorsing_rights
  in
  let all_slots_by_round =
    all_delegate_slots |> SlotMap.bindings |> List.split |> fst |> Array.of_list
  in
  return {own_delegate_slots; all_delegate_slots; all_slots_by_round}

let create_cache () =
  let open Baking_cache in
  {
    known_timestamps = Timestamp_of_round_cache.create cache_size_limit;
    round_timestamps = Round_timestamp_interval_cache.create cache_size_limit;
  }

(* Pretty-printers *)

let pp_validation_mode fmt = function
  | Node -> Format.fprintf fmt "node"
  | Local _ -> Format.fprintf fmt "local"

let pp_global_state fmt {chain_id; config; validation_mode; delegates; _} =
  Format.fprintf
    fmt
    "@[<v 2>Global state:@ chain_id: %a@ @[<v 2>config:@ %a@]@ \
     validation_mode: %a@ @[<v 2>delegates:@ %a@]@]"
    Chain_id.pp
    chain_id
    Baking_configuration.pp
    config
    pp_validation_mode
    validation_mode
    Format.(pp_print_list pp_consensus_key)
    delegates

let pp_option pp fmt = function
  | None -> Format.fprintf fmt "none"
  | Some v -> Format.fprintf fmt "%a" pp v

let pp_prequorum fmt {level; round; block_payload_hash; preendorsements} =
  Format.fprintf
    fmt
    "level: %ld, round: %a, payload_hash: %a, preendorsements: %d"
    level
    Round.pp
    round
    Block_payload_hash.pp_short
    block_payload_hash
    (List.length preendorsements)

let pp_block_info fmt
    {
      hash;
      shell;
      payload_hash;
      round;
      prequorum;
      quorum;
      payload;
      payload_round;
    } =
  Format.fprintf
    fmt
    "@[<v 2>Block:@ hash: %a@ payload_hash: %a@ level: %ld@ round: %a@ \
     prequorum: %a@ quorum: %d endorsements@ payload: %a@ payload round: %a@]"
    Block_hash.pp
    hash
    Block_payload_hash.pp_short
    payload_hash
    shell.level
    Round.pp
    round
    (pp_option pp_prequorum)
    prequorum
    (List.length quorum)
    Operation_pool.pp_payload
    payload
    Round.pp
    payload_round

let pp_proposal fmt {block; _} = pp_block_info fmt block

let pp_locked_round fmt ({payload_hash; round} : locked_round) =
  Format.fprintf
    fmt
    "payload hash: %a, round: %a"
    Block_payload_hash.pp_short
    payload_hash
    Round.pp
    round

let pp_endorsable_payload fmt {proposal; prequorum} =
  Format.fprintf
    fmt
    "proposal: %a, prequorum: %a"
    Block_hash.pp
    proposal.block.hash
    pp_prequorum
    prequorum

let pp_elected_block fmt {proposal; endorsement_qc} =
  Format.fprintf
    fmt
    "@[<v 2>%a@ nb quorum endorsements: %d@]"
    pp_block_info
    proposal.block
    (List.length endorsement_qc)

let pp_endorsing_slot fmt
    (consensus_key_and_delegate, {first_slot; endorsing_power}) =
  Format.fprintf
    fmt
    "slots: @[<h>first_slot: %a@],@ delegate: %a,@ endorsing_power: %d"
    Slot.pp
    first_slot
    pp_consensus_key_and_delegate
    consensus_key_and_delegate
    endorsing_power

let pp_delegate_slots fmt {own_delegate_slots; _} =
  Format.fprintf
    fmt
    "@[<v>%a@]"
    Format.(
      pp_print_list ~pp_sep:pp_print_cut (fun fmt (slot, endorsing_slot) ->
          Format.fprintf
            fmt
            "slot: %a, %a"
            Slot.pp
            slot
            pp_endorsing_slot
            endorsing_slot))
    (SlotMap.bindings own_delegate_slots)

let pp_level_state fmt
    {
      current_level;
      latest_proposal;
      is_latest_proposal_applied;
      delayed_prequorum;
      injected_preendorsements;
      locked_round;
      endorsable_payload;
      elected_block;
      delegate_slots;
      next_level_delegate_slots;
      next_level_proposed_round;
    } =
  Format.fprintf
    fmt
    "@[<v 2>Level state:@ current level: %ld@ @[<v 2>proposal (applied:%b, \
     delayed prequorum:%b, injected preendorsements: %d):@ %a@]@ locked round: \
     %a@ endorsable payload: %a@ elected block: %a@ @[<v 2>own delegate \
     slots:@ %a@]@ @[<v 2>next level own delegate slots:@ %a@]@ next level \
     proposed round: %a@]"
    current_level
    is_latest_proposal_applied
    (Option.is_some delayed_prequorum)
    (match injected_preendorsements with None -> 0 | Some l -> List.length l)
    pp_proposal
    latest_proposal
    (pp_option pp_locked_round)
    locked_round
    (pp_option pp_endorsable_payload)
    endorsable_payload
    (pp_option pp_elected_block)
    elected_block
    pp_delegate_slots
    delegate_slots
    pp_delegate_slots
    next_level_delegate_slots
    (pp_option Round.pp)
    next_level_proposed_round

let pp_phase fmt = function
  | Idle -> Format.fprintf fmt "idle"
  | Awaiting_preendorsements -> Format.fprintf fmt "awaiting preendorsements"
  | Awaiting_application -> Format.fprintf fmt "awaiting application"
  | Awaiting_endorsements -> Format.fprintf fmt "awaiting endorsements"

let pp_round_state fmt {current_round; current_phase} =
  Format.fprintf
    fmt
    "@[<v 2>Round state:@ round: %a@ phase: %a@]"
    Round.pp
    current_round
    pp_phase
    current_phase

let pp fmt {global_state; level_state; round_state} =
  Format.fprintf
    fmt
    "@[<v 2>State:@ %a@ %a@ %a@]"
    pp_global_state
    global_state
    pp_level_state
    level_state
    pp_round_state
    round_state

let pp_timeout_kind fmt = function
  | End_of_round {ending_round} ->
      Format.fprintf fmt "end of round %a" Round.pp ending_round
  | Time_to_bake_next_level {at_round} ->
      Format.fprintf fmt "time to bake next level at round %a" Round.pp at_round

let pp_event fmt = function
  | New_valid_proposal proposal ->
      Format.fprintf
        fmt
        "new valid proposal received: %a"
        pp_block_info
        proposal.block
  | New_head_proposal proposal ->
      Format.fprintf
        fmt
        "new head proposal received: %a"
        pp_block_info
        proposal.block
  | Prequorum_reached (candidate, preendos) ->
      Format.fprintf
        fmt
        "prequorum reached with %d preendorsements for %a at round %a"
        (List.length preendos)
        Block_hash.pp
        candidate.Operation_worker.hash
        Round.pp
        candidate.round_watched
  | Quorum_reached (candidate, endos) ->
      Format.fprintf
        fmt
        "quorum reached with %d endorsements for %a at round %a"
        (List.length endos)
        Block_hash.pp
        candidate.Operation_worker.hash
        Round.pp
        candidate.round_watched
  | Timeout kind ->
      Format.fprintf fmt "timeout reached: %a" pp_timeout_kind kind
