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

open Protocol
open Alpha_context
open Protocol_client_context
open Client_baking_blocks
module Events = Delegate_events.Denunciator
module B_Events = Delegate_events.Baking_scheduling

module HLevel = Hashtbl.Make (struct
  type t = Chain_id.t * Raw_level.t * Round.t

  let equal (c, l, r) (c', l', r') =
    Chain_id.equal c c' && Raw_level.equal l l' && Round.equal r r'

  let hash (c, lvl, r) = Hashtbl.hash (c, lvl, r)
end)

(* Blocks are associated to the delegates who baked them *)
module Delegate_map = Signature.Public_key_hash.Map

module Validators_cache =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      type t = Raw_level.t

      let equal = Raw_level.equal

      let hash = Hashtbl.hash
    end)

(* type of operations stream, as returned by monitor_operations RPC *)
type ops_stream =
  ((Operation_hash.t * packed_operation) * error trace option) list Lwt_stream.t

type 'kind recorded_consensus =
  | No_operation_seen
  | Operation_seen of {
      operation : 'kind operation;
      previously_denounced_oph : Operation_hash.t option;
    }

type recorded_consensus_operations = {
  endorsement : Kind.endorsement recorded_consensus;
  preendorsement : Kind.preattestation recorded_consensus;
}

type 'a state = {
  (* Validators rights for the last preserved levels *)
  validators_rights : public_key_hash Slot.Map.t Validators_cache.t;
  (* Consensus operations seen so far *)
  consensus_operations_table :
    recorded_consensus_operations Delegate_map.t HLevel.t;
  (* Blocks received so far *)
  blocks_table : Block_hash.t Delegate_map.t HLevel.t;
  (* Maximum delta of level to register *)
  preserved_levels : int;
  (* Highest level seen in a block *)
  mutable highest_level_encountered : Raw_level.t;
  (* This constant allows to set at which frequency (expressed in blocks levels)
     the tables above are cleaned. Cleaning the table means removing information
     stored about old levels up to
     'highest_level_encountered - preserved_levels'.
  *)
  clean_frequency : int;
  (* the decreasing cleaning countdown for the next cleaning *)
  mutable cleaning_countdown : int;
  (* stream of all valid blocks *)
  blocks_stream : (block_info, 'a) result Lwt_stream.t;
  (* operations stream. Reset on new heads flush *)
  mutable ops_stream : ops_stream;
  (* operatons stream stopper. Used when a q new *)
  mutable ops_stream_stopper : unit -> unit;
}

let create_state ~preserved_levels blocks_stream ops_stream ops_stream_stopper =
  let clean_frequency = max 1 (preserved_levels / 10) in
  let validators_rights = Validators_cache.create (preserved_levels + 2) in
  (* We keep rights for [preserved_levels] in the past, and 2 levels in the
     future from [highest_level_encountered] *)
  Lwt.return
    {
      validators_rights;
      consensus_operations_table = HLevel.create preserved_levels;
      blocks_table = HLevel.create preserved_levels;
      preserved_levels;
      highest_level_encountered = Raw_level.root (* 0l *);
      clean_frequency;
      cleaning_countdown = clean_frequency;
      blocks_stream;
      ops_stream;
      ops_stream_stopper;
    }

(* We choose a previous offset (5 blocks from head) to ensure that the
   injected operation is branched from a valid
   predecessor. Denunciation operations can be emitted when the
   consensus is under attack and may occur so you want to inject the
   operation from a block which is considered "final". *)
let get_block_offset level =
  match Raw_level.of_int32 5l with
  | Ok min_level ->
      let offset = Raw_level.diff level min_level in
      if Compare.Int32.(offset >= 0l) then Lwt.return (`Head 5)
      else
        (* offset < 0l *)
        let negative_offset = Int32.to_int offset in
        (* We cannot inject at at level 0 : this is the genesis
           level. We inject starting from level 1 thus the '- 1'. *)
        Lwt.return (`Head (5 + negative_offset - 1))
  | Error errs ->
      Events.(emit invalid_level_conversion) (Environment.wrap_tztrace errs)
      >>= fun () -> Lwt.return (`Head 0)

let get_payload_hash (type kind) (op_kind : kind consensus_operation_type)
    (op : kind Operation.t) =
  match (op_kind, op.protocol_data.contents) with
  | Preendorsement, Single (Preendorsement consensus_content)
  | Endorsement, Single (Endorsement consensus_content) ->
      consensus_content.block_payload_hash

let get_slot (type kind) (op_kind : kind consensus_operation_type)
    (op : kind Operation.t) =
  match (op_kind, op.protocol_data.contents) with
  | Preendorsement, Single (Preendorsement consensus_content)
  | Endorsement, Single (Endorsement consensus_content) ->
      consensus_content.slot

let double_consensus_op_evidence (type kind) :
    kind consensus_operation_type ->
    #Protocol_client_context.full ->
    'a ->
    branch:Block_hash.t ->
    op1:kind Alpha_context.operation ->
    op2:kind Alpha_context.operation ->
    unit ->
    bytes Environment.Error_monad.shell_tzresult Lwt.t = function
  | Endorsement -> Plugin.RPC.Forge.double_endorsement_evidence
  | Preendorsement -> Plugin.RPC.Forge.double_preendorsement_evidence

let lookup_recorded_consensus (type kind) consensus_key
    (op_kind : kind consensus_operation_type) map : kind recorded_consensus =
  match Delegate_map.find consensus_key map with
  | None -> No_operation_seen
  | Some {endorsement; preendorsement} -> (
      match op_kind with
      | Endorsement -> endorsement
      | Preendorsement -> preendorsement)

let add_consensus_operation (type kind) consensus_key
    (op_kind : kind consensus_operation_type)
    (recorded_operation : kind recorded_consensus) map =
  Delegate_map.update
    consensus_key
    (fun x ->
      let record =
        Option.value
          ~default:
            {
              endorsement = No_operation_seen;
              preendorsement = No_operation_seen;
            }
          x
      in
      match op_kind with
      | Endorsement -> Some {record with endorsement = recorded_operation}
      | Preendorsement -> Some {record with preendorsement = recorded_operation})
    map

let get_validator_rights state cctxt level =
  let open Lwt_result_syntax in
  match Validators_cache.find_opt state.validators_rights level with
  | None ->
      let* validators =
        Plugin.RPC.Validators.get cctxt (cctxt#chain, `Head 0) ~levels:[level]
      in
      let validators =
        List.fold_left
          (fun acc ({consensus_key; slots; _} : RPC.Validators.t) ->
            List.fold_left
              (fun acc slot -> Slot.Map.add slot consensus_key acc)
              acc
              slots)
          Slot.Map.empty
          validators
      in
      Validators_cache.replace state.validators_rights level validators ;
      return validators
  | Some t -> return t

let process_consensus_op (type kind) state cctxt
    (op_kind : kind consensus_operation_type) (new_op : kind Operation.t)
    chain_id level round slot =
  let open Lwt_result_syntax in
  let diff = Raw_level.diff state.highest_level_encountered level in
  if Int32.(diff > of_int state.preserved_levels) then
    (* We do not handle operations older than [preserved_levels] *)
    let*! () =
      Events.(emit consensus_operation_too_old) (Operation.hash new_op)
    in
    return_unit
  else if diff < -2l then
    (* We do not handle operations too far in the future *)
    let*! () =
      Events.(emit consensus_operation_too_far_in_future)
        (Operation.hash new_op)
    in
    return_unit
  else
    let* endorsing_rights = get_validator_rights state cctxt level in
    match Slot.Map.find slot endorsing_rights with
    | None ->
        (* We do not handle operations that do not have a valid slot *)
        return_unit
    | Some consensus_key -> (
        let round_map =
          Option.value ~default:Delegate_map.empty
          @@ HLevel.find
               state.consensus_operations_table
               (chain_id, level, round)
        in
        match lookup_recorded_consensus consensus_key op_kind round_map with
        | No_operation_seen ->
            return
            @@ HLevel.add
                 state.consensus_operations_table
                 (chain_id, level, round)
                 (add_consensus_operation
                    consensus_key
                    op_kind
                    (Operation_seen
                       {operation = new_op; previously_denounced_oph = None})
                    round_map)
        | Operation_seen {operation = existing_op; previously_denounced_oph}
          when Block_payload_hash.(
                 get_payload_hash op_kind existing_op
                 <> get_payload_hash op_kind new_op)
               || Slot.(get_slot op_kind existing_op <> slot)
               || Block_hash.(existing_op.shell.branch <> new_op.shell.branch)
          ->
            (* Same level, round, and delegate, and:
               different payload hash OR different slot OR different branch *)
            let new_op_hash, existing_op_hash =
              (Operation.hash new_op, Operation.hash existing_op)
            in
            let op1, op2 =
              if Operation_hash.(new_op_hash < existing_op_hash) then
                (new_op, existing_op)
              else (existing_op, new_op)
            in
            let*! block = get_block_offset level in
            let chain = `Hash chain_id in
            let* block_hash =
              Alpha_block_services.hash cctxt ~chain ~block ()
            in
            let* bytes =
              double_consensus_op_evidence
                op_kind
                cctxt
                (`Hash chain_id, block)
                ~branch:block_hash
                ~op1
                ~op2
                ()
            in
            let bytes = Signature.concat bytes Signature.zero in
            let double_op_detected, double_op_denounced =
              Events.(
                match op_kind with
                | Endorsement ->
                    (double_attestation_detected, double_attestation_denounced)
                | Preendorsement ->
                    ( double_preattestation_detected,
                      double_preattestation_denounced ))
            in
            let*! () =
              Events.(emit double_op_detected) (new_op_hash, existing_op_hash)
            in
            let* op_hash =
              Shell_services.Injection.private_operation cctxt ~chain bytes
            in
            let*! () =
              match previously_denounced_oph with
              | Some oph -> Events.(emit double_consensus_already_denounced) oph
              | None -> Lwt.return_unit
            in
            HLevel.replace
              state.consensus_operations_table
              (chain_id, level, round)
              (add_consensus_operation
                 consensus_key
                 op_kind
                 (Operation_seen
                    {
                      operation = new_op;
                      previously_denounced_oph = Some op_hash;
                    })
                 round_map) ;
            let*! () = Events.(emit double_op_denounced) (op_hash, bytes) in
            return_unit
        | _ -> return_unit)

let process_operations (cctxt : #Protocol_client_context.full) state
    (endorsements : 'a list) ~packed_op chain_id =
  List.iter_es
    (fun op ->
      let {shell; protocol_data; _} = packed_op op in
      match protocol_data with
      | Operation_data
          ({contents = Single (Preendorsement {round; slot; level; _}); _} as
          protocol_data) ->
          let new_preendorsement : Kind.preattestation Alpha_context.operation =
            {shell; protocol_data}
          in
          process_consensus_op
            state
            cctxt
            Preendorsement
            new_preendorsement
            chain_id
            level
            round
            slot
      | Operation_data
          ({contents = Single (Endorsement {round; slot; level; _}); _} as
          protocol_data) ->
          let new_endorsement : Kind.endorsement Alpha_context.operation =
            {shell; protocol_data}
          in
          process_consensus_op
            state
            cctxt
            Endorsement
            new_endorsement
            chain_id
            level
            round
            slot
      | _ ->
          (* not a consensus operation *)
          return_unit)
    endorsements

let context_block_header cctxt ~chain b_hash =
  Alpha_block_services.header cctxt ~chain ~block:(`Hash (b_hash, 0)) ()
  >>=? fun ({shell; protocol_data; _} : Alpha_block_services.block_header) ->
  return {Alpha_context.Block_header.shell; protocol_data}

let process_block (cctxt : #Protocol_client_context.full) state
    (header : Alpha_block_services.block_info) =
  match header with
  | {hash; metadata = None; _} ->
      Events.(emit unexpected_pruned_block) hash >>= fun () -> return_unit
  | {
   Alpha_block_services.chain_id;
   hash = new_hash;
   metadata = Some {protocol_data = {baker; level_info = {level; _}; _}; _};
   header = {shell = {fitness; _}; _};
   _;
  } -> (
      let fitness = Fitness.from_raw fitness in
      Lwt.return
        (match fitness with
        | Ok fitness -> Ok (Fitness.round fitness)
        | Error errs -> Error (Environment.wrap_tztrace errs))
      >>=? fun round ->
      let chain = `Hash chain_id in
      let map =
        Option.value ~default:Delegate_map.empty
        @@ HLevel.find state.blocks_table (chain_id, level, round)
      in
      match Delegate_map.find baker.delegate map with
      | None ->
          return
          @@ HLevel.add
               state.blocks_table
               (chain_id, level, round)
               (Delegate_map.add baker.delegate new_hash map)
      | Some existing_hash when Block_hash.(existing_hash = new_hash) ->
          (* This case should never happen *)
          Events.(emit double_baking_but_not) () >>= fun () ->
          return
          @@ HLevel.replace
               state.blocks_table
               (chain_id, level, round)
               (Delegate_map.add baker.delegate new_hash map)
      | Some existing_hash ->
          (* If a previous block made by this pkh is found for
             the same (level, round) we inject a double_baking_evidence *)
          context_block_header cctxt ~chain existing_hash >>=? fun bh1 ->
          context_block_header cctxt ~chain new_hash >>=? fun bh2 ->
          let hash1 = Block_header.hash bh1 in
          let hash2 = Block_header.hash bh2 in
          let bh1, bh2 =
            if Block_hash.(hash1 < hash2) then (bh1, bh2) else (bh2, bh1)
          in
          (* If the blocks are on different chains then skip it *)
          get_block_offset level >>= fun block ->
          Alpha_block_services.hash cctxt ~chain ~block ()
          >>=? fun block_hash ->
          Plugin.RPC.Forge.double_baking_evidence
            cctxt
            (chain, block)
            ~branch:block_hash
            ~bh1
            ~bh2
            ()
          >>=? fun bytes ->
          let bytes = Signature.concat bytes Signature.zero in
          Events.(emit double_baking_detected) () >>= fun () ->
          Shell_services.Injection.operation cctxt ~chain bytes
          >>=? fun op_hash ->
          Events.(emit double_baking_denounced) (op_hash, bytes) >>= fun () ->
          return
          @@ HLevel.replace
               state.blocks_table
               (chain_id, level, round)
               (Delegate_map.add baker.delegate new_hash map))

(* Remove levels that are lower than the
   [highest_level_encountered] minus [preserved_levels] *)
let cleanup_old_operations state =
  state.cleaning_countdown <- state.cleaning_countdown - 1 ;
  if state.cleaning_countdown < 0 then (
    (* It's time to remove old levels *)
    state.cleaning_countdown <- state.clean_frequency ;
    let highest_level_encountered =
      Int32.to_int (Raw_level.to_int32 state.highest_level_encountered)
    in
    let diff = highest_level_encountered - state.preserved_levels in
    let threshold =
      if diff < 0 then Raw_level.root
      else
        Raw_level.of_int32 (Int32.of_int diff) |> function
        | Ok threshold -> threshold
        | Error _ -> Raw_level.root
    in
    let filter hmap =
      HLevel.filter_map_inplace
        (fun (_, level, _) x ->
          if Raw_level.(level < threshold) then None else Some x)
        hmap
    in
    filter state.consensus_operations_table ;
    filter state.blocks_table)

(* Each new block is processed :
   - Checking that every baker injected only once at this level
   - Checking that every (pre)endorser operated only once at this level
*)
let process_new_block (cctxt : #Protocol_client_context.full) state
    {hash; chain_id; level; protocol; next_protocol; _} =
  if Protocol_hash.(protocol <> next_protocol) then
    Events.(emit protocol_change_detected) () >>= fun () -> return_unit
  else
    Events.(emit accuser_saw_block) (level, hash) >>= fun () ->
    let chain = `Hash chain_id in
    let block = `Hash (hash, 0) in
    state.highest_level_encountered <-
      Raw_level.max level state.highest_level_encountered ;
    (* Processing blocks *)
    (Alpha_block_services.info cctxt ~chain ~block () >>= function
     | Ok block_info -> (
         process_block cctxt state block_info >>=? fun () ->
         (* Processing (pre)endorsements in the block *)
         match block_info.operations with
         | consensus_ops :: _ ->
             let packed_op {Alpha_block_services.shell; protocol_data; _} =
               {shell; protocol_data}
             in
             process_operations cctxt state consensus_ops ~packed_op chain_id
         | _ ->
             (* Should not happen as a block should contain 4 lists of
                operations, the first list being dedicated to consensus
                operations. *)
             Events.(emit fetch_operations_error hash) >>= fun () -> return_unit
         )
     | Error errs ->
         Events.(emit accuser_block_error) (hash, errs) >>= fun () ->
         return_unit)
    >>=? fun () ->
    cleanup_old_operations state ;
    return_unit

let process_new_block cctxt state bi =
  process_new_block cctxt state bi >>= function
  | Ok () -> Events.(emit accuser_processed_block) bi.hash >>= Lwt.return
  | Error errs ->
      Events.(emit accuser_block_error) (bi.hash, errs) >>= Lwt.return

let log_errors_and_continue ~name p =
  p >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> B_Events.(emit daemon_error) (name, errs)

let start_ops_monitor cctxt =
  Alpha_block_services.Mempool.monitor_operations
    cctxt
    ~chain:cctxt#chain
    ~validated:true
    ~branch_delayed:true
    ~branch_refused:false
    ~refused:false
    ~outdated:false
    ()

let create (cctxt : #Protocol_client_context.full) ?canceler ~preserved_levels
    valid_blocks_stream =
  B_Events.(emit daemon_setup) name >>= fun () ->
  start_ops_monitor cctxt >>=? fun (ops_stream, ops_stream_stopper) ->
  create_state
    ~preserved_levels
    valid_blocks_stream
    ops_stream
    ops_stream_stopper
  >>= fun state ->
  Option.iter
    (fun canceler ->
      Lwt_canceler.on_cancel canceler (fun () ->
          state.ops_stream_stopper () ;
          Lwt.return_unit))
    canceler ;
  let last_get_block = ref None in
  let get_block () =
    match !last_get_block with
    | None ->
        let t = Lwt_stream.get state.blocks_stream >|= fun e -> `Block e in
        last_get_block := Some t ;
        t
    | Some t -> t
  in
  let last_get_ops = ref None in
  let get_ops () =
    match !last_get_ops with
    | None ->
        let t = Lwt_stream.get state.ops_stream >|= fun e -> `Operations e in
        last_get_ops := Some t ;
        t
    | Some t -> t
  in
  Chain_services.chain_id cctxt () >>=? fun chain_id ->
  (* main loop *)
  (* Only allocate once the termination promise *)
  let terminated = Lwt_exit.clean_up_starts >|= fun _ -> `Termination in
  let rec worker_loop () =
    Lwt.choose [terminated; get_block (); get_ops ()] >>= function
    (* event matching *)
    | `Termination -> return_unit
    | `Block (None | Some (Error _)) ->
        (* exit when the node is unavailable *)
        last_get_block := None ;
        B_Events.(emit daemon_connection_lost) name >>= fun () ->
        fail Baking_errors.Node_connection_lost
    | `Block (Some (Ok bi)) ->
        last_get_block := None ;
        process_new_block cctxt state bi >>= fun () -> worker_loop ()
    | `Operations None ->
        (* restart a new operations monitor stream *)
        last_get_ops := None ;
        state.ops_stream_stopper () ;
        start_ops_monitor cctxt >>=? fun (ops_stream, ops_stream_stopper) ->
        state.ops_stream <- ops_stream ;
        state.ops_stream_stopper <- ops_stream_stopper ;
        worker_loop ()
    | `Operations (Some ops) ->
        last_get_ops := None ;
        log_errors_and_continue ~name
        @@ process_operations
             cctxt
             state
             ops
             ~packed_op:(fun ((_h, op), _errl) -> op)
             chain_id
        >>= fun () -> worker_loop ()
  in
  B_Events.(emit daemon_start) name >>= fun () -> worker_loop ()
