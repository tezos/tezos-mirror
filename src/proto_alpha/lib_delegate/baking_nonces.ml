(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
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
module Events = Baking_events.Nonces

type state = {
  cctxt : Protocol_client_context.full;
  chain : Chain_services.chain;
  constants : Constants.t;
  config : Baking_configuration.nonce_config;
  legacy_location : [`Legacy_nonce] Baking_files.location;
  stateful_location : [`Stateful_nonce] Baking_files.location;
  mutable last_predecessor : Block_hash.t;
}

type t = state

(** [nonce_state] tracks the current state of committed nonces. It is used to
    optimise nonce processing by further reducing queries for nonce metadata. *)
type nonce_state =
  | Committed
      (** [Committed] is initial state and signals that the nonce was committed at
          some cycle:
       - If baker in current cycle: do nothing
       - If baker in cycle + 1:
           - If block with commitment is part of the
             cannonical chain: then transition to injected
           - else: safe to delete
       - If current cycle > cycle + 1: too stale, so safe to delete *)
  | Revealed of Raw_level.t
      (** [Revealed] signals that the nonce revelation operation was injected at
      [injected_level]
      - At each level, get the last finalized block (level - 2)
      - If the revelation operation found in that block: nonce is safe to
        delete
      - If not found:
          - If pass `re_injection_threshold`:
              - consider revelation operation lost, so re-inject
              - transition to injected with updated level
          - If pass `nonce_revelation_threshold`:
              - missed out revelation operation, so delete
          - else: do nothing *)

type nonce_data = {
  nonce : Nonce.t;
  nonce_hash : Nonce_hash.t;
  block_hash : Block_hash.t; (* Keep around for legacy purposes *)
  cycle : Cycle.t;
  level : Raw_level.t;
  round : Round.t option;
  nonce_state : nonce_state;
}

type nonces = nonce_data Nonce_hash.Map.t

let empty = Nonce_hash.Map.empty

let legacy_empty = Block_hash.Map.empty

let nonce_state_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Committed"
        (constant "committed")
        (function Committed -> Some () | _ -> None)
        (fun () -> Committed);
      case
        (Tag 1)
        ~title:"Revealed"
        (obj1 (req "injection_level" Raw_level.encoding))
        (function
          | Revealed injection_level -> Some injection_level | _ -> None)
        (fun injection_level -> Revealed injection_level);
    ]

let nonce_data_encoding =
  let open Data_encoding in
  def "nonce_data"
  @@ conv
       (fun {nonce; nonce_hash; block_hash; cycle; level; round; nonce_state} ->
         (nonce, nonce_hash, block_hash, cycle, level, round, nonce_state))
       (fun (nonce, nonce_hash, block_hash, cycle, level, round, nonce_state) ->
         {nonce; nonce_hash; block_hash; cycle; level; round; nonce_state})
       (obj7
          (req "nonce" Nonce.encoding)
          (req "hash" Nonce_hash.encoding)
          (req "block" Block_hash.encoding)
          (req "cycle" Cycle.encoding)
          (req "level" Raw_level.encoding)
          (opt "round" Round.encoding)
          (req "state" nonce_state_encoding))

let legacy_encoding =
  let open Data_encoding in
  def "legacy_seed_nonce"
  @@ conv
       (fun m ->
         Block_hash.Map.fold (fun hash nonce acc -> (hash, nonce) :: acc) m [])
       (fun l ->
         List.fold_left
           (fun map (hash, nonce) -> Block_hash.Map.add hash nonce map)
           Block_hash.Map.empty
           l)
  @@ list (obj2 (req "block" Block_hash.encoding) (req "nonce" Nonce.encoding))

let encoding =
  let open Data_encoding in
  def "seed_nonce"
  @@ conv
       (fun m ->
         Nonce_hash.Map.fold
           (fun _hash nonce_data acc -> nonce_data :: acc)
           m
           [])
       (fun l ->
         List.fold_left
           (fun map data -> Nonce_hash.Map.add data.nonce_hash data map)
           Nonce_hash.Map.empty
           l)
  @@ list nonce_data_encoding

let load (wallet : #Client_context.wallet)
    ~(stateful_location : [`Stateful_nonce] Baking_files.location) =
  let location = Baking_files.filename stateful_location in
  wallet#load location ~default:empty encoding

let save (wallet : #Client_context.wallet)
    ~(legacy_location : [`Legacy_nonce] Baking_files.location)
    ~(stateful_location : [`Stateful_nonce] Baking_files.location) nonces =
  let open Lwt_result_syntax in
  let legacy_location = Baking_files.filename legacy_location in
  let stateful_location = Baking_files.filename stateful_location in
  (* We always write in both files *)
  let* () = wallet#write stateful_location nonces encoding in
  let legacy_nonces =
    (* Fix: This will overwrite legacy file and any nonces that could not be ported. *)
    Nonce_hash.Map.fold
      (fun _ {nonce; block_hash; _} legacy_nonces ->
        Block_hash.Map.add block_hash nonce legacy_nonces)
      nonces
      legacy_empty
  in
  wallet#write legacy_location legacy_nonces legacy_encoding

let add nonces nonce = Nonce_hash.Map.add nonce.nonce_hash nonce nonces

let remove nonces hash = Nonce_hash.Map.remove hash nonces

let remove_all nonces nonces_to_remove =
  Nonce_hash.Map.fold
    (fun hash _ acc -> remove acc hash)
    nonces_to_remove
    nonces

let is_outdated constants committed_cycle current_cycle =
  let {Constants.parametric = {consensus_rights_delay; _}; _} = constants in
  Int32.sub (Cycle.to_int32 current_cycle) (Cycle.to_int32 committed_cycle)
  > Int32.of_int consensus_rights_delay

(** [try_migrate_legacy_nonces] makes a best effort to migrate nonces in the legacy format
    to new format. Legacy nonces that cannot be migrated are dropped. No updates to the legacy
    file are made *)
let try_migrate_legacy_nonces state =
  let {cctxt; chain; legacy_location; stateful_location; constants; _} =
    state
  in
  let migrate () =
    let open Lwt_result_syntax in
    let legacy_location = Baking_files.filename legacy_location in
    let* legacy_nonces =
      cctxt#load legacy_location ~default:legacy_empty legacy_encoding
    in
    let new_location = Baking_files.filename stateful_location in
    let* nonces = cctxt#load new_location ~default:empty encoding in
    let* {cycle = current_cycle; _} =
      Plugin.RPC.current_level cctxt (chain, `Head 0)
    in
    let*! nonces, failed_migration =
      Block_hash.Map.fold_s
        (fun block_hash nonce (existing_nonces, failed_migration) ->
          let*! updated_nonces =
            if Nonce_hash.Map.mem (Nonce.hash nonce) existing_nonces then
              (* Current nonce already exists in the new format, no migration needed. *)
              return existing_nonces
            else
              let* {cycle = nonce_cycle; level = nonce_level; _} =
                Plugin.RPC.current_level cctxt (chain, `Hash (block_hash, 0))
              in
              match is_outdated constants nonce_cycle current_cycle with
              | true ->
                  let*! () = Events.(emit outdated_nonce block_hash) in
                  return existing_nonces
              | false -> (
                  let* nonce_info =
                    Alpha_services.Nonce.get cctxt (chain, `Head 0) nonce_level
                  in
                  match nonce_info with
                  | Missing nonce_hash when Nonce.check_hash nonce nonce_hash ->
                      (* If a nonce is found locally but revelation not in context,
                         assume that it is [Committed] and not already [Revealed].
                         In the case that it is already in mempool, the mempool
                         will reject the operation. *)
                      let data =
                        {
                          nonce;
                          nonce_hash = Nonce.hash nonce;
                          block_hash;
                          cycle = nonce_cycle;
                          level = nonce_level;
                          round = None;
                          nonce_state = Committed;
                        }
                      in
                      return
                        (Nonce_hash.Map.add
                           data.nonce_hash
                           data
                           existing_nonces)
                  | Missing _nonce_hash ->
                      let*! () = Events.(emit unexpected_nonce block_hash) in
                      return existing_nonces
                  | Revealed _nonce_hash ->
                      let*! () = Events.(emit revealed_nonce block_hash) in
                      return existing_nonces
                  | Forgotten -> return existing_nonces)
          in
          Lwt.return
            (match updated_nonces with
            | Ok updated_nonces -> (updated_nonces, failed_migration)
            | Error _ -> (existing_nonces, block_hash :: failed_migration)))
        legacy_nonces
        (nonces, [])
    in
    let+ () =
      cctxt#write (Baking_files.filename stateful_location) nonces encoding
    in
    failed_migration
  in
  let open Lwt_syntax in
  let* res = migrate () in
  match res with
  | Ok [] -> Events.(emit success_migrate_nonces ())
  | Ok failed_migration ->
      Events.(emit ignore_failed_nonce_migration failed_migration)
  | Error _ -> Lwt.return_unit

(** [get_outdated_nonces state nonces currnet_cycle] returns an (orphans, outdated) 
    pair of lists of nonces (paired with their block hashes); "orphans" are nonces
    for which we could not retrieve the block level, which can happen in case
    of a snapshot import or of a block reorganisation; "outdated" nonces
    appear in cycles which are [consensus_rights_delay] older than the 
    [current_cycle]. *)
let get_outdated_nonces state nonces current_cycle =
  Nonce_hash.Map.fold
    (fun _hash nonce acc ->
      let orphans, outdated = acc in
      if is_outdated state.constants nonce.cycle current_cycle then
        (orphans, add outdated nonce)
      else (orphans, outdated))
    nonces
    (empty, empty)

(** [filter_outdated_nonces state nonces current_cyce] computes the pair of "orphaned" 
    and "outdated" nonces; emits a warning in case our map contains too many "orphaned"
    nonces, and removes all the oudated nonces. *)
let filter_outdated_nonces state nonces current_cycle =
  let open Lwt_result_syntax in
  let _orphans, outdated_nonces =
    get_outdated_nonces state nonces current_cycle
  in
  return (remove_all nonces outdated_nonces)

(** [get_unrevealed_nonces state nonces current_cycle] retrieves all the nonces which 
    have been computed for blocks from the previous cycle (w.r.t. [current_cycle]), 
    which have not been yet revealed. *)
let get_unrevealed_nonces {cctxt; chain; _} nonces current_cycle =
  let open Lwt_result_syntax in
  match Cycle.pred current_cycle with
  | None ->
      (* This will be [None] iff [current_cycle = 0] which only
         occurs during genesis. *)
      return_nil
  | Some previous_cycle ->
      Nonce_hash.Map.fold_es
        (fun _hash {nonce; cycle; level; block_hash; _} acc ->
          match cycle with
          | cycle when Cycle.(cycle = previous_cycle) -> (
              let* nonce_info =
                Alpha_services.Nonce.get cctxt (chain, `Head 0) level
              in
              match nonce_info with
              | Missing nonce_hash when Nonce.check_hash nonce nonce_hash ->
                  let*! () =
                    Events.(emit found_nonce_to_reveal (block_hash, level))
                  in
                  return ((level, nonce) :: acc)
              | Missing _nonce_hash ->
                  let*! () = Events.(emit incoherent_nonce level) in
                  return acc
              | Forgotten | Revealed _ -> return acc)
          | _cycle -> return acc)
        nonces
        []

(* Nonce creation *)

let generate_seed_nonce (nonce_config : Baking_configuration.nonce_config)
    (delegate : Baking_state.consensus_key) level =
  let open Lwt_result_syntax in
  let* nonce =
    match nonce_config with
    | Deterministic ->
        let data = Data_encoding.Binary.to_bytes_exn Raw_level.encoding level in
        let* nonce =
          Client_keys.deterministic_nonce delegate.secret_key_uri data
        in
        return (Data_encoding.Binary.of_bytes_exn Nonce.encoding nonce)
    | Random -> (
        match
          Nonce.of_bytes (Tezos_crypto.Rand.generate Constants.nonce_length)
        with
        | Error _errs -> assert false
        | Ok nonce -> return nonce)
  in
  return (Nonce.hash nonce, nonce)

let register_nonce (cctxt : #Protocol_client_context.full) ~chain_id block_hash
    nonce ~cycle ~level ~round =
  let open Lwt_result_syntax in
  let*! () = Events.(emit registering_nonce block_hash) in
  (* Register the nonce *)
  let legacy_location = Baking_files.resolve_location ~chain_id `Legacy_nonce in
  let stateful_location =
    Baking_files.resolve_location ~chain_id `Stateful_nonce
  in
  cctxt#with_lock @@ fun () ->
  let* nonces = load cctxt ~stateful_location in
  let nonces =
    add
      nonces
      {
        nonce;
        nonce_hash = Nonce.hash nonce;
        block_hash;
        cycle;
        level;
        round = Some round;
        nonce_state = Committed;
      }
  in
  let* () = save cctxt ~legacy_location ~stateful_location nonces in
  return_unit

(** [inject_seed_nonce_revelation cctxt ~chain ~block ~branch nonces] forges one 
    [Seed_nonce_revelation] operation per each nonce to be revealed, together with
    a signature and then injects these operations. *)
let inject_seed_nonce_revelation (cctxt : #Protocol_client_context.full) ~chain
    ~block ~branch nonces =
  let open Lwt_result_syntax in
  match nonces with
  | [] ->
      let*! () = Events.(emit nothing_to_reveal branch) in
      return_unit
  | _ ->
      List.iter_es
        (fun (level, nonce) ->
          let* bytes =
            Plugin.RPC.Forge.seed_nonce_revelation
              cctxt
              (chain, block)
              ~branch
              ~level
              ~nonce
              ()
          in
          let bytes = Signature.concat bytes Signature.zero in
          let* oph =
            Shell_services.Injection.operation ~async:true cctxt ~chain bytes
          in
          let*! () =
            Events.(
              emit
                revealing_nonce
                (Raw_level.to_int32 level, Chain_services.to_string chain, oph))
          in
          return_unit)
        nonces

(** [reveal_potential_nonces state new_proposal] updates the internal [state] 
    of the worker each time a proposal with a new predecessor is received; this means
    revealing the necessary nonces. *)
let reveal_potential_nonces state new_proposal =
  let open Lwt_result_syntax in
  let {cctxt; chain; legacy_location; stateful_location; last_predecessor; _} =
    state
  in
  let new_predecessor_hash = new_proposal.Baking_state.predecessor.hash in
  if
    Block_hash.(last_predecessor <> new_predecessor_hash)
    && not (Baking_state.is_first_block_in_protocol new_proposal)
  then (
    (* only try revealing nonces when the proposal's predecessor is a new one *)
    state.last_predecessor <- new_predecessor_hash ;
    let block = `Head 0 in
    let branch = new_predecessor_hash in
    (* improve concurrency *)
    cctxt#with_lock @@ fun () ->
    let*! nonces = load cctxt ~stateful_location in
    match nonces with
    | Error err ->
        let*! () = Events.(emit cannot_read_nonces err) in
        return_unit
    | Ok nonces -> (
        let* {cycle = current_cycle; _} =
          Plugin.RPC.current_level cctxt (chain, `Head 0)
        in
        let*! nonces_to_reveal =
          get_unrevealed_nonces state nonces current_cycle
        in
        match nonces_to_reveal with
        | Error err ->
            let*! () = Events.(emit cannot_retrieve_unrevealed_nonces err) in
            return_unit
        | Ok [] -> return_unit
        | Ok nonces_to_reveal -> (
            let*! result =
              inject_seed_nonce_revelation
                cctxt
                ~chain
                ~block
                ~branch
                nonces_to_reveal
            in
            match result with
            | Error err ->
                let*! () = Events.(emit cannot_inject_nonces err) in
                return_unit
            | Ok () ->
                (* If some nonces are to be revealed it means:
                   - We entered a new cycle and we can clear old nonces ;
                   - A revelation was not included yet in the cycle beginning.
                     So, it is safe to only filter outdated_nonces there *)
                let* live_nonces =
                  filter_outdated_nonces state nonces current_cycle
                in
                let* () =
                  save cctxt ~legacy_location ~stateful_location live_nonces
                in
                return_unit)))
  else return_unit

(* We suppose that the block stream is cloned by the caller *)
let start_revelation_worker cctxt config chain_id constants block_stream =
  let open Lwt_syntax in
  let legacy_location = Baking_files.resolve_location ~chain_id `Legacy_nonce in
  let stateful_location =
    Baking_files.resolve_location ~chain_id `Stateful_nonce
  in
  let chain = `Hash chain_id in
  let canceler = Lwt_canceler.create () in
  let should_shutdown = ref false in
  let state =
    {
      cctxt;
      chain;
      constants;
      config;
      legacy_location;
      stateful_location;
      last_predecessor = Block_hash.zero;
    }
  in
  let* () = try_migrate_legacy_nonces state in
  let rec worker_loop () =
    Lwt_canceler.on_cancel canceler (fun () ->
        should_shutdown := true ;
        Lwt.return_unit) ;
    let* new_proposal = Lwt_stream.get block_stream in
    match new_proposal with
    | None ->
        (* The head stream closed meaning that the connection
           with the node was interrupted: exit *)
        return_unit
    | Some new_proposal ->
        if !should_shutdown then return_unit
        else
          let* _ = reveal_potential_nonces state new_proposal in
          worker_loop ()
  in
  Lwt.dont_wait
    (fun () ->
      Lwt.finalize
        (fun () ->
          let* () = Events.(emit revelation_worker_started ()) in
          let* () = worker_loop () in
          (* never ending loop *) Lwt.return_unit)
        (fun () -> (* TODO *) Lwt.return_unit))
    (fun _exn -> ()) ;
  Lwt.return canceler
