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
open Baking_cache
module Events = Baking_events.Nonces

type state = {
  cctxt : Protocol_client_context.full;
  chain : Chain_services.chain;
  constants : Constants.t;
  config : Baking_configuration.nonce_config;
  nonces_location : [`Nonce] Baking_files.location;
  mutable last_predecessor : Block_hash.t;
  cycle_cache : Block_hash.t list Cycle_cache.t;
}

type t = state

type nonces = Nonce.t Block_hash.Map.t

let empty = Block_hash.Map.empty

let encoding =
  let open Data_encoding in
  def "seed_nonce"
  @@ conv
       (fun m ->
         Block_hash.Map.fold (fun hash nonce acc -> (hash, nonce) :: acc) m [])
       (fun l ->
         List.fold_left
           (fun map (hash, nonce) -> Block_hash.Map.add hash nonce map)
           Block_hash.Map.empty
           l)
  @@ list (obj2 (req "block" Block_hash.encoding) (req "nonce" Nonce.encoding))

let may_migrate (wallet : Protocol_client_context.full) location =
  let open Lwt_syntax in
  let base_dir = wallet#get_base_dir in
  let current_file =
    Filename.Infix.((base_dir // Baking_files.filename location) ^ "s")
  in
  let* exists = Lwt_unix.file_exists current_file in
  match exists with
  | true ->
      (* Migration already occured *)
      return_unit
  | false -> (
      let legacy_file = Filename.Infix.(base_dir // "nonces") in
      let* exists = Lwt_unix.file_exists legacy_file in
      match exists with
      | false ->
          (* Do nothing *)
          return_unit
      | true -> Lwt_utils_unix.copy_file ~src:legacy_file ~dst:current_file)

let load (wallet : #Client_context.wallet) location =
  wallet#load (Baking_files.filename location) ~default:empty encoding

let save (wallet : #Client_context.wallet) location nonces =
  wallet#write (Baking_files.filename location) nonces encoding

let find_opt nonces hash = Block_hash.Map.find hash nonces

let add nonces hash nonce = Block_hash.Map.add hash nonce nonces

let remove nonces hash = Block_hash.Map.remove hash nonces

let remove_all nonces nonces_to_remove =
  Block_hash.Map.fold
    (fun hash _ acc -> remove acc hash)
    nonces_to_remove
    nonces

(** [get_block_level_opt cctxt ~chain ~block] makes an RPC call to 
    retrieve the block level associated to the [~block], given the 
    client context [cctxt] and chain [~chain]. *)
let get_block_level_opt cctxt ~chain ~block =
  let open Lwt_syntax in
  let* result =
    Shell_services.Blocks.Header.shell_header cctxt ~chain ~block ()
  in
  match result with
  | Ok {level; _} -> return_some level
  | Error errs ->
      let* () =
        Events.(
          emit
            cant_retrieve_block_header_for_nonce
            (Block_services.to_string block, errs))
      in
      return_none

(** [get_outdated_nonces state nonces] returns an (orphans, outdated) pair 
    of lists of nonces (paired with their block hashes); "orphans" are nonces
    for which we could not retrieve the block level, which can happen in case
    of a snapshot import or of a block reorganisation; "outdated" nonces
    appear in cycles which are [consensus_rights_delay] older than the 
    current cycle.  *)
let get_outdated_nonces {cctxt; constants; chain; _} nonces =
  let open Lwt_result_syntax in
  let {Constants.parametric = {blocks_per_cycle; consensus_rights_delay; _}; _}
      =
    constants
  in
  let*! current_level = get_block_level_opt cctxt ~chain ~block:(`Head 0) in
  match current_level with
  | None ->
      let*! () = Events.(emit cannot_fetch_chain_head_level ()) in
      return (empty, empty)
  | Some current_level ->
      let current_cycle = Int32.(div current_level blocks_per_cycle) in
      let is_older_than_consensus_rights_delay block_level =
        let block_cycle = Int32.(div block_level blocks_per_cycle) in
        Int32.sub current_cycle block_cycle
        > Int32.of_int consensus_rights_delay
      in
      Block_hash.Map.fold
        (fun hash nonce acc ->
          let* orphans, outdated = acc in
          let*! level =
            get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0))
          in
          match level with
          | Some level ->
              if is_older_than_consensus_rights_delay level then
                return (orphans, add outdated hash nonce)
              else acc
          | None -> return (add orphans hash nonce, outdated))
        nonces
        (return (empty, empty))

(** [filter_outdated_nonces state nonces] computes the pair of "orphaned" and
    "outdated" nonces; emits a warning in case our map contains too many "orphaned"
    nonces, and removes all the oudated nonces. *)
let filter_outdated_nonces state nonces =
  let open Lwt_result_syntax in
  let* orphans, outdated_nonces = get_outdated_nonces state nonces in
  let* () =
    when_
      (Block_hash.Map.cardinal orphans >= 50)
      (fun () ->
        let*! () =
          Events.(
            emit
              too_many_nonces
              (Baking_files.filename state.nonces_location ^ "s"))
        in
        return_unit)
  in
  return (remove_all nonces outdated_nonces)

(** [blocks_from_previous_cycle state] retrieves all the block hashes corresponding
    to all the blocks from the cycle right before the current one. *)
let blocks_from_previous_cycle {cctxt; chain; _} =
  let open Lwt_result_syntax in
  let current_head = `Head 0 in
  let*! cycle_levels =
    Plugin.RPC.levels_in_current_cycle cctxt ~offset:(-1l) (chain, current_head)
  in
  match cycle_levels with
  | Error (Tezos_rpc.Context.Not_found _ :: _) -> return_nil
  | Error _ as err -> Lwt.return err
  | Ok (first, last) -> (
      let first, last = (Raw_level.to_int32 first, Raw_level.to_int32 last) in
      let* last_level_hash =
        Shell_services.Blocks.hash cctxt ~chain ~block:(`Level last) ()
      in
      (* Compute how many blocks there are between the first and last level of the
         previous cycle *)
      let length = Int32.to_int (Int32.sub last first) in
      let* blocks_list =
        Shell_services.Blocks.list
          cctxt
          ~chain
          ~heads:[last_level_hash]
          ~length
          ()
        (* Looks like this function call retrieves a list of blocks ordered from
           latest to earliest - decreasing order of insertion in the chain *)
      in
      match blocks_list with
      | [blocks] -> return blocks
      | l ->
          failwith
            "Baking_nonces.blocks_from_current_cycle: unexpected block list of \
             size %d (expected 1)"
            (List.length l))

(** [cached_blocks_from_previous_cycle state] attempts to use a cache containing
    a list of blocks per cycle key; in case of a cache miss, the blocks are 
    computed and stored in a cache, in an LRU fashion. *)
let cached_blocks_from_previous_cycle ({cctxt; chain; cycle_cache; _} as state)
    =
  let open Lwt_result_syntax in
  let* {cycle = current_cycle; _} =
    Plugin.RPC.current_level cctxt (chain, `Head 0)
  in
  match Cycle.pred current_cycle with
  | None ->
      (* This will be [None] iff [current_cycle = 0] which only
         occurs during genesis. *)
      return_nil
  | Some cycle_key -> (
      match Cycle_cache.find_opt cycle_cache cycle_key with
      | Some blocks -> return blocks
      | None ->
          let* blocks = blocks_from_previous_cycle state in
          Cycle_cache.replace cycle_cache cycle_key blocks ;
          return blocks)

(** [get_unrevealed_nonces state nonces] retrieves all the nonces which have been
    computed for blocks from the previous cycle, which have not been yet revealed. *)
let get_unrevealed_nonces ({cctxt; chain; _} as state) nonces =
  let open Lwt_result_syntax in
  let* blocks = cached_blocks_from_previous_cycle state in
  List.filter_map_es
    (fun hash ->
      match find_opt nonces hash with
      | None -> return_none
      | Some nonce -> (
          let*! level =
            get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0))
          in
          match level with
          | Some level -> (
              let*? level =
                Environment.wrap_tzresult (Raw_level.of_int32 level)
              in
              let* nonce_info =
                Alpha_services.Nonce.get cctxt (chain, `Head 0) level
              in
              match nonce_info with
              | Missing nonce_hash when Nonce.check_hash nonce nonce_hash ->
                  let*! () =
                    Events.(
                      emit found_nonce_to_reveal (hash, Raw_level.to_int32 level))
                  in
                  return_some (level, nonce)
              | Missing _nonce_hash ->
                  let*! () =
                    Events.(emit incoherent_nonce (Raw_level.to_int32 level))
                  in
                  return_none
              | Forgotten -> return_none
              | Revealed _ -> return_none)
          | None -> return_none))
    blocks

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
    nonce =
  let open Lwt_result_syntax in
  let*! () = Events.(emit registering_nonce block_hash) in
  (* Register the nonce *)
  let nonces_location = Baking_files.resolve_location ~chain_id `Nonce in
  cctxt#with_lock @@ fun () ->
  let* nonces = load cctxt nonces_location in
  let nonces = add nonces block_hash nonce in
  let* () = save cctxt nonces_location nonces in
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

(** [reveal_potential_nonces state new_proposal] updates the internal [state] of 
    the worker each time a proposal with a new predecessor is received; this means
    revealing the necessary nonces. *)
let reveal_potential_nonces state new_proposal =
  let open Lwt_result_syntax in
  let {cctxt; chain; nonces_location; last_predecessor; _} = state in
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
    let*! nonces = load cctxt nonces_location in
    match nonces with
    | Error err ->
        let*! () = Events.(emit cannot_read_nonces err) in
        return_unit
    | Ok nonces -> (
        let*! nonces_to_reveal = get_unrevealed_nonces state nonces in
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
                let* live_nonces = filter_outdated_nonces state nonces in
                let* () = save cctxt nonces_location live_nonces in
                return_unit)))
  else return_unit

(* We suppose that the block stream is cloned by the caller *)
let start_revelation_worker cctxt config chain_id constants block_stream =
  let open Lwt_result_syntax in
  let nonces_location = Baking_files.resolve_location ~chain_id `Nonce in
  let*! () = may_migrate cctxt nonces_location in
  let chain = `Hash chain_id in
  let canceler = Lwt_canceler.create () in
  let should_shutdown = ref false in
  let state =
    {
      cctxt;
      chain;
      constants;
      config;
      nonces_location;
      last_predecessor = Block_hash.zero;
      cycle_cache = Cycle_cache.create 2;
    }
  in
  let rec worker_loop () =
    Lwt_canceler.on_cancel canceler (fun () ->
        should_shutdown := true ;
        Lwt.return_unit) ;
    let*! new_proposal = Lwt_stream.get block_stream in
    match new_proposal with
    | None ->
        (* The head stream closed meaning that the connection
           with the node was interrupted: exit *)
        return_unit
    | Some new_proposal ->
        if !should_shutdown then return_unit
        else
          let* () = reveal_potential_nonces state new_proposal in
          worker_loop ()
  in
  Lwt.dont_wait
    (fun () ->
      Lwt.finalize
        (fun () ->
          let*! () = Events.(emit revelation_worker_started ()) in
          let*! _ = worker_loop () in
          (* never ending loop *) Lwt.return_unit)
        (fun () -> (* TODO *) Lwt.return_unit))
    (fun _exn -> ()) ;
  Lwt.return canceler
