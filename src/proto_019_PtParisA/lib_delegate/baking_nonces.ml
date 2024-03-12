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
  nonces_location : [`Nonce] Baking_files.location;
  mutable last_predecessor : Block_hash.t;
}

(* This is a feature flag which decides whether the existing "nonces" file is
   augmented with the new cycle field (and implicitly with the new encoding).
   For compatibility purposes, we start this "upgrade" by first saving the
   "upgraded" form of information in a different file. This happens as a first stage,
   while this flag remains set to false. Once it is set to true, the new file
   will no longer be needed, as the more detailed information will be directly
   stored in the legacy nonces file. *)
let override_legacy_nonces_flag = false

type t = state

type nonce_data = {nonce : Nonce.t; cycle : Cycle.t option}

type nonces = nonce_data Block_hash.Map.t

let empty = Block_hash.Map.empty

let nonce_data_encoding =
  let open Data_encoding in
  def "nonce_data"
  @@ conv
       (fun {nonce; cycle} -> (nonce, cycle))
       (fun (nonce, cycle) -> {nonce; cycle})
       (obj2 (req "nonce" Nonce.encoding) (req "cycle" (option Cycle.encoding)))

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
         Block_hash.Map.fold
           (fun hash nonce_data acc -> (hash, nonce_data) :: acc)
           m
           [])
       (fun l ->
         List.fold_left
           (fun map (hash, nonce) -> Block_hash.Map.add hash nonce map)
           Block_hash.Map.empty
           l)
  @@ list
       (obj2
          (req "block" Block_hash.encoding)
          (req "nonce_data" nonce_data_encoding))

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

let get_detailed_nonces_location nonces_location = "detailed_" ^ nonces_location

let load (wallet : #Client_context.wallet) location =
  let open Lwt_result_syntax in
  let nonces_location = Baking_files.filename location in
  try
    (* If the flag is set, then we no longer save into a separate file, so we can
       try to load the new data format from the legacy file *)
    if override_legacy_nonces_flag then
      wallet#load nonces_location ~default:empty encoding
    else
      (* Otherwise, we must load from the detailed file *)
      wallet#load
        (get_detailed_nonces_location nonces_location)
        ~default:empty
        encoding
  with _exn ->
    (* If we fail to load the more detailed format, we try with the legacy format,
       which can only be found in the legacy file *)
    let* legacy_nonces =
      wallet#load nonces_location ~default:empty legacy_encoding
    in
    return
    @@ Block_hash.Map.map (fun nonce -> {nonce; cycle = None}) legacy_nonces

let save (wallet : #Client_context.wallet) location nonces =
  let open Lwt_result_syntax in
  let nonces_location = Baking_files.filename location in
  (* If the flag is set, we can overwrite the nonces data in the legacy file *)
  if override_legacy_nonces_flag then
    wallet#write nonces_location nonces encoding
    (* Otherwise, we put the detailed data in a specific file, and the legacy data in
       the legacy file. *)
  else
    let* () =
      wallet#write
        (get_detailed_nonces_location nonces_location)
        nonces
        encoding
    in
    let legacy_nonces = Block_hash.Map.map (fun {nonce; _} -> nonce) nonces in
    wallet#write nonces_location legacy_nonces legacy_encoding

let add nonces hash nonce = Block_hash.Map.add hash nonce nonces

let remove nonces hash = Block_hash.Map.remove hash nonces

let remove_all nonces nonces_to_remove =
  Block_hash.Map.fold
    (fun hash _ acc -> remove acc hash)
    nonces_to_remove
    nonces

let may_create_detailed_nonces_file (wallet : #Client_context.wallet) location =
  let open Lwt_result_syntax in
  let nonces_location = Baking_files.filename location in
  if not override_legacy_nonces_flag then
    let* legacy_nonces =
      wallet#load nonces_location ~default:empty legacy_encoding
    in
    let detailed_nonces =
      Block_hash.Map.map (fun nonce -> {nonce; cycle = None}) legacy_nonces
    in
    wallet#write
      (get_detailed_nonces_location nonces_location)
      detailed_nonces
      encoding
  else return_unit

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

(** [find_missing_cycle cctxt chain hash cycle] makes an RPC call to retrieve the cycle of the 
     current block [hash] only when this information is missing. *)
let find_missing_cycle (cctxt : #Protocol_client_context.full) chain hash cycle
    =
  let open Lwt_result_syntax in
  match cycle with
  | Some _ -> return cycle
  | None ->
      let* {cycle; _} =
        Plugin.RPC.current_level cctxt (chain, `Hash (hash, 0))
      in
      return (Some cycle)

(** [fill_missing_fields cctxt chain nonces_location nonces] iterates through the nonces map and
    fills the [None] fields with the necessary information, then it synchronises the information
    with the [nonces_location] file. *)
let fill_missing_fields (cctxt : #Protocol_client_context.full) chain
    nonces_location nonces =
  let open Lwt_result_syntax in
  let* filled_nonces =
    Block_hash.Map.fold_es
      (fun hash {nonce; cycle} acc ->
        let+ cycle = find_missing_cycle cctxt chain hash cycle in
        Block_hash.Map.add hash {nonce; cycle} acc)
      nonces
      empty
  in
  let* () = save cctxt nonces_location filled_nonces in
  return filled_nonces

(** [get_unrevealed_nonces state nonces] retrieves all the nonces which have been
    computed for blocks from the previous cycle, which have not been yet revealed. *)
let get_unrevealed_nonces {cctxt; chain; nonces_location; _} nonces =
  let open Lwt_result_syntax in
  let* {cycle = current_cycle; _} =
    Plugin.RPC.current_level cctxt (chain, `Head 0)
  in
  match Cycle.pred current_cycle with
  | None ->
      (* This will be [None] iff [current_cycle = 0] which only
         occurs during genesis. *)
      return_nil
  | Some previous_cycle ->
      let* nonces = fill_missing_fields cctxt chain nonces_location nonces in
      Block_hash.Map.fold_es
        (fun hash {nonce; cycle} acc ->
          match cycle with
          | Some cycle when Cycle.(cycle = previous_cycle) -> (
              let*! level_opt =
                get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0))
              in
              match level_opt with
              | Some int_level -> (
                  let*? level =
                    Environment.wrap_tzresult (Raw_level.of_int32 int_level)
                  in
                  let* nonce_info =
                    Alpha_services.Nonce.get cctxt (chain, `Head 0) level
                  in
                  match nonce_info with
                  | Missing nonce_hash when Nonce.check_hash nonce nonce_hash ->
                      let*! () =
                        Events.(emit found_nonce_to_reveal (hash, int_level))
                      in
                      return ((level, nonce) :: acc)
                  | Missing _nonce_hash ->
                      let*! () = Events.(emit incoherent_nonce int_level) in
                      return acc
                  | Forgotten | Revealed _ -> return acc)
              | None -> raise Not_found)
          | Some _cycle -> return acc
          | None -> assert false)
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
    nonce ~cycle =
  let open Lwt_result_syntax in
  let*! () = Events.(emit registering_nonce block_hash) in
  (* Register the nonce *)
  let nonces_location = Baking_files.resolve_location ~chain_id `Nonce in
  cctxt#with_lock @@ fun () ->
  let* nonces = load cctxt nonces_location in
  let nonces = add nonces block_hash {nonce; cycle = Some cycle} in
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
  let*! _ = may_create_detailed_nonces_file cctxt nonces_location in
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
