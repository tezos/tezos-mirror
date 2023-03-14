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
module Events = Baking_events.Nonces

type state = {
  cctxt : Protocol_client_context.full;
  chain : Chain_services.chain;
  constants : Constants.t;
  config : Baking_configuration.nonce_config;
  nonces_location : [`Nonce] Baking_files.location;
  mutable last_predecessor : Block_hash.t;
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
  let base_dir = wallet#get_base_dir in
  let current_file =
    Filename.Infix.((base_dir // Baking_files.filename location) ^ "s")
  in
  Lwt_unix.file_exists current_file >>= function
  | true ->
      (* Migration already occured *)
      Lwt.return_unit
  | false -> (
      let legacy_file = Filename.Infix.(base_dir // "nonces") in
      Lwt_unix.file_exists legacy_file >>= function
      | false ->
          (* Do nothing *)
          Lwt.return_unit
      | true -> Lwt_utils_unix.copy_file ~src:legacy_file ~dst:current_file)

let load (wallet : #Client_context.wallet) location =
  wallet#load (Baking_files.filename location) ~default:empty encoding

let save (wallet : #Client_context.wallet) location nonces =
  wallet#write (Baking_files.filename location) nonces encoding

let mem nonces hash = Block_hash.Map.mem hash nonces

let find_opt nonces hash = Block_hash.Map.find hash nonces

let add nonces hash nonce = Block_hash.Map.add hash nonce nonces

let remove nonces hash = Block_hash.Map.remove hash nonces

let remove_all nonces nonces_to_remove =
  Block_hash.Map.fold
    (fun hash _ acc -> remove acc hash)
    nonces_to_remove
    nonces

let get_block_level_opt cctxt ~chain ~block =
  Shell_services.Blocks.Header.shell_header cctxt ~chain ~block () >>= function
  | Ok {level; _} -> Lwt.return_some level
  | Error errs ->
      Events.(
        emit
          cant_retrieve_block_header_for_nonce
          (Block_services.to_string block, errs))
      >>= fun () -> Lwt.return_none

let get_outdated_nonces {cctxt; constants; chain; _} nonces =
  let {Constants.parametric = {blocks_per_cycle; preserved_cycles; _}; _} =
    constants
  in
  get_block_level_opt cctxt ~chain ~block:(`Head 0) >>= function
  | None ->
      Events.(emit cannot_fetch_chain_head_level ()) >>= fun () ->
      return (empty, empty)
  | Some current_level ->
      let current_cycle = Int32.(div current_level blocks_per_cycle) in
      let is_older_than_preserved_cycles block_level =
        let block_cycle = Int32.(div block_level blocks_per_cycle) in
        Int32.sub current_cycle block_cycle > Int32.of_int preserved_cycles
      in
      Block_hash.Map.fold
        (fun hash nonce acc ->
          acc >>=? fun (orphans, outdated) ->
          get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0)) >>= function
          | Some level ->
              if is_older_than_preserved_cycles level then
                return (orphans, add outdated hash nonce)
              else acc
          | None -> return (add orphans hash nonce, outdated))
        nonces
        (return (empty, empty))

let filter_outdated_nonces state nonces =
  get_outdated_nonces state nonces >>=? fun (orphans, outdated_nonces) ->
  when_
    (Block_hash.Map.cardinal orphans >= 50)
    (fun () ->
      Events.(
        emit too_many_nonces (Baking_files.filename state.nonces_location ^ "s"))
      >>= fun () -> return_unit)
  >>=? fun () -> return (remove_all nonces outdated_nonces)

let blocks_from_current_cycle {cctxt; chain; _} block ?(offset = 0l) () =
  Plugin.RPC.levels_in_current_cycle cctxt ~offset (chain, block) >>= function
  | Error (Tezos_rpc.Context.Not_found _ :: _) -> return_nil
  | Error _ as err -> Lwt.return err
  | Ok (first, last) -> (
      Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
      Shell_services.Blocks.Header.shell_header cctxt ~chain ~block ()
      >>=? fun {level; _} ->
      (* FIXME: crappy algorithm, change this *)
      (* Compute how many blocks below current level we should ask for *)
      let length = Int32.to_int (Int32.sub level (Raw_level.to_int32 first)) in
      Shell_services.Blocks.list cctxt ~chain ~heads:[hash] ~length ()
      (* Looks like this function call retrieves a list of blocks ordered from
         latest to earliest - decreasing order of insertion in the chain *)
      >>=?
      function
      | [blocks] ->
          if Int32.equal level (Raw_level.to_int32 last) then
            (* We have just retrieved a block list of the right size starting at
               first until last *)
            return blocks
          else
            (* Remove all the latest blocks from last up to length*)
            List.drop_n
              (length - Int32.to_int (Raw_level.diff last first))
              blocks
            |> return
      | l ->
          failwith
            "Baking_nonces.blocks_from_current_cycle: unexpected block list of \
             size %d (expected 1)"
            (List.length l))

let get_unrevealed_nonces ({cctxt; chain; _} as state) nonces =
  blocks_from_current_cycle state (`Head 0) ~offset:(-1l) () >>=? fun blocks ->
  List.filter_map_es
    (fun hash ->
      match find_opt nonces hash with
      | None -> return_none
      | Some nonce -> (
          get_block_level_opt cctxt ~chain ~block:(`Hash (hash, 0)) >>= function
          | Some level -> (
              Lwt.return (Environment.wrap_tzresult (Raw_level.of_int32 level))
              >>=? fun level ->
              Alpha_services.Nonce.get cctxt (chain, `Head 0) level
              >>=? function
              | Missing nonce_hash when Nonce.check_hash nonce nonce_hash ->
                  Events.(
                    emit found_nonce_to_reveal (hash, Raw_level.to_int32 level))
                  >>= fun () -> return_some (level, nonce)
              | Missing _nonce_hash ->
                  Events.(emit incoherent_nonce (Raw_level.to_int32 level))
                  >>= fun () -> return_none
              | Forgotten -> return_none
              | Revealed _ -> return_none)
          | None -> return_none))
    blocks

(* Nonce creation *)

let generate_seed_nonce (nonce_config : Baking_configuration.nonce_config)
    (delegate : Baking_state.consensus_key) level =
  (match nonce_config with
  | Deterministic ->
      let data = Data_encoding.Binary.to_bytes_exn Raw_level.encoding level in
      Client_keys.deterministic_nonce delegate.secret_key_uri data
      >>=? fun nonce ->
      return (Data_encoding.Binary.of_bytes_exn Nonce.encoding nonce)
  | Random -> (
      match
        Nonce.of_bytes (Tezos_crypto.Rand.generate Constants.nonce_length)
      with
      | Error _errs -> assert false
      | Ok nonce -> return nonce))
  >>=? fun nonce -> return (Nonce.hash nonce, nonce)

let register_nonce (cctxt : #Protocol_client_context.full) ~chain_id block_hash
    nonce =
  Events.(emit registering_nonce block_hash) >>= fun () ->
  (* Register the nonce *)
  let nonces_location = Baking_files.resolve_location ~chain_id `Nonce in
  cctxt#with_lock @@ fun () ->
  load cctxt nonces_location >>=? fun nonces ->
  let nonces = add nonces block_hash nonce in
  save cctxt nonces_location nonces >>=? fun () -> return_unit

let inject_seed_nonce_revelation (cctxt : #Protocol_client_context.full) ~chain
    ~block ~branch nonces =
  match nonces with
  | [] -> Events.(emit nothing_to_reveal branch) >>= fun () -> return_unit
  | _ ->
      List.iter_es
        (fun (level, nonce) ->
          Plugin.RPC.Forge.seed_nonce_revelation
            cctxt
            (chain, block)
            ~branch
            ~level
            ~nonce
            ()
          >>=? fun bytes ->
          let bytes = Signature.concat bytes Signature.zero in
          Shell_services.Injection.operation ~async:true cctxt ~chain bytes
          >>=? fun oph ->
          Events.(
            emit
              revealing_nonce
              (Raw_level.to_int32 level, Chain_services.to_string chain, oph))
          >>= fun () -> return_unit)
        nonces

(** [reveal_potential_nonces] reveal registered nonces *)
let reveal_potential_nonces state new_proposal =
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
    load cctxt nonces_location >>= function
    | Error err ->
        Events.(emit cannot_read_nonces err) >>= fun () -> return_unit
    | Ok nonces -> (
        get_unrevealed_nonces state nonces >>= function
        | Error err ->
            Events.(emit cannot_retrieve_unrevealed_nonces err) >>= fun () ->
            return_unit
        | Ok [] -> return_unit
        | Ok nonces_to_reveal -> (
            inject_seed_nonce_revelation
              cctxt
              ~chain
              ~block
              ~branch
              nonces_to_reveal
            >>= function
            | Error err ->
                Events.(emit cannot_inject_nonces err) >>= fun () -> return_unit
            | Ok () ->
                (* If some nonces are to be revealed it means:
                   - We entered a new cycle and we can clear old nonces ;
                   - A revelation was not included yet in the cycle beginning.
                     So, it is safe to only filter outdated_nonces there *)
                filter_outdated_nonces state nonces >>=? fun live_nonces ->
                save cctxt nonces_location live_nonces >>=? fun () ->
                return_unit)))
  else return_unit

(* We suppose that the block stream is cloned by the caller *)
let start_revelation_worker cctxt config chain_id constants block_stream =
  let nonces_location = Baking_files.resolve_location ~chain_id `Nonce in
  may_migrate cctxt nonces_location >>= fun () ->
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
    Lwt_stream.get block_stream >>= function
    | None ->
        (* The head stream closed meaning that the connection
           with the node was interrupted: exit *)
        return_unit
    | Some new_proposal ->
        if !should_shutdown then return_unit
        else
          reveal_potential_nonces state new_proposal >>=? fun () ->
          worker_loop ()
  in
  Lwt.dont_wait
    (fun () ->
      Lwt.finalize
        (fun () ->
          Events.(emit revelation_worker_started ()) >>= fun () ->
          worker_loop () >>= fun _ -> (* never ending loop *) Lwt.return_unit)
        (fun () -> (* TODO *) Lwt.return_unit))
    (fun _exn -> ()) ;
  Lwt.return canceler
