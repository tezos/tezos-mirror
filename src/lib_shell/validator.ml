(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type t = {
  state : Store.t;
  db : Distributed_db.t;
  block_validator : Block_validator.t;
  chain_validator_limits : Chain_validator.limits;
  peer_validator_limits : Peer_validator.limits;
  block_validator_limits : Block_validator.limits;
  prevalidator_limits : Prevalidator.limits;
  start_testchain : bool;
  valid_block_input : Store.Block.t Lwt_watcher.input;
  chains_input : (Chain_id.t * bool) Lwt_watcher.input;
  active_chains : Chain_validator.t Chain_id.Table.t;
}

let create state db peer_validator_limits block_validator_limits
    block_validator_kind prevalidator_limits chain_validator_limits
    ~start_testchain =
  let open Lwt_result_syntax in
  let* block_validator =
    Block_validator.create
      block_validator_limits
      db
      block_validator_kind
      ~start_testchain
  in
  let valid_block_input = Lwt_watcher.create_input () in
  let chains_input = Lwt_watcher.create_input () in
  return
    {
      state;
      db;
      start_testchain;
      block_validator;
      block_validator_limits;
      prevalidator_limits;
      peer_validator_limits;
      chain_validator_limits;
      valid_block_input;
      chains_input;
      active_chains = Chain_id.Table.create 7;
    }

let activate v ~start_prevalidator ~validator_process chain_store =
  let open Lwt_syntax in
  let chain_id = Store.Chain.chain_id chain_store in
  let* () = Validator_event.(emit activate_chain) chain_id in
  match Chain_id.Table.find v.active_chains chain_id with
  | Some chain -> return_ok chain
  | None ->
      Chain_validator.create
        ~start_prevalidator
        ~start_testchain:v.start_testchain
        ~active_chains:v.active_chains
        ~block_validator_process:validator_process
        v.peer_validator_limits
        v.prevalidator_limits
        v.block_validator
        v.valid_block_input
        v.chains_input
        v.db
        chain_store
        v.chain_validator_limits

let get {active_chains; _} chain_id =
  let open Result_syntax in
  match Chain_id.Table.find active_chains chain_id with
  | Some nv -> return nv
  | None -> tzfail (Validation_errors.Inactive_chain chain_id)

let get_active_chains {active_chains; _} =
  let l = Chain_id.Table.fold (fun c _ acc -> c :: acc) active_chains [] in
  List.rev l

let read_block store h =
  let open Lwt_option_syntax in
  let*! chain_stores = Store.all_chain_stores store in
  List.find_map_s
    (fun chain_store ->
      let* b = Store.Block.read_block_opt chain_store h in
      let id = Store.Chain.chain_id chain_store in
      return (id, b))
    chain_stores

let read_block_header db h =
  let open Lwt_option_syntax in
  let* (chain_id, block) = read_block (Distributed_db.store db) h in
  let header = Store.Block.header block in
  return (chain_id, header)

let validate_block v ?(force = false) ?chain_id bytes operations =
  let open Lwt_result_syntax in
  let hash = Block_hash.hash_bytes [bytes] in
  match Block_header.of_bytes bytes with
  | None -> failwith "Cannot parse block header."
  | Some block ->
      if not (Clock_drift.is_not_too_far_in_the_future block.shell.timestamp)
      then failwith "Block in the future."
      else
        let* nv =
          match chain_id with
          | None -> (
              let*! o = read_block_header v.db block.shell.predecessor in
              match o with
              | None ->
                  failwith
                    "Unknown predecessor (%a), cannot inject the block."
                    Block_hash.pp_short
                    block.shell.predecessor
              | Some (chain_id, _bh) -> Lwt.return (get v chain_id))
          | Some chain_id -> (
              let* nv = Lwt.return (get v chain_id) in
              if force then return nv
              else
                let*! b =
                  Distributed_db.Block_header.known
                    (Chain_validator.chain_db nv)
                    block.shell.predecessor
                in
                match b with
                | true -> return nv
                | false ->
                    failwith
                      "Unknown predecessor (%a), cannot inject the block."
                      Block_hash.pp_short
                      block.shell.predecessor)
        in
        let validation =
          Chain_validator.validate_block nv ~force hash block operations
        in
        return (hash, validation)

let shutdown {active_chains; block_validator; _} =
  let open Lwt_syntax in
  let chain_validator_jobs =
    List.of_seq
    @@ Seq.map
         (fun (id, nv) ->
           let* () = Validator_event.(emit shutdown_chain_validator) id in
           Chain_validator.shutdown nv)
         (Chain_id.Table.to_seq active_chains)
  in
  (* Shutdown the chain_validator (peer_validators, prevalidator,
     etc.) before the block_validator *)
  let* () = Lwt.join chain_validator_jobs in
  let* () = Validator_event.(emit shutdown_block_validator) () in
  Block_validator.shutdown block_validator

let watcher {valid_block_input; _} = Lwt_watcher.create_stream valid_block_input

let chains_watcher {chains_input; _} = Lwt_watcher.create_stream chains_input

let inject_operation v ?chain_id ~force op =
  let open Lwt_result_syntax in
  let inject_operation_on nv ~handle_missing_prevalidator =
    match Chain_validator.prevalidator nv with
    | Some pv -> Prevalidator.inject_operation pv ~force op
    | None -> handle_missing_prevalidator
  in
  let handle_missing_prevalidator =
    failwith "Prevalidator is not running, cannot inject the operation."
  in
  match chain_id with
  | None -> (
      let*! o = read_block_header v.db op.Operation.shell.branch in
      match o with
      | None ->
          if force then
            Chain_id.Table.iter_es
              (fun _chain_id chain ->
                inject_operation_on
                  chain
                  ~handle_missing_prevalidator:return_unit)
              v.active_chains
          else
            failwith
              "Unknown branch (%a), cannot inject the operation."
              Block_hash.pp_short
              op.shell.branch
      | Some (chain_id, _bh) ->
          let*? nv = get v chain_id in
          inject_operation_on nv ~handle_missing_prevalidator)
  | Some chain_id ->
      let*? nv = get v chain_id in
      inject_operation_on nv ~handle_missing_prevalidator

let distributed_db {db; _} = db
