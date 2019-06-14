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

(* Delegated storage changed type of value from Contract_hash to
   Contract_repr. Move all 'delegated' data into a storage with
   the original type, then copy over into the new storage. *)
let migrate_delegated ctxt contract =
  let path = "contracts" :: (* module Contract *)
             "index" :: (* module Indexed_context *)
             Contract_repr.Index.to_path contract [
               "delegated" ; (* module Delegated *)
             ] in
  let path_tmp = "contracts" :: (* module Contract *)
                 "index" :: (* module Indexed_context *)
                 Contract_repr.Index.to_path contract [
                   "delegated_004" ; (* module Delegated *)
                 ] in
  Raw_context.dir_mem ctxt path >>= fun exists ->
  if exists then
    Raw_context.copy ctxt path path_tmp >>=? fun ctxt ->
    Raw_context.remove_rec ctxt path >>= fun ctxt ->
    Storage.Contract.Delegated_004.fold (ctxt, contract) ~init:(Ok ctxt) ~f:(fun delegated ctxt ->
        Lwt.return ctxt >>=? fun ctxt ->
        let originated = Contract_repr.originated_contract_004 delegated in
        Storage.Contract.Delegated.add (ctxt, contract) originated >>= fun ctxt ->
        return ctxt
      ) >>=? fun ctxt ->
    Raw_context.remove_rec ctxt path_tmp >>= fun ctxt ->
    return ctxt
  else
    return ctxt

let transform_script:
  (manager_pkh: Signature.Public_key_hash.t ->
   script_code: Script_repr.lazy_expr ->
   script_storage: Script_repr.lazy_expr ->
   (Script_repr.lazy_expr * Script_repr.lazy_expr) tzresult Lwt.t) ->
  Raw_context.t ->
  Contract_repr.t ->
  Script_repr.lazy_expr ->
  Raw_context.t tzresult Lwt.t =
  fun transformation ctxt contract code ->
  (* Get the manager of the originated contract *)
  Contract_storage.get_manager ctxt contract >>=? fun manager_pkh ->
  Storage.Contract.Storage.get ctxt contract >>=? fun (_ctxt, storage) ->
  transformation manager_pkh code storage >>=? fun (migrated_code, migrated_storage) ->
  (* Set the migrated script code for free *)
  Storage.Contract.Code.set_free ctxt contract migrated_code >>=? fun (ctxt, code_size_diff) ->
  (* Set the migrated script storage for free *)
  Storage.Contract.Storage.set_free ctxt contract migrated_storage >>=? fun (ctxt, storage_size_diff) ->
  Storage.Contract.Used_storage_space.get ctxt contract >>=? fun used_space ->
  let total_size = Z.(add (of_int code_size_diff) (add (of_int storage_size_diff) used_space)) in
  (* Free storage space for migrated contracts *)
  Storage.Contract.Used_storage_space.set ctxt contract total_size >>=? fun ctxt ->
  Storage.Contract.Paid_storage_space.get ctxt contract >>=? fun paid_space ->
  if Compare.Z.(paid_space < total_size) then
    Storage.Contract.Paid_storage_space.set ctxt contract total_size >>=? fun ctxt ->
    return ctxt
  else
    return ctxt

let manager_script_storage: Signature.Public_key_hash.t -> Script_repr.lazy_expr =
  fun manager_pkh ->
  let open Micheline in
  Script_repr.lazy_expr @@ strip_locations @@
  (* store in optimized binary representation - as unparsed with [Optimized]. *)
  let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding manager_pkh in
  Bytes (0, bytes)

(* Process an individual contract *)
let process_contract contract ctxt =
  let open Legacy_script_support_repr in
  match Contract_repr.is_originated contract with
  | None -> return ctxt (* Only process originated contracts *)
  | Some _ -> begin
      Storage.Contract.Spendable.mem ctxt contract >>= fun is_spendable ->
      Storage.Contract.Delegatable.mem ctxt contract >>= fun is_delegatable ->
      (* Try to get script code (ignore ctxt update to discard the initialization) *)
      Storage.Contract.Code.get_option ctxt contract >>=? fun (_ctxt, code) ->
      match code with
      | Some code ->
          (*
          | spendable | delegatable | template         |
          |-----------+-------------+------------------|
          | true      | true        | add_do           |
          | true      | false       | add_do           |
          | false     | true        | add_set_delegate |
          | false     | false       | nothing          |
          *)
          if is_spendable then
            transform_script add_do ctxt contract code
          else if is_delegatable then
            transform_script add_set_delegate ctxt contract code
          else
            return ctxt
      | None -> begin
          (* Check that the contract is spendable *)
          (* Get the manager of the originated contract *)
          Contract_storage.get_manager ctxt contract >>=? fun manager_pkh ->
          (* Initialize the script code for free *)
          Storage.Contract.Code.init_free ctxt contract manager_script_code >>=? fun (ctxt, code_size) ->
          let storage = manager_script_storage manager_pkh in
          (* Initialize the script storage for free *)
          Storage.Contract.Storage.init_free ctxt contract storage >>=? fun (ctxt, storage_size) ->
          let total_size = Z.(add (of_int code_size) (of_int storage_size)) in
          (* Free storage space for migrated contracts *)
          Storage.Contract.Paid_storage_space.init_set ctxt contract total_size >>= fun ctxt ->
          Storage.Contract.Used_storage_space.init_set ctxt contract total_size >>= fun ctxt ->
          return ctxt
        end
    end


let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block
    ~level ~timestamp ~fitness ctxt >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments >>=? fun ctxt ->
      Roll_storage.init ctxt >>=? fun ctxt ->
      Seed_storage.init ctxt >>=? fun ctxt ->
      Contract_storage.init ctxt >>=? fun ctxt ->
      Bootstrap_storage.init ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt >>=? fun ctxt ->
      Vote_storage.init ctxt >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0 >>=? fun ctxt ->
      Vote_storage.freeze_listings ctxt >>=? fun ctxt ->
      return ctxt
  | Alpha_previous ->
      Storage.Vote.Current_quorum_004.get ctxt >>=? fun quorum ->
      Storage.Vote.Participation_ema.init ctxt quorum >>=? fun ctxt ->
      Storage.Vote.Current_quorum_004.delete ctxt >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0 >>=? fun ctxt ->
      Storage.Last_block_priority.delete ctxt >>=? fun ctxt ->
      Storage.Contract.fold ctxt ~init:(Ok ctxt)
        ~f:(fun contract ctxt ->
            Lwt.return ctxt >>=? fun ctxt ->
            migrate_delegated ctxt contract >>=? fun ctxt ->
            process_contract contract ctxt)
      >>=? fun ctxt ->
      return ctxt

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
