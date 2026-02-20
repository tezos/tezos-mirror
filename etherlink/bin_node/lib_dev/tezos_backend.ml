(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module type Backend = sig
  include Simulator.SimulationBackend

  val block_param_to_block_number :
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t
end

module Make (Backend : Backend) (Block_storage : Tezlink_block_storage_sig.S) :
  Tezlink_backend_sig.S = struct
  type block_param =
    [ `Head of int32
    | `Level of int32
    | `Hash of Ethereum_types.block_hash * int32 ]

  let shell_block_param_to_block_number =
    let open Lwt_result_syntax in
    let compute_offset (Ethereum_types.Qty block_number) offset =
      let result = Int32.sub (Z.to_int32 block_number) offset in
      return (max 0l result)
    in
    function
    | `Head offset ->
        let* current_block_number =
          Backend.block_param_to_block_number (Block_parameter Latest)
        in
        compute_offset current_block_number offset
    | `Hash (hash, offset) ->
        let* current_block_number =
          Backend.block_param_to_block_number
            (Block_hash {hash; require_canonical = false})
        in
        compute_offset current_block_number offset
    | `Level l -> return l

  let on_head_block (block : block_param) k =
    let open Lwt_result_syntax in
    match block with
    | `Head 0l ->
        let* state = Backend.get_state ~block:(Block_parameter Latest) () in
        k state
    | _ -> failwith "Only `head` is supported"

  let on_implicit_account (c : Tezos_types.Contract.t) k =
    match c with
    | Implicit pkh -> k pkh
    | Originated _ -> failwith "Only implicit account are supported"

  let contract_path contract suffix =
    Durable_storage_path.etherlink_root ^ "/contracts/index/"
    ^ Tezlink_durable_storage.Path.to_path
        Tezos_types.Contract.encoding
        contract
    ^ suffix

  let constants _chain (_block : block_param) =
    failwith "Not Implemented Yet (%s)" __LOC__

  let current_level _ _ ~offset:_ = failwith "Not Implemented Yet (%s)" __LOC__

  let balance _chain block contract =
    let open Lwt_result_syntax in
    on_head_block block @@ fun state ->
    match (contract : Tezos_types.Contract.t) with
    | Implicit pkh -> (
        let* read_result =
          Backend.read
            state
            (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh)
        in
        match read_result with
        | Some bytes ->
            let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
            return info.balance
        | None -> return Tezos_types.Tez.zero)
    | Originated _ -> (
        let path = contract_path contract "/balance" in
        let* read_result = Backend.read state path in
        match read_result with
        | Some bytes -> (
            match
              Data_encoding.Binary.of_bytes Tezos_types.Tez.encoding bytes
            with
            | Ok balance -> return balance
            | Error e ->
                failwith
                  "Cannot decode KT1 balance: %a"
                  Data_encoding.Binary.pp_read_error
                  e)
        | None -> return Tezos_types.Tez.zero)

  let list_contracts _chain _block = failwith "Not Implemented Yet (%s)" __LOC__

  let bootstrap_accounts () = failwith "Not Implemented Yet (%s)" __LOC__

  let get_storage _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let get_code _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let get_script _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let manager_key _chain block contract =
    let open Lwt_result_syntax in
    on_head_block block @@ fun state ->
    on_implicit_account contract @@ fun pkh ->
    let* read_result =
      Backend.read state (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh)
    in
    match read_result with
    | Some bytes ->
        let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
        return info.public_key
    | None -> failwith "Account not found"

  let counter _chain _block _c = failwith "Not Implemented Yet (%s)" __LOC__

  let big_map_get _chain _block _id _key_hash =
    failwith "Not Implemented Yet (%s)" __LOC__

  let big_map_raw_info _chain _block _id =
    failwith "Not Implemented Yet (%s)" __LOC__

  let block _chain block =
    let open Lwt_result_syntax in
    let* block_number = shell_block_param_to_block_number block in
    Block_storage.nth_block (Z.of_int32 block_number)

  let monitor_heads _chain _query =
    Stdlib.failwith (Format.sprintf "Not Implemented Yet (%s)" __LOC__)

  let bootstrapped () = failwith "Not Implemented Yet (%s)" __LOC__

  let block_hash _chain _block = failwith "Not Implemented Yet (%s)" __LOC__

  let simulate_operation ~chain_id:_ ~skip_signature:_
      (op : Tezlink_imports.SeouLo_context.packed_operation) hash _block =
    let open Lwt_result_syntax in
    let op = op.protocol_data in
    let*? mock_result =
      Tezlink_mock.Operation_metadata.operation_metadata hash op
    in
    return mock_result
end
