(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
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
  include Simulator.MakeTezlink (Backend)

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

  let shell_block_param_to_eth_block_param =
    let open Lwt_result_syntax in
    function
    | `Head 0l ->
        return
        @@ Ethereum_types.Block_parameter.Block_parameter
             Ethereum_types.Block_parameter.Latest
    | `Hash (hash, 0l) ->
        return
        @@ Ethereum_types.Block_parameter.Block_hash
             {hash; require_canonical = false}
    | block ->
        let* num = shell_block_param_to_block_number block in
        return
        @@ Ethereum_types.Block_parameter.Block_parameter
             (Number (Ethereum_types.quantity_of_z (Z.of_int32 num)))

  let current_level chain block ~offset =
    let open Lwt_result_syntax in
    let `Main = chain in

    let* offset =
      (* Tezos l1 requires non-negative offset #7845 *)
      if offset >= 0l then return offset
      else failwith "The specified level offset should be positive."
    in

    let* block_number = shell_block_param_to_block_number block in

    let constants = Tezlink_constants.all_constants in
    let level = Int32.add block_number offset in
    return
      Tezos_types.
        {
          level;
          cycle =
            Int32.(div (sub level 1l) constants.parametric.blocks_per_cycle);
          cycle_position =
            Int32.(rem (sub level 1l) constants.parametric.blocks_per_cycle);
        }

  let constants chain (_block : block_param) =
    let open Lwt_result_syntax in
    let `Main = chain in
    return Tezlink_constants.all_constants

  let read ~block p =
    let open Lwt_result_syntax in
    let* block = shell_block_param_to_eth_block_param block in
    let* state = Backend.get_state ~block () in
    Backend.read state p

  let subkeys ~block p =
    let open Lwt_result_syntax in
    let* block = shell_block_param_to_eth_block_param block in
    let* state = Backend.get_state ~block () in
    Backend.subkeys state p

  let balance chain block c =
    let `Main = chain in
    Tezlink_durable_storage.balance (read ~block) c

  let list_contracts chain block =
    let open Lwt_result_syntax in
    let `Main = chain in
    let+ contracts_keys =
      subkeys ~block Tezlink_durable_storage.Path.accounts_index
    in
    List.filter_map
      (fun k ->
        let open Option_syntax in
        let* bytes = Hex.to_string (`Hex k) in
        Data_encoding.Binary.of_string_opt Tezos_types.Contract.encoding bytes)
      contracts_keys

  let bootstrap_accounts () =
    let open Lwt_result_syntax in
    (* We call bootstrap accounts those that were present in durable storage
       at the start of the chain. *)
    let block = `Level 0l in
    let chain = `Main in
    let* contracts_keys =
      subkeys ~block Tezlink_durable_storage.Path.accounts_index
    in
    let contracts =
      List.filter_map Tezlink_durable_storage.contract_of_path contracts_keys
    in
    List.map_es
      (fun c ->
        let* balance =
          balance chain block (Tezos_types.Contract.of_implicit c)
        in
        return (c, balance))
      contracts

  let get_storage chain block c =
    (* TODO: #7986
       Support unparsing_mode argument. *)
    let `Main = chain in
    Lwt_result.map (Option.value ~default:None)
    @@ Durable_storage.inspect_durable_and_decode_opt
         (read ~block)
         (Tezlink_durable_storage.Path.storage c)
         (Data_encoding.Binary.of_bytes_opt
            Tezlink_imports.Alpha_context.Script.expr_encoding)

  let get_code chain block c =
    (* TODO: #7986
       Support unparsing_mode argument. *)
    let `Main = chain in
    Lwt_result.map (Option.value ~default:None)
    @@ Durable_storage.inspect_durable_and_decode_opt
         (read ~block)
         (Tezlink_durable_storage.Path.code c)
         (Data_encoding.Binary.of_bytes_opt
            Tezlink_imports.Alpha_context.Script.expr_encoding)

  let get_script chain block c =
    let open Lwt_result_syntax in
    match Tezlink_mock.mocked_script c with
    | Some c -> return_some c
    | None -> (
        let* code = get_code chain block c in
        let* storage = get_storage chain block c in
        match (code, storage) with
        | Some code, Some storage ->
            return
            @@ Some
                 Tezlink_imports.Imported_protocol.Alpha_context.Script.
                   {code = lazy_expr code; storage = lazy_expr storage}
        | _ -> return_none)

  let manager_key chain block c =
    let open Lwt_result_syntax in
    (* TODO: #7831 !17664
       Support non-default chain and block parameters. *)
    let `Main = chain in
    let* manager_opt = Tezlink_durable_storage.manager (read ~block) c in
    match manager_opt with
    | Some (Public_key k) -> return_some k
    | _ -> return_none

  let counter chain block c =
    (* TODO: #7831 !17664
       Support non-default chain and block parameters. *)
    let `Main = chain in
    Tezlink_durable_storage.counter (read ~block) c

  let big_map_get chain block id key_hash =
    let `Main = chain in
    Tezlink_durable_storage.big_map_get (read ~block) id key_hash

  let big_map_raw_info chain block id =
    let open Lwt_result_syntax in
    let `Main = chain in
    let read = read ~block in
    let* key_type = Tezlink_durable_storage.big_map_key_type read id in
    let* value_type = Tezlink_durable_storage.big_map_value_type read id in
    match (key_type, value_type) with
    | Some kt, Some vt ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/8229
           - total_bytes:
             Not yet implemented, requires kernel-side tracking
             (L1 stores this at /big_maps/index/<id>/total_bytes)
           - contents: intentionally empty, consistent with L1 raw context
             behavior (L1 never returns big_map contents in this RPC) *)
        return_some (kt, vt, Z.zero, [])
    | _ -> return_none

  let block chain block =
    let open Lwt_result_syntax in
    let `Main = chain in
    let* block_number = shell_block_param_to_block_number block in
    Block_storage.nth_block (Z.of_int32 block_number)

  let monitor_heads chain query =
    (* TODO: #7831
       take chain into account
       For the moment this implementation only supports the main chain, once
       the rpc support of tezlink is more stable, we can add support for other chains *)
    ignore (chain, query) ;

    let blueprint_stream, stopper = Broadcast.create_blueprint_stream () in

    let retry_delays_ms = [0.; 50.; 100.; 500.] in

    (* Convert blueprint notifications into full blocks, giving the store a
       short grace period if the block is not yet written.
       Note that this delay does not correspond to the time between blueprint production
       and block application, but rather from the time the Database says the data has been
       written in the storage and the moment it actually becomes available to be read. *)
    let rec fetch_block level =
      let open Lwt_syntax in
      function
      | [] ->
          (* After all retries failed, emit warning event. *)
          let* () = Events.missing_block @@ Z.to_int32 level in
          return_none
      | delay_ms :: rest -> (
          let* () = Lwt_unix.sleep (delay_ms /. 1000.) in
          let* block_result = Block_storage.nth_block level in
          match block_result with
          | Ok block -> return_some block
          | Error _ -> fetch_block level rest)
    in

    let block_stream =
      Lwt_stream.filter_map_s
        (fun (bp_with_events : Blueprint_types.Legacy.with_events) ->
          (* Extract the level from the blueprint. *)
          let (Ethereum_types.Qty level) = bp_with_events.blueprint.number in
          fetch_block level retry_delays_ms)
        blueprint_stream
    in
    (block_stream, stopper)

  (* TODO: #7963 Support Observer Mode
     Here the catchup mechanism to fetch blueprints is not taken into account as
     the observer mode is not supported yet *)
  let bootstrapped () =
    let open Lwt_result_syntax in
    let* (Qty current_block_number) =
      Backend.block_param_to_block_number (Block_parameter Latest)
    in
    let* block = Block_storage.nth_block current_block_number in
    return (block.hash, block.timestamp)

  let block_hash chain block =
    let open Lwt_result_syntax in
    let `Main = chain in
    let* number = shell_block_param_to_block_number block in
    Block_storage.nth_block_hash (Z.of_int32 number)

  let simulate_operation ~chain_id ~skip_signature op hash block =
    let open Lwt_result_syntax in
    let read = read ~block in
    let* block = shell_block_param_to_eth_block_param block in
    simulate_operation ~read ~chain_id ~skip_signature op hash block
end
