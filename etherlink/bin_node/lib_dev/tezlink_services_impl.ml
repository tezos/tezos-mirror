(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

module Path = struct
  (** [to_path encoding value] uses [encoding] to encode [value] in
      hexadecimal *)
  let to_path encoding value =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding value in
    let (`Hex s) = Hex.of_bytes raw_key in
    s

  let account contract =
    "/tezlink/context/contracts/index/" ^ to_path Contract.encoding contract

  let balance contract = account contract ^ "/balance"

  let manager_key contract = account contract ^ "/manager_key"

  let counter contract = account contract ^ "/counter"
end

module type Backend = sig
  include Durable_storage.READER

  val block_param_to_block_number :
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t
end

module Make (Backend : Backend) (Block_storage : Tezlink_block_storage_sig.S) :
  Tezlink_backend_sig.S = struct
  type block_param = [`Head of int32 | `Level of int32]

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
    | `Level l -> return l

  let shell_block_param_to_eth_block_param =
    let open Lwt_result_syntax in
    function
    | `Head 0l ->
        return
        @@ Ethereum_types.Block_parameter.Block_parameter
             Ethereum_types.Block_parameter.Latest
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
          cycle = Int32.div level constants.parametric.blocks_per_cycle;
          cycle_position = Int32.rem level constants.parametric.blocks_per_cycle;
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

  let balance chain block c =
    let `Main = chain in
    Tezlink_durable_storage.balance (read ~block) c

  let manager_key chain block c =
    (* TODO: #7831 !17664
       Support non-default chain and block parameters. *)
    let `Main = chain in
    Tezlink_durable_storage.manager_key (read ~block) c

  let counter chain block c =
    (* TODO: #7831 !17664
       Support non-default chain and block parameters. *)
    let `Main = chain in
    Tezlink_durable_storage.counter (read ~block) c

  let header chain block =
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
        (fun (bp_with_events : Blueprint_types.with_events) ->
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
end
