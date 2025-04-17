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

let balance read chain block c =
  (* TODO: #7831 !17664
     Support non-default chain and block parameters. *)
  ignore chain ;
  ignore block ;

  Durable_storage.inspect_durable_and_decode_default
    ~default:Tezos_types.Tez.zero
    read
    (Path.balance c)
    (Data_encoding.Binary.of_bytes_exn Tez.encoding)

let manager_key read chain block c =
  (* TODO: #7831 !17664
     Support non-default chain and block parameters. *)
  ignore chain ;
  ignore block ;

  Durable_storage.inspect_durable_and_decode_opt
    read
    (Path.manager_key c)
    (Data_encoding.Binary.of_bytes_exn Signature.V1.Public_key.encoding)

let counter read chain block c =
  (* TODO: #7831 !17664
     Support non-default chain and block parameters. *)
  ignore chain ;
  ignore block ;

  Durable_storage.inspect_durable_and_decode_default
  (* FIXME: #7960
     This default should be the global counter *)
    ~default:Z.one
    read
    (Path.counter c)
    (Data_encoding.Binary.of_bytes_exn Data_encoding.z)

module type Backend = sig
  include Durable_storage.READER

  val block_param_to_block_number :
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  val tez_nth_block : Z.t -> L2_types.Tezos_block.t tzresult Lwt.t
end

module Make (Backend : Backend) : Tezlink_backend_sig.S = struct
  let current_level chain block ~offset =
    let open Lwt_result_syntax in
    let* offset =
      (* Tezos l1 requires non-negative offset #7845 *)
      if offset >= 0l then return offset
      else failwith "The specified level offset should be positive."
    in

    (* TODO: #7831
       take chain into account
       For the moment this implementation only supports the main chain, once
       the rpc support of tezlink is more stable, we can add support for other chains *)
    let* () =
      match chain with
      | `Main -> return_unit
      | _ -> failwith "Unsupported chain"
    in

    (* TODO: #7831
       take block into account
       For the moment this implementation only supports the head block, once
       the rpc support of tezlink is more stable, we can add support for other blocks *)
    let* () =
      match block with
      | `Head _ -> return_unit
      | _ -> failwith "Unsupported block"
    in

    let* (Qty current_block_number) =
      Backend.block_param_to_block_number (Block_parameter Latest)
    in

    let current_block_number = Z.to_int32 current_block_number in

    let constants = Tezlink_constants.mainnet in
    let level = Int32.add current_block_number offset in
    return
      Tezos_types.
        {
          level;
          cycle = Int32.div level constants.blocks_per_cycle;
          cycle_position = Int32.rem level constants.blocks_per_cycle;
        }

  let constants chain block =
    let open Lwt_result_syntax in
    (* TODO: #7831
       take chain into account
       For the moment this implementation only supports the main chain, once
       the rpc support of tezlink is more stable, we can add support for other chains *)
    let* () =
      match chain with
      | `Main -> return_unit
      | _ -> failwith "Unsupported chain"
    in

    (* TODO: #7831
       take block into account
       For the moment this implementation only supports the head block, once
       the rpc support of tezlink is more stable, we can add support for other blocks *)
    let* () =
      match block with
      | `Head _ -> return_unit
      | _ -> failwith "Unsupported block"
    in

    let fixed_values =
      Tezlink_constants.
        ( ( proof_of_work_nonce_size,
            nonce_length,
            max_anon_ops_per_block,
            max_operation_data_length,
            max_proposals_per_delegate,
            max_micheline_node_count,
            max_micheline_bytes_limit,
            max_allowed_global_constant_depth,
            cache_layout_size,
            michelson_maximum_type_size ),
          ( max_slashing_period,
            sc_max_wrapped_proof_binary_size,
            sc_rollup_message_size_limit,
            sc_rollup_max_number_of_messages_per_level ) )
    in

    let* fixed =
      match Tezlink_constants.values_to_fixed fixed_values with
      | Ok fixed -> return fixed
      | Error err ->
          failwith
            "Failed to get fixed constants: %a"
            Error_monad.pp_print_trace
            err
    in
    return Tezlink_constants.{fixed; parametric = Tezlink_constants.mainnet}

  let read p =
    let open Lwt_result_syntax in
    let* state = Backend.get_state () in
    Backend.read state p

  (* TODO: #7831 !17664
     we type [chain], even though we don't use it, to satisfy the compiler. *)
  let balance (chain : [> `Main]) = balance read chain

  (* TODO: #7831 !17664
     we type [chain], even though we don't use it, to satisfy the compiler. *)
  let manager_key (chain : [> `Main]) = manager_key read chain

  (* TODO: #7831 !17664
     we type [chain], even though we don't use it, to satisfy the compiler. *)
  let counter (chain : [> `Main]) = counter read chain

  let _header chain block =
    let open Lwt_result_syntax in
    (* TODO: #7831
       take chain and block into account
       For the moment this implementation only supports the main chain and head block, once
       the rpc support of tezlink is more stable, we can add support for other chains and blocks. *)
    ignore (chain, block) ;

    let* (Qty block_number) =
      Backend.block_param_to_block_number (Block_parameter Latest)
    in

    Backend.tez_nth_block block_number
end
