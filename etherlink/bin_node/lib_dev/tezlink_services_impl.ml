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

    let constants = Tezlink_constants.all_constants in
    let level = Int32.add current_block_number offset in
    return
      Tezos_types.
        {
          level;
          cycle = Int32.div level constants.parametric.blocks_per_cycle;
          cycle_position = Int32.rem level constants.parametric.blocks_per_cycle;
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
    return Tezlink_constants.all_constants

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

  let header chain block =
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
