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

let balance read chain c =
  let `Main = chain in

  Durable_storage.inspect_durable_and_decode_default
    ~default:Tezos_types.Tez.zero
    read
    (Path.balance c)
    (Data_encoding.Binary.of_bytes_exn Tez.encoding)

let manager_key read chain c =
  (* TODO: #7831 !17664
     Support non-default chain and block parameters. *)
  let `Main = chain in

  Durable_storage.inspect_durable_and_decode_opt
    read
    (Path.manager_key c)
    (Data_encoding.Binary.of_bytes_exn Signature.V1.Public_key.encoding)

let counter read chain c =
  (* TODO: #7831 !17664
     Support non-default chain and block parameters. *)
  let `Main = chain in

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
  type block_param = [`Head]

  let current_level chain block ~offset =
    let open Lwt_result_syntax in
    let* offset =
      (* Tezos l1 requires non-negative offset #7845 *)
      if offset >= 0l then return offset
      else failwith "The specified level offset should be positive."
    in

    let `Main = chain in
    let `Head = block in

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
    let `Main = chain in
    let `Head = block in
    return Tezlink_constants.all_constants

  let read ~block p =
    let open Lwt_result_syntax in
    let block =
      match block with
      | `Head -> Ethereum_types.Block_parameter.(Block_parameter Latest)
    in
    let* state = Backend.get_state ~block () in
    Backend.read state p

  let balance chain block = balance (read ~block) chain

  let manager_key chain block = manager_key (read ~block) chain

  let counter chain block = counter (read ~block) chain

  let header chain block =
    let open Lwt_result_syntax in
    let `Main = chain in
    let `Head = block in
    let* (Qty block_number) =
      Backend.block_param_to_block_number (Block_parameter Latest)
    in
    Backend.tez_nth_block block_number
end
