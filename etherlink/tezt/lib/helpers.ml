(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

let evm_type =
  "or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes"

let no_0x s =
  if String.starts_with ~prefix:"0x" s then String.sub s 2 (String.length s - 2)
  else s

let normalize s = String.lowercase_ascii @@ no_0x s

let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  Bytes.to_string bytes

let leftPad32 s =
  let s = no_0x s in
  let len = String.length s in
  String.make (64 - len) '0' ^ s

let add_0x s = "0x" ^ s

let mapping_position index map_position =
  Tezos_crypto.Hacl.Hash.Keccak_256.digest
    (Hex.to_bytes
       (`Hex (leftPad32 index ^ leftPad32 (string_of_int map_position))))
  |> Hex.of_bytes |> Hex.show |> add_0x

let hex_string_to_int x = `Hex x |> Hex.to_string |> Z.of_bits |> Z.to_int

let next_rollup_node_level ~sc_rollup_node ~node ~client =
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* level = Node.get_level node in
  Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level

let next_evm_level ~evm_node ~sc_rollup_node ~node ~client =
  match Evm_node.mode evm_node with
  | Proxy _ -> next_rollup_node_level ~sc_rollup_node ~node ~client
  | Sequencer _ ->
      let open Rpc.Syntax in
      let* _ = Rpc.produce_block evm_node in
      let*@ level = Rpc.block_number evm_node in
      return (Int32.to_int level)
  | Observer _ -> Test.fail "Cannot create a new level with an Observer node"

let kernel_inputs_path = "etherlink/tezt/tests/evm_kernel_inputs"

let read_tx_from_file () =
  read_file (kernel_inputs_path ^ "/100-inputs-for-proxy")
  |> String.trim |> String.split_on_char '\n'
  |> List.map (fun line ->
         match String.split_on_char ' ' line with
         | [tx_raw; tx_hash] -> (tx_raw, tx_hash)
         | _ -> failwith "Unexpected tx_raw and tx_hash.")

let force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~client ~node =
  let force_kernel_upgrade_payload =
    (* Framed protocol tag. *)
    "\000"
    (* Smart rollup address bytes. *)
    ^ Tezos_crypto.Hashed.Smart_rollup_address.(
        of_b58check_exn sc_rollup_address |> to_string)
    ^ (* Force kernel upgrade tag.
         See [FORCE_KERNEL_UPGRADE_TAG] in [etherlink/kernel_evm/kernel/src/parsing.rs] *)
    "\255"
    |> Hex.of_string |> Hex.show
  in
  let* () =
    Sc_rollup_helpers.send_message
      client
      (sf "hex:[%S]" force_kernel_upgrade_payload)
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client ~node in
  unit

let upgrade ~sc_rollup_node ~sc_rollup_address ~admin ~admin_contract ~client
    ~upgrade_to ~activation_timestamp =
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let* {root_hash; _} =
    Sc_rollup_helpers.prepare_installer_kernel ~preimages_dir upgrade_to
  in
  let* payload = Evm_node.upgrade_payload ~root_hash ~activation_timestamp in
  (* Sends the upgrade to L1. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:admin
      ~receiver:admin_contract
      ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address payload)
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  unit

let check_head_consistency ~left ~right ?error_msg () =
  let open Rpc.Syntax in
  let error_msg =
    Option.value
      ~default:
        Format.(
          sprintf
            "Nodes do not have the same head (%s is %%L while %s is %%R"
            (Evm_node.name left)
            (Evm_node.name right))
      error_msg
  in
  let*@ left_head = Rpc.get_block_by_number ~block:"latest" left in
  let*@ right_head = Rpc.get_block_by_number ~block:"latest" right in
  Check.((left_head.hash = right_head.hash) string) ~error_msg ;
  unit

let sequencer_upgrade ~sc_rollup_address ~sequencer_admin
    ~sequencer_admin_contract ~client ~upgrade_to ~pool_address
    ~activation_timestamp =
  let* payload =
    Evm_node.sequencer_upgrade_payload
      ~client
      ~public_key:upgrade_to
      ~pool_address
      ~activation_timestamp
      ()
  in
  (* Sends the upgrade to L1. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:sequencer_admin
      ~receiver:sequencer_admin_contract
      ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address payload)
      ~burn_cap:Tez.one
      client
  in
  Client.bake_for_and_wait ~keys:[] client
