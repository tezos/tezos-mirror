(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Smart Optimistic Rollups: EVM Kernel
   Requirement:  * make -f kernels.mk build
                 * provide a `kernel_upgrade_scenario.json` such as:
                 ```
                 {
                   "smart-rollup": {
                       "address": "<smart-rollup-address>",
                       "current-preimages-dir": "<preimages-dir-used-for-address-above>"
                   }, #OPTIONAL_FIELD
                   "kernel-dir": "<new-kernel-dir>",
                   "new-kernel": "<incoming-kernel-file-name>"
                 }
                 ```
   Invocation:   dune exec src/bin_testnet_scenarios/main.exe -- --file evm_rollup_upgrade.ml --verbose
                 NB: if your configuration files are not provided at the root of the project add
                 additional `-a configuration=<PATH> -a upgrade_configuration=<PATH>` arguments.
*)

type upgrade_config = {
  (* Rollup where the previous kernel is deployed *)
  smart_rollup : Evm_rollup.existing_rollup option;
  (* Directory where to find the new kernel. *)
  kernel_dir : string;
  (* Name of the new incoming kernel to upgrade to.
     NB: the name should not be suffixed by '.wasm'. *)
  new_kernel : string;
}

let get_upgrade_config path =
  let config = JSON.parse_file path in
  let smart_rollup_opt = JSON.(config |-> "smart-rollup" |> as_object_opt) in
  let smart_rollup =
    match smart_rollup_opt with
    | Some [(_, address); (_, current_preimages_dir)] ->
        let address = JSON.(address |> as_string) in
        let current_preimages_dir = JSON.(current_preimages_dir |> as_string) in
        Some Evm_rollup.{address; current_preimages_dir}
    | _ -> None
  in
  let kernel_dir = JSON.(config |-> "kernel-dir" |> as_string) in
  let new_kernel = JSON.(config |-> "new-kernel" |> as_string) in
  {smart_rollup; kernel_dir; new_kernel}

let durable_storage_rpc ~smart_rollup_node ~key =
  Sc_rollup_helpers.call_rpc
    ~smart_rollup_node
    ~service:("global/block/head/durable/wasm_2_0_0/value?key=" ^ key)
  |> Lwt.map JSON.as_string

let send_external_message_and_wait ~client ~node ~sender ~hex_msg =
  let* () =
    Client.Sc_rollup.send_message
      ~src:sender
      ~msg:("hex:[ \"" ^ hex_msg ^ "\" ]")
      client
  in
  let* _ = Node.wait_for_level node (Node.get_level node + 2) in
  unit

let strip_0x s =
  if String.starts_with ~prefix:"0x" s then
    let n = String.length s in
    String.sub s 2 (n - 2)
  else s

let rec retrieve_signature () =
  (* NB: you can use the octez-evm-upgrade-signer binary to provide the signature *)
  let signature = strip_0x @@ read_line () in
  if String.length signature != 128 then (
    Log.info "Provided signature must be 128 characters long, please try again." ;
    retrieve_signature ())
  else signature

let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  bytes

let hex_to_int hex_value =
  let hex_value = `Hex hex_value in
  let bytes_value = Hex.to_bytes hex_value in
  Bytes.get_uint16_le bytes_value 0

let get_upgrade_message ~smart_rollup_node ~preimage_root_hash
    ~smart_rollup_address =
  let* current_kernel_upgrade_nonce =
    Lwt.catch
      (fun () ->
        durable_storage_rpc ~smart_rollup_node ~key:"/evm/upgrade_nonce")
      (function _ -> return "0100")
  in
  let incoming_kernel_upgrade_nonce =
    hex_to_int current_kernel_upgrade_nonce + 1
  in
  (* Create external upgrade message in hexadecimal. *)
  Log.info
    "Smart rollup address: %s\n\
     Kernel upgrade nonce: %d\n\
     Preimage root hash: %s\n\
     Waiting for upgrade signature..."
    smart_rollup_address
    incoming_kernel_upgrade_nonce
    preimage_root_hash ;
  let signature = retrieve_signature () in
  let kernel_upgrade_nonce =
    u16_to_bytes incoming_kernel_upgrade_nonce |> Hex.of_bytes |> Hex.show
  in
  let* hex_smart_rollup_address =
    durable_storage_rpc
      ~smart_rollup_node
      ~key:"/evm/metadata/smart_rollup_address"
  in
  let upgrade_kernel_tag = "03" in
  return @@ hex_smart_rollup_address ^ upgrade_kernel_tag ^ kernel_upgrade_nonce
  ^ preimage_root_hash ^ signature

let replace_preimages ~smart_rollup_node ~kernel_dir ~new_kernel =
  let preimages_dir =
    Sc_rollup_node.data_dir smart_rollup_node // "wasm_2_0_0"
  in
  let* _, root_hash =
    Sc_rollup_helpers.prepare_installer_kernel_gen
      ~preimages_dir
      ~base_installee:kernel_dir
      ~display_root_hash:true
      new_kernel
  in
  match root_hash with
  | Some root_hash -> return root_hash
  | None ->
      failwith
        "Couldn't obtain the root hash of the preimages of the chunked kernel."

let upgrade_kernel ~testnet () =
  let path =
    Cli.get_string
      ~default:"kernel_upgrade_scenario.json"
      "upgrade_configuration"
  in
  let upgrade_config = get_upgrade_config path in
  let* client, node = Helpers.setup_octez_node ~testnet () in
  let* operator = Client.gen_and_show_keys client in
  let mode =
    Cli.get_string ~default:"Operator" "mode" |> Sc_rollup_node.mode_of_string
  in
  let* () = Evm_rollup.check_operator_balance ~node ~client ~mode ~operator in
  let* smart_rollup_address, smart_rollup_node, _evm_proxy_server =
    Evm_rollup.setup_evm_infra
      ~mode
      ~operator
      ?preexisting_rollup:upgrade_config.smart_rollup
      node
      client
  in
  (* Wait for the kernel to initialize. *)
  let* _ = Node.wait_for_level node (Node.get_level node + 2) in
  let new_kernel =
    project_root // upgrade_config.kernel_dir
    // (upgrade_config.new_kernel ^ ".wasm")
  in
  let* preimage_root_hash =
    replace_preimages
      ~smart_rollup_node
      ~kernel_dir:upgrade_config.kernel_dir
      ~new_kernel:upgrade_config.new_kernel
  in
  let* upgrade_message =
    get_upgrade_message
      ~smart_rollup_node
      ~preimage_root_hash
      ~smart_rollup_address
  in
  let* () =
    send_external_message_and_wait
      ~client
      ~node
      ~sender:operator.alias
      ~hex_msg:upgrade_message
  in
  let* boot_wasm =
    durable_storage_rpc ~smart_rollup_node ~key:"/kernel/boot.wasm"
  in
  let boot_wasm_expected =
    Hex.show @@ Hex.of_bytes @@ Bytes.of_string @@ read_file new_kernel
  in
  Check.(boot_wasm_expected = boot_wasm)
    Check.string
    ~error_msg:"Unexpected kernel [boot.wasm]" ;
  Evm_rollup.stop_or_keep_going ~node

let register ~testnet =
  Test.register
    ~__FILE__
    ~title:"Upgrade an EVM rollup"
    ~tags:["upgrade"]
    (upgrade_kernel ~testnet)
