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

type existing_rollup = {address : string; current_preimages_dir : string}

let preset_preimages ~rollup_preimages_dir ~preimages_dir =
  let* () = Process.run "mkdir" ["-p"; preimages_dir] in
  Process.run "cp" ["-rT"; rollup_preimages_dir; preimages_dir]

let setup_evm_infra ~mode ~(testnet : Testnet.t) ~operator ?runner
    ?preexisting_rollup ?rollup_node_name ?loser_mode node client =
  let rollup_node =
    Sc_rollup_node.create
      ?runner
      ?name:rollup_node_name
      ~protocol:testnet.protocol
      ~base_dir:(Client.base_dir client)
      ~default_operator:operator.Account.alias
      mode
      node
  in
  (* Start a rollup node *)
  let preimages_dir = Sc_rollup_node.data_dir rollup_node // "wasm_2_0_0" in
  let* rollup_address =
    match preexisting_rollup with
    | Some {address; _} -> return address
    | None ->
        let* boot_sector =
          Sc_rollup_helpers.prepare_installer_kernel
            ~base_installee:"./"
            ~preimages_dir
            "evm_kernel"
        in
        Log.info "EVM Kernel installer ready." ;
        let* rollup_address =
          Sc_rollup.originate_new_rollup
            ~boot_sector
            ~src:operator.Account.alias
            client
        in
        return rollup_address
  in
  let* _ = Sc_rollup_node.config_init ?loser_mode rollup_node rollup_address in
  let* () =
    match preexisting_rollup with
    | Some {current_preimages_dir; _} ->
        preset_preimages
          ~rollup_preimages_dir:current_preimages_dir
          ~preimages_dir
    | None -> Lwt.return_unit
  in
  Log.info "Starting a smart rollup node to track %s" rollup_address ;
  Log.info
    "Smart rollup node API is available at %s."
    (Sc_rollup_node.endpoint rollup_node) ;
  let* () = Sc_rollup_node.run rollup_node rollup_address [] in
  let* () = Sc_rollup_node.wait_for_ready rollup_node in
  Log.info "Smart rollup node started." ;
  (* EVM Kernel installation level. *)
  let* _ = Sc_rollup_node.wait_for_level rollup_node (Node.get_level node) in
  let* evm_proxy_server = Evm_proxy_server.init ?runner rollup_node in
  Log.info
    "Proxy server API is available at %s."
    (Evm_proxy_server.endpoint evm_proxy_server) ;
  return (rollup_address, rollup_node, evm_proxy_server)

let check_operator_balance ~node ~client ~mode ~operator =
  let min_balance =
    (* If the mode needs to publish commitments, it needs enough money to stake. *)
    if List.mem mode Sc_rollup_node.[Operator; Maintenance; Accuser] then
      Tez.(of_int 11_000)
    else Tez.(of_mutez_int 100)
  in
  Helpers.wait_for_funded_key node client min_balance operator

let stop_or_keep_going ~node =
  (* If asked, the scenario will keep going, making the EVM rollup available
     for testing. *)
  if Cli.get_bool ~default:false "keep-going" then
    let* _ = Node.wait node in
    unit
  else unit

let deploy_evm_rollup ~(testnet : Testnet.t) () =
  let* client, node = Helpers.setup_octez_node ~testnet () in
  let* operator = Client.gen_and_show_keys client in
  let mode =
    Cli.get_string ~default:"Operator" "mode" |> Sc_rollup_node.mode_of_string
  in
  let* () = check_operator_balance ~node ~client ~mode ~operator in
  let* _rollup_address, _rollup_node, _evm_proxy_server =
    setup_evm_infra ~mode ~testnet ~operator node client
  in
  stop_or_keep_going ~node

let register ~testnet =
  Test.register
    ~__FILE__
    ~title:"Deploy an EVM rollup"
    ~tags:["deploy"]
    (deploy_evm_rollup ~testnet)
