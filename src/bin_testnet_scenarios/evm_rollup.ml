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

(* Testing
   -------
   Component:    Smart Optimistic Rollups: EVM Kernel
   Requirement:  * make -f kernels.mk build
                 * provide a `evm_configuration.json` such as:
                 ```
                 {
                   "mode": "<mode>",
                   "setup-file": "<path>",
                   "keep-going": "<bool>"
                 }
                 ```
   Invocation:   dune exec src/bin_testnet_scenarios/main.exe -- --file evm_rollup.ml --verbose
                 NB: if your configuration files are not provided at the root of the project add
                 additional `-a configuration=<PATH> -a evm_configuration=<PATH>` arguments.
*)

(** EVM Rollup specific configuration, can be parametrized via the CLI. *)
type config = {
  mode : Sc_rollup_node.mode;
      (** Mode of rollup node, e.g. observer, batcher etc. Defaults to operator
          if not specified. *)
  setup_file : string option;
      (** Path to a YAML setup file given to the smart-rollup-installer. *)
  keep_going : bool;
      (** Whether the binaries should keep going after the scenario is done.
          Defaults to [false]. *)
}

type existing_rollup = {address : string; current_preimages_dir : string}

let get_config config =
  let open JSON in
  let mode =
    config |-> "mode" |> as_string_opt
    |> Option.value ~default:"Operator"
    |> Sc_rollup_node.mode_of_string
  in
  let setup_file = config |-> "setup_file" |> as_string_opt in
  let keep_going =
    config |-> "keep-going" |> as_bool_opt |> Option.value ~default:false
  in
  {mode; setup_file; keep_going}

let preset_preimages ~rollup_preimages_dir ~preimages_dir =
  let* () = Process.run "mkdir" ["-p"; preimages_dir] in
  Process.run "cp" ["-rT"; rollup_preimages_dir; preimages_dir]

let setup_evm_infra ~config ~operator ?runner ?preexisting_rollup
    ?rollup_node_name ?loser_mode node client =
  let rollup_node =
    Sc_rollup_node.create
      ?runner
      ?name:rollup_node_name
      ~base_dir:(Client.base_dir client)
      ~default_operator:operator.Account.alias
      config.mode
      node
  in
  (* Start a rollup node *)
  let preimages_dir = Sc_rollup_node.data_dir rollup_node // "wasm_2_0_0" in
  let* rollup_address =
    match preexisting_rollup with
    | Some {address; _} -> return address
    | None ->
        let setup_file =
          let path =
            Option.value
              ~default:(project_root // "etherlink/kernel_evm/config/dev.yaml")
              config.setup_file
          in
          `Path path
        in
        let* boot_sector =
          Sc_rollup_helpers.prepare_installer_kernel
            ~base_installee:"./"
            ~preimages_dir
            ~config:setup_file
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
  let* current_level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level rollup_node current_level in
  let* evm_node = Evm_node.init ~devmode:true ?runner rollup_node in
  Log.info "Node API is available at %s." (Evm_node.endpoint evm_node) ;
  return (rollup_address, rollup_node, evm_node)

let check_operator_balance ~node ~client ~mode ~operator =
  let min_balance =
    (* If the mode needs to publish commitments, it needs enough money to stake. *)
    if List.mem mode Sc_rollup_node.[Operator; Maintenance; Accuser] then
      Tez.(of_int 11_000)
    else Tez.(of_mutez_int 100)
  in
  Helpers.wait_for_funded_key node client min_balance operator

let stop_or_keep_going ~config ~node =
  (* If asked, the scenario will keep going, making the EVM rollup available
     for testing. *)
  if config.keep_going then
    let* _ = Node.wait node in
    unit
  else unit

let deploy_evm_rollup ~configuration_path ~(testnet : unit -> Testnet.t) () =
  let config = get_config (JSON.parse_file configuration_path) in
  let testnet = testnet () in
  let* client, node = Helpers.setup_octez_node ~testnet () in
  let* operator = Client.gen_and_show_keys client in
  let* () = check_operator_balance ~node ~client ~mode:config.mode ~operator in
  let* _rollup_address, _rollup_node, _evm_node =
    setup_evm_infra ~config ~operator node client
  in
  stop_or_keep_going ~config ~node

let register ~testnet =
  let configuration_path =
    Cli.get_string ~default:"evm_configuration.json" "evm_configuration"
  in
  Test.register
    ~__FILE__
    ~title:"Deploy an EVM rollup"
    ~tags:["deploy"]
    (deploy_evm_rollup ~configuration_path ~testnet)
