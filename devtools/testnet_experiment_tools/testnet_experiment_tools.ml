(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

(* Testnet experiment tools
   ------------------------
   Invocation:
     dune exec devtools/testnet_experiment_tools/testnet_experiment_tools.exe
   Requirements:
     OUTPUT_DIR - sets the directory to output generated outputs.
                  Defaults to /tmp/<unique_dir>
     BAKERS     - sets the number of baker keys to generate.
                  Defaults to 10.
   Description: This file contains scripts to generate config information
                towards bootstrapping an experimental test network.
*)

open Tezt
open Tezt_tezos
open Tezos_crypto
module Node_config = Octez_node_config.Config_file

let ensure_dir_exists dir =
  Lwt.catch
    (fun () ->
      let* () = Lwt_utils_unix.create_dir ~perm:0o744 dir in
      Lwt.return_unit)
    (fun exn ->
      Test.fail
        "Failed to create directory '%s': %s"
        dir
        (Printexc.to_string exn))

let default_number_of_bakers = 10

let bakers = "BAKERS"

let baker_prefix = "baker_"

let baker_alias n = Printf.sprintf "%s%d" baker_prefix n

let number_of_bakers =
  Sys.getenv_opt bakers |> Option.map int_of_string
  |> Option.value ~default:default_number_of_bakers

let output_dir_name = "OUTPUT_DIR"

let default_output_dir =
  let base_dir = Filename.temp_file ~temp_dir:"/tmp" "" "" in
  let _ = Lwt_unix.unlink base_dir in
  let _ = Lwt_unix.mkdir base_dir 0o700 in
  base_dir

let output_dir =
  Sys.getenv_opt output_dir_name |> Option.value ~default:default_output_dir

let network_name_default = "TEZOS_EXPERIMENT_NET"

let network_name =
  Sys.getenv_opt "NETWORK" |> Option.value ~default:network_name_default

let output_parameters_filename =
  Sys.getenv_opt "NETWORK_PARAMETERS_OUTPUT"
  |> Option.value
       ~default:Filename.(concat output_dir "network_parameters.json")

let network_parameters_templates_dir =
  Filename.current_dir_name // "devtools" // "testnet_experiment_tools"
  // "activation_parameters_templates"

let protocol_alpha_parameters_template =
  Filename.current_dir_name // "src" // "proto_alpha" // "parameters"
  // "mainnet_parameters.json"

let network_activation_parameters_templates protocol_hash =
  match protocol_hash with
  | Tezt_tezos.Protocol.Oxford ->
      Some
        (Filename.concat
           network_parameters_templates_dir
           "proto_018_Proxford_mainnet.json")
  | Tezt_tezos.Protocol.Alpha ->
      (* Fetching the network parameters from the src/proto_alpha directory,
         to be sure that we are in synch with current protocl parameters. *)
      Some protocol_alpha_parameters_template
(* | _ -> None *)

let genesis_prefix = "BLockGenesisGenesisGenesisGenesisGenesis"

let generate_baker_accounts n client =
  let rec generate_baker_account i =
    if i < 0 then Lwt.return_unit
    else
      let* _alias = Client.gen_keys ~alias:(baker_alias i) client in
      let* () = Lwt_io.printf "." in
      generate_baker_account (i - 1)
  in
  let* () = Lwt_io.printf "Generating accounts" in
  let* () = generate_baker_account (n - 1) in
  Lwt_io.printf "\n"

let rec genesis () =
  let* time = Lwt_process.pread_line (Lwt_process.shell "date -u +%FT%TZ") in
  let suffix = String.sub Digest.(to_hex (string time)) 0 5 in
  match Base58.raw_decode (genesis_prefix ^ suffix ^ "crcCRC") with
  | None -> genesis ()
  | Some p ->
      let p = String.sub p 0 (String.length p - 4) in
      let b58_block_hash = Base58.safe_encode p in
      let block =
        Tezos_crypto.Hashed.Block_hash.of_b58check_exn b58_block_hash
      in
      return (block, Tezos_base.Time.Protocol.of_notation_exn time)

let save_config (Node_config.{data_dir; _} as configuration) =
  let file = Filename.concat data_dir "config.json" in
  let* () =
    Lwt_io.printf
      "Configuration for %s will be written in %s\n."
      network_name
      file
  in
  let* () = ensure_dir_exists data_dir in
  let* res =
    Lwt_utils_unix.with_atomic_open_out file @@ fun chan ->
    let json =
      Data_encoding.Json.construct Node_config.encoding configuration
    in
    let content = Data_encoding.Json.to_string json in
    Lwt_utils_unix.write_string chan content
  in
  match res with
  | Ok () -> Lwt.return_unit
  | Error _e -> Test.fail "Cannot save configuration file"

let default_number_of_nodes = 1

let number_of_nodes =
  Sys.getenv_opt "NODES" |> Option.map int_of_string
  |> Option.value ~default:default_number_of_nodes

let write_to_node_dir output_dir node_id json =
  let* () = ensure_dir_exists output_dir in
  let file = Filename.concat output_dir node_id in
  let* res =
    Lwt_utils_unix.with_atomic_open_out file @@ fun chan ->
    let content = Data_encoding.Json.to_string json in
    Lwt_utils_unix.write_string chan content
  in
  match res with
  | Ok () -> Lwt.return_unit
  | Error _e -> Test.fail "Failed to write to node directory: %s" file

let get_bakers client =
  let* client_accounts_with_pkhs = Client.list_known_addresses client in
  let baker_accounts =
    client_accounts_with_pkhs |> List.map fst
    |> List.filter (String.starts_with ~prefix:baker_prefix)
  in
  Lwt_list.map_s (fun alias -> Client.show_address ~alias client) baker_accounts

let baker_index baker_alias =
  let start_index = String.length baker_prefix in
  let baker_index_str =
    String.sub baker_alias start_index (String.length baker_alias - start_index)
  in
  int_of_string baker_index_str

let node_prefix = "node_"

let group_by ~f list =
  List.fold_left
    (fun map el ->
      let key = f el in
      String_map.update
        key
        (fun map_els ->
          let map_els = Option.value map_els ~default:[] in
          Some (el :: map_els))
        map)
    String_map.empty
    list

let alias_encoding = Data_encoding.(obj1 (req "alias" @@ string' Plain))

(* These tests can be run locally to generate the data needed to run a
   stresstest. *)
module Local = struct
  let format_baker_accounts () =
    Format_baker_accounts.format_baker_accounts output_dir

  let generate_baker_accounts n () =
    let* () =
      Lwt_io.printf
        "Keys will be saved in %s. You can change this by setting the \
         OUTPUT_DIR environment variable\n"
        output_dir
    in
    let* () =
      Lwt_io.printf
        "%d baker accounts will be generated. You can change this by setting \
         the BAKERS environment variable.\n"
        number_of_bakers
    in
    let client = Client.create ~base_dir:output_dir () in
    let* () = ensure_dir_exists output_dir in
    let* () = generate_baker_accounts n client in
    Lwt.return_unit

  let generate_network_configuration network_name data_dir () =
    let protocol =
      Tezos_crypto.Hashed.Protocol_hash.of_b58check_exn
        "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
    in
    let* block, time = genesis () in
    let genesis = Tezos_base.Genesis.{block; time; protocol} in
    let chain_name =
      Tezos_base.Distributed_db_version.Name.of_string network_name
    in
    let sandboxed_chain_name =
      Tezos_base.Distributed_db_version.Name.of_string @@ network_name
      ^ "_SANDBOXED"
    in
    let client = Client.create ~base_dir:output_dir () in
    let baker_0 = baker_alias 0 in
    let* {public_key = genesis_pubkey; _} =
      Client.show_address ~alias:baker_0 client
    in
    let blockchain_network : Node_config.blockchain_network =
      Node_config.
        {
          alias = None;
          genesis;
          chain_name;
          sandboxed_chain_name;
          old_chain_name = None;
          incompatible_chain_name = None;
          user_activated_upgrades = [];
          user_activated_protocol_overrides = [];
          default_bootstrap_peers = [];
          dal_config =
            {
              activated = false;
              use_mock_srs_for_testing = None;
              bootstrap_peers = [];
            };
          genesis_parameters =
            Some
              {
                context_key = "sandbox_parameter";
                values = `O [("genesis_pubkey", `String genesis_pubkey)];
              };
        }
    in
    let node_configuration =
      Node_config.{default_config with blockchain_network; data_dir}
    in
    let* () = save_config node_configuration in
    Lwt.return_unit

  let generate_network_activation_parameters protocol_hash () =
    let* () =
      Lwt_io.printf
        "All relative paths in commands will use %s as the current working \
         directory\n"
        Filename.current_dir_name
    in
    let activation_parameters_filename =
      match network_activation_parameters_templates protocol_hash with
      | None ->
          Test.fail "Protocol %s not supported" (Protocol.name protocol_hash)
      | Some activation_parameters_filename -> activation_parameters_filename
    in
    let client = Client.create ~base_dir:output_dir () in
    let* () = Lwt_io.printf "Fetching client accounts\n" in
    let* client_accounts_with_pkhs = Client.list_known_addresses client in
    let baker_accounts =
      client_accounts_with_pkhs |> List.map fst
      |> List.filter (String.starts_with ~prefix:baker_prefix)
    in
    let* () = Lwt_io.printf "Fetching client accounts from %s\n" output_dir in
    let bootstrap_amount_mutez = Some 4_000_000_000_000 in
    let* bootstrap_accounts =
      baker_accounts
      |> Lwt_list.map_s (fun alias ->
             let* () = Lwt_io.printf "." in
             let* account_key = Client.show_address ~alias client in
             return (account_key, bootstrap_amount_mutez))
    in
    let* () = Lwt_io.printf "\n" in
    let* () =
      Lwt_io.printf
        "Retrieving activation parameters template from %s\n"
        activation_parameters_filename
    in
    let* _filename =
      Tezt_tezos.Protocol.write_parameter_file
        ~bootstrap_accounts
        ~base:(Either.Left activation_parameters_filename)
        ~output_file:output_parameters_filename
        []
    in
    Lwt.return_unit

  let partition_bakers num_nodes () =
    let client = Client.create ~base_dir:output_dir () in
    (* This fetches the account keys although it is not needed. *)
    let* bakers = get_bakers client in
    let bakers = List.map (fun {Account.alias; _} -> alias) bakers in
    let f baker_alias =
      let baker_index = baker_index baker_alias in
      let node_index = baker_index mod num_nodes in
      Format.sprintf "%s%d" node_prefix node_index
    in
    let bakers_node_partitions = group_by ~f bakers in
    let bakers_node_partitions =
      List.of_seq @@ String_map.to_seq bakers_node_partitions
    in
    Lwt_list.iter_s
      (fun (node_id, bakers) ->
        let json =
          Data_encoding.(Json.construct (list alias_encoding) bakers)
        in
        write_to_node_dir output_dir node_id json)
      bakers_node_partitions
end

(* These tests must be run remotely by the nodes participating in
   a network that wants to be stresstested. *)
module Remote = struct
  let run_stresstest () = Test.fail "Not implemented"
end

let () =
  let open Tezt.Test in
  register
    ~__FILE__
    ~title:"Generate baker accounts"
    ~tags:["generate_baker_accounts"]
    (Local.generate_baker_accounts number_of_bakers) ;
  register
    ~__FILE__
    ~title:"Format baker accounts"
    ~tags:["format_baker_accounts"]
    Local.format_baker_accounts ;
  register
    ~__FILE__
    ~title:"Generate Network Configuration"
    ~tags:["generate_network_configuration"]
    (Local.generate_network_configuration network_name output_dir) ;
  register
    ~__FILE__
    ~title:"Generate Network Activation Parameters"
    ~tags:["generate_activation_parameters"]
    (Local.generate_network_activation_parameters Protocol.Oxford) ;
  register
    ~__FILE__
    ~title:"Partition bakers by node"
    ~tags:["partition_bakers"]
    (Local.partition_bakers number_of_nodes) ;
  register
    ~__FILE__
    ~title:"Run stresstest"
    ~tags:["run_stresstest"]
    Remote.run_stresstest ;
  Tezt.Test.run ()
