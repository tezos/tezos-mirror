(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type public = [`Mainnet | `Ghostnet | `Weeklynet of string]

type t = [public | `Sandbox]

let to_string = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Weeklynet date -> sf "weeklynet-%s" date
  | `Sandbox -> "sandbox"

let default_protocol : t -> Protocol.t = function
  | `Mainnet -> ParisC
  | `Ghostnet -> ParisC
  | `Weeklynet _ -> Alpha
  | `Sandbox -> Alpha

let public_rpc_endpoint testnet =
  Endpoint.
    {
      scheme = "https";
      host =
        (match testnet with
        | `Mainnet -> "rpc.tzbeta.net"
        | `Ghostnet -> "rpc.ghostnet.teztnets.com"
        | `Weeklynet date -> sf "rpc.weeklynet-%s.teztnets.com" date);
      port = 443;
    }

let snapshot_service = function
  | `Mainnet -> "https://snapshots.eu.tzinit.org/mainnet"
  | `Ghostnet -> "https://snapshots.eu.tzinit.org/ghostnet"
  | `Weeklynet _ -> "https://snapshots.eu.tzinit.org/weeklynet"

(* Argument to give to the --network option of `octez-node config init`. *)
let to_octez_network_options = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Weeklynet date -> sf "https://teztnets.com/weeklynet-%s" date

let default_bootstrap = function
  | `Mainnet -> "boot.tzinit.org"
  | `Ghostnet -> "ghostnet.tzinit.org" (* Taken from ghostnet configuration *)
  | `Weeklynet date -> sf "weeklynet-%s.teztnets.com" date

let default_dal_bootstrap = function
  | `Mainnet -> "dalboot.mainnet.tzboot.net"
  | `Ghostnet ->
      "dalboot.ghostnet.tzboot.net" (* Taken from ghostnet configuration *)
  | `Weeklynet date -> sf "dal.weeklynet-%s.teztnets.com" date

let get_level endpoint =
  let* json = RPC_core.call endpoint (RPC.get_chain_block_header_shell ()) in
  JSON.(json |-> "level" |> as_int) |> Lwt.return

let expected_pow = function `Sandbox -> 0. | _ -> 26.

let versions network =
  match network with
  | (`Mainnet | `Ghostnet) as public_network ->
      let uri =
        Format.sprintf
          "https://api.%s.tzkt.io/v1/delegates?limit=5000&active=true"
          (to_string public_network)
      in
      let* output =
        Process.spawn "curl" [uri] |> Process.check_and_read_stdout
      in
      let json_accounts =
        JSON.parse ~origin:"Network.versions" output |> JSON.as_list
      in
      json_accounts |> List.to_seq
      |> Seq.map (fun json_account ->
             JSON.
               ( json_account |-> "address" |> as_string,
                 json_account |-> "software" |-> "version" |> as_string_opt ))
      |> Seq.filter_map (function
             | address, None -> Some (address, "unknown")
             | address, Some version -> Some (address, version))
      |> Hashtbl.of_seq |> Lwt.return
  | `Weeklynet _ ->
      (* No easy way to get this information. *)
      Lwt.return (Hashtbl.create 0)
  | `Sandbox ->
      (* Not sure what to do here since it depends on the docker image. We can figure that out later. *)
      Lwt.return (Hashtbl.create 0)

let delegates ?(accounts = []) network =
  match network with
  | (`Mainnet | `Ghostnet) as network ->
      let uri =
        Format.sprintf
          "https://api.%s.tzkt.io/v1/delegates?limit=5000&active=true"
          (to_string network)
      in
      let* output =
        Process.spawn ~log_output:false "curl" [uri]
        |> Process.check_and_read_stdout
      in
      let json_accounts =
        JSON.parse ~origin:"Network.aliases" output |> JSON.as_list
      in
      json_accounts
      |> List.map (fun json_account ->
             JSON.
               ( json_account |-> "alias" |> as_string_opt,
                 json_account |-> "address" |> as_string,
                 json_account |-> "publicKey" |> as_string ))
      |> Lwt.return
  | `Weeklynet _ ->
      (* There is no aliases for weeklynet. *)
      Lwt.return_nil
  | `Sandbox ->
      accounts
      |> List.map (fun account ->
             ( Some account.Account.alias,
               account.public_key_hash,
               account.public_key ))
      |> Lwt.return

let aliases ?accounts network =
  let* aliases = delegates ?accounts network in
  aliases |> List.to_seq
  |> Seq.filter_map (function
         | None, _, _ -> None
         | Some alias, address, _ -> Some (address, alias))
  |> Hashtbl.of_seq |> Lwt.return
