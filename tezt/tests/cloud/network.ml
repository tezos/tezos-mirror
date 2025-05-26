(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type public =
  [`Mainnet | `Ghostnet | `Nextnet of string | `Weeklynet of string | `Rionet]

type t = [public | `Sandbox]

let to_public = function
  | `Sandbox -> failwith "Sandbox is not public"
  | #public as p -> p

let to_string = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Nextnet _ -> "nextnet"
  | `Weeklynet date -> sf "weeklynet-%s" date
  | `Sandbox -> "sandbox"
  | `Rionet -> "rionet"

let default_protocol : t -> Protocol.t = function
  | `Mainnet -> R022
  | `Ghostnet -> R022
  | `Weeklynet _ -> Alpha
  | `Sandbox -> Alpha
  | `Nextnet _ -> failwith "Next is not active"
  | `Rionet -> R022

let public_rpc_endpoint testnet =
  Endpoint.make
    ~scheme:"https"
    ~host:
      (match testnet with
      | `Mainnet -> "rpc.tzbeta.net"
      | `Ghostnet -> "rpc.ghostnet.teztnets.com"
      | `Nextnet date -> sf "rpc.nextnet-%s.teztnets.com" date
      | `Weeklynet date -> sf "rpc.weeklynet-%s.teztnets.com" date
      | `Rionet -> "rpc.rionet.teztnets.com")
    ~port:443
    ()

let snapshot_service = function
  | `Mainnet -> "https://snapshots.eu.tzinit.org/mainnet"
  | `Ghostnet -> "https://snapshots.eu.tzinit.org/ghostnet"
  | `Nextnet _ -> "https://snapshots.eu.tzinit.org/nextnet"
  | `Weeklynet _ -> "https://snapshots.eu.tzinit.org/weeklynet"
  | `Rionet -> "https://snapshots.eu.tzinit.org/rionet"

(* Argument to give to the --network option of `octez-node config init`. *)
let to_octez_network_options = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Nextnet date -> sf "https://teztnets.com/nextnet-%s" date
  | `Weeklynet date -> sf "https://teztnets.com/weeklynet-%s" date
  | `Rionet -> "https://teztnets.com/rionet"

let default_bootstrap = function
  | `Mainnet -> "boot.tzinit.org"
  | `Ghostnet -> "ghostnet.tzinit.org" (* Taken from ghostnet configuration *)
  | `Nextnet date -> sf "nextnet-%s.teztnets.com" date
  | `Weeklynet date -> sf "weeklynet-%s.teztnets.com" date
  | `Rionet -> "rionet.teztnets.com"

let default_dal_bootstrap = function
  | `Mainnet -> "dalboot.mainnet.tzboot.net"
  | `Ghostnet ->
      "dalboot.ghostnet.tzboot.net" (* Taken from ghostnet configuration *)
  | `Nextnet date -> sf "dal.nextnet-%s.teztnets.com" date
  | `Weeklynet date -> sf "dal.weeklynet-%s.teztnets.com" date
  | `Rionet -> "dal.rionet.teztnets.com"

let get_level endpoint =
  let* json = RPC_core.call endpoint (RPC.get_chain_block_header_shell ()) in
  JSON.(json |-> "level" |> as_int) |> Lwt.return

let expected_pow = function `Sandbox -> 0. | _ -> 26.

let versions network =
  match network with
  | (`Mainnet | `Nextnet _ | `Ghostnet) as public_network -> (
      let decoder json =
        json |> JSON.as_list |> List.to_seq
        |> Seq.map (fun json_account ->
               JSON.
                 ( json_account |-> "address" |> as_string,
                   json_account |-> "software" |-> "version" |> as_string_opt ))
        |> Seq.filter_map (function
               | address, None -> Some (address, "unknown")
               | address, Some version -> Some (address, version))
        |> Hashtbl.of_seq
      in
      let rpc =
        RPC_core.(
          make
            ~query_string:[("limit", "5000"); ("active", "true")]
            GET
            ["v1"; "delegates"]
            decoder)
      in
      let endpoint =
        Endpoint.make
          ~host:(Format.asprintf "api.%s.tzkt.io" (to_string public_network))
          ~scheme:"https"
          ~port:443
          ()
      in
      let* response = RPC_core.call_raw endpoint rpc in
      try
        RPC_core.decode_raw ~origin:"Network.versions" rpc response.body
        |> Lwt.return_some
      with exn ->
        Log.warn
          "Unexpected error while fetching versions (code: %d): '%s'"
          response.code
          (Printexc.to_string exn) ;
        Lwt.return_none)
  | `Weeklynet _ | `Rionet ->
      (* No easy way to get this information. *)
      Lwt.return_some (Hashtbl.create 0)
  | `Sandbox ->
      (* Not sure what to do here since it depends on the docker image. We can
         figure that out later. *)
      Lwt.return_some (Hashtbl.create 0)

let delegates ?(accounts = []) network =
  match network with
  | (`Mainnet | `Ghostnet | `Rionet) as network -> (
      let decoder json =
        json |> JSON.as_list
        |> List.map (fun json_account ->
               JSON.
                 ( json_account |-> "alias" |> as_string_opt,
                   json_account |-> "address" |> as_string,
                   json_account |-> "publicKey" |> as_string ))
      in
      let rpc =
        RPC_core.(
          make
            ~query_string:[("limit", "5000"); ("active", "true")]
            GET
            ["v1"; "delegates"]
            decoder)
      in
      let endpoint =
        Endpoint.make
          ~host:(Format.asprintf "api.%s.tzkt.io" (to_string network))
          ~scheme:"https"
          ~port:443
          ()
      in
      let* response = RPC_core.call_raw endpoint rpc in
      try
        RPC_core.decode_raw ~origin:"Network.aliases" rpc response.body
        |> Lwt.return_some
      with exn ->
        Log.warn
          "Unexpected error while fetching aliases (code: %d): '%s'"
          response.code
          (Printexc.to_string exn) ;
        Lwt.return_none)
  | `Weeklynet _ | `Nextnet _ ->
      (* There are no aliases for these networks. *)
      Lwt.return_none
  | `Sandbox ->
      accounts
      |> List.map (fun account ->
             ( Some account.Account.alias,
               account.public_key_hash,
               account.public_key ))
      |> Lwt.return_some

let aliases ?accounts network =
  let* aliases = delegates ?accounts network in
  match aliases with
  | None -> Lwt.return_none
  | Some aliases ->
      aliases |> List.to_seq
      |> Seq.filter_map (function
             | None, _, _ -> None
             | Some alias, address, _ -> Some (address, alias))
      |> Hashtbl.of_seq |> Lwt.return_some
