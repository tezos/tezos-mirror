(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type public =
  [ `Mainnet
  | `Ghostnet
  | `Shadownet
  | `Nextnet of string
  | `Weeklynet of string
  | `Seoulnet
  | `Tallinnnet ]

type t = [`Sandbox | public]

let is_public = function #public -> true | _ -> false

let to_public = function
  | `Sandbox -> failwith "Sandbox is not public"
  | #public as p -> p

let to_string = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Shadownet -> "shadownet"
  | `Nextnet date -> sf "nextnet-%s" date
  | `Weeklynet date -> sf "weeklynet-%s" date
  | `Sandbox -> "sandbox"
  | `Seoulnet -> "seoulnet"
  | `Tallinnnet -> "tallinnnet"

let parse = function
  | "mainnet" -> Some `Mainnet
  | "ghostnet" -> Some `Ghostnet
  | "shadownet" -> Some `Shadownet
  | "seoulnet" -> Some `Seoulnet
  | "tallinnnet" -> Some `Tallinnnet
  | s when String.length s = 20 && String.sub s 0 10 = "weeklynet-" ->
      (* format: weeklynet-2025-01-29 (with dashes) *)
      let date = String.sub s 10 10 in
      Some (`Weeklynet date)
  | s when String.length s = 16 && String.sub s 0 8 = "nextnet-" ->
      (* format: nextnet-20250203 (without dashes) *)
      let date = String.sub s 8 8 in
      Some (`Nextnet date)
  | "sandbox" -> Some `Sandbox
  | _ -> None

let public_encoding =
  let open Data_encoding in
  conv
    (fun (p : public) -> to_string p)
    (fun s ->
      match parse s with
      | Some (#public as p) -> p
      | Some `Sandbox -> invalid_arg "public_encoding: sandbox is not public"
      | None -> invalid_arg ("public_encoding: invalid network: " ^ s))
    string

let encoding =
  let open Data_encoding in
  conv
    to_string
    (fun s ->
      match parse s with
      | Some v -> v
      | None -> invalid_arg ("Network.encoding: invalid network: " ^ s))
    string

let default_protocol : t -> Protocol.t = function
  | `Mainnet -> S023
  | `Ghostnet -> S023
  | `Shadownet -> S023
  | `Weeklynet _ -> Alpha
  | `Sandbox -> Alpha
  | `Nextnet _ -> T024
  | `Seoulnet -> S023
  | `Tallinnnet -> T024

let block_time : t -> int = function
  | `Mainnet -> 8
  | `Shadownet -> 8
  | `Ghostnet -> 4
  | `Seoulnet -> 4
  | `Tallinnnet -> 4
  | network ->
      failwith
        (Format.sprintf
           "Block time not available for this network: %s."
           (to_string network))

let next_protocol : t -> Protocol.t = function
  | `Mainnet | `Ghostnet | `Shadownet | `Seoulnet -> T024
  | _ -> Alpha

let public_rpc_endpoint testnet =
  Endpoint.make
    ~scheme:"https"
    ~host:
      (match testnet with
      | `Mainnet -> "rpc.tzbeta.net"
      | `Ghostnet -> "rpc.ghostnet.teztnets.com"
      | `Shadownet -> "rpc.shadownet.teztnets.com"
      | `Nextnet date -> sf "rpc.nextnet-%s.teztnets.com" date
      | `Weeklynet date -> sf "rpc.weeklynet-%s.teztnets.com" date
      | `Seoulnet -> "rpc.seoulnet.teztnets.com"
      | `Tallinnnet -> "rpc.tallinnnet.teztnets.com")
    ~port:443
    ()

let snapshot_service = function
  | `Mainnet -> "https://snapshots.eu.tzinit.org/mainnet"
  | `Ghostnet -> "https://snapshots.eu.tzinit.org/ghostnet"
  | `Shadownet -> "https://snapshots.tzinit.org/shadownet"
  | `Nextnet _ -> "https://snapshots.eu.tzinit.org/nextnet"
  | `Weeklynet _ -> "https://snapshots.eu.tzinit.org/weeklynet"
  | `Seoulnet -> "https://snapshots.eu.tzinit.org/seoulnet"
  | `Tallinnnet -> "https://snapshots.tzinit.org/tallinnnet"

(* Argument to give to the --network option of `octez-node config init`. *)
let to_octez_network_options = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Sandbox -> "sandbox"
  | `Shadownet -> "https://teztnets.com/shadownet"
  | `Nextnet date -> sf "https://teztnets.com/nextnet-%s" date
  | `Weeklynet date -> sf "https://teztnets.com/weeklynet-%s" date
  | `Seoulnet -> "https://teztnets.com/seoulnet"
  | `Tallinnnet -> "https://teztnets.com/tallinnnet"

let default_bootstrap = function
  | `Mainnet -> "boot.tzinit.org"
  | `Ghostnet -> "ghostnet.tzinit.org" (* Taken from ghostnet configuration *)
  | `Shadownet -> "shadownet.teztnets.com"
  | `Nextnet date -> sf "nextnet-%s.teztnets.com" date
  | `Weeklynet date -> sf "weeklynet-%s.teztnets.com" date
  | `Seoulnet -> "seoulnet.teztnets.com"
  | `Tallinnnet -> "tallinnnet.teztnets.com"

let default_dal_bootstrap = function
  | `Mainnet -> "dalboot.mainnet.tzboot.net"
  | `Ghostnet ->
      "dalboot.ghostnet.tzboot.net" (* Taken from ghostnet configuration *)
  | `Shadownet -> "dal.shadownet.teztnets.com"
  | `Nextnet date -> sf "dal.nextnet-%s.teztnets.com" date
  | `Weeklynet date -> sf "dal.weeklynet-%s.teztnets.com" date
  | `Seoulnet -> "dal.seoulnet.teztnets.com"
  | `Tallinnnet -> "dal.tallinnnet.teztnets.com"

let get_level endpoint =
  let* json = RPC_core.call endpoint (RPC.get_chain_block_header_shell ()) in
  JSON.(json |-> "level" |> as_int) |> Lwt.return

let expected_pow = function `Sandbox -> 0. | _ -> 26.

let versions network =
  match network with
  | (`Mainnet | `Ghostnet | `Shadownet | `Seoulnet | `Tallinnnet) as
    public_network -> (
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
  | `Weeklynet _ | `Nextnet _ ->
      (* No easy way to get this information. *)
      Lwt.return_some (Hashtbl.create 0)
  | `Sandbox ->
      (* Not sure what to do here since it depends on the docker image. We can
         figure that out later. *)
      Lwt.return_some (Hashtbl.create 0)

let delegates ?(accounts = []) network =
  match network with
  | (`Mainnet | `Ghostnet | `Shadownet | `Seoulnet | `Tallinnnet) as network
    -> (
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
