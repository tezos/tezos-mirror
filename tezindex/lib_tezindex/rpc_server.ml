(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type rpc_server_kind = Local_rpc_server of RPC_server.server | No_server

let health_service =
  Tezos_rpc.Service.get_service
    ~description:"Returns whether the tezindex service is running."
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.string
    Tezos_rpc.Path.(open_root / "health")

let build_rpc_directory _cctxt =
  let dir = Tezos_rpc.Directory.empty in
  let dir =
    Tezos_rpc.Directory.register dir health_service (fun () () () ->
        Lwt.return_ok "ok")
  in
  dir

let init _cctxt (config : Config.config) =
  let open Lwt_result_syntax in
  let dir = build_rpc_directory () in
  let dir =
    Tezos_rpc.Directory.register_describe_directory_service
      dir
      Tezos_rpc.Service.description_service
  in
  match config.rpc_addr with
  | None -> return No_server
  | Some uri ->
      let host = Option.value ~default:"localhost" (Uri.host uri) in
      let port = Option.value ~default:8733 (Uri.port uri) in
      let acl = RPC_server.Acl.allow_all in
      let server =
        RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
      in
      let mode = `TCP (`Port port) in
      let*! () =
        RPC_server.launch
          ~host
          server
          ~callback:(RPC_server.resto_callback server)
          mode
      in
      return (Local_rpc_server server)

let shutdown = function
  | No_server -> Lwt.return_unit
  | Local_rpc_server server -> RPC_server.shutdown server
