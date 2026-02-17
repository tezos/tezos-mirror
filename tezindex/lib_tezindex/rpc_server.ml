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

let baker_arg =
  Tezos_rpc.Arg.make
    ~descr:"Baker public key hash"
    ~name:"baker"
    ~destruct:(fun s ->
      match Signature.Public_key_hash.of_b58check_opt s with
      | Some pkh -> Ok pkh
      | None -> Error "invalid baker address")
    ~construct:Signature.Public_key_hash.to_b58check
    ()

let cycle_arg =
  Tezos_rpc.Arg.make
    ~descr:"Cycle number"
    ~name:"cycle"
    ~destruct:(fun s ->
      match Int32.of_string_opt s with
      | Some n -> Ok n
      | None -> Error "invalid cycle number")
    ~construct:Int32.to_string
    ()

let v1_rewards_split_service =
  Tezos_rpc.Service.get_service
    ~description:"Returns baker rewards for a cycle in TzKT-compatible format."
    ~query:Tezos_rpc.Query.empty
    ~output:Split_data.tzkt_baker_rewards_encoding
    Tezos_rpc.Path.(
      open_root / "v1" / "rewards" / "split" /: baker_arg /: cycle_arg)

let query_cycle_rewards pool baker cycle =
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.fold
        Sql_requests.select_cycle_balance_updates
        (fun (category_str, result_str, total) acc ->
          match
            ( Data.Balance_update.category_of_string category_str,
              Data.Balance_update.result_of_string result_str )
          with
          | Some category, Some result -> (category, result, total) :: acc
          | _ -> acc)
        (baker, cycle)
        [])
    pool

let query_cycle_delegators pool baker cycle =
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.fold
        Sql_requests.select_cycle_delegators
        (fun (addr, balance) acc ->
          {
            Split_data.del_address = addr;
            delegated_balance = balance;
            emptied = false;
          }
          :: acc)
        (baker, cycle)
        [])
    pool

let query_num_blocks pool baker cycle =
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find Sql_requests.select_num_blocks (baker, cycle))
    pool

let query_expected_blocks pool baker cycle =
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Sql_requests.select_expected_blocks (baker, cycle))
    pool

let build_rpc_directory pool =
  let dir = Tezos_rpc.Directory.empty in
  let dir =
    Tezos_rpc.Directory.register dir health_service (fun () () () ->
        Lwt.return_ok "ok")
  in
  let dir =
    Tezos_rpc.Directory.register2
      dir
      v1_rewards_split_service
      (fun baker cycle () () ->
        let open Lwt_result_syntax in
        let*! result = query_cycle_rewards pool baker cycle in
        let*! delegators_result = query_cycle_delegators pool baker cycle in
        let*! num_blocks_result = query_num_blocks pool baker cycle in
        let*! expected_blocks_result = query_expected_blocks pool baker cycle in
        match (result, delegators_result) with
        | Ok entries, Ok delegators ->
            let num_blocks =
              match num_blocks_result with
              | Ok n -> Int64.to_int n
              | Error _ -> 0
            in
            let expected_blocks =
              match expected_blocks_result with
              | Ok (Some n) -> Int32.to_int n
              | Ok None | Error _ -> 0
            in
            let rewards =
              Split_data.tzkt_baker_rewards_of_entries
                ~cycle
                ~num_blocks
                ~expected_blocks
                entries
            in
            let baker_b58 = Signature.Public_key_hash.to_b58check baker in
            let own_delegated_balance, external_delegated_balance =
              List.fold_left
                (fun (own, ext) (d : Split_data.tzkt_delegator) ->
                  if String.equal d.del_address baker_b58 then
                    (Int64.add own d.delegated_balance, ext)
                  else (own, Int64.add ext d.delegated_balance))
                (0L, 0L)
                delegators
            in
            return
              {
                rewards with
                tzkt_delegators = delegators;
                delegators_count = List.length delegators;
                own_delegated_balance;
                external_delegated_balance;
              }
        | Error e, _ | _, Error e ->
            let msg = Caqti_error.show e in
            failwith "Database error: %s" msg)
  in
  dir

let init _cctxt (config : Config.config) pool =
  let open Lwt_result_syntax in
  let dir = build_rpc_directory pool in
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
