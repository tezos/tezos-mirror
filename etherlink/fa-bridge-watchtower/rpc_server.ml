(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type ctx = {
  config : Config.t;
  db : Db.t;
  mutable ws_client : Websocket_client.t option;
}

module Event = struct
  include Internal_event.Simple

  let section = ["fa_bridge_watchtower"; "rpc"]

  let rpc_server_started =
    emit
    @@ declare_2
         ~section
         ~name:"rpc_server_started"
         ~msg:"RPC server listening on {addr}:{port}"
         ~level:Notice
         ("addr", Data_encoding.string)
         ("port", Data_encoding.int31)

  let rpc_server_error =
    emit__dont_wait__use_with_care
    @@ declare_1
         ~section
         ~name:"rpc_server_error"
         ~msg:"RPC server error: {error}"
         ~level:Error
         ("error", Data_encoding.string)
end

let routes = ref []

let register meth path ?input_encoding ?include_default_fields output_encoding
    handler =
  let open Lwt_syntax in
  let route ctx =
    meth path @@ fun message ->
    let* response =
      let open Lwt_result_syntax in
      let* input =
        match input_encoding with
        | None -> return_none
        | Some input_encoding ->
            let*! body = Dream.body message in
            let*? json =
              match Ezjsonm.value_from_string_result body with
              | Ok json -> Ok json
              | Error e ->
                  Error (`Invalid_json (Ezjsonm.read_error_description e))
            in
            let*? input =
              try Ok (Data_encoding.Json.destruct input_encoding json)
              with _ -> Error `Invalid_body
            in
            return_some input
      in
      let*! res = protect @@ fun () -> handler message input ctx in
      let* res =
        match res with
        | Error e -> fail (`Handler_error e)
        | Ok res -> return res
      in
      let resp_json =
        Data_encoding.Json.construct ?include_default_fields output_encoding res
      in
      return (Ezjsonm.value_to_string resp_json)
    in
    match response with
    | Ok response -> Dream.json (response ^ "\n")
    | Error `Invalid_body ->
        Dream.respond ~status:`Bad_Request "Invalid data in body\n"
    | Error (`Invalid_json e) ->
        Dream.respond
          ~status:`Bad_Request
          (Format.sprintf "Invalid JSON in body: %s\n" e)
    | Error (`Handler_error e) ->
        Dream.respond
          ~status:`Internal_Server_Error
          (Format.asprintf "%a\n" pp_print_trace e)
  in
  routes := route :: !routes

let make_routes db = List.rev_map (fun f -> f db) !routes

let start db config Config.{addr; port} =
  let open Lwt_syntax in
  let ctx = {config; db; ws_client = None} in
  let notify_ws_change ws = ctx.ws_client <- Some ws in
  Lwt.dont_wait
    (fun () ->
      make_routes ctx |> Dream.router |> Dream.serve ~interface:addr ~port)
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
          Logs.err (fun m ->
              m "Cannot start RPC server on port %d, already in use." port) ;
          exit 1
      | exn -> Event.rpc_server_error (Printexc.to_string exn)) ;
  let* () = Event.rpc_server_started (addr, port) in
  return notify_ws_change

module Encodings = struct
  open Data_encoding

  let quantity_hum_encoding =
    union
      [
        case
          Json_only
          ~title:"int"
          int31
          (fun (Ethereum_types.Qty z) ->
            if Z.fits_int z then Some (Z.to_int z) else None)
          (fun i -> Ethereum_types.quantity_of_z (Z.of_int i));
        case
          Json_only
          ~title:"z"
          z
          (fun (Ethereum_types.Qty z) -> Some z)
          (fun z -> Ethereum_types.quantity_of_z z);
      ]

  type deposit = {
    nonce : Ethereum_types.quantity;
    proxy : Ethereum_types.Address.t;
    ticket_hash : Ethereum_types.hash;
    receiver : Ethereum_types.Address.t;
    amount : float;
    token : string option;
  }

  let deposit =
    conv
      (fun {nonce; proxy; ticket_hash; receiver; amount; token} ->
        (nonce, proxy, ticket_hash, receiver, amount, token))
      (fun (nonce, proxy, ticket_hash, receiver, amount, token) ->
        {nonce; proxy; ticket_hash; receiver; amount; token})
    @@ obj6
         (req "nonce" quantity_hum_encoding)
         (req "proxy" Ethereum_types.address_encoding)
         (req "ticket_hash" Ethereum_types.hash_encoding)
         (req "receiver" Ethereum_types.address_encoding)
         (req "amount" float)
         (opt "token" string)

  let log_info =
    conv
      (fun Db.
             {
               transactionHash;
               transactionIndex;
               logIndex;
               blockHash;
               blockNumber;
               removed;
             } ->
        ( transactionHash,
          transactionIndex,
          logIndex,
          blockHash,
          blockNumber,
          removed ))
      (fun ( transactionHash,
             transactionIndex,
             logIndex,
             blockHash,
             blockNumber,
             removed ) ->
        Db.
          {
            transactionHash;
            transactionIndex;
            logIndex;
            blockHash;
            blockNumber;
            removed;
          })
    @@ obj6
         (req "transactionHash" Ethereum_types.hash_encoding)
         (req "transactionIndex" quantity_hum_encoding)
         (req "logIndex" quantity_hum_encoding)
         (req "blockHash" Ethereum_types.block_hash_encoding)
         (req "blockNumber" quantity_hum_encoding)
         (dft "removed" bool false)

  let execution_info =
    conv
      (fun Db.{transactionHash; transactionIndex; blockHash; blockNumber} ->
        (transactionHash, transactionIndex, blockHash, blockNumber))
      (fun (transactionHash, transactionIndex, blockHash, blockNumber) ->
        Db.{transactionHash; transactionIndex; blockHash; blockNumber})
    @@ obj4
         (req "transactionHash" Ethereum_types.hash_encoding)
         (req "transactionIndex" quantity_hum_encoding)
         (req "blockHash" Ethereum_types.block_hash_encoding)
         (req "blockNumber" quantity_hum_encoding)

  let deposit_log =
    merge_objs
      (merge_objs deposit log_info)
      (obj1 (opt "claimed" execution_info))
end

let deposit_with_token_info ctx Db.{nonce; proxy; ticket_hash; receiver; amount}
    =
  let open Lwt_result_syntax in
  match ctx.ws_client with
  | None ->
      let (Ethereum_types.Qty amount) = amount in
      let amount = Z.to_float amount in
      return
        Encodings.{nonce; proxy; ticket_hash; receiver; amount; token = None}
  | Some ws_client ->
      let+ amount, token = Token_info.get_for_rpc ws_client proxy amount in
      Encodings.{nonce; proxy; ticket_hash; receiver; amount; token}

let () =
  register Dream.get "/health" Data_encoding.unit @@ fun _ _ _ ->
  Lwt_result_syntax.return_unit

let () =
  register Dream.get "/config" ~include_default_fields:`Always Config.encoding
  @@ fun _ _ ctx ->
  Lwt_result_syntax.return
    {ctx.config with secret_key = None (* erase secret key from output *)}

let () =
  register
    Dream.get
    "/unclaimed"
    Data_encoding.(list (merge_objs Encodings.deposit Encodings.log_info))
  @@ fun _ _ ctx ->
  let open Lwt_result_syntax in
  let* deposits = Db.Deposits.get_unclaimed_full ctx.db in
  List.map_es
    (fun (deposit, log) ->
      let+ deposit = deposit_with_token_info ctx deposit in
      (deposit, log))
    deposits

let () =
  register Dream.get "/deposits" (Data_encoding.list Encodings.deposit_log)
  @@ fun req _ ctx ->
  let open Lwt_result_syntax in
  let limit =
    Dream.query req "limit" |> Option.map int_of_string
    |> Option.value ~default:100
  in
  let offset =
    Dream.query req "offset" |> Option.map int_of_string
    |> Option.value ~default:0
  in
  let receiver =
    Dream.query req "receiver" |> Option.map Ethereum_types.Address.of_string
  in
  let* deposits =
    match receiver with
    | None -> Db.Deposits.list ctx.db ~limit ~offset
    | Some receiver ->
        Db.Deposits.list_by_receiver ctx.db receiver ~limit ~offset
  in
  List.map_es
    (fun Db.{deposit; log_info; claimed} ->
      let+ deposit = deposit_with_token_info ctx deposit in
      ((deposit, log_info), claimed))
    deposits
