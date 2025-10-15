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

module OpenAPI = struct
  open Tezos_openapi

  let version =
    Format.sprintf
      "fa-bridge-watchtower.%s (%s)@."
      Tezos_version_value.Current_git_info.abbreviated_commit_hash
      Tezos_version_value.Current_git_info.committer_date

  let openapi = String.Hashtbl.create 3

  let openapi_env = ref Convert.empty_env

  let update_env env = openapi_env := Convert.merge_envs !openapi_env env

  let service ~description ?query ?input_encoding output_encoding =
    let request_body =
      match input_encoding with
      | None -> None
      | Some e ->
          let env, schema =
            Convert.convert_schema
              (Json.annotate
                 ~origin:""
                 (Json_schema.to_json (Data_encoding.Json.schema e)))
          in
          update_env env ;
          Some schema
    in
    let env, ok_response =
      Convert.convert_schema
        (Json.annotate
           ~origin:""
           (Json_schema.to_json (Data_encoding.Json.schema output_encoding)))
    in
    update_env env ;
    let responses =
      [
        Openapi.Response.make ~code:200 ~description:"success" ok_response;
        Openapi.Response.make
          ~code:400
          ~description:"Bad request"
          (Openapi.Schema.string
             ~title:"Invalid body"
             ~description:"Invalid data in body"
             ());
        Openapi.Response.make
          ~code:400
          ~description:"Bad JSON in request"
          (Openapi.Schema.string
             ~title:"Invalid JSON in body"
             ~description:"Invalid JSON in body"
             ());
        Openapi.Response.make
          ~code:500
          ~description:"Internal server error"
          (Openapi.Schema.any ~title:"error" ~description:"Server error" ());
      ]
    in
    Openapi.Service.make ~description ?query ?request_body responses

  let method_ = function
    | `GET -> Method.GET
    | `POST -> POST
    | `PUT -> PUT
    | `DELETE -> DELETE
    | `PATCH -> PATCH

  let register meth path ~description ?query ?input_encoding output_encoding =
    let service = service ~description ?query ?input_encoding output_encoding in
    let endpoint =
      match String.Hashtbl.find openapi path with
      | None ->
          let path =
            String.split_no_empty '/' path |> List.map Openapi.Path.static
          in
          Openapi.Endpoint.{path; methods = []}
      | Some e -> e
    in
    let meth = method_ meth in
    let endpoint =
      Openapi.Endpoint.
        {
          endpoint with
          methods =
            (meth, service) :: Stdlib.List.remove_assoc meth endpoint.methods;
        }
    in
    String.Hashtbl.replace openapi path endpoint

  let get () =
    let definitions = Convert.String_map.bindings !openapi_env in
    Openapi.make
      ~title:"FA bridge watchtower"
      ~version
      ~definitions
      (String.Hashtbl.to_seq_values openapi |> List.of_seq)
end

let dream_meth = function
  | `GET -> Dream.get
  | `POST -> Dream.post
  | `PUT -> Dream.put
  | `DELETE -> Dream.delete
  | `PATCH -> Dream.patch

let register meth path ~description ?query ?input_encoding
    ?include_default_fields output_encoding handler =
  let open Lwt_syntax in
  let route ctx =
    dream_meth meth path @@ fun message ->
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
  routes := route :: !routes ;
  OpenAPI.register meth path ~description ?query ?input_encoding output_encoding

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
             }
         ->
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
             removed )
         ->
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
  register
    `GET
    "/health"
    Data_encoding.unit
    ~description:"Health endpoint to check watchtower is running"
  @@ fun _ _ _ -> Lwt_result_syntax.return_unit

let () =
  register
    `GET
    "/version"
    Data_encoding.(
      obj2
        (req
           "commit"
           string
           ~description:"Git commit hash of the watchtower binary")
        (req
           "date"
           string
           ~description:"Date of the git commit of the watchtower binary"))
    ~description:"Version of the FA bridge watchtower (git commit)"
  @@ fun _ _ _ ->
  let open Tezos_version_value.Current_git_info in
  Lwt_result.return (abbreviated_commit_hash, committer_date)

let json_remove_path p json = Ezjsonm.update json p None

let () =
  register
    `GET
    "/config"
    ~include_default_fields:`Always
    (Data_encoding.conv_with_guard
       ~schema:(Data_encoding.Json.schema Config.encoding)
       (fun config ->
         Data_encoding.Json.construct
           ~include_default_fields:`Always
           Config.encoding
           config
         |> json_remove_path ["secret_key"]
         |> json_remove_path ["evm_node_endpoint"])
       (fun _ -> Error "Config read only encoding")
       Data_encoding.json)
    ~description:"Retrieve configuration of watchtower"
  @@ fun _ _ ctx ->
  Lwt_result_syntax.return
    {
      ctx.config with
      secret_key =
        None
        (* erase secret key from output, although it's removed from the encoding
           above, this is a safety measure in case it changes. *);
    }

let () =
  register
    `GET
    "/unclaimed"
    Data_encoding.(list (merge_objs Encodings.deposit Encodings.log_info))
    ~description:"List unclaimed deposits"
    ~query:
      (* TODO: query parameters are reported by hand for now *)
      [
        {
          parameter =
            {
              name = "ignore_whitelist";
              description = Some "List deposits independently of whitelist";
              schema = Tezos_openapi.Openapi.Schema.boolean ();
            };
          required = false;
        };
      ]
  @@ fun req _ ctx ->
  let open Lwt_result_syntax in
  let ignore_whitelist =
    Dream.query req "ignore_whitelist"
    |> Option.map bool_of_string
    |> Option.value ~default:false
  in
  let* deposits =
    if ignore_whitelist then
      Db.Deposits.get_unclaimed_full_ignore_whitelist ctx.db
    else Db.Deposits.get_unclaimed_full ctx.db
  in
  List.map_es
    (fun (deposit, log) ->
      let+ deposit = deposit_with_token_info ctx deposit in
      (deposit, log))
    deposits

let () =
  register
    `GET
    "/deposits"
    (Data_encoding.list Encodings.deposit_log)
    ~description:"List all deposits"
    ~query:
      (* TODO: query parameters are reported by hand for now *)
      [
        {
          parameter =
            {
              name = "limit";
              description = Some "Number of deposits to retrieve";
              schema = Tezos_openapi.Openapi.Schema.integer ~minimum:0 ();
            };
          required = false;
        };
        {
          parameter =
            {
              name = "offset";
              description = Some "Offset number for deposits to retrieve";
              schema = Tezos_openapi.Openapi.Schema.integer ~minimum:0 ();
            };
          required = false;
        };
        {
          parameter =
            {
              name = "receiver";
              description = Some "Filter deposits by receiver address";
              schema =
                Tezos_openapi.Openapi.Schema.string ~description:"address" ();
            };
          required = false;
        };
      ]
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
  let ignore_whitelist =
    Dream.query req "ignore_whitelist"
    |> Option.map bool_of_string
    |> Option.value ~default:false
  in
  let* deposits =
    match (receiver, ignore_whitelist) with
    | None, false -> Db.Deposits.list ctx.db ~limit ~offset
    | Some receiver, false ->
        Db.Deposits.list_by_receiver ctx.db receiver ~limit ~offset
    | None, true -> Db.Deposits.list_ignore_whitelist ctx.db ~limit ~offset
    | Some receiver, true ->
        Db.Deposits.list_by_receiver_ignore_whitelist
          ctx.db
          receiver
          ~limit
          ~offset
  in
  List.map_es
    (fun Db.{deposit; log_info; claimed} ->
      let+ deposit = deposit_with_token_info ctx deposit in
      ((deposit, log_info), claimed))
    deposits

let () =
  register
    `GET
    "/last_seen_block"
    Encodings.quantity_hum_encoding
    ~description:"Retrieve last Etherlink block number seen by the watchtower"
  @@ fun _req _ ctx -> Db.Pointers.L2_head.get ctx.db

let () =
  register
    `GET
    "/openapi"
    Data_encoding.json
    ~description:"OpenAPI specification for watchtower API"
  @@ fun _ _ _ ->
  let open Lwt_result_syntax in
  let openapi = OpenAPI.get () in
  return (Tezos_openapi.Openapi.to_json openapi)
