(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

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

let register meth path ?input_encoding output_encoding handler =
  let open Lwt_syntax in
  let route db =
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
      let*! res = handler input db in
      let* res =
        match res with
        | Error e -> fail (`Handler_error e)
        | Ok res -> return res
      in
      let resp_json = Data_encoding.Json.construct output_encoding res in
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

let start db Config.{addr; port} =
  let open Lwt_syntax in
  let stop, resolve_stop = Lwt.wait () in
  let shutdown () =
    Lwt.wakeup_later resolve_stop () ;
    Lwt.return_unit
  in
  Lwt.dont_wait
    (fun () ->
      make_routes db |> Dream.router |> Dream.serve ~interface:addr ~port ~stop)
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
          Logs.err (fun m ->
              m "Cannot start RPC server on port %d, already in use." port) ;
          exit 1
      | exn -> Event.rpc_server_error (Printexc.to_string exn)) ;
  let* () = Event.rpc_server_started (addr, port) in
  return shutdown

let () =
  register Dream.get "/health" Data_encoding.unit @@ fun _ _ ->
  Lwt_result_syntax.return_unit
