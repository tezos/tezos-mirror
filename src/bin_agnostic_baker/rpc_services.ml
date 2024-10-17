(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)
type error +=
  | Cannot_connect_to_node of string
  | Cannot_decode_node_data of string

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_connect_to_node"
    ~title:"Cannot connect to node"
    ~description:"Cannot connect to node."
    ~pp:(fun ppf uri ->
      Format.fprintf
        ppf
        "Cannot connect to node. Connection refused (ECONNREFUSED): %s"
        uri)
    Data_encoding.(obj1 (req "uri" string))
    (function Cannot_connect_to_node uri -> Some uri | _ -> None)
    (fun uri -> Cannot_connect_to_node uri) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_decode_node_data"
    ~title:"Cannot decode node data"
    ~description:"Cannot decode node data."
    ~pp:(fun ppf err -> Format.fprintf ppf "Cannot decode node data: %s" err)
    Data_encoding.(obj1 (req "err" string))
    (function Cannot_decode_node_data err -> Some err | _ -> None)
    (fun err -> Cannot_decode_node_data err)

open Cohttp_lwt_unix

let request_uri ~node_addr ~uri =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! r = Client.get (Uri.of_string uri) in
      return r)
    (function
      | Unix.(Unix_error (ECONNREFUSED, _, _)) ->
          tzfail (Cannot_connect_to_node node_addr)
      | e -> raise e)

let call_and_wrap_rpc ~node_addr ~uri ~f =
  let open Lwt_result_syntax in
  let* resp, body = request_uri ~node_addr ~uri in
  let*! body_str = Cohttp_lwt.Body.to_string body in
  match resp.status with
  | `OK ->
      let* json =
        match Data_encoding.Json.from_string body_str with
        | Ok json -> return json
        | Error e -> tzfail (Cannot_decode_node_data e)
      in
      f json
  | #Cohttp.Code.status_code ->
      let*! () =
        Lwt_fmt.printf
          "Cannot fetch from node %s. Response status code %d\n%!"
          uri
          (Cohttp.Code.code_of_status resp.status)
      in
      raise Not_found

let get_next_protocol_hash ~node_addr =
  let open Lwt_result_syntax in
  let f json =
    (* Next_protocol hash field in the RPC result *)
    let name = "next_protocol" in
    let* v =
      match json with
      | `O fields -> (
          match List.assoc_opt ~equal:( = ) name fields with
          | None -> tzfail (Cannot_decode_node_data ("missing field " ^ name))
          | Some node -> return node)
      | _ -> tzfail (Cannot_decode_node_data "not an object")
    in
    let hash = Protocol_hash.of_b58check_exn (Ezjsonm.get_string v) in
    return hash
  in
  let uri = Format.sprintf "%s/chains/main/blocks/head/metadata" node_addr in
  call_and_wrap_rpc ~node_addr ~uri ~f

let get_current_proposal ~node_addr =
  let open Lwt_result_syntax in
  let f json =
    match json with
    | `Null -> return_none
    | `String s -> return_some @@ Protocol_hash.of_b58check_exn s
    | _ -> tzfail (Cannot_decode_node_data "not an object")
  in
  let uri =
    Format.sprintf "%s/chains/main/blocks/head/votes/current_proposal" node_addr
  in
  call_and_wrap_rpc ~node_addr ~uri ~f

let get_current_period ~node_addr =
  let open Lwt_result_syntax in
  let voting_period_field = "voting_period" in
  let kind_field = "kind" in
  let remaining_field = "remaining" in
  let f json =
    let* kind =
      match json with
      | `O fields -> (
          match List.assoc_opt ~equal:( = ) voting_period_field fields with
          | None ->
              tzfail
                (Cannot_decode_node_data ("missing field " ^ voting_period_field))
          | Some node -> (
              match node with
              | `O fields -> (
                  match List.assoc_opt ~equal:( = ) kind_field fields with
                  | None ->
                      tzfail
                        (Cannot_decode_node_data
                           ("missing field " ^ voting_period_field))
                  | Some node -> return @@ Ezjsonm.get_string node)
              | _ -> tzfail (Cannot_decode_node_data "not an object")))
      | _ -> tzfail (Cannot_decode_node_data "not an object")
    in
    let* remaining =
      match json with
      | `O fields -> (
          match List.assoc_opt ~equal:( = ) remaining_field fields with
          | None ->
              tzfail
                (Cannot_decode_node_data ("missing field " ^ remaining_field))
          | Some node -> return @@ Ezjsonm.get_int node)
      | _ -> tzfail (Cannot_decode_node_data "not an object")
    in
    return (kind, remaining)
  in
  let uri =
    Format.sprintf "%s/chains/main/blocks/head/votes/current_period" node_addr
  in
  call_and_wrap_rpc ~node_addr ~uri ~f
