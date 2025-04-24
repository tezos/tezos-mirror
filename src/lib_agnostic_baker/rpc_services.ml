(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Cohttp_lwt_unix
open Errors

(* RPC helper functions *)

let request_uri ~node_addr ~uri =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! resp = Client.get (Uri.of_string uri) in
      return resp)
    (function
      | Unix.(Unix_error (ECONNREFUSED, _, _)) ->
          tzfail (Cannot_connect_to_node node_addr)
      | e -> raise e)

(** [call_and_wrap_rpc ~node_addr ~uri ~f] makes the RPC call given
    by the [~uri] against [~node_addr], and in case of a well-formed
    response, it applies [~f] to it. *)
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

(* Field extraction helpers *)

(** [get_field ~name json] extracts the field [~name] from the JSON object [json].
    It fails if [json] is not an object or if the field is missing. *)
let get_field ~name =
  let open Lwt_result_syntax in
  function
  | `O fields -> (
      match List.assoc_opt ~equal:( = ) name fields with
      | None -> tzfail (Cannot_decode_node_data ("missing field " ^ name))
      | Some v -> return v)
  | _ -> tzfail (Cannot_decode_node_data "not an object")

(** [get_int_field ~name json] extracts an integer field named [~name] from [json]. *)
let get_int_field ~name json =
  let open Lwt_result_syntax in
  let+ v = get_field ~name json in
  Ezjsonm.get_int v

(** [get_string_field ~name json] extracts a string field named [~name] from [json]. *)
let get_string_field ~name json =
  let open Lwt_result_syntax in
  let+ v = get_field ~name json in
  Ezjsonm.get_string v

(* RPC specific functions *)

let get_level ~node_addr =
  let uri =
    Format.sprintf "%s/chains/main/blocks/head/header/shell" node_addr
  in
  call_and_wrap_rpc ~node_addr ~uri ~f:(get_int_field ~name:"level")

let get_block_hash ~node_addr =
  let open Lwt_result_syntax in
  let uri = Format.sprintf "%s/chains/main/blocks/head/header" node_addr in
  call_and_wrap_rpc ~node_addr ~uri ~f:(fun json ->
      let+ block_hash = get_string_field ~name:"hash" json in
      Block_hash.of_b58check_exn block_hash)

let get_next_protocol_hash ~node_addr =
  let open Lwt_result_syntax in
  let uri = Format.sprintf "%s/chains/main/blocks/head/metadata" node_addr in
  call_and_wrap_rpc ~node_addr ~uri ~f:(fun json ->
      let+ next_protocol = get_string_field ~name:"next_protocol" json in
      Protocol_hash.of_b58check_exn next_protocol)

let get_current_period ~node_addr =
  let open Lwt_result_syntax in
  let uri =
    Format.sprintf "%s/chains/main/blocks/head/votes/current_period" node_addr
  in
  call_and_wrap_rpc ~node_addr ~uri ~f:(fun json ->
      let* voting_period = get_field ~name:"voting_period" json in
      let* kind = get_string_field ~name:"kind" voting_period in
      let+ remaining = get_int_field ~name:"remaining" json in
      (kind, remaining))
