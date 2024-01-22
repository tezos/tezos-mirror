(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <marcin.pastudzki@tqtezos.com> *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type cors = Resto_cohttp.Cors.t = {
  allowed_headers : string list;
  allowed_origins : string list;
}

module RPC_logging = struct
  open Internal_event
  open Internal_event.Simple

  let rpc_http_event name level =
    declare_1
      ~section:["rpc_server"]
      ~name
      ~msg:"{msg}"
      ~level
      ~pp1:Format.pp_print_text
      ("msg", Data_encoding.string)

  let rpc_http_event_debug = rpc_http_event "rpc_http_event_debug" Debug

  let rpc_http_event_info = rpc_http_event "rpc_http_event_info" Info

  let rpc_http_event_notice = rpc_http_event "rpc_http_event_notice" Notice

  let rpc_http_event_warning = rpc_http_event "rpc_http_event_warning" Warning

  let rpc_http_event_error = rpc_http_event "rpc_http_event_error" Error

  let emit_async event fmt =
    Format.kasprintf (fun message -> Lwt.ignore_result (emit event message)) fmt

  let emit_lwt event fmt =
    Format.kasprintf (fun message -> emit event message) fmt

  let debug f = emit_async rpc_http_event_debug f

  let log_info f = emit_async rpc_http_event_info f

  let log_notice f = emit_async rpc_http_event_notice f

  let warn f = emit_async rpc_http_event_warning f

  let log_error f = emit_async rpc_http_event_error f

  let lwt_debug f = emit_lwt rpc_http_event_debug f

  let lwt_log_info f = emit_lwt rpc_http_event_info f

  let lwt_log_notice f = emit_lwt rpc_http_event_notice f

  let lwt_warn f = emit_lwt rpc_http_event_warning f

  let lwt_log_error f = emit_lwt rpc_http_event_error f
end

include Resto_cohttp_server.Server.Make (Tezos_rpc.Encoding) (RPC_logging)

module Acl = struct
  include Resto_acl.Acl

  type endpoint = P2p_point.Id.addr_port_id

  type policy = (endpoint * t) list

  let secure =
    Deny_all
      {
        except =
          List.map
            parse
            [
              "GET /chains/*/blocks";
              "GET /chains/*/blocks/*";
              "GET /chains/*/blocks/*/context/**";
              "GET /chains/*/blocks/*/hash";
              "GET /chains/*/blocks/*/header";
              "GET /chains/*/blocks/*/header/**";
              "GET /chains/*/blocks/*/helpers/current_level";
              "GET /chains/*/blocks/*/live_blocks";
              "GET /chains/*/blocks/*/metadata";
              "GET /chains/*/blocks/*/metadata_hash";
              "GET /chains/*/blocks/*/minimal_valid_time";
              "GET /chains/*/blocks/*/operation_hashes";
              "GET /chains/*/blocks/*/operation_hashes/**";
              "GET /chains/*/blocks/*/operation_metadata_hash";
              "GET /chains/*/blocks/*/operations";
              "GET /chains/*/blocks/*/operations/**";
              "GET /chains/*/blocks/*/operations_metadata_hash";
              "GET /chains/*/blocks/*/protocols";
              "GET /chains/*/blocks/*/votes/**";
              "GET /chains/*/chain_id";
              "GET /chains/*/checkpoint";
              "GET /chains/*/invalid_blocks";
              "GET /chains/*/invalid_blocks/*";
              "GET /chains/*/is_bootstrapped";
              "GET /chains/*/mempool/filter";
              "GET /chains/*/mempool/monitor_operations";
              "GET /chains/*/mempool/pending_operations";
              "GET /config/network/user_activated_protocol_overrides";
              "GET /config/network/user_activated_upgrades";
              "GET /config/network/dal";
              "GET /describe/**";
              "GET /errors";
              "GET /monitor/**";
              "GET /network/greylist/ips";
              "GET /network/greylist/peers";
              "GET /network/self";
              "GET /network/self";
              "GET /network/stat";
              "GET /network/version";
              "GET /network/versions";
              "GET /protocols";
              "GET /protocols/*";
              "GET /protocols/*/environment";
              "GET /version";
              "POST /chains/*/blocks/*/context/contracts/*/big_map_get";
              "POST /injection/operation";
            ];
      }

  let allow_all = Allow_all {except = []}

  let default (address : P2p_addr.t) =
    let open Ipaddr in
    if V6.scope address = Interface then allow_all else secure

  let empty_policy = []

  let match_address_and_port point1 point2 =
    let open P2p_point.Id in
    point1.addr = point2.addr && point1.port = point2.port

  let rec put_policy (addr, acl) = function
    | [] -> [(addr, acl)]
    | (a, _) :: policy when match_address_and_port addr a ->
        (addr, acl) :: policy
    | entry :: policy -> entry :: put_policy (addr, acl) policy

  (* FIXME (https://gitlab.com/tezos/tezos/-/issues/1320).
     Use resto functions instead.
  *)
  let meth_to_string = function
    | Any -> ""
    | Exact `GET -> "GET"
    | Exact `PUT -> "PUT"
    | Exact `POST -> "POST"
    | Exact `PATCH -> "PATCH"
    | Exact `DELETE -> "DELETE"

  let escaped_asterisk_seq = String.to_seq "%2A"

  let chunk_to_string = function
    | Wildcard -> "*"
    | Literal l ->
        let s = Uri.pct_encode l in
        if String.contains s '*' then
          (* slow path *)
          String.of_seq
            (Seq.flat_map
               (function '*' -> escaped_asterisk_seq | c -> Seq.return c)
               (String.to_seq s))
        else s

  let chunk_list_to_string l =
    "/" ^ String.concat "/" (List.map chunk_to_string l)

  let path_to_string = function
    | FollowedByAnySuffix l -> chunk_list_to_string l ^ "/**"
    | Exact l -> chunk_list_to_string l

  let matcher_to_string {meth; path} = meth_to_string meth ^ path_to_string path

  let matcher_encoding : matcher Data_encoding.t =
    let open Data_encoding in
    conv matcher_to_string parse string

  let endpoint_encoding : endpoint Data_encoding.t =
    let open Data_encoding in
    let open P2p_point.Id in
    let parse str =
      match parse_addr_port_id str with
      | Ok endpoint -> endpoint
      | Error e ->
          raise
            (Invalid_argument
               (Format.sprintf "%s in address" (string_of_parsing_error e)))
    in
    conv addr_port_id_to_string parse string

  let policy_type p =
    if p = secure then "Secure"
    else if p = allow_all then "AllowAll"
    else "Custom"

  let policy_encoding : (endpoint * t) list Data_encoding.t =
    let open Data_encoding in
    list
    @@ union
         [
           case
             ~title:"Whitelist"
             (Tag 0)
             (obj2
                (req "address" endpoint_encoding)
                (req "whitelist" @@ list matcher_encoding))
             (function
               | addr, Deny_all {except} -> Some (addr, except) | _ -> None)
             (fun (addr, except) -> (addr, Deny_all {except}));
           case
             ~title:"Blacklist"
             (Tag 1)
             (obj2
                (req "address" endpoint_encoding)
                (req "blacklist" @@ list matcher_encoding))
             (function
               | addr, Allow_all {except} -> Some (addr, except) | _ -> None)
             (fun (addr, except) -> (addr, Allow_all {except}));
         ]

  let policy_to_string policy =
    let open Data_encoding in
    Json.construct policy_encoding policy |> Json.to_string

  let find_policy policy (address, port) =
    let match_addr searched_port searched_addr (endpoint, acl) =
      let open P2p_point.Id in
      match (endpoint.addr = searched_addr, endpoint.port, searched_port) with
      | true, None, _ -> Some acl
      | true, Some port, Some searched_port when port = searched_port ->
          Some acl
      | _ -> None
    in
    List.find_map (match_addr port address) policy

  let acl_type = function Allow_all _ -> `Blacklist | Deny_all _ -> `Whitelist

  module Internal_for_test = struct
    type nonrec endpoint = endpoint

    let rec resolve_domain_names resolve =
      let open Lwt_syntax in
      function
      | [] -> return_nil
      | (endpoint, acl) :: remainder ->
          let open P2p_point.Id in
          let* resolved = resolve endpoint in
          let resolved =
            List.map
              (fun (ip_addr, _) ->
                ( {
                    endpoint with
                    addr = Format.asprintf "%a" Ipaddr.V6.pp ip_addr;
                  },
                  acl ))
              resolved
          in
          let+ rem = resolve_domain_names resolve remainder in
          resolved @ rem
  end

  let resolve_domain_names =
    let open P2p_point.Id in
    let resolve endpoint =
      let service = Option.fold ~none:"" ~some:Int.to_string endpoint.port in
      Lwt_utils_unix.getaddrinfo ~node:endpoint.addr ~service ~passive:false
    in
    Internal_for_test.resolve_domain_names resolve
end
