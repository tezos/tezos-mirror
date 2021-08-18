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

include Resto_cohttp_server.Server.Make (RPC_encoding) (RPC_logging)

module Acl = struct
  include Resto_acl.Acl

  type endpoint = P2p_point.Id.addr_port_id

  type policy = (endpoint * t) list

  let default = Allow_all {except = []}

  let empty_policy = []

  let default_policy = []

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
               | (addr, Deny_all {except}) -> Some (addr, except) | _ -> None)
             (fun (addr, except) -> (addr, Deny_all {except}));
           case
             ~title:"Blacklist"
             (Tag 1)
             (obj2
                (req "address" endpoint_encoding)
                (req "blacklist" @@ list matcher_encoding))
             (function
               | (addr, Allow_all {except}) -> Some (addr, except) | _ -> None)
             (fun (addr, except) -> (addr, Allow_all {except}));
         ]

  let policy_to_string policy =
    let open Data_encoding in
    Json.construct policy_encoding policy |> Json.to_string

  let find_policy policies address =
    let match_addr searched_port searched_addr (endpoint, acl) =
      let open P2p_point.Id in
      match (endpoint.addr = searched_addr, endpoint.port, searched_port) with
      | (true, None, _) -> Some acl
      | (true, Some port, Some searched_port) when port = searched_port ->
          Some acl
      | _ -> None
    in
    match P2p_point.Id.parse_addr_port_id address with
    | Error _ -> None
    | Ok {addr; port; _} -> List.find_map (match_addr port addr) policies
end
