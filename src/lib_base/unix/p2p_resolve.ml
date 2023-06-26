(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type error += Failed_to_parse_address of (string * string)

let () =
  (* Parsing of an address failed with an explanation *)
  register_error_kind
    `Permanent
    ~id:"p2p_resolve.parsing_address_failed"
    ~title:"Parsing of an address failed"
    ~description:"Failed to parse the address given."
    ~pp:(fun ppf (addr, explanation) ->
      Format.fprintf ppf "Failed to parse address '%s': %s@." addr explanation)
    Data_encoding.(obj2 (req "addr" string) (req "explanation" string))
    (function Failed_to_parse_address s -> Some s | _ -> None)
    (fun s -> Failed_to_parse_address s)

let peer_id_unused =
  let open Internal_event.Simple in
  declare_1
    ~section:["config"]
    ~level:Warning
    ~name:"config_peer_id_unused"
    ~msg:"While parsing {string} a peer id was provided but will not be used."
    ("string", Data_encoding.string)

let no_points_found =
  let open Internal_event.Simple in
  declare_1
    ~section:["config"]
    ~level:Warning
    ~name:"config_no_points_found"
    ~msg:"The DNS lookup of {string} returns 0 point."
    ("string", Data_encoding.string)

let resolve_addr_with_peer_id ~default_addr ~default_port ?(passive = false)
    peer : (P2p_point.Id.t * P2p_peer.Id.t option) list tzresult Lwt.t =
  let open Lwt_result_syntax in
  match P2p_point.Id.parse_addr_port_id peer with
  | Error err ->
      tzfail
        (Failed_to_parse_address (peer, P2p_point.Id.string_of_parsing_error err))
  | Ok {addr; port; peer_id} ->
      let service_port =
        match (port, default_port) with
        | Some port, _ -> port
        | None, default_port -> default_port
      in
      let service = string_of_int service_port in
      let node = if addr = "" || addr = "_" then default_addr else addr in
      let*! l = Lwt_utils_unix.getaddrinfo ~passive ~node ~service in
      let*! () =
        if List.is_empty l then
          Internal_event.Simple.(emit no_points_found peer)
        else Lwt.return_unit
      in
      return (List.map (fun point -> (point, peer_id)) l)

let resolve_addr ~default_addr ~default_port ?passive peer :
    P2p_point.Id.t list tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* l =
    resolve_addr_with_peer_id ~default_addr ~default_port ?passive peer
  in
  let points = List.map fst l in
  let*! () =
    if List.exists (function _, Some _ -> true | _ -> false) l then
      Internal_event.Simple.(emit peer_id_unused) peer
    else Lwt.return_unit
  in
  return points
