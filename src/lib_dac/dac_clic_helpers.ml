(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

module Parsed_rpc = struct
  type t = {scheme : string; host : string; port : int}

  let default_https_port = 443

  let default_http_port = 80

  let http = "http"

  let https = "https"

  let is_http s =
    [http; https]
    |> List.map (fun scheme -> scheme ^ "://")
    |> List.exists (fun prefix -> String.starts_with ~prefix s)

  let get_host uri =
    match Uri.host uri with
    | Some host -> Ok host
    | None -> Error (`Parse_rpc_error "No host provided.")

  let of_string x =
    let open Result_syntax in
    if is_http x then
      let parsed = Uri.of_string x in
      let scheme = Stdlib.Option.get (Uri.scheme parsed) in
      let+ host = get_host parsed in
      let port =
        Option.value_f
          ~default:(fun () ->
            if scheme = https then default_https_port else default_http_port)
          (Uri.port parsed)
      in
      {scheme; host; port}
    else
      match Ipaddr.with_port_of_string ~default:default_http_port x with
      | Ok (ipaddr, port) ->
          let scheme = http in
          let host = Ipaddr.to_string ipaddr in
          Ok {scheme; host; port}
      | Error (`Msg _) -> Error (`Parse_rpc_error "Not a valid rpc address.")
end
