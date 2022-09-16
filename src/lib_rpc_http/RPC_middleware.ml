(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let make_transform_callback forwarding_endpoint callback conn req body =
  let open Lwt_syntax in
  let open Cohttp in
  let* answer = callback conn req body in
  let uri = Request.uri req in
  let answer_has_not_found_status = function
    | `Expert (response, _) | `Response (response, _) ->
        Response.status response = `Not_found
  in
  if answer_has_not_found_status answer then
    let overriding = Uri.to_string forwarding_endpoint ^ Uri.path uri in
    let headers = Header.of_list [("Location", overriding)] in
    let response = Response.make ~status:`Moved_permanently ~headers () in
    Lwt.return
      (`Response
        ( response,
          Cohttp_lwt.Body.of_string
            (Format.asprintf
               "tezos-proxy-server: request unsupported for proxy server, \
                redirecting to node endpoint at %s"
               overriding) ))
  else Lwt.return answer

let rpc_metrics_transform_callback ~update_metrics dir callback conn req body =
  let open Lwt_result_syntax in
  let do_call () = callback conn req body in
  let cohttp_meth = Cohttp.Request.meth req in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let decoded = Resto.Utils.decode_split_path path in
  let*! description =
    let* resto_meth =
      match cohttp_meth with
      | #Resto.meth as meth -> Lwt.return_ok meth
      | _ -> Lwt.return_error @@ `Method_not_allowed []
    in
    let* uri_desc = RPC_directory.lookup_uri_desc dir () resto_meth decoded in
    Lwt.return_ok (uri_desc, Resto.string_of_meth resto_meth)
  in
  match description with
  | Ok (uri, meth) ->
      (* We update the metric only if the URI can succesfully
         be matched in the directory tree. *)
      update_metrics uri meth do_call
  | Error _ ->
      (* Otherwise, the call must be done anyway. *)
      do_call ()

let proxy_server_query_forwarder forwarding_endpoint =
  make_transform_callback forwarding_endpoint
