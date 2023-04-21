(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Events = Proxy_server_run_events

type error += Proxy_server_RPC_Port_already_in_use of P2p_point.Id.t list

type args = {
  address : P2p_addr.t;
  port : int;
  tls_cert_and_key : (string * string) option;
  forwarding_endpoint : Uri.t;
}

let () =
  register_error_kind
    `Permanent
    ~id:"main.run.port_already_in_use"
    ~title:"Cannot start proxy server: RPC port already in use"
    ~description:"Another process is running on the same RPC port."
    ~pp:(fun ppf addrlist ->
      Format.fprintf
        ppf
        "Another process is probably running on one of these addresses (%a). \
         Please choose another RPC port."
        (Format.pp_print_list P2p_point.Id.pp)
        addrlist)
    Data_encoding.(obj1 (req "addrlist" (list P2p_point.Id.encoding)))
    (function
      | Proxy_server_RPC_Port_already_in_use addrlist -> Some addrlist
      | _ -> None)
    (fun addrlist -> Proxy_server_RPC_Port_already_in_use addrlist)

let launch_rpc_server dir {address; port; tls_cert_and_key; forwarding_endpoint}
    =
  let open Lwt_result_syntax in
  let host = Ipaddr.V6.to_string address in
  let mode =
    match tls_cert_and_key with
    | None -> `TCP (`Port port)
    | Some (cert, key) ->
        `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port)
  in
  let server =
    Tezos_rpc_http_server.RPC_server.init_server
      dir
      ~media_types:Tezos_rpc_http.Media_type.all_media_types
  in
  let middleware =
    Tezos_rpc_http_server.RPC_middleware.proxy_server_query_forwarder
      forwarding_endpoint
  in
  let callback =
    Tezos_rpc_http_server.RPC_server.resto_callback server |> middleware
  in
  Lwt.catch
    (fun () ->
      let*! () =
        Tezos_rpc_http_server.RPC_server.launch ~host server ~callback mode
      in
      Lwt.return_ok server)
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          tzfail (Proxy_server_RPC_Port_already_in_use [(address, port)])
      | exn -> fail_with_exn exn)

let run dir ({address; port; _} as args) =
  let open Lwt_result_syntax in
  let*! () = Tezos_base_unix.Internal_event_unix.init () in
  let node_downer =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        Events.(emit shutting_down_proxy_server) ())
  in
  let* rpc = launch_rpc_server dir args in
  let rpc_downer =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[node_downer]
      (fun _ ->
        let*! () = Events.(emit shutting_down_rpc_server) () in
        Tezos_rpc_http_server.RPC_server.shutdown rpc)
  in
  let*! () =
    Events.(emit starting_rpc_server) (P2p_addr.to_string address, port)
  in
  let _ =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[rpc_downer]
      (fun _exit_status -> Tezos_base_unix.Internal_event_unix.close ())
  in
  Lwt_utils.never_ending ()
