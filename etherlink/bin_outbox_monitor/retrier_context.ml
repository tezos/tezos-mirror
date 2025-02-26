(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  include Internal_event.Simple

  let section = ["outbox_monitor"; "http_retrier"]

  let retry =
    declare_3
      ~section
      ~name:"retry"
      ~msg:"RPC {uri} failed with {error}, retrying in {delay}s."
      ~level:Error
      ("uri", Data_encoding.(conv Uri.to_string Uri.of_string string))
      ("error", trace_encoding)
      ("delay", Data_encoding.float)
      ~pp1:Uri.pp
      ~pp2:pp_print_trace
end

type error += Timeout of {timeout : float; uri : Uri.t}

let () =
  register_error_kind
    `Temporary
    ~id:"http_retrier.timeout"
    ~title:"Timeout in RPC"
    ~description:"Timeout in RPC"
    ~pp:(fun ppf (timeout, uri) ->
      Format.fprintf ppf "RPC %a timeouted after %fs." Uri.pp uri timeout)
    Data_encoding.(
      obj2
        (req "timeout" Data_encoding.float)
        (req "limit" Data_encoding.(conv Uri.to_string Uri.of_string string)))
    (function Timeout {timeout; uri} -> Some (timeout, uri) | _ -> None)
    (fun (timeout, uri) -> Timeout {timeout; uri})

let regexp_ocaml_exception_connection_error =
  Re.compile @@ Re.Perl.re "in connect:"

let match_connection_error s =
  Re.execp regexp_ocaml_exception_connection_error s

let is_connection_error trace =
  TzTrace.fold
    (fun yes error ->
      yes
      ||
      match error with
      | RPC_client_errors.(
          Request_failed
            {error = Connection_failed "Lwt.Resolution_loop.Canceled"; _}) ->
          false
      | RPC_client_errors.(Request_failed {error = Connection_failed _; _}) ->
          true
      | Timeout _ -> true
      | RPC_client_errors.(Request_failed {error = OCaml_exception s; _}) ->
          (* This error can surface if the external RPC servers of the L1 node are
             shutdown but the request is still in the RPC worker. *)
          match_connection_error s
      | _ -> false)
    false
    trace

let with_timeout ~timeout uri f =
  let open Lwt_result_syntax in
  let alarm =
    let*! () = Lwt_unix.sleep timeout in
    tzfail (Timeout {timeout; uri})
  in
  Lwt.pick [f (); alarm]

let rec retry_call ~timeout uri ?(delay = 1.0) f =
  let open Lwt_syntax in
  let* res = with_timeout ~timeout uri f in
  match res with
  | Error e when is_connection_error e ->
      let* () = Event.(emit retry) (uri, e, delay) in
      let* () = Lwt_unix.sleep delay in
      let delay = min (delay *. 2.) 30. in
      retry_call ~timeout uri ~delay f
  | _ -> return res

class type t = object
  inherit Tezos_rpc.Context.generic

  method endpoint : Uri.t
end

class ctxt ~timeout endpoint medias : t =
  let open Tezos_rpc_http_client_unix.RPC_client_unix in
  object
    inherit http_ctxt {default_config with endpoint} medias as super

    method endpoint = endpoint

    method! generic_media_type_call meth ?body uri =
      retry_call ~timeout uri @@ fun () ->
      super#generic_media_type_call meth ?body uri

    method! call_service service params query body =
      retry_call
        ~timeout
        (Tezos_rpc.Service.forge_partial_request
           service
           ~base:endpoint
           params
           query)
          .uri
      @@ fun () -> super#call_service service params query body

    method! call_streamed_service service ~on_chunk ~on_close params query body
        =
      retry_call
        ~timeout
        (Tezos_rpc.Service.forge_partial_request
           service
           ~base:endpoint
           params
           query)
          .uri
      @@ fun () ->
      super#call_streamed_service service ~on_chunk ~on_close params query body
  end
