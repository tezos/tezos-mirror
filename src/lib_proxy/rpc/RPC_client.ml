(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Tezos_proxy
module Service = Tezos_rpc.Service
module Events = Proxy_events

let rec print_path : type pr p. (pr, p) Resto.Internal.path -> string list =
 fun path ->
  match path with
  | Root -> []
  | Static (path, s) -> s :: print_path path
  | Dynamic (path, arg) ->
      Printf.sprintf "<%s>" arg.descr.name :: print_path path
  | DynamicTail (path, arg) ->
      Printf.sprintf "<%s>" arg.descr.name :: print_path path

(* TODO Once https://gitlab.com/nomadic-labs/resto/-/issues/3 is
   fixed, use the corresponding function from resto. The same applies
   to print_path above.

   And at the same time, do the same in the mockup mode; which
   has these functions too. *)
let print_service : type p q i o. (_, _, p, q, i, o) Service.t -> string =
 fun serv ->
  let iserv = Service.Internal.to_service serv in
  String.concat "/" (List.rev (print_path iserv.path))

let method_is_writer = function
  | `POST | `DELETE | `PUT | `PATCH -> true
  | `GET -> false

class http_local_ctxt (printer : Tezos_client_base.Client_context.printer)
  (http_ctxt : Tezos_rpc.Context.generic) (mode : Proxy_services.mode) protocol :
  Tezos_rpc.Context.generic =
  let local_ctxt =
    Tezos_mockup_proxy.RPC_client.local_ctxt
      (Proxy_services.build_directory printer http_ctxt mode protocol)
  in
  let dispatch_local_or_distant ~debug_name ~local ~distant meth path =
    let open Lwt_syntax in
    let meth_string = Tezos_rpc.Service.string_of_meth meth in
    let delegate () =
      let* () =
        Events.(emit delegate_to_http) (meth_string, debug_name, path)
      in
      distant ()
    in
    if method_is_writer meth then delegate ()
    else
      let* r = local () in
      match r with
      | Ok x ->
          let* () =
            Events.(emit done_locally) (meth_string, debug_name, path)
          in
          return_ok x
      | Error [Tezos_rpc.Context.Not_found _] -> delegate ()
      | Error _ as err -> Lwt.return err
  in
  object
    method base = Uri.empty

    method call_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        'p ->
        'q ->
        'i ->
        'o tzresult Lwt.t =
      fun service params query input ->
        let local () = local_ctxt#call_service service params query input in
        let distant () = http_ctxt#call_service service params query input in
        let meth = Tezos_rpc.Service.meth service in
        dispatch_local_or_distant
          ~debug_name:"call_service"
          ~local
          ~distant
          meth
        @@ print_service service

    method call_streamed_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        on_chunk:('o -> unit) ->
        on_close:(unit -> unit) ->
        'p ->
        'q ->
        'i ->
        (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query input ->
        let local () =
          local_ctxt#call_streamed_service
            service
            ~on_chunk
            ~on_close
            params
            query
            input
        in
        let distant () =
          http_ctxt#call_streamed_service
            service
            ~on_chunk
            ~on_close
            params
            query
            input
        in
        let meth = Tezos_rpc.Service.meth service in
        dispatch_local_or_distant
          ~debug_name:"call_streamed_service"
          ~local
          ~distant
          meth
        @@ print_service service

    method generic_media_type_call :
        Service.meth ->
        ?body:Data_encoding.json ->
        Uri.t ->
        Tezos_rpc.Context.generic_call_result
        Tezos_error_monad.Error_monad.tzresult
        Lwt.t =
      let open Lwt_syntax in
      fun meth ?body uri ->
        let meth_string = Tezos_rpc.Service.string_of_meth meth in
        let uri_string = Uri.to_string uri in
        let delegate () =
          let* () =
            Events.(emit delegate_media_type_call_to_http)
              (meth_string, uri_string)
          in
          http_ctxt#generic_media_type_call meth ?body uri
        in
        if method_is_writer meth then delegate ()
        else
          let* answer = local_ctxt#generic_media_type_call meth ?body uri in
          match answer with
          | Ok (`Json (`Not_found _))
          | Ok (`Binary (`Not_found _))
          | Ok (`Other (_, `Not_found _))
          | Error [Tezos_rpc.Context.Not_found _] ->
              delegate ()
          | Ok x ->
              let* () =
                Events.(emit done_media_type_call_locally)
                  (meth_string, uri_string)
              in
              return_ok x
          | Error _ as err -> Lwt.return err
  end
