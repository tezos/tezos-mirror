(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module EndpointMap = Map.Make (struct
  type t = Cohttp.Code.meth * string

  let compare = Stdlib.compare
end)

type resto_dir = {
  dir : unit Tezos_rpc.Directory.t;
  extra :
    (Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    Cohttp_lwt_unix.Server.response_action Lwt.t)
    EndpointMap.t;
}

type t = Resto of resto_dir | Dream of Dream.route list

let empty = function
  | Configuration.Resto ->
      Resto {dir = Tezos_rpc.Directory.empty; extra = EndpointMap.empty}
  | Configuration.Dream -> Dream []

let init_from_resto_directory dir = Resto {dir; extra = EndpointMap.empty}

let register dir service handler =
  match dir with
  | Resto {dir; extra} ->
      Resto {dir = Tezos_rpc.Directory.register dir service handler; extra}
  | Dream routes ->
      let route =
        Router.make_tz_route service (fun ~params ~query input ->
            handler params query input)
      in
      Dream (route :: routes)

let register_describe = function
  | Resto {dir; extra} ->
      let dir =
        Tezos_rpc.Directory.register_describe_directory_service
          dir
          Tezos_rpc.Service.description_service
      in
      Resto {dir; extra}
  | dir -> dir

let opt_register dir service handler =
  match dir with
  | Resto {dir; extra} ->
      Resto {dir = Tezos_rpc.Directory.opt_register dir service handler; extra}
  | Dream routes ->
      let route =
        Router.make_opt_tz_route service (fun ~params ~query input ->
            handler params query input)
      in
      Dream (route :: routes)

let lwt_register dir service handler =
  match dir with
  | Resto {dir; extra} ->
      Resto {dir = Tezos_rpc.Directory.lwt_register dir service handler; extra}
  | Dream routes ->
      let route =
        Router.make_route service (fun ~params ~query input ->
            handler params query input)
      in
      Dream (route :: routes)

let streamed_register dir service handler =
  let open Lwt_syntax in
  match dir with
  | Resto {dir; extra} ->
      let dir =
        Tezos_rpc.Directory.gen_register dir service (fun params query input ->
            let* stream, shutdown = handler params query input in
            let next () = Lwt_stream.get stream in
            Tezos_rpc.Answer.return_stream {next; shutdown})
      in
      Resto {dir; extra}
  | Dream routes ->
      let route =
        Router.make_stream_route service (fun ~params ~query input ->
            handler params query input)
      in
      Dream (route :: routes)

let register_metrics path dir =
  match dir with
  | Resto {dir; extra} ->
      let open Lwt_syntax in
      let callback conn req body =
        let+ response = Metrics.Metrics_server.callback conn req body in
        `Response response
      in
      Resto {dir; extra = EndpointMap.add (`GET, path) callback extra}
  | Dream routes ->
      let route = Router.make_metrics_route path in
      Dream (route :: routes)

let jsonrpc_websocket_register ?monitor ~max_message_length dir path handler =
  match dir with
  | Resto {dir; extra} ->
      let callback =
        Evm_websocket.cohttp_callback ?monitor ~max_message_length handler
      in
      Resto {dir; extra = EndpointMap.add (`GET, path) callback extra}
  | Dream routes ->
      let route = Router.make_jsonrpc_websocket_route path handler in
      Dream (route :: routes)

module Curry = struct
  type (_, _, _, _, _, _) conv =
    | Z : (unit, 'g, 'g, unit, 'f, 'f) conv
    | S :
        ('t, 'g, 'b * 's, 'rt, 'f, 'r) conv
        -> ('t * 'b, 'g, 's, 'a * 'rt, 'a -> 'f, 'r) conv

  let reverse : type a c d e f. (a, c, unit, d, e, f) conv -> a -> c =
   fun c v ->
    let rec reverse : type a c d e f g. (a, c, d, e, f, g) conv -> a -> d -> c =
     fun c v acc ->
      match (c, v) with Z, _ -> acc | S c, (v, x) -> reverse c v (x, acc)
    in
    reverse c v ()

  let rec curry : type a b c d e f. (a, b, c, d, e, f) conv -> e -> d -> f =
   fun c f ->
    match c with Z -> fun () -> f | S c -> fun (v, x) -> curry c (f v) x

  let curry c f =
    let f = curry c f in
    fun x -> f (reverse c x)
end

let register0 dir service handler = register dir service Curry.(curry Z handler)

let register1 dir service handler =
  register dir service Curry.(curry (S Z) handler)

let register2 dir service handler =
  register dir service Curry.(curry (S (S Z)) handler)

let opt_register0 dir service handler =
  opt_register dir service Curry.(curry Z handler)

let opt_register1 dir service handler =
  opt_register dir service Curry.(curry (S Z) handler)

let opt_register2 dir service handler =
  opt_register dir service Curry.(curry (S (S Z)) handler)

let lwt_register0 dir service handler =
  lwt_register dir service Curry.(curry Z handler)

let lwt_register1 dir service handler =
  lwt_register dir service Curry.(curry (S Z) handler)

let lwt_register2 dir service handler =
  lwt_register dir service Curry.(curry (S (S Z)) handler)

let streamed_register0 dir service handler =
  streamed_register dir service Curry.(curry Z handler)

let streamed_register1 dir service handler =
  streamed_register dir service Curry.(curry (S Z) handler)

let streamed_register2 dir service handler =
  streamed_register dir service Curry.(curry (S (S Z)) handler)
