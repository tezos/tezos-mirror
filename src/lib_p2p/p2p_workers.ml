(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module type COMPONENT = sig
  val base : string list
end

module Unique_name_maker (Component : COMPONENT) = struct
  let base = "lib_p2p" :: Component.base

  type t = unit

  let pp _ _ = ()

  let equal () () = true

  let encoding = Data_encoding.unit
end

module type P2P_REQUEST = sig
  type ('response, 'error) t

  type view = View : ('reponse, 'error) t -> view

  include
    Tezos_base.Worker_intf.REQUEST
      with type ('response, 'error) t := ('response, 'error) t
       and type view := view

  val default_callback_value : view
end

type ('response, 'error) loop = Loop : (unit, tztrace) loop

module Loop_request :
  P2P_REQUEST with type ('response, 'error) t = ('response, 'error) loop =
struct
  type ('response, 'error) t = ('response, 'error) loop

  type view = View : ('response, 'error) loop -> view

  let view r = View r

  let encoding =
    let open Data_encoding in
    conv (fun (View Loop) -> ()) (fun () -> View Loop) unit

  let pp ppf (View Loop) = Format.fprintf ppf "loop"

  let default_callback_value = View Loop
end

module Make
    (Name : Tezos_base.Worker_intf.NAME)
    (P2p_request : P2P_REQUEST)
    (Types : Tezos_base.Worker_intf.TYPES) =
struct
  include Tezos_workers.Worker.MakeSingle (Name) (P2p_request) (Types)

  type activator = unit Lwt.u

  type activated_worker = {worker_state : callback t; activate : activator}

  let default_callback =
    let (View default_callback_value) = P2p_request.default_callback_value in
    fun () -> Lwt.return (Any_request (default_callback_value, {scope = None}))

  let activated_callback ?(callback = default_callback) () =
    let activate, activate_resolver = Lwt.wait () in
    ( Callback
        (fun () ->
          let open Lwt_syntax in
          let* () = activate in
          callback ()),
      activate_resolver )

  let create ?timeout ?callback name parameters handlers =
    let open Lwt_result_syntax in
    let callback, activate = activated_callback ?callback () in
    let* worker_state =
      launch (create_table callback) ?timeout name parameters handlers
    in
    return {worker_state; activate}

  let activate worker = Lwt.wakeup worker.activate ()

  let shutdown worker = shutdown worker.worker_state
end
