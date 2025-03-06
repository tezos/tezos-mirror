(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module type COMPONENT = sig
  val base : string list
end

module UniqueNameMaker (Component : COMPONENT) = struct
  let base = "lib_p2p" :: Component.base

  type t = unit

  let pp _ _ = ()

  let equal () () = true

  let encoding = Data_encoding.unit
end

type ('response, 'error) loop = Loop : (unit, tztrace) loop

module LoopRequest = struct
  type ('response, 'error) t = ('response, 'error) loop

  type view = View : ('response, 'error) t -> view

  let view r = View r

  let encoding =
    let open Data_encoding in
    conv (fun (View Loop) -> ()) (fun () -> View Loop) unit

  let pp ppf (View Loop) = Format.fprintf ppf "loop"
end

module Make
    (Name : Tezos_base.Worker_intf.NAME)
    (Types : Tezos_base.Worker_intf.TYPES) =
struct
  include Tezos_workers.Worker.MakeSingle (Name) (LoopRequest) (Types)

  type activator = unit Lwt.u

  type nonrec activated_worker = {
    worker_state : callback t;
    activate : activator;
  }

  let default_callback () = Lwt.return (Any_request (Loop, {scope = None}))

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
end
