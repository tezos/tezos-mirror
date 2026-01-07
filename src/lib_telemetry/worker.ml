(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type REQUEST = sig
  include Tezos_base.Worker_intf.REQUEST

  val name : (_, _) t -> string
end

module Hook_request (Request : Tezos_base.Worker_intf.REQUEST) : sig
  include Tezos_base.Worker_intf.REQUEST

  val make :
    parent_scope:Opentelemetry.Scope.t -> ('a, 'b) Request.t -> ('a, 'b) t

  val parent_scope : (_, _) t -> Opentelemetry.Scope.t

  val request : ('a, 'b) t -> ('a, 'b) Request.t
end = struct
  type ('a, 'b) t = {
    parent_scope : Opentelemetry.Scope.t;
    request : ('a, 'b) Request.t;
  }

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    conv
      (fun (View {request; _}) -> Request.view request)
      (fun _ -> assert false)
      Request.encoding

  let pp ppf (View {request; _}) = Request.pp ppf (Request.view request)

  let parent_scope {parent_scope; _} = parent_scope

  let request {request; _} = request

  let make ~parent_scope request = {parent_scope; request}
end

module MakeSingle
    (Name : Tezos_base.Worker_intf.NAME)
    (Request : REQUEST)
    (Types : Tezos_base.Worker_intf.TYPES) =
struct
  module Raw_name = struct
    include Name

    (* We are calling [MakeSingle] twice. If we were to use the same [Name]
       module each time, these two functor calls would generate the exact same
       events, leading to a runtime error. *)
    let base = base @ ["raw"]
  end

  module Request_with_hook = Hook_request (Request)
  module Raw_worker =
    Tezos_workers.Worker.MakeSingle (Raw_name) (Request) (Types)
  module Worker =
    Tezos_workers.Worker.MakeSingle (Name) (Request_with_hook) (Types)

  let service_name = String.concat "." Name.base

  module Hook_handlers (Handlers : Raw_worker.HANDLERS) :
    Worker.HANDLERS
      with type launch_error = Handlers.launch_error
       and type self = Handlers.self = struct
    include Handlers

    let on_request self request =
      Trace.with_result
        ~message_on_success:(Fun.const "Request completed successfully")
        ~message_on_error:(Fun.const "Request returned an error")
        ~scope:(Request_with_hook.parent_scope request)
        ~service_name
        ~attrs:
          [
            ( "worker.request_name",
              `String (Request.name (Request_with_hook.request request)) );
          ]
        Format.(
          sprintf
            "%s/handler"
            (Request.name (Request_with_hook.request request)))
      @@ fun _scope -> on_request self (Request_with_hook.request request)

    let on_error self status request error =
      on_error self status (Request_with_hook.request request) error

    let on_completion self request result status =
      on_completion self (Request_with_hook.request request) result status
  end

  let with_ request f =
    Opentelemetry_lwt.Trace.with_
      ~service_name
      ~attrs:[("worker.request_name", `String (Request.name request))]
      Format.(sprintf "%s/call" (Request.name request))
    @@ fun scope -> f (Request_with_hook.make ~parent_scope:scope request)

  let with_no_lwt request f =
    Opentelemetry.Trace.with_
      ~service_name
      ~attrs:[("worker.request_name", `String (Request.name request))]
      Format.(sprintf "%s/call" (Request.name request))
    @@ fun scope -> f (Request_with_hook.make ~parent_scope:scope request)

  module Instrumented_queue = struct
    let push_request_and_wait worker request =
      with_ request @@ Worker.Queue.push_request_and_wait worker

    let push_request worker request =
      with_ request @@ Worker.Queue.push_request worker
  end

  module Instrumented_dropbox = struct
    let put_request worker request =
      with_no_lwt request @@ Worker.Dropbox.put_request worker

    let put_request_and_wait worker request =
      with_ request @@ Worker.Dropbox.put_request_and_wait worker
  end

  include Worker
  module Queue = Instrumented_queue
  module Dropbox = Instrumented_dropbox

  let launch (type kind launch_error) table i parameters
      (module Handlers : Raw_worker.HANDLERS
        with type launch_error = launch_error
         and type self = kind Worker.t) =
    let module Hooked_handlers = Hook_handlers (Handlers) in
    Worker.launch table i parameters (module Hooked_handlers)
end
