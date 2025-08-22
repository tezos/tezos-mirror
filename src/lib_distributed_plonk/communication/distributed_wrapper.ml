(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Wrapper for the Distributed library.
    The new API is designed for the distribution of protocols based on a
    1 master N workers architecture, in which at each step of the protocol:
    {ul
      {li
        The master asks all the workers to compute the same function over
        different inputs. We call these messages {e requests}. Each protocol
        step will define a [request] (see {!Message} module), whose payload
        will change for every worker.}
      {li
        Each worker computes the {e reply} to the received request, and sends
        it back to the master. Each protocol step will define a [reply]
        (see {!Message} module).
      }
      {li
        The master waits to receive all the replies from the workers.
      }
    }

    Instead of directly using the [send] and [receive], the new API provides
    two abstractions, `dmap` and `handle_request`, to implement this
    interaction from the master and worker respectively. These abstractions
    will enforce via the type-system that the [request] and [reply] used
    correspond to the same protocol step.
*)

module Logger = struct
  let log_src =
    Logs.Src.create
      "distributed"
      ~doc:"logs events related to the distributed library"

  module Log = (val Logs_lwt.src_log log_src : Logs_lwt.LOG)

  let msg = Log.msg

  (* slightly modified version of reporter defined in Logs_lwt manual : http://erratique.ch/software/logs/doc/Logs_lwt.html#report_ex*)
  let lwt_reporter () =
    let buf_fmt () =
      let b = Buffer.create 512 in
      ( Format.formatter_of_buffer b,
        fun () ->
          let m = Buffer.contents b in
          Buffer.reset b ;
          m )
    in
    let app, app_flush = buf_fmt () in
    let reporter = Logs.format_reporter ~app ~dst:app () in
    let report src level ~over k msgf =
      let k' () =
        let write () = Lwt_io.write Lwt_io.stdout (app_flush ()) in
        let unblock () =
          over () ;
          Lwt.return_unit
        in
        Lwt.finalize write unblock |> ignore ;
        k ()
      in
      reporter.Logs.report src level ~over:(fun () -> ()) k' msgf
    in
    {Logs.report}
end

(**
  Messages are refined into either a [request] or a [reply].
  Both these types are parameterized by their protocol [step].
  The ground type of messages ([t]) is enforced to be [bytes],
  to avoid relying on the Marshalling performed by the Distributed library.
*)
module type Enriched_message_type = sig
  include Distributed.Message_type with type t = bytes

  type 'a step

  type 'step request

  type 'step reply

  val request_step : 'step request -> 'step step

  val of_request : 'a request -> t

  val of_reply : 'a reply -> t

  val to_request : 'step step -> t -> 'step request option

  val to_reply : 'step step -> t -> 'step reply option

  val index : t -> int
end

module type Enriched_process = sig
  include Distributed.Process

  module M : Enriched_message_type

  (** Additional monadic interface *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t

  (** [dmap ~pids ~request ~reply l] sends requests built by applying
      [request] to the elements of [l] to the workers [pids] and waits
      to receive a valid [reply] from each worker.
  *)
  val dmap :
    pids:Distributed.Process_id.t list ->
    request:('a -> index:int -> 'step M.request) ->
    reply:('step M.reply -> (unit -> 'b t) option) ->
    'a list ->
    'b list t

  (** [handle_request master_pid ~setp ~handler l] waits to receive a request
      for a given [step], process it through [handler] and sends the reply to
      [master_pid].
      The [handler] might also return some additional data ('b) that isn't
      meant to be sent back to the master, but rather kept by the worker for
      future computation.
  *)
  val handle_request :
    Distributed.Process_id.t ->
    step:'step M.step ->
    handler:('step M.request -> (unit -> ('step M.reply * 'b) t) option) ->
    'b t
end

module Make (A : Enriched_message_type) :
  Enriched_process
    with type message_type = A.t
     and type M.t = A.t
     and type 'a M.step = 'a A.step
     and type 'a M.request = 'a A.request
     and type 'a M.reply = 'a A.reply
     and type 'a io = 'a Lwt.t = struct
  include Distributed_lwt.Make (A) (Logger)
  module M = A

  let ( let* ) = ( >>= )

  let ( let+ ) : 'a t -> ('a -> 'b) -> 'b t =
   fun m f -> m >>= fun x -> return (f x)

  let mapM : ('a -> 'b t) -> 'a list -> 'b list t =
   fun f ->
    let rec go acc = function
      | [] -> return (List.rev acc)
      | x :: xs ->
          let* y = f x in
          go (y :: acc) xs
    in
    go []

  let dmap ~pids ~request ~reply l =
    let module IMap = Map.Make (Int) in
    let expected = List.length pids in
    let replies = ref IMap.empty in
    mapM
      (fun ((index, pid), x) ->
        let* () = send pid (M.of_request @@ request x ~index) in
        return (M.request_step @@ request x ~index))
      List.(combine (combine (init (length pids) Fun.id) pids) l)
    >>= fun steps ->
    let step = List.hd steps in
    receive_loop
      (case (function m ->
          Option.bind (M.to_reply step m) @@ fun r ->
          Option.map
            (fun f () ->
              lift_io
              @@ Lwt_io.printlf "got message %s from remote node\n"
              @@ M.string_of_message m
              >>= fun () ->
              let* () = lift_io @@ Lwt_io.flush_all () in
              f () >>= fun y ->
              replies := IMap.add (M.index m) y !replies ;
              return (IMap.cardinal !replies < expected))
            (reply r)))
    >>= fun _ -> return (List.map snd @@ IMap.bindings !replies)

  let handle_request :
      Distributed.Process_id.t ->
      step:'step M.step ->
      handler:('step M.request -> (unit -> ('step M.reply * 'b) t) option) ->
      'b t =
   fun pid ~step ~handler ->
    let* x =
      receive
        (case (fun m ->
             Option.map (fun f () ->
                 let* () =
                   lift_io
                   @@ Lwt_io.printlf "got message %s from remote node\n"
                   @@ M.string_of_message m
                 in
                 f ())
             @@ Option.bind (M.to_request step m) handler))
    in
    let repl, v = Option.get x in
    let* () = send pid (M.of_reply repl) in
    return v
end
