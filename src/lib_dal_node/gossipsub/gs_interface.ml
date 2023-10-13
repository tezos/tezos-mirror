(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

open Gossipsub_intf
module Types = Tezos_dal_node_services.Types

type peer = P2p_peer.Id.t

module Validate_message_hook = struct
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5674

     Refactor gossipsub integration to avoid this mutable hook in the lib. *)
  let check_message =
    ref (fun ?message:_ ~message_id:_ () ->
        Format.eprintf
          "Gs interface: messages validatation function is not set@." ;
        `Unknown)

  let set func = check_message := func
end

let message_valid ?message ~message_id () =
  !Validate_message_hook.check_message ?message ~message_id ()

let get_value ~__LOC__ func =
  Option.value_f ~default:(fun () ->
      Stdlib.failwith
        (Format.sprintf "%s: Unexpected overflow in %s" __LOC__ func))

module Time = struct
  type span = Types.Span.t

  type t = Ptime.t

  include Compare.Make (Ptime)

  let pp = Ptime.pp

  let now = Ptime_clock.now

  let add t span = Ptime.add_span t span |> get_value ~__LOC__ __FUNCTION__

  let sub t span = Ptime.sub_span t span |> get_value ~__LOC__ __FUNCTION__

  let to_span = Ptime.to_span
end

module Automaton_config :
  AUTOMATON_CONFIG
    with type Time.t = Ptime.t
     and type Span.t = Types.Span.t
     and type Time.span = Types.Span.t
     and type Subconfig.Peer.t = peer
     and type Subconfig.Topic.t = Types.Topic.t
     and type Subconfig.Message_id.t = Types.Message_id.t
     and type Subconfig.Message.t = Types.Message.t = struct
  module Span = Types.Span
  module Time = Time

  module Subconfig = struct
    module Peer = Types.Peer
    module Topic = Types.Topic
    module Message_id = Types.Message_id

    module Message = struct
      include Types.Message

      let valid = message_valid
    end
  end
end

module Monad = struct
  type 'a t = 'a Lwt.t

  let ( let* ) = Lwt.bind

  let return = Lwt.return

  let sleep (span : Types.Span.t) = Lwt_unix.sleep @@ Types.Span.to_float_s span
end

(** Instantiate the worker functor *)
module Worker_config :
  Gossipsub_intf.WORKER_CONFIGURATION
    with type GS.Topic.t = Types.Topic.t
     and type GS.Message_id.t = Types.Message_id.t
     and type GS.Message.t = Types.Message.t
     and type GS.Peer.t = Types.Peer.t
     and type GS.Span.t = Types.Span.t
     and type 'a Monad.t = 'a Lwt.t = struct
  module GS = Tezos_gossipsub.Make (Automaton_config)
  module Monad = Monad

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5596

     Use Seq_s instead of Lwt_stream to implement module Stream. *)
  module Stream = struct
    type 'a t = {stream : 'a Lwt_stream.t; pusher : 'a option -> unit}

    let empty () =
      let stream, pusher = Lwt_stream.create () in
      {stream; pusher}

    let push e t = t.pusher (Some e)

    let pop t =
      let open Lwt_syntax in
      let* r = Lwt_stream.get t.stream in
      match r with
      | Some r -> Lwt.return r
      | None ->
          Stdlib.failwith
            "Invariant: None values are never pushed in the stream"

    let get_available t = Lwt_stream.get_available t.stream
  end
end

module Worker_instance = Tezos_gossipsub.Worker (Worker_config)
