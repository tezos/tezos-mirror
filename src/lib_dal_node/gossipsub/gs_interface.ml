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

module Automaton_config :
  AUTOMATON_CONFIG
    with type Time.t = Types.Time.t
     and type Span.t = Types.Span.t
     and type Subconfig.Peer.t = Types.Peer.t
     and type Subconfig.Topic.t = Types.Topic.t
     and type Subconfig.Message_id.t = Types.Message_id.t
     and type Subconfig.Message.t = Types.Message.t = struct
  module Span = Types.Span
  module Time = Types.Time

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
     and type GS.Time.t = Types.Time.t
     and type 'a Monad.t = 'a Lwt.t = struct
  module GS = Tezos_gossipsub.Automaton (Automaton_config)
  module Monad = Monad

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5596

     Use Seq_s instead of Lwt_stream to implement module Stream. *)
  module Stream = struct
    type 'a t = {
      stream : 'a Lwt_stream.t;
      pusher : 'a option -> unit;
      mutable length : int;
          (* The [length] field counts the number of elements in the stream. It
             is incremented on calls to {!push}, decremented on succesful calls to
             {!get}, and reset on calls to {!get_available}. *)
    }

    let empty () =
      let stream, pusher = Lwt_stream.create () in
      {stream; pusher; length = 0}

    let push e t =
      t.pusher (Some e) ;
      t.length <- t.length + 1

    let pop t =
      let open Lwt_syntax in
      let* r = Lwt_stream.get t.stream in
      match r with
      | Some r ->
          t.length <- t.length - 1 ;
          Lwt.return r
      | None ->
          Stdlib.failwith
            "Invariant: None values are never pushed in the stream"

    let get_available t =
      t.length <- 0 ;
      Lwt_stream.get_available t.stream

    let length t = t.length
  end
end

module Worker_instance = Tezos_gossipsub.Worker (Worker_config)
