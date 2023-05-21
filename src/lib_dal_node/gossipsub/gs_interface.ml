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

type topic = {slot_index : int; pkh : Signature.Public_key_hash.t}

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5543

   Refine the GS message_id to save bandwidth.

   With the defintion below: commitment * level * slot_index * shard_index *
   attestor => BW = About 48 + 4 + 2 + 2 + 20 (non bls pkh) = 76 bytes.

   However,

    1. we could compute the pkh when needed from L1 information instead of
    providing it;

    2. we could give the payload round instead of the commitment. Together with
    the level, it could identify the commitment (except if there is a double
    baking);

    3. we could also provide the first characters of the commitment.

    With 1 and 2, we would get:
    BW' = BW - 48 - 20 + 1 (Z.n on small numbers up 127) = 9 bytes
*)
type message_id = {
  commitment : Cryptobox.Commitment.t;
  level : int32;
  slot_index : int;
  shard_index : int;
  pkh : Signature.Public_key_hash.t;
}

type message = {share : Cryptobox.share; shard_proof : Cryptobox.shard_proof}

type peer = P2p_peer.Id.t

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5607

   Bound / add checks for bounds of these encodings *)
let topic_encoding : topic Data_encoding.t =
  let open Data_encoding in
  conv
    (fun ({slot_index; pkh} : topic) -> (slot_index, pkh))
    (fun (slot_index, pkh) -> {slot_index; pkh})
    (obj2
       (req "slot_index" uint8)
       (req "pkh" Signature.Public_key_hash.encoding))

let message_id_encoding : message_id Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {level; slot_index; commitment; shard_index; pkh} ->
      (level, slot_index, commitment, shard_index, pkh))
    (fun (level, slot_index, commitment, shard_index, pkh) ->
      {level; slot_index; commitment; shard_index; pkh})
    (obj5
       (req "level" int32)
       (req "slot_index" uint8)
       (req "commitment" Cryptobox.Commitment.encoding)
       (req "shard_index" uint16)
       (req "pkh" Signature.Public_key_hash.encoding))

let message_encoding : message Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {share; shard_proof} -> (share, shard_proof))
    (fun (share, shard_proof) -> {share; shard_proof})
    (obj2
       (req "share" Cryptobox.share_encoding)
       (req "shard_proof" Cryptobox.shard_proof_encoding))

(* Modules needed to instantiate the Gossipsub worker. *)
module Iterable (Cmp : sig
  type t

  val compare : t -> t -> int
end) =
struct
  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)
end

module Topic = struct
  type t = topic = {slot_index : int; pkh : Signature.Public_key_hash.t}

  include Iterable (struct
    type nonrec t = t

    let compare topic {slot_index; pkh} =
      let c = Int.compare topic.slot_index slot_index in
      if c <> 0 then c else Signature.Public_key_hash.compare topic.pkh pkh
  end)

  let pp fmt {pkh; slot_index} =
    Format.fprintf
      fmt
      "{ pkh=%a; slot_index=%d }"
      Signature.Public_key_hash.pp
      pkh
      slot_index
end

module Message_id = struct
  type t = message_id

  include Iterable (struct
    type nonrec t = t

    let compare id {level; slot_index; commitment; shard_index; pkh} =
      let c = Int32.compare id.level level in
      if c <> 0 then c
      else
        let c = Int.compare id.shard_index shard_index in
        if c <> 0 then c
        else
          let c = Cryptobox.Commitment.compare id.commitment commitment in
          if c <> 0 then c
          else
            Topic.compare
              {slot_index = id.slot_index; pkh = id.pkh}
              {slot_index; pkh}
  end)

  let pp fmt {level; slot_index; commitment; shard_index; pkh} =
    Format.fprintf
      fmt
      "{ level=%ld; shard_index=%d; commitment=%a; topic=%a }"
      level
      shard_index
      Cryptobox.Commitment.pp
      commitment
      Topic.pp
      {slot_index; pkh}

  let get_topic {slot_index; pkh; _} = {slot_index; pkh}
end

module Message = struct
  type t = message

  let pp fmt {share; shard_proof} =
    Format.fprintf
      fmt
      "{ share=%s; shard_proof=%s }"
      (Data_encoding.Binary.to_string_exn Cryptobox.share_encoding share)
      (Data_encoding.Binary.to_string_exn
         Cryptobox.shard_proof_encoding
         shard_proof)

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5558

     Define the notion of validity in the DAL node. *)
  let valid _msg = `Valid
end

module Peer = struct
  type t = peer

  include Iterable (struct
    type nonrec t = t

    let compare p1 p2 = P2p_peer.Id.compare p1 p2
  end)

  let pp = P2p_peer.Id.pp
end

let get_value ~__LOC__ func =
  Option.value_f ~default:(fun () ->
      Stdlib.failwith
        (Format.sprintf "%s: Unexpected overflow in %s" __LOC__ func))

module Span : Gossipsub_intf.SPAN with type t = Ptime.Span.t = struct
  type t = Ptime.Span.t

  include Compare.Make (Ptime.Span)

  let zero = Ptime.Span.zero

  let to_int_s t = Ptime.Span.to_int_s t |> get_value ~__LOC__ __FUNCTION__

  let to_float_s t = Ptime.Span.to_float_s t

  let of_int_s = Ptime.Span.of_int_s

  let of_float_s f = Ptime.Span.of_float_s f |> get_value ~__LOC__ __FUNCTION__

  let mul span n = to_float_s span *. float n |> of_float_s

  let pp = Ptime.Span.pp
end

module Time = struct
  type span = Span.t

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
     and module Span = Span
     and type Subconfig.Peer.t = peer
     and type Subconfig.Topic.t = topic
     and type Subconfig.Message_id.t = message_id
     and type Subconfig.Message.t = message = struct
  module Span = Span
  module Time = Time

  module Subconfig = struct
    module Peer = Peer
    module Topic = Topic
    module Message_id = Message_id
    module Message = Message
  end
end

module Monad = struct
  type 'a t = 'a Lwt.t

  let ( let* ) = Lwt.bind

  let return = Lwt.return

  let sleep (span : Span.t) = Lwt_unix.sleep @@ Span.to_float_s span
end

(** Instantiate the worker functor *)
module Worker_config :
  Gossipsub_intf.WORKER_CONFIGURATION
    with type GS.Topic.t = topic
     and type GS.Message_id.t = message_id
     and type GS.Message.t = message
     and type GS.Peer.t = peer
     and module GS.Span = Span
     and module Monad = Monad = struct
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

let span_encoding : Span.t Data_encoding.t =
  let open Data_encoding in
  (* We limit the size of a {!Span.t} value to 2 bytes. It is sufficient for the
     spans sent via the network by Gossipsub, while avoiding overflows when
     adding them to values of type {!Time.t}. *)
  let span_size = 2 in
  check_size span_size
  @@ conv
       (fun span -> Span.to_int_s span)
       (fun span -> Span.of_int_s span)
       (obj1 (req "span" int16))

module Worker_instance = Tezos_gossipsub.Worker (Worker_config)
