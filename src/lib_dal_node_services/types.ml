(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Cryptobox = Tezos_crypto_dal.Cryptobox

type level = int32

type slot_index = int

type page_index = int

module Topic = struct
  type t = {slot_index : int; pkh : Signature.Public_key_hash.t}

  type topic = t

  let compare topic {slot_index; pkh} =
    let c = Int.compare topic.slot_index slot_index in
    if c <> 0 then c else Signature.Public_key_hash.compare topic.pkh pkh

  module Cmp = struct
    type nonrec t = t

    let compare = compare
  end

  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)

  let pp fmt {pkh; slot_index} =
    Format.fprintf
      fmt
      "{ pkh=%a; slot_index=%d }"
      Signature.Public_key_hash.pp
      pkh
      slot_index

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5607

     Bound / add checks for bounds of these encodings *)
  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun ({slot_index; pkh} : topic) -> (slot_index, pkh))
      (fun (slot_index, pkh) -> {slot_index; pkh})
      (obj2
         (req "slot_index" uint8)
         (req "pkh" Signature.Public_key_hash.encoding))
end

module Message_id = struct
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5543

     Refine the GS message_id to save bandwidth.

     With the defintion below: commitment * level * slot_index * shard_index *
     attester => BW = About 48 + 4 + 2 + 2 + 20 (non bls pkh) = 76 bytes.

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
  type t = {
    commitment : Cryptobox.Commitment.t;
    level : int32;
    slot_index : int;
    shard_index : int;
    pkh : Signature.Public_key_hash.t;
  }

  let compare id {level; slot_index; commitment; shard_index; pkh} =
    (* If you change the semantics of this function, consider the effect it can
       have on handling shards in the future. Currently, this ordering is used to
       know the priority of when a shard should be handled. Shards with a lower
       level should be handled first when handling shards that were received in the
       future. *)
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

  module Cmp = struct
    type nonrec t = t

    let compare = compare
  end

  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)

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

  let encoding : t Data_encoding.t =
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

  let get_topic {slot_index; pkh; _} = Topic.{slot_index; pkh}
end

module Message = struct
  type t = {share : Cryptobox.share; shard_proof : Cryptobox.shard_proof}

  let pp fmt {share; shard_proof} =
    Format.fprintf
      fmt
      "{ share=%s; shard_proof=%s }"
      (Data_encoding.Binary.to_string_exn Cryptobox.share_encoding share)
      (Data_encoding.Binary.to_string_exn
         Cryptobox.shard_proof_encoding
         shard_proof)

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {share; shard_proof} -> (share, shard_proof))
      (fun (share, shard_proof) -> {share; shard_proof})
      (obj2
         (req "share" Cryptobox.share_encoding)
         (req "shard_proof" Cryptobox.shard_proof_encoding))
end

module Peer = struct
  type t = P2p_peer.Id.t

  module Cmp = struct
    type nonrec t = t

    let compare p1 p2 = P2p_peer.Id.compare p1 p2
  end

  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)

  let pp = P2p_peer.Id.pp

  let encoding = P2p_peer.Id.encoding
end

module Point = struct
  type t = P2p_point.Id.t

  module Cmp = struct
    type nonrec t = t

    let compare p1 p2 = P2p_point.Id.compare p1 p2
  end

  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)

  let pp = P2p_point.Id.pp

  let encoding = P2p_point.Id.encoding
end

let get_value ~__LOC__ func =
  Option.value_f ~default:(fun () ->
      Stdlib.failwith
        (Format.sprintf "%s: Unexpected overflow in %s" __LOC__ func))

module Span = struct
  type t = Ptime.Span.t

  include Compare.Make (Ptime.Span)

  let zero = Ptime.Span.zero

  let to_int_s t = Ptime.Span.to_int_s t |> get_value ~__LOC__ __FUNCTION__

  let to_float_s t = Ptime.Span.to_float_s t

  let of_int_s = Ptime.Span.of_int_s

  let of_float_s f = Ptime.Span.of_float_s f |> get_value ~__LOC__ __FUNCTION__

  let mul span n = to_float_s span *. float n |> of_float_s

  let pp = Ptime.Span.pp

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    (* We limit the size of a {!Span.t} value to 2 bytes. It is sufficient for the
       spans sent via the network by Gossipsub, while avoiding overflows when
       adding them to values of type {!Time.t}. *)
    let span_size = 2 in
    check_size span_size
    @@ conv
         (fun span -> to_int_s span)
         (fun span -> of_int_s span)
         (obj1 (req "span" int16))
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

  let encoding = Time.System.encoding
end

module Score = struct
  type t = float

  let encoding = Data_encoding.float
end

(* Declaration of types used as inputs and/or outputs. *)
module Slot_id = struct
  type t = {slot_level : level; slot_index : slot_index}

  let compare {slot_level = l1; slot_index = i1}
      {slot_level = l2; slot_index = i2} =
    let open Compare in
    or_else (Int32.compare l1 l2) (fun () -> Int.compare i1 i2)

  module Comparable = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (Comparable)
  module Map = Map.Make (Comparable)
end

type slot_id = Slot_id.t

type slot_set = {slots : bool list; published_level : int32}

type attestable_slots = Attestable_slots of slot_set | Not_in_committee

type header_status = [`Waiting_attestation | `Attested | `Unattested]

type shard_index = int

type slot_header = {
  slot_id : slot_id;
  commitment : Cryptobox.Commitment.t;
  status : header_status;
}

type profile = Bootstrap | Operator of Operator_profile.t | Random_observer

type with_proof = {with_proof : bool}

(* Auxiliary functions.  *)

(* Encodings associated  to the types. *)

let slot_id_encoding =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4396
      Reuse protocol encodings. *)
  let open Data_encoding in
  let open Slot_id in
  conv
    (fun {slot_level; slot_index} -> (slot_level, slot_index))
    (fun (slot_level, slot_index) -> {slot_level; slot_index})
    (obj2 (req "slot_level" int32) (req "slot_index" uint8))

let slot_encoding = Data_encoding.bytes

let attestable_slots_encoding : attestable_slots Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"attestable_slots_set"
        (Tag 0)
        (obj3
           (req "kind" (constant "attestable_slots_set"))
           (req "attestable_slots_set" Data_encoding.(list bool))
           (req "published_level" int32))
        (function
          | Attestable_slots {slots; published_level} ->
              Some ((), slots, published_level)
          | _ -> None)
        (function
          | (), slots, published_level ->
              Attestable_slots {slots; published_level});
      case
        ~title:"not_in_committee"
        (Tag 1)
        (obj1 (req "kind" (constant "not_in_committee")))
        (function Not_in_committee -> Some () | _ -> None)
        (function () -> Not_in_committee);
    ]

(* Note: this encoding is used to store statuses on disk using the
   [Key_value_store] module. As such, it's important that this is a
   fixed-size encoding. *)
let header_status_encoding : header_status Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"waiting_attestation"
        (Tag 0)
        (constant "waiting_attestation")
        (function `Waiting_attestation -> Some () | _ -> None)
        (function () -> `Waiting_attestation);
      case
        ~title:"attested"
        (Tag 1)
        (constant "attested")
        (function `Attested -> Some () | _ -> None)
        (function () -> `Attested);
      case
        ~title:"unattested"
        (Tag 2)
        (constant "unattested")
        (function `Unattested -> Some () | _ -> None)
        (function () -> `Unattested);
    ]

let slot_header_encoding =
  let open Data_encoding in
  conv
    (fun {slot_id; commitment; status} -> (slot_id, (commitment, status)))
    (fun (slot_id, (commitment, status)) -> {slot_id; commitment; status})
    (merge_objs
       slot_id_encoding
       (obj2
          (req "commitment" Cryptobox.Commitment.encoding)
          (req "status" header_status_encoding)))

let profile_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Boostrap node"
        (Tag 1)
        (obj1 (req "kind" (constant "bootstrap")))
        (function Bootstrap -> Some () | _ -> None)
        (function () -> Bootstrap);
      case
        ~title:"Controller"
        (Tag 2)
        (obj2
           (req "kind" (constant "controller"))
           (req "controller_profiles" Operator_profile.encoding))
        (function
          | Operator operator_profiles -> Some ((), operator_profiles)
          | _ -> None)
        (function (), operator_profiles -> Operator operator_profiles);
      case
        ~title:"Random_observer"
        (Tag 3)
        (obj1 (req "kind" (constant "random_observer")))
        (function Random_observer -> Some () | _ -> None)
        (function () -> Random_observer);
    ]

let with_proof_encoding =
  let open Data_encoding in
  conv
    (fun {with_proof} -> with_proof)
    (fun with_proof -> {with_proof})
    (obj1 (req "with_proof" bool))

(* String parameters queries. *)

let header_status_arg =
  let destruct s =
    let s = `String s in
    try Ok (Data_encoding.Json.destruct header_status_encoding s)
    with _ -> Error "Cannot parse header status value"
  in
  let construct = Data_encoding.Binary.to_string_exn header_status_encoding in
  Tezos_rpc.Arg.make ~name:"header_status" ~destruct ~construct ()

let char =
  Resto.Arg.make
    ~name:"char"
    ~destruct:(fun str ->
      if String.length str = 1 then Ok (String.get str 0)
      else Error "A single character string is expected")
    ~construct:(fun c -> String.make 1 c)
    ()

let option_int =
  Resto.Arg.make
    ~name:"optional int"
    ~destruct:(function
      | "" -> Ok None
      | str -> (
          try Ok (Some (int_of_string str))
          with Failure _ -> Error "int expected"))
    ~construct:(function None -> "" | Some i -> string_of_int i)
    ()

let slot_query =
  let open Tezos_rpc.Query in
  query (fun padding slot_index ->
      object
        method padding = padding

        method slot_index = slot_index
      end)
  |+ field "padding" char '\x00' (fun obj -> obj#padding)
  |+ field "slot_index" option_int None (fun obj -> obj#slot_index)
  |> seal

let wait_query =
  let open Tezos_rpc.Query in
  query (fun wait ->
      object
        method wait = wait
      end)
  |+ flag "wait" (fun t -> t#wait)
  |> seal

let connected_query =
  let open Tezos_rpc.Query in
  query (fun connected ->
      object
        method connected = connected
      end)
  |+ flag "connected" (fun t -> t#connected)
  |> seal

let subscribed_query =
  let open Tezos_rpc.Query in
  query (fun subscribed ->
      object
        method subscribed = subscribed
      end)
  |+ flag "subscribed" (fun t -> t#subscribed)
  |> seal

let slot_id_query =
  let open Tezos_rpc in
  let open Query in
  query (fun slot_level slot_index -> (slot_level, slot_index))
  |+ opt_field "slot_level" Arg.int32 fst
  |+ opt_field "slot_index" Arg.int snd
  |> seal

let opt_header_status_query =
  let open Tezos_rpc in
  let open Query in
  query (fun header_status -> header_status)
  |+ opt_field "status" header_status_arg (fun hs -> hs)
  |> seal

module Store = struct
  (** Data kind stored in DAL. *)
  type kind = Commitment | Header_status | Slot_id | Slot | Shard | Profile

  let encoding : kind Data_encoding.t =
    Data_encoding.string_enum
      [
        ("commitment", Commitment);
        ("header_status", Header_status);
        ("slot_id", Slot_id);
        ("slot", Slot);
        ("shard", Shard);
        ("profile", Profile);
      ]

  let to_string data_kind =
    Data_encoding.Binary.to_string_exn encoding data_kind
end

module P2P = struct
  module Metadata = struct
    module Peer = struct
      type t = unit

      let encoding = Data_encoding.unit

      let config : t P2p_params.peer_meta_config =
        let empty () = () in
        let score (_ : t) = 1.0 in
        {peer_meta_encoding = encoding; peer_meta_initial = empty; score}
    end

    module Connection = struct
      type t = {
        advertised_net_addr : P2p_addr.t option;
        advertised_net_port : int option;
        is_bootstrap_peer : bool;
      }

      let encoding =
        let open Data_encoding in
        (conv
           (fun {advertised_net_addr; advertised_net_port; is_bootstrap_peer} ->
             (advertised_net_addr, advertised_net_port, is_bootstrap_peer))
           (fun (advertised_net_addr, advertised_net_port, is_bootstrap_peer) ->
             {advertised_net_addr; advertised_net_port; is_bootstrap_peer}))
          (obj3
             (opt "advertised_net_addr" P2p_addr.encoding)
             (opt "advertised_net_port" uint16)
             (req "is_bootstrap_peer" bool))

      let config cfg : t P2p_params.conn_meta_config =
        P2p_params.
          {
            conn_meta_encoding = encoding;
            private_node = (fun _cfg -> false);
            conn_meta_value = (fun () -> cfg);
          }
    end
  end

  module Peer = struct
    module Info = struct
      type t = (Metadata.Peer.t, Metadata.Connection.t) P2p_peer.Info.t

      let encoding =
        P2p_peer.Info.encoding
          Metadata.Peer.encoding
          Metadata.Connection.encoding
    end
  end
end

module Gossipsub = struct
  type connection = {
    topics : Topic.t list;
    direct : bool;
    outbound : bool;
    bootstrap : bool;
  }

  let connection_encoding =
    let open Data_encoding in
    conv
      (fun {topics; direct; outbound; bootstrap} ->
        (topics, direct, outbound, bootstrap))
      (fun (topics, direct, outbound, bootstrap) ->
        {topics; direct; outbound; bootstrap})
      (obj4
         (req "topics" (list Topic.encoding))
         (req "direct" bool)
         (req "outbound" bool)
         (req "bootstrap" bool))
end

module Version = struct
  type t = {network_version : Network_version.t}

  let make ~network_version = {network_version}

  (* We redefine the encoding so that we can specify the correct name
     for the "distributed_db_version" field. *)
  let network_version_encoding =
    let open Data_encoding in
    conv
      (fun Network_version.{chain_name; distributed_db_version; p2p_version} ->
        (chain_name, distributed_db_version, p2p_version))
      (fun (chain_name, distributed_db_version, p2p_version) ->
        Network_version.{chain_name; distributed_db_version; p2p_version})
      (obj3
         (req "chain_name" Distributed_db_version.Name.encoding)
         (req "gossipsub" Distributed_db_version.encoding)
         (req "p2p_version" P2p_version.encoding))

  let encoding =
    let open Data_encoding in
    conv
      (fun {network_version} -> network_version)
      (fun network_version -> {network_version})
      (obj1 (req "network_version" network_version_encoding))
end

module Health = struct
  type status = Up | Degraded | Down | Ok | Ko

  let status_encoding =
    Data_encoding.string_enum
      [
        ("up", Up);
        ("degraded", Degraded);
        ("down", Down);
        ("ok", Ok);
        ("ko", Ko);
      ]

  type t = {status : status; checks : (string * status) list}

  let checks_encoding =
    let open Data_encoding in
    list (obj2 (req "name" string) (req "status" status_encoding))

  let encoding =
    let open Data_encoding in
    conv
      (fun {status; checks} -> (status, checks))
      (fun (status, checks) -> {status; checks})
      (obj2 (req "status" status_encoding) (req "checks" checks_encoding))
end
