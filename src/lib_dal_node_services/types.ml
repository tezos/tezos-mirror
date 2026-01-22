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

type attestation_lag = int

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

  module Cmp = struct
    type nonrec t = t

    let compare t1 t2 =
      Compare.or_else (Cryptobox.Share.compare t1.share t2.share) (fun () ->
          Cryptobox.Proof.compare t1.shard_proof t2.shard_proof)
  end

  include Compare.Make (Cmp)
end

module Peer = struct
  type t = {peer_id : P2p_peer.Id.t; maybe_reachable_point : P2p_point.Id.t}

  module Cmp = struct
    type nonrec t = t

    (* We only compare the peer id here. The reason is that it is safe to assume:

       1. For connected peers, they can be identified by a unique IP address.

       2. For not connected peers we want to advertise only one point
       that can change with time. *)
    let compare p1 p2 = P2p_peer.Id.compare p1.peer_id p2.peer_id
  end

  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)

  let encoding =
    let open Data_encoding in
    conv
      (fun {peer_id; maybe_reachable_point} -> (peer_id, maybe_reachable_point))
      (fun (peer_id, maybe_reachable_point) -> {peer_id; maybe_reachable_point})
      (obj2
         (req "peer_id" P2p_peer.Id.encoding)
         (req "maybe_reachable_point" P2p_point.Id.encoding))

  let pp fmt {peer_id; maybe_reachable_point} =
    Format.fprintf
      fmt
      "{ peer_id=%a;@,maybe_reachable_point=%a }"
      P2p_peer.Id.pp
      peer_id
      P2p_point.Id.pp
      maybe_reachable_point
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

  let rpc_encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun span -> Format.asprintf "%a" Ptime.Span.pp span)
      (fun _ -> Stdlib.failwith "This is only used for encoding")
      Data_encoding.string

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

  let equal left right = compare left right = 0

  let hash = Stdlib.Hashtbl.hash

  let pp fmt {slot_level; slot_index} =
    Format.fprintf fmt "{ level=%ld; index=%d }" slot_level slot_index

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

type header_status =
  [ `Waiting_attestation
  | `Attested of attestation_lag
  | `Unattested
  | `Unpublished ]

type shard_index = int

type slot_header = {
  slot_id : slot_id;
  commitment : Cryptobox.Commitment.t;
  status : header_status;
}

type profile = Bootstrap | Controller of Controller_profiles.t

type with_proof = {with_proof : bool}

type proto_parameters = {
  feature_enable : bool;
  incentives_enable : bool;
  dynamic_lag_enable : bool;
  number_of_slots : int;
  attestation_lag : int;
  attestation_lags : int list;
  attestation_threshold : int;
  traps_fraction : Q.t;
  cryptobox_parameters : Cryptobox.Verifier.parameters;
  sc_rollup_challenge_window_in_blocks : int;
  commitment_period_in_blocks : int;
  dal_attested_slots_validity_lag : int;
  blocks_per_cycle : int32;
  minimal_block_delay : int64;
}

type trap = {
  delegate : Signature.Public_key_hash.t;
  slot_index : slot_index;
  shard : Cryptobox.shard;
  shard_proof : Cryptobox.shard_proof;
}

(* Encodings associated to the types. *)

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

let legacy_attestation_lag = 8

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
        (function _ -> None (* Don't encode with this case anymore *))
        (function () -> `Attested legacy_attestation_lag);
      case
        ~title:"unattested"
        (Tag 2)
        (constant "unattested")
        (function `Unattested -> Some () | _ -> None)
        (function () -> `Unattested);
      case
        ~title:"unpublished"
        (Tag 3)
        (constant "unpublished")
        (function `Unpublished -> Some () | _ -> None)
        (function () -> `Unpublished);
      case
        ~title:"attested_with_lag"
        (Tag 4)
        (obj2 (req "kind" (constant "attested")) (req "attestation_lag" uint8))
        (function
          | `Attested attestation_lag -> Some ((), attestation_lag) | _ -> None)
        (function (), attestation_lag -> `Attested attestation_lag);
    ]

let pp_header_status fmt = function
  | `Waiting_attestation -> Format.fprintf fmt "waiting_attestation"
  | `Attested lag -> Format.fprintf fmt "attested(lag:%d)" lag
  | `Unattested -> Format.fprintf fmt "unattested"
  | `Unpublished -> Format.fprintf fmt "unpublished"

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
        ~title:"Bootstrap node"
        (Tag 1)
        (obj1 (req "kind" (constant "bootstrap")))
        (function Bootstrap -> Some () | _ -> None)
        (function () -> Bootstrap);
      case
        ~title:"Controller"
        (Tag 2)
        (obj2
           (req "kind" (constant "controller"))
           (req "controller_profiles" Controller_profiles.encoding))
        (function
          | Controller controller_profiles -> Some ((), controller_profiles)
          | _ -> None)
        (function (), controller_profiles -> Controller controller_profiles);
    ]

let with_proof_encoding =
  let open Data_encoding in
  conv
    (fun {with_proof} -> with_proof)
    (fun with_proof -> {with_proof})
    (obj1 (req "with_proof" bool))

let q_encoding =
  Data_encoding.(
    conv
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) -> Q.make num den)
      (obj2 (req "numerator" z) (req "denominator" z)))

let proto_parameters_encoding : proto_parameters Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           feature_enable;
           incentives_enable;
           dynamic_lag_enable;
           number_of_slots;
           attestation_lag;
           attestation_lags;
           attestation_threshold;
           traps_fraction;
           cryptobox_parameters;
           sc_rollup_challenge_window_in_blocks;
           commitment_period_in_blocks;
           dal_attested_slots_validity_lag;
           blocks_per_cycle;
           minimal_block_delay;
         }
       ->
      ( ( feature_enable,
          incentives_enable,
          dynamic_lag_enable,
          number_of_slots,
          attestation_lag,
          attestation_lags,
          attestation_threshold,
          traps_fraction ),
        ( cryptobox_parameters,
          sc_rollup_challenge_window_in_blocks,
          commitment_period_in_blocks,
          dal_attested_slots_validity_lag,
          blocks_per_cycle,
          minimal_block_delay ) ))
    (fun ( ( feature_enable,
             incentives_enable,
             dynamic_lag_enable,
             number_of_slots,
             attestation_lag,
             attestation_lags,
             attestation_threshold,
             traps_fraction ),
           ( cryptobox_parameters,
             sc_rollup_challenge_window_in_blocks,
             commitment_period_in_blocks,
             dal_attested_slots_validity_lag,
             blocks_per_cycle,
             minimal_block_delay ) )
       ->
      {
        feature_enable;
        incentives_enable;
        dynamic_lag_enable;
        number_of_slots;
        attestation_lag;
        attestation_lags;
        attestation_threshold;
        traps_fraction;
        cryptobox_parameters;
        sc_rollup_challenge_window_in_blocks;
        commitment_period_in_blocks;
        dal_attested_slots_validity_lag;
        blocks_per_cycle;
        minimal_block_delay;
      })
    (merge_objs
       (obj8
          (req "feature_enable" bool)
          (req "incentives_enable" bool)
          (req "dynamic_lag_enable" bool)
          (req "number_of_slots" int31)
          (req "attestation_lag" int31)
          (req "attestation_lags" (list int31))
          (req "attestation_threshold" int31)
          (req "traps_fraction" q_encoding))
       (obj6
          (req "cryptobox_parameters" Cryptobox.Verifier.parameters_encoding)
          (req "sc_rollup_challenge_window_in_blocks" int31)
          (req "commitment_period_in_blocks" int31)
          (req "dal_attested_slots_validity_lag" int31)
          (req "blocks_per_cycle" int32)
          (req "minimal_block_delay" int64)))

let trap_encoding =
  Data_encoding.(
    conv
      (fun {delegate; slot_index; shard; shard_proof} ->
        (delegate, slot_index, shard, shard_proof))
      (fun (delegate, slot_index, shard, shard_proof) ->
        {delegate; slot_index; shard; shard_proof})
      (obj4
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "slot_index" uint8)
         (req "shard" Cryptobox.shard_encoding)
         (req "proof" Cryptobox.shard_proof_encoding)))

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
  type status = Up | Degraded | Down | Ok | Ko | No

  let status_encoding =
    Data_encoding.string_enum
      [
        ("up", Up);
        ("degraded", Degraded);
        ("down", Down);
        ("ok", Ok);
        ("ko", Ko);
        ("no", No);
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

  let pp_status fmt status =
    Format.pp_print_string
      fmt
      (match status with
      | Up -> "up"
      | Degraded -> "degraded"
      | Down -> "down"
      | Ok -> "ok"
      | Ko -> "ko"
      | No -> "no")

  let pp fmt {status; checks} =
    Format.fprintf
      fmt
      "status: %a; checks: %a"
      pp_status
      status
      (Format.pp_print_list
         ~pp_sep:Format.pp_print_cut
         (fun fmt (name, status) ->
           Format.fprintf fmt "(%s: %a)" name pp_status status))
      checks
end

module SlotIdSet =
  Aches.Vache.Set (Aches.Vache.LRU_Sloppy) (Aches.Vache.Strong)
    (struct
      type t = Slot_id.t

      let equal = Slot_id.equal

      let hash = Slot_id.hash
    end)

module Attestable_event = struct
  type backfill_payload = {
    slot_ids : slot_id list;
    trap_slot_ids : slot_id list;
    no_shards_attestation_levels : level list;
  }

  type t =
    | Attestable_slot of {slot_id : slot_id}
    | No_shards_assigned of {committee_level : level}
    | Slot_has_trap of {slot_id : slot_id}
    | Backfill of {backfill_payload : backfill_payload}

  let backfill_payload_encoding =
    let open Data_encoding in
    conv
      (fun {slot_ids; trap_slot_ids; no_shards_attestation_levels} ->
        (slot_ids, trap_slot_ids, no_shards_attestation_levels))
      (fun (slot_ids, trap_slot_ids, no_shards_attestation_levels) ->
        {slot_ids; trap_slot_ids; no_shards_attestation_levels})
      (obj3
         (req "slot_ids" (list slot_id_encoding))
         (req "trap_slot_ids" (list slot_id_encoding))
         (req "no_shards_attestation_levels" (list int32)))

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"attestable_slot"
          (Tag 0)
          (obj2
             (req "kind" (constant "attestable_slot"))
             (req "slot_id" slot_id_encoding))
          (function
            | Attestable_slot {slot_id} -> Some ((), slot_id) | _ -> None)
          (fun ((), slot_id) -> Attestable_slot {slot_id});
        case
          ~title:"no_shards_assigned"
          (Tag 1)
          (obj2
             (req "kind" (constant "no_shards_assigned"))
             (req "committee_level" int32))
          (function
            | No_shards_assigned {committee_level} -> Some ((), committee_level)
            | _ -> None)
          (fun ((), committee_level) -> No_shards_assigned {committee_level});
        case
          ~title:"slot_has_trap"
          (Tag 2)
          (obj2
             (req "kind" (constant "slot_has_trap"))
             (req "slot_id" slot_id_encoding))
          (function Slot_has_trap {slot_id} -> Some ((), slot_id) | _ -> None)
          (fun ((), slot_id) -> Slot_has_trap {slot_id});
        case
          ~title:"backfill"
          (Tag 3)
          (obj2
             (req "kind" (constant "backfill"))
             (req "backfill_payload" backfill_payload_encoding))
          (function
            | Backfill {backfill_payload} -> Some ((), backfill_payload)
            | _ -> None)
          (fun ((), backfill_payload) -> Backfill {backfill_payload});
      ]
end
