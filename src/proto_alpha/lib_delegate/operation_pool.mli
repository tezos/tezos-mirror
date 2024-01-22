(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context

module Operation_set : Set.S with type elt = packed_operation

(** Generic base type for pools *)
type 'collection t = {
  consensus : 'collection;
  votes : 'collection;
  anonymous : 'collection;
  managers : 'collection;
}

(** A pool of operations for a single origin, or undifferenciated origin,
   typically used for operations coming from the node *)
type pool = Operation_set.t t

(** A pool of operations for a single origin, or undifferenciated origin,
   typically used for operations coming from the node *)

(**  on pool *)
val empty : pool

val pp_pool : Format.formatter -> pool -> unit

val filter_pool : (packed_operation -> bool) -> pool -> pool

val add_operation : pool -> packed_operation -> pool

val add_operations : pool -> packed_operation list -> pool

(** {2 Ordered pool of operations} *)
type ordered_pool = packed_operation list t

val ordered_pool_encoding : ordered_pool Data_encoding.t

val empty_ordered : ordered_pool

val pp_ordered_pool : Format.formatter -> ordered_pool -> unit

type payload = {
  votes_payload : packed_operation list;
  anonymous_payload : packed_operation list;
  managers_payload : packed_operation list;
}

val empty_payload : payload

val payload_encoding : payload Data_encoding.t

val pp_payload : Format.formatter -> payload -> unit

val payload_of_ordered_pool : ordered_pool -> payload

val ordered_pool_of_payload :
  consensus_operations:packed_operation list -> payload -> ordered_pool

type consensus_filter = {
  level : int32;
  round : Round.t;
  payload_hash : Block_payload_hash.t;
}

val filter_with_relevant_consensus_ops :
  attestation_filter:consensus_filter ->
  preattestation_filter:consensus_filter option ->
  Operation_set.t ->
  Operation_set.t

val unpack_preattestation :
  packed_operation -> Kind.preattestation operation option

val unpack_attestation : packed_operation -> Kind.attestation operation option

val filter_preattestations :
  packed_operation list -> Kind.preattestation operation list

val filter_attestations :
  packed_operation list -> Kind.attestation operation list

val ordered_to_list_list : ordered_pool -> packed_operation list list

val ordered_of_list_list : packed_operation list list -> ordered_pool option

(** [preattestation] <> None => (List.length preattestations > 0) *)
val extract_operations_of_list_list :
  packed_operation list list ->
  (Kind.preattestation operation list option
  * Kind.attestation operation list
  * payload)
  option

module Prioritized_operation : sig
  type t

  (** prioritize operations coming from an external source (file, uri, ...).
      An operation with higher [priority] (aka a bigger integer) will be
      included before others with lower [priority]. *)
  val extern : ?priority:int -> packed_operation -> t

  (** prioritize operations coming from a node *)
  val node : packed_operation -> t

  (** [packed t] retrieves the [packed_operation] wrapped inside [t] *)
  val packed : t -> packed_operation

  (** [compare_priority o1 o2] compares whether [o1] has higher priority than [o2] *)
  val compare_priority : t -> t -> int

  (** [compare] is [compare_priority] when non-zero. This is suitable to
      construct sets of prioritized operations **)
  val compare : t -> t -> int
end

module Prioritized_operation_set : sig
  include Set.S with type elt = Prioritized_operation.t

  (** [operations set] is [elements set |> List.map Prioritized_operation.packed]*)
  val operations : t -> packed_operation list
end

(** Pool of prioritized operations *)
module Prioritized : sig
  (** Same record fields as [type pool], but with a different set base *)
  type nonrec t = Prioritized_operation_set.t t

  (** [of_pool pool] transforms [pool] into a prioritized pool of operations of
      low priority. *)
  val of_pool : pool -> t

  (** [merge_external_operations ?initial_priority pool extern_ops] creates a prioritized pool
     from a [pool] and [extern_ops] coming from an external source, which we
     prioritize.

     Priorities for these operations is given according to the order of the
     list. The first element of the list has highest priority.
   *)
  val merge_external_operations : t -> packed_operation list -> t

  val filter : (packed_operation -> bool) -> t -> t

  val add_operations : t -> Prioritized_operation.t list -> t
end
