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

val consensus_index : int

val votes_index : int

val anonymous_index : int

val managers_index : int

module OpSet : Set.S with type elt = packed_operation

type pool = {
  consensus : OpSet.t;
  votes : OpSet.t;
  anonymous : OpSet.t;
  managers : OpSet.t;
}

val empty : pool

val pp_pool : Format.formatter -> pool -> unit

val pool_to_list_list : pool -> packed_operation list list

val pool_of_list_list : packed_operation list list -> pool

val filter_pool : (packed_operation -> bool) -> pool -> pool

type ordered_pool = {
  ordered_consensus : packed_operation list;
  ordered_votes : packed_operation list;
  ordered_anonymous : packed_operation list;
  ordered_managers : packed_operation list;
}

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

val add_operation : pool -> packed_operation -> pool

val add_operations : pool -> packed_operation list -> pool

type consensus_filter = {
  level : int32;
  round : Round.t;
  payload_hash : Block_payload_hash.t;
}

val filter_with_relevant_consensus_ops :
  endorsement_filter:consensus_filter ->
  preendorsement_filter:consensus_filter option ->
  OpSet.t ->
  OpSet.t

val unpack_preendorsement :
  packed_operation -> Kind.preendorsement operation option

val unpack_endorsement : packed_operation -> Kind.endorsement operation option

val filter_preendorsements :
  packed_operation list -> Kind.preendorsement operation list

val filter_endorsements :
  packed_operation list -> Kind.endorsement operation list

val ordered_to_list_list : ordered_pool -> packed_operation list list

val ordered_of_list_list : packed_operation list list -> ordered_pool option

(** [preendorsements] <> None => (List.length preendorsements > 0) *)
val extract_operations_of_list_list :
  packed_operation list list ->
  (Kind.preendorsement operation list option
  * Kind.endorsement operation list
  * payload)
  option
