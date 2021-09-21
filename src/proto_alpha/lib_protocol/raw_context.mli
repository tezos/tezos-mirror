(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** {1 Errors} *)

type error += Too_many_internal_operations (* `Permanent *)

type missing_key_kind = Get | Set | Del | Copy

(** An internal storage error that should not happen *)
type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

type error += Storage_error of storage_error

type error += Failed_to_parse_parameter of bytes

type error += Failed_to_decode_parameter of Data_encoding.json * string

val storage_error : storage_error -> 'a tzresult

(** {1 Abstract Context} *)

(** Abstract view of the context.
    Includes a handle to the functional key-value database
    ({!Context.t}) along with some in-memory values (gas, etc.). *)
type t

type root = t

(** Retrieves the state of the database and gives its abstract view.
    It also returns wether this is the first block validated
    with this version of the protocol. *)
val prepare :
  level:Int32.t ->
  predecessor_timestamp:Time.t ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  Context.t ->
  t tzresult Lwt.t

type previous_protocol = Genesis of Parameters_repr.t | Hangzhou_011

val prepare_first_block :
  level:int32 ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  Context.t ->
  (previous_protocol * t) tzresult Lwt.t

val activate : t -> Protocol_hash.t -> t Lwt.t

(** Returns the state of the database resulting of operations on its
    abstract view *)
val recover : t -> Context.t

val current_level : t -> Level_repr.t

val predecessor_timestamp : t -> Time.t

val current_timestamp : t -> Time.t

val current_fitness : t -> Int64.t

val set_current_fitness : t -> Int64.t -> t

val constants : t -> Constants_repr.parametric

val patch_constants :
  t -> (Constants_repr.parametric -> Constants_repr.parametric) -> t Lwt.t

(** Retrieve the cycle eras. *)
val cycle_eras : t -> Level_repr.cycle_eras

(** Increment the current block fee stash that will be credited to baker's
    frozen_fees account at finalize_application *)
val add_fees : t -> Tez_repr.t -> t tzresult

(** Increment the current block reward stash that will be credited to baker's
    frozen_fees account at finalize_application *)
val add_rewards : t -> Tez_repr.t -> t tzresult

val get_fees : t -> Tez_repr.t

val get_rewards : t -> Tez_repr.t

type error += Gas_limit_too_high (* `Permanent *)

val check_gas_limit_is_valid : t -> 'a Gas_limit_repr.Arith.t -> unit tzresult

val consume_gas_limit_in_block : t -> 'a Gas_limit_repr.Arith.t -> t tzresult

val set_gas_limit : t -> 'a Gas_limit_repr.Arith.t -> t

val set_gas_unlimited : t -> t

val gas_level : t -> Gas_limit_repr.t

val gas_consumed : since:t -> until:t -> Gas_limit_repr.Arith.fp

val remaining_operation_gas : t -> Gas_limit_repr.Arith.fp

val update_remaining_operation_gas : t -> Gas_limit_repr.Arith.fp -> t

val block_gas_level : t -> Gas_limit_repr.Arith.fp

val storage_space_to_pay : t -> Z.t option

val init_storage_space_to_pay : t -> t

val update_storage_space_to_pay : t -> Z.t -> t

val update_allocated_contracts_count : t -> t

val clear_storage_space_to_pay : t -> t * Z.t * int

type error += Undefined_operation_nonce (* `Permanent *)

val init_origination_nonce : t -> Operation_hash.t -> t

val get_origination_nonce : t -> Contract_repr.origination_nonce tzresult

val increment_origination_nonce :
  t -> (t * Contract_repr.origination_nonce) tzresult

val unset_origination_nonce : t -> t

(** {1 Generic accessors} *)

type key = string list

type value = bytes

type tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

include T with type t := t

(** Initialize the local nonce used for preventing a script to
    duplicate an internal operation to replay it. *)
val reset_internal_nonce : t -> t

(** Increments the internal operation nonce. *)
val fresh_internal_nonce : t -> (t * int) tzresult

(** Mark an internal operation nonce as taken. *)
val record_internal_nonce : t -> int -> t

(** Check is the internal operation nonce has been taken. *)
val internal_nonce_already_recorded : t -> int -> bool

(** Returns a map where to each endorser's pkh is associated the list of its
    endorsing slots (in increasing order) for a given level. *)
val allowed_endorsements :
  t ->
  (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t

(** Keep track of the number of endorsements that are included in a block *)
val included_endorsements : t -> int

(** Initializes the map of allowed endorsements, this function must only be
    called once. *)
val init_endorsements :
  t ->
  (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t ->
  t

(** Marks an endorsement in the map as used. *)
val record_endorsement : t -> Signature.Public_key_hash.t -> t

val fold_map_temporary_lazy_storage_ids :
  t ->
  (Lazy_storage_kind.Temp_ids.t -> Lazy_storage_kind.Temp_ids.t * 'res) ->
  t * 'res

val map_temporary_lazy_storage_ids_s :
  t ->
  (Lazy_storage_kind.Temp_ids.t -> (t * Lazy_storage_kind.Temp_ids.t) Lwt.t) ->
  t Lwt.t

module Cache :
  Context.CACHE
    with type t := t
     and type size := int
     and type index := int
     and type identifier := string
     and type key = Context.Cache.key
     and type value = Context.Cache.value
