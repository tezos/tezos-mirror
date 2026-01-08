(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Returns an object of type {!Cryptobox.t} from the given DAL paramters. *)
val mk_cryptobox : Cryptobox.parameters -> Cryptobox.t tzresult Lwt.t

(** Derive new DAL parameters from the given ones by:
    - setting the given redundancy factor ;
    - dividing the other fields by the given factor.
*)
val derive_dal_parameters :
  Cryptobox.parameters ->
  redundancy_factor:int ->
  constants_divider:int ->
  Cryptobox.parameters

(** Returns the slot id of the given cell's content . *)
val content_slot_id :
  Dal_slot_repr.History.cell_content -> Dal_slot_repr.Header.id

(** Builds a DAL attestation bitset containing the provided slot indexes. *)
val dal_attestation :
  Alpha_context.Dal.Slot_index.t list -> Alpha_context.Dal.Attestation.t

(** Returns [true] iff the given delegate has at least one assigned shard at the
    given level. *)
val has_assigned_shards :
  Context.t ->
  ?level:Protocol.Alpha_context.Raw_level.t ->
  Alpha_context.public_key_hash ->
  bool tzresult Lwt.t

module Make (P : sig
  val dal_parameters : Alpha_context.Constants.Parametric.dal

  val cryptobox : Cryptobox.t tzresult Lwt.t Lazy.t
end) : sig
  (** Some global constants. *)

  val genesis_history : Dal_slot_repr.History.t

  val genesis_history_cache : Dal_slot_repr.History.History_cache.t

  val level_one : Raw_level_repr.t

  val level_ten : Raw_level_repr.t

  (** Helper functions. *)

  (** Retrieves the history from a given cache. *)
  val get_history :
    Dal_slot_repr.History.History_cache.t ->
    Dal_slot_repr.History.hash ->
    Dal_slot_repr.History.t option Lwt.t

  (** Returns the slot's polynomial from the given slot's data. *)
  val dal_mk_polynomial_from_slot : bytes -> Cryptobox.polynomial tzresult Lwt.t

  (* Commits to the given polynomial. *)
  val dal_commit :
    Cryptobox.t ->
    Cryptobox.polynomial ->
    (Cryptobox.commitment, error trace) result

  (** Using the given slot's polynomial, this function computes the page proof of
      the page whose ID is provided.  *)
  val dal_mk_prove_page :
    Cryptobox.polynomial ->
    Dal_slot_repr.Page.t ->
    Cryptobox.page_proof tzresult Lwt.t

  (** Constructs a slot whose ID is defined from the given level and given
      index, and whose data are built using the given fill function. The function
      returns the slot's data, polynomial, and header (in the sense: ID + kate
      commitment). *)
  val mk_slot :
    ?level:Raw_level_repr.t ->
    ?index:Dal_slot_index_repr.t ->
    ?fill_function:(int -> char) ->
    unit ->
    (bytes * Cryptobox.polynomial * Dal_slot_repr.Header.t) tzresult Lwt.t

  (** Constructs a record value of type Page.id. *)
  val mk_page_id :
    Raw_level_repr.t -> Dal_slot_index_repr.t -> int -> Dal_slot_repr.Page.t

  val no_data : (default_char:char -> int -> bytes option) option

  (** Constructs a page whose level and slot indexes are those of the given slot
      (except if level is redefined via [?level]), and whose page index and data
      are given by arguments [page_index] and [mk_data]. If [mk_data] is set to
      [No], the function returns the pair (None, page_id). Otherwise, the page's
      [data] and [proof] is computed, and the function returns [Some (data,
      proof), page_id]. *)
  val mk_page_info :
    ?default_char:char ->
    ?level:Raw_level_repr.t ->
    ?slot_index:Dal_slot_index_repr.t ->
    ?page_index:int ->
    ?custom_data:(default_char:char -> int -> bytes option) option ->
    Dal_slot_repr.Header.t ->
    Cryptobox.polynomial ->
    ( (bytes * Cryptobox.page_proof) option * Dal_slot_repr.Page.t,
      error trace )
    result
    Lwt.t

  (** Returns the char after [c]. Restarts from the char whose code is 0 if [c]'s
      code is 255. *)
  val next_char : char -> char

  (** Increment the given slot index. Returns zero in case of overflow. *)
  val succ_slot_index : Dal_slot_index_repr.t -> Dal_slot_index_repr.t

  (** Auxiliary test function used by both unit and PBT tests: This function
      produces a proof from the given information and verifies the produced
      result, if any. The result of each step is checked with
      [check_produce_result] and [check_verify_result], respectively. *)
  val produce_and_verify_proof :
    check_produce:
      ((Dal_slot_repr.History.proof * bytes option) tzresult ->
      (bytes * Cryptobox.page_proof) option ->
      (unit, tztrace) result Lwt.t) ->
    ?check_verify:
      (bytes option tzresult ->
      (bytes * Cryptobox.page_proof) option ->
      (unit, tztrace) result Lwt.t) ->
    get_history:
      (Dal_slot_repr.History.hash -> Dal_slot_repr.History.t option Lwt.t) ->
    Dal_slot_repr.History.t ->
    page_info:(bytes * Cryptobox.page_proof) option ->
    page_id:Dal_slot_repr.Page.t ->
    (unit, tztrace) result Lwt.t

  (** Check if two page proofs are equal. *)
  val eq_page_proof : Cryptobox.page_proof -> Cryptobox.page_proof -> bool

  (** Helper for the case where produce_proof is expected to succeed. *)
  val successful_check_produce_result :
    __LOC__:string ->
    [`Confirmed | `Unconfirmed] ->
    (Dal_slot_repr.History.proof * bytes option) tzresult ->
    (bytes * 'a) option ->
    (unit, tztrace) result Lwt.t

  (** Helper for the case where verify_proof is expected to succeed. *)
  val successful_check_verify_result :
    __LOC__:string ->
    [> `Confirmed] ->
    bytes option tzresult ->
    (bytes * 'a) option ->
    (unit, tztrace) result Lwt.t

  (** Helper for the case where produce_proof is expected to fail because the slot
      is confirmed but no page information are provided. *)
  val slot_confirmed_but_page_data_not_provided :
    __LOC__:string -> 'a tzresult -> 'b -> unit tzresult Lwt.t

  (** Helper for the case where produce_proof is expected to fail because the slot
      is not confirmed but page_info are provided. *)
  val slot_not_confirmed_but_page_data_provided :
    __LOC__:string -> 'a tzresult -> 'b -> unit tzresult Lwt.t

  (** Helper for the case where produce_proof is expected to fail. *)
  val failing_check_produce_result :
    __LOC__:string ->
    expected_error:Environment.Error_monad.error ->
    'a tzresult ->
    'b ->
    unit tzresult Lwt.t

  (** Helper for the case where produce_proof is expected to fail because some
      cells are missing in the history cache. *)
  val bad_history_cache :
    __LOC__:string -> 'a tzresult -> 'b -> unit tzresult Lwt.t
end

(** Builds a {!Alpha_context.type-dal_content} from its {!Z.t}
    representation, that is, the sum of powers of two of the indexes
    of attested slots.

    Raises an exception when the given argument is negative. *)
val dal_content_of_z : Z.t -> Alpha_context.dal_content tzresult Lwt.t

(** Builds a {!Alpha_context.type-dal_content} from a list of attested
    slots.

    @param [number_of_slots] defaults to
      {!Default_parameters.constants_test.dal.number_of_slots}.

    Fails when any of the attested slots is negative or greater than
    or equal to [number_of_slots]. *)
val dal_content_of_int_list :
  ?number_of_slots:int -> int list -> Alpha_context.dal_content

(** A list of varied dal_content options, for tests where we want to
    build attestations with different dal contents. *)
val various_dal_contents : Alpha_context.dal_content option list

(** Transform a list of committee members into a list of [(member,
    dal)] where [dal] is picked successively from
    {!various_dal_contents} (going back to the beginning of
    {!various_dal_contents} if it is shorter than the provided
    committee.

    Depending on when this function is called, ['a] may be e.g. the
    {!Signature.public_key_hash} of delegates or their consensus key,
    or {!Op.attesting_slot}. *)
val committee_with_various_dal_contents :
  'a list -> ('a * Alpha_context.dal_content option) list
