(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** To verify the proof of a page membership in its associated slot, the
     Cryptobox module needs the following Dal parameters. These are part of the
     protocol's parameters. See {!Dal.Config.default}. *)
type parameters = Dal.parameters = {
  redundancy_factor : int;
  page_size : int;
  slot_size : int;
  number_of_shards : int;
}

(** An encoding for values of type {!parameters}. *)
val parameters_encoding : parameters Data_encoding.t

(** Slot header representation for the data-availability layer.

    {1 Overview}

    For the data-availability layer, the L1 provides a list of slots
   at every level. A slot is a blob of data that can be interpreted by
   the users of the data-availability layer (such as SCORU).

    The purpose of the data-availability layer is to increase the
   bandwidth of the layer 1 thanks to the distribution of "slots". A
   slot is never posted directly onto the layer 1 blocks but on the
   data-availability layer. The producer of a slot still has to post a
   slot header onto the layer 1. A slot header is an abstract datatype
   certifying that the corresponding slot has some maximum size
   (provided by the layer 1). In other words, the whole data contained
   into the slot cannot exceed some fixed size. This is to avoid
   attacks where a slot header would be posted onto the layer 1 block,
   declared available by the protocol, but actually the slot size
   would be too large to be refuted a posteriori.

   The slot header can also be used to prove that a blob of data is a
   portion of the initial slot. *)

module Commitment : sig
  (** A slot commitment is provided via the environment. *)
  type t = Dal.commitment

  val encoding : t Data_encoding.t

  (** A dummy value for a commitment. This commitment does not
     correspond to any valid pre-image. *)
  val zero : t

  (** Attempt to convert the input representing a commitment encoded as a b58
      string. *)
  val of_b58check_opt : string -> t option
end

module Commitment_proof : sig
  (** A slot commitment proof is provided via the environment. *)
  type t = Dal.commitment_proof

  val encoding : t Data_encoding.t

  (** A dummy value for a commitment proof. *)
  val zero : t
end

(** The module below provides some data types and helper functions for DAL
    commitments published at some level on some slot index. There are mainly
    three important levels for a DAL commitment successfully included in a
    block:

    - `published_level`: The level of the block that contains the DAL publish
    commitment operation. Such operations are typically injected on top of the
    block whose level is `published_level - 1` or earlier.

    - `attested_level`: The level of the block / context that includes the
    attestation status of a slot published `attestation_lag` levels
    earlier. Formally, `attested_level = published_level + attestation_lag`.

   - `attestation_level`: The level of the block on top of which DAL
   attestations are injected for a commitment published at published_level.
   Formally, attestation_level = attested_level - 1 = published_level +
   attestation_lag - 1. *)
module Header : sig
  (** For Layer-1, a slot is identified by the level at which it is published
      and the slot's index. *)
  type id = {published_level : Raw_level_repr.t; index : Dal_slot_index_repr.t}

  (** For Layer-1, a slot is described by its slot {!type-id} and the
     slot's commitment. *)
  type t = {id : id; commitment : Commitment.t}

  (** encoding for values of type {!type-id}. *)
  val id_encoding : id Data_encoding.t

  (** encoding for values of type {!t}. *)
  val encoding : t Data_encoding.t

  (** pretty-printer for values of type {!type-id}. *)
  val pp_id : Format.formatter -> id -> unit

  (** pretty-printer for values of type {!t}. *)
  val pp : Format.formatter -> t -> unit

  (** equal function for values of type {!t}. *)
  val equal : t -> t -> bool

  (** equal function for values of type id. *)
  val slot_id_equal : id -> id -> bool

  (** [verify_commitment cryptobox commitment commitment_proof] check
     that for the given commitment, the commitment proof is correct
     using the [cryptbox] primitives. *)
  val verify_commitment :
    Dal.t -> Commitment.t -> Commitment_proof.t -> bool tzresult
end

module Shard_with_proof : sig
  type t = {shard : Dal.shard; proof : Dal.shard_proof}

  type error += Dal_shard_proof_error of string

  val encoding : t Data_encoding.t

  (* [verify cryptobox commitment t] verifies the shard proof from [t] against
     the shard in [t] and the provided [commitment]. Returns
     [Dal_shard_proof_error] if the verification fails. *)
  val verify : Dal.t -> Commitment.t -> t -> unit tzresult

  (** [share_is_trap pkh share ~traps_fraction] determines whether the given
      [share] is classified as a "trap" for the delegate identified by [pkh],
      based on the fraction [traps_fraction] of shards that should be traps.

    The function computes the hash of the concatenation of [pkh] and [share],
    denoted as `hash(pkh . share)`, where the dot represents concatenation.
    It then checks if this hash value is less than `traps_fraction * 2^n`, where
    `n` is the (fixed) bit size of the hash.

    The function returns:

    - [Ok true] if the share is considered a trap for the given [pkh].

    - [Ok false] if the share is not a trap.

    - [Error Dal_encoding_error_in_share_is_trap] if there is an issue encoding
    [pkh] or [share].

    This function assumes [traps_fraction] is valid (i.e., a rational number within
    [0, 1]).
*)
  val share_is_trap :
    Signature.Public_key_hash.t ->
    Dal.share ->
    traps_fraction:Q.t ->
    bool tzresult
end

(** A DAL slot is decomposed to a successive list of pages with fixed content
   size. The size is chosen so that it's possible to inject a page in a Tezos
   L1 operation if needed during the proof phase of a refutation game.
*)
module Page : sig
  type content = Bytes.t

  type slot_index = Dal_slot_index_repr.t

  val pages_per_slot : Dal.parameters -> int

  module Index : sig
    type t = int

    val zero : int

    val encoding : int Data_encoding.t

    val pp : Format.formatter -> int -> unit

    val compare : int -> int -> int

    val equal : int -> int -> bool

    type error += Invalid_page_index of {given : int; min : int; max : int}

    (** [is_in_range ~number_of_pages page_id] returns true if and only if the
      provided [page_id] is within the bounds of allowed pages. *)
    val check_is_in_range : number_of_pages:int -> int -> unit tzresult
  end

  (** Encoding for page contents. *)
  val content_encoding : content Data_encoding.t

  (** A page is identified by its slot ID and by its own index in the list
     of pages of the slot. *)
  type t = {slot_id : Header.id; page_index : Index.t}

  type proof = Dal.page_proof

  (** equal function for values of type {!t}. *)
  val equal : t -> t -> bool

  (** encoding for values of type {!t}. *)
  val encoding : t Data_encoding.t

  (** encoding for values of type {!proof}. *)
  val proof_encoding : proof Data_encoding.t

  (** pretty-printer for values of type {!t}. *)
  val pp : Format.formatter -> t -> unit
end

(** Only one slot header is accepted per slot index. If two slots
   headers are included into a block, the second one will fail.

   Consequently, we rely on the order of operations which is done
   thanks to the fee market.

  This is encapsulated in the following module.  *)
module Slot_market : sig
  (** Represent the fee market for a list of slots. *)
  type t

  (** [init ~length] encodes a list of [length] slots without
     candidates. *)
  val init : length:int -> t

  (** [length t] returns the [length] provided at initialisation time
     (see {!val:init}). *)
  val length : t -> int

  (** [register t slot_header ~source] possibly updates the candidate associated
      to [slot_header.id] with the [slot_header] published by [source].

      The function returns [Some (_, true)] if the candidate is
      registered. Returns [Some (_, false)] otherwise. Returns [None] if
      [slot_header.id] is not a valid slot id. *)
  val register : t -> Header.t -> source:Contract_repr.t -> (t * bool) option

  (** [candidates t] returns a list of slot header candidates associated to the
      contract address who published them. *)
  val candidates : t -> (Header.t * Contract_repr.t) list
end

(** This module provides an abstract data structure (type {!History.t}) that
    represents a skip list used to store successive DAL slots confirmed/attested
    on L1. There is one slot per cell in the skip list. The slots are sorted in
    increasing order by level, and by slot index, for the slots of the same
    level.

    This module also defines a bounded history cache (type
    {!type-History.History_cache.t}) that allows to remember recent values of a
    skip list of type {!History.t} (indexed by the skip lists' hashes). This
    structure is meant to be maintained and used by the rollup node to produce
    refutation proofs involving DAL slot inputs.

    Note on terminology: "confirmed slot" is another name for "attested slot".
*)
module History : sig
  (** Abstract representation of a skip list specialized for
       confirmed slot headers. *)
  type t

  (** The content of a cell in the DAL skip list. We have [number_of_slots] new
      cells per level (one per slot index). For a given slot index in
      [0..number_of_slots-1], the commitment is either [Unpublished] or
      [Published]. In this second case, we attach extra information in addition
      to the id such as the commitment, the publisher, the number of attested
      shards and whether the commitment is attested from the point of view of
      the protocol. *)
  type cell_content = private
    | Unpublished of Header.id
    | Published of {
        header : Header.t;
        publisher : Contract_repr.t;
        is_proto_attested : bool;
        attested_shards : int;
        total_shards : int;
      }

  (** Returns the {!cell_content} of the given skip list cell. *)
  val content : t -> cell_content

  val pp_content : Format.formatter -> cell_content -> unit

  (** Returns the slot id of the cell whose content is given. *)
  val content_id : cell_content -> Header.id

  module Pointer_hash : S.HASH

  (** Type of hashes of history. *)
  type hash = Pointer_hash.t

  (** Encoding of the datatype. *)
  val encoding : t Data_encoding.t

  (** The genesis skip list that contains one dummy cell. This cell has
      {!Raw_level_repr.root} as published level and no attested slots. Since Dal
      is not necessarily activated in the genesis block (e.g. this will be the case
      on mainnet), the skip list is reset at the first call to
      {!update_skip_list} to enforce the invariant that there are no gaps
      in the levels of the cells of the skip list.

      So, a skip list is initialized with this genesis cell. It's then replaced
      with a growing (non-dummy) skip list as soon as a call to
      {!update_skip_list} with a level bigger than
      {!Raw_level_repr.root} is performed. This allows to activate Dal at any
      level and having a contiguous skip list (w.r.t. L1 levels). This
      representation allows to produce simpler proofs with a bounded history
      cache. *)
  val genesis : t

  (** Returns the hash of an history. *)
  val hash : t -> hash

  (** The [History_cache.t] structure is basically a bounded lookup table of
      {!t} skip lists. (See {!Bounded_history_repr.S}). In the L1 layer, the
      capacity (bound) is set to zero (nothing is remembered). By contrast,
      the rollup node uses a history cache with a (sufficiently) large capacity
      to participate in all potential refutation games occurring during the
      challenge period. Indeed, the successive recent skip-lists stored in
      the cache are needed to produce proofs involving slots' pages. *)
  module History_cache :
    Bounded_history_repr.S with type key = hash and type value = t

  (** [update_skip_list hist cache ~published_level
      ~number_of_slots slot_headers_with_statuses] updates the given structure
      [hist] with the list of [slot_headers_with_statuses]. The given [cache] is
      also updated to add successive values of [cell] to it.


      This function checks the following pre-conditions before updating the
      list:

      - The given [published_level] should match all the levels of the slots in
      [slot_headers], if any;

      - [published_level] is the successor the last inserted cell's level.

      - [slot_headers_with_statuses] is sorted in increasing order w.r.t. slots
      indices. *)
  val update_skip_list :
    t ->
    History_cache.t ->
    published_level:Raw_level_repr.t ->
    number_of_slots:int ->
    (Header.t
    * Contract_repr.t
    * Dal_attestation_repr.Accountability.attestation_status)
    list ->
    (t * History_cache.t) tzresult

  (** Similiar to {!update_skip_list}, but no cache is provided or
      updated. *)
  val update_skip_list_no_cache :
    t ->
    published_level:Raw_level_repr.t ->
    number_of_slots:int ->
    (Header.t
    * Contract_repr.t
    * Dal_attestation_repr.Accountability.attestation_status)
    list ->
    t tzresult

  (** [equal a b] returns true iff a is equal to b. *)
  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  (** {1 Dal slots/pages proofs} *)

  (** When a SCORU kernel's inputs come from the DAL, they are provided as
      pages' content for confirmed slots, or None in case the slot doesn't
      exist or is not confirmed.

      In a refutation game involving an import tick of a Dal page input, a
      honest user should be able to provide:

      - When the PVM is requesting a page of a confirmed slot: a proof that the
      slot is confirmed, in addition to needed information to check that the
      page (whose id and content are given) is part of the slot;

      - When the opponent pretends that the PVM is requesting a page of some
      unconfirmed slot, but that slot is not published or not confirmed on L1:
      a proof that the slot (whose id is given via the page's id) cannot be
      confirmed on L1.

      See the documentation in the ml file for more technical details. *)
  type proof

  (** Encoding for {!proof}. *)
  val proof_encoding : proof Data_encoding.t

  (** Pretty-printer for {!proof}. If [serialized] is [false] it will print
      the abstracted proof representation, otherwise if it's [true] it will
      print the serialized version of the proof (i.e. a sequence of bytes). *)
  val pp_proof : serialized:bool -> Format.formatter -> proof -> unit

  (** This function returns some commitment if and only if the skip list cell
      whose content is given is supposed to be attested.

      Attestation status is either checked by inspecting the protocol's boolean
      flag stored in the content if [attestation_threshold_percent] is None
      (Regular DAL), or by checking that attestation threshold is reached and
      the publisher is whitelisted, if [restricted_commitments_publishers] is
      set to some whitelist, when using Adjustable DAL. *)
  val is_commitment_attested :
    attestation_threshold_percent:int option ->
    restricted_commitments_publishers:Contract_repr.t list option ->
    cell_content ->
    Commitment.t option

  (** [produce_proof dal_parameters page_id ~attestation_threshold_percent
      ~restricted_commitments_publishers page_info ~get_history slots_hist]
      produces a proof that either:

      - there exists a confirmed slot in the skip list that contains
        the page identified by [page_id] whose data and slot inclusion proof
        are given by [page_info], or

      - there cannot exist a confirmed slot in the skip list (whose head is
        given by [slots_hist]) containing the page identified by [page_id].

      In the first case above, [page_info] should contain the page's content
      and the proof that the page is part of the (confirmed) slot whose
      id is given in [page_id]. In the second case, no page content or proof
      should be provided, as they are not needed to construct a non-confirmation
      proof.

      The function returns an error in case the slot is not confirmed but the
      page's content and proof are given. It also fails if the slot is confirmed
      but no or bad information about the page are provided.

      Note that, in case the level of the page is far in the past (the Dal skip
      list was not populated yet or the slots of the associated level are not
      valid anymore) should be handled by the caller.

      [dal_parameters] is used when verifying that/if the page is part of
      the candidate slot (if any).

      In case [attestation_threshold_percent] is set to some threshold, the
      attestation threshold of the commitment is used instead of the Protocol's
      flag to decide if the page's slot is attested. Furthermore, in case some
      publishers are provided in [restricted_commitments_publishers], we also
      check if the commitment's publisher is whitelisted in that list. *)
  val produce_proof :
    parameters ->
    attestation_threshold_percent:int option ->
    restricted_commitments_publishers:Contract_repr.t list option ->
    Page.t ->
    page_info:(Page.content * Page.proof) option ->
    get_history:(hash -> t option Lwt.t) ->
    t ->
    (proof * Page.content option) tzresult Lwt.t

  (** [verify_proof dal_params page_id snapshot proof] verifies
      that the given [proof] is a valid proof to show that either:

      - the page identified by [page_id] belongs to a confirmed slot stored in
      the skip list whose head is [snapshot], or

      - there is not confirmed slot in the skip list (whose head is) [snapshot]
      that could contain the page identified by [page_id].

      [dal_parameters] is used when verifying that/if the page is part of
      the candidate slot (if any). *)
  val verify_proof : parameters -> Page.t -> t -> proof -> bytes option tzresult

  (** Given a DAL proof, this function returns the values of the fields
      [attestation_threshold_percent] [restricted_commitments_publishers] stored
      in it. *)
  val adal_parameters_of_proof :
    proof -> (int option * Contract_repr.t list option) tzresult

  type error += Add_element_in_slots_skip_list_violates_ordering

  type error +=
    | Dal_page_proof_error of string
    | Unexpected_page_size of {expected_size : int; page_size : int}

  module Internal_for_tests : sig
    (** [proof_statement_is serialized_proof expected] will return [true] if
        the deserialized proof and the [expected] proof shape match and [false]
        otherwise.
        Note that it will also return [false] if deserialization fails.  *)
    val proof_statement_is : proof -> [`Confirmed | `Unconfirmed] -> bool
  end
end
