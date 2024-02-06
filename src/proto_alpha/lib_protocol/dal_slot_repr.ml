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

type parameters = Dal.parameters = {
  redundancy_factor : int;
  page_size : int;
  slot_size : int;
  number_of_shards : int;
}

let parameters_encoding = Dal.parameters_encoding

module Commitment = struct
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3389

     It is not clear whether the size of the slot associated to the
     commitment should be given here. *)
  type t = Dal.commitment

  let equal = Dal.Commitment.equal

  let encoding = Dal.Commitment.encoding

  let pp = Dal.Commitment.pp

  let zero = Dal.Commitment.zero

  let of_b58check_opt = Dal.Commitment.of_b58check_opt
end

module Commitment_proof = struct
  type t = Dal.commitment_proof

  let encoding = Dal.Commitment_proof.encoding

  let zero = Dal.Commitment_proof.zero
end

module Header = struct
  type id = {published_level : Raw_level_repr.t; index : Dal_slot_index_repr.t}

  type t = {id : id; commitment : Commitment.t}

  let slot_id_equal {published_level; index} s2 =
    Raw_level_repr.equal published_level s2.published_level
    && Dal_slot_index_repr.equal index s2.index

  let equal {id; commitment} s2 =
    slot_id_equal id s2.id && Commitment.equal commitment s2.commitment

  let id_encoding =
    let open Data_encoding in
    conv
      (fun {published_level; index} -> (published_level, index))
      (fun (published_level, index) -> {published_level; index})
      (obj2
         (req "level" Raw_level_repr.encoding)
         (req "index" Dal_slot_index_repr.encoding))

  let encoding =
    let open Data_encoding in
    conv
      (fun {id; commitment} -> (id, commitment))
      (fun (id, commitment) -> {id; commitment})
      (merge_objs id_encoding (obj1 (req "commitment" Commitment.encoding)))

  let pp_id fmt {published_level; index} =
    Format.fprintf
      fmt
      "published_level: %a, index: %a"
      Raw_level_repr.pp
      published_level
      Dal_slot_index_repr.pp
      index

  let pp fmt {id; commitment = c} =
    Format.fprintf fmt "id:(%a), commitment: %a" pp_id id Commitment.pp c

  let verify_commitment cryptobox commitment proof =
    Ok (Dal.verify_commitment cryptobox commitment proof)
end

module Slot_index = Dal_slot_index_repr

module Page = struct
  type content = Bytes.t

  type slot_index = Dal_slot_index_repr.t

  let pages_per_slot = Dal.pages_per_slot

  module Index = struct
    type t = int

    let zero = 0

    let encoding = Data_encoding.int16

    let pp = Format.pp_print_int

    let compare = Compare.Int.compare

    let equal = Compare.Int.equal
  end

  type t = {slot_id : Header.id; page_index : Index.t}

  type proof = Dal.page_proof

  let encoding =
    let open Data_encoding in
    conv
      (fun {slot_id = {published_level; index}; page_index} ->
        (published_level, index, page_index))
      (fun (published_level, index, page_index) ->
        {slot_id = {published_level; index}; page_index})
      (obj3
         (req "published_level" Raw_level_repr.encoding)
         (req "slot_index" Slot_index.encoding)
         (req "page_index" Index.encoding))

  let equal {slot_id; page_index} p =
    Header.slot_id_equal slot_id p.slot_id
    && Index.equal page_index p.page_index

  let proof_encoding = Dal.page_proof_encoding

  let content_encoding = Data_encoding.(bytes Hex)

  let pp fmt {slot_id = {published_level; index}; page_index} =
    Format.fprintf
      fmt
      "(published_level: %a, slot_index: %a, page_index: %a)"
      Raw_level_repr.pp
      published_level
      Slot_index.pp
      index
      Index.pp
      page_index

  let pp_proof fmt proof =
    Data_encoding.Json.pp
      fmt
      (Data_encoding.Json.construct proof_encoding proof)
end

module Slot_market = struct
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3108

     Think harder about this data structure and whether it can be
     optimized. *)

  module Slot_index_map = Map.Make (Dal_slot_index_repr)

  type t = {length : int; slot_headers : Header.t Slot_index_map.t}

  let init ~length =
    if Compare.Int.(length < 0) then
      invalid_arg "Dal_slot_repr.Slot_market.init: length cannot be negative" ;
    let slot_headers = Slot_index_map.empty in
    {length; slot_headers}

  let length {length; _} = length

  let register t new_slot_header =
    let open Header in
    if
      not
        Compare.Int.(
          0 <= Dal_slot_index_repr.to_int new_slot_header.id.index
          && Dal_slot_index_repr.to_int new_slot_header.id.index < t.length)
    then None
    else
      let has_changed = ref false in
      let update = function
        | None ->
            has_changed := true ;
            Some new_slot_header
        | Some x -> Some x
      in
      let slot_headers =
        Slot_index_map.update new_slot_header.id.index update t.slot_headers
      in
      let t = {t with slot_headers} in
      Some (t, !has_changed)

  let candidates t =
    t.slot_headers |> Slot_index_map.to_seq |> Seq.map snd |> List.of_seq
end

module History = struct
  (* History is represented via a skip list. The content of the cell
     is the hash of a merkle proof. *)

  module Content_prefix = struct
    let (_prefix : string) = "dash1"

    (* 32 *)
    let b58check_prefix = "\002\224\072\094\219" (* dash1(55) *)

    let size = Some 32

    let name = "dal_skip_list_content"

    let title = "A hash to represent the content of a cell in the skip list"
  end

  module Content_hash = Blake2B.Make (Base58) (Content_prefix)

  (* Pointers of the skip lists are used to encode the content and the
     backpointers. *)
  module Pointer_prefix = struct
    let (_prefix : string) = "dask1"

    (* 32 *)
    let b58check_prefix = "\002\224\072\115\035" (* dask1(55) *)

    let size = Some 32

    let name = "dal_skip_list_pointer"

    let title = "A hash that represents the skip list pointers"
  end

  module Pointer_hash = Blake2B.Make (Base58) (Pointer_prefix)

  module Skip_list_parameters = struct
    let basis = 4
  end

  type error += Add_element_in_slots_skip_list_violates_ordering

  let () =
    register_error_kind
      `Temporary
      ~id:"Dal_slot_repr.add_element_in_slots_skip_list_violates_ordering"
      ~title:"Add an element in slots skip list that violates ordering"
      ~description:
        "Attempting to add an element on top of the Dal confirmed slots skip \
         list that violates the ordering."
      Data_encoding.unit
      (function
        | Add_element_in_slots_skip_list_violates_ordering -> Some ()
        | _ -> None)
      (fun () -> Add_element_in_slots_skip_list_violates_ordering)

  module Content = struct
    (** Each cell of the skip list is either a slot header that has been
        attested, or a published level and a slot index for which no slot header
        is attested (so, no associated commitment). *)
    type t = Unattested of Header.id | Attested of Header.t

    let content_id = function
      | Unattested slot_id -> slot_id
      | Attested {id; _} -> id

    let encoding =
      let open Data_encoding in
      union
        ~tag_size:`Uint8
        [
          case
            ~title:"unattested"
            (Tag 0)
            (merge_objs
               (obj1 (req "kind" (constant "unattested")))
               Header.id_encoding)
            (function
              | Unattested slot_id -> Some ((), slot_id) | Attested _ -> None)
            (fun ((), slot_id) -> Unattested slot_id);
          case
            ~title:"attested"
            (Tag 1)
            (merge_objs
               (obj1 (req "kind" (constant "attested")))
               Header.encoding)
            (function
              | Unattested _ -> None
              | Attested slot_header -> Some ((), slot_header))
            (fun ((), slot_header) -> Attested slot_header);
        ]

    let equal t1 t2 =
      match (t1, t2) with
      | Unattested sid1, Unattested sid2 -> Header.slot_id_equal sid1 sid2
      | Attested sh1, Attested sh2 -> Header.equal sh1 sh2
      | Unattested _, _ | Attested _, _ -> false

    let zero, zero_level =
      let zero_level = Raw_level_repr.root in
      let zero_index = Dal_slot_index_repr.zero in
      (Unattested {published_level = zero_level; index = zero_index}, zero_level)

    let pp fmt = function
      | Unattested slot_id ->
          Format.fprintf fmt "Unattested (%a)" Header.pp_id slot_id
      | Attested slot_header ->
          Format.fprintf fmt "Attested (%a)" Header.pp slot_header
  end

  module Skip_list = struct
    include Skip_list.Make (Skip_list_parameters)

    (** All Dal slot indices for all levels will be stored in a skip list
        (with or without a commitment depending on attestation status of each
        slot), where only the last cell is needed to be remembered in the L1
        context. The skip list is used in the proof phase of a refutation game
        to verify whether a given slot is inserted as [Attested] or not in the
        skip list. The skip list is supposed to be sorted, as its 'search'
        function explicitly uses a given `compare` function during the list
        traversal to quickly (in log(size)) reach the target slot header id.
        Two cells compare in lexicographic ordering of their levels and slot indexes.

        Below, we redefine the [next] function (that allows adding elements
        on top of the list) to enforce that the constructed skip list is
        well-sorted. We also define a wrapper around the [search] function to
        guarantee that it can only be called with the adequate compare function.
    *)
    let next ~prev_cell ~prev_cell_ptr ~number_of_slots elt =
      let open Result_syntax in
      let well_ordered =
        (* For each cell we insert in the skip list, we ensure that it complies
           with the following invariant:
           - Either the published levels are successive (no gaps). In this case:
             * The last inserted slot's index for the previous level is
               [number_of_slots - 1];
             * The first inserted slot's index for the current level is 0
           - Or, levels are equal, but slot indices are successive. *)
        let Header.{published_level = l1; index = i1} =
          content prev_cell |> Content.content_id
        in
        let Header.{published_level = l2; index = i2} =
          Content.content_id elt
        in
        (Raw_level_repr.equal l2 (Raw_level_repr.succ l1)
        && Compare.Int.(Dal_slot_index_repr.to_int i1 = number_of_slots - 1)
        && Compare.Int.(Dal_slot_index_repr.to_int i2 = 0))
        || Raw_level_repr.equal l2 l1
           && Dal_slot_index_repr.is_succ i1 ~succ:i2
      in
      let* () =
        error_unless
          well_ordered
          Add_element_in_slots_skip_list_violates_ordering
      in
      return @@ next ~prev_cell ~prev_cell_ptr elt

    let search =
      let compare_with_slot_id (target_slot_id : Header.id)
          (content : Content.t) =
        let Header.{published_level = target_level; index = target_index} =
          target_slot_id
        in
        let Header.{published_level; index} = Content.content_id content in
        let c = Raw_level_repr.compare published_level target_level in
        if Compare.Int.(c <> 0) then c
        else Dal_slot_index_repr.compare index target_index
      in
      fun ~deref ~cell ~target_slot_id ->
        Lwt.search ~deref ~cell ~compare:(compare_with_slot_id target_slot_id)
  end

  module V1 = struct
    type content = Content.t

    (* A pointer to a cell is the hash of its content and all the back
       pointers. *)
    type hash = Pointer_hash.t

    type history = (content, hash) Skip_list.cell

    type t = history

    let genesis, genesis_level =
      (Skip_list.genesis Content.zero, Content.zero_level)

    let history_encoding =
      let open Data_encoding in
      (* The history_encoding is given as a union of two versions of the skip
         list. The legacy case is only used to deserialize the skip list cells
         which may appear in refutation games started on a previous version of
         the protocol, before the activation of the DAL. In this case, the
         snapshotted cells are always the genesis one and cannot be used by the
         players so we deserialize it on the fly to the new representation of
         the genesis cell. *)
      union
        ~tag_size:`Uint8
        [
          case
            ~title:"dal_skip_list_legacy"
            (Tag 0)
            (obj2
               (req "kind" (constant "dal_skip_list_legacy"))
               (req "skip_list" (Data_encoding.Fixed.bytes Hex 57)))
            (fun _ -> None)
            (fun ((), _) -> genesis);
          case
            ~title:"dal_skip_list"
            (Tag 1)
            (obj2
               (req "kind" (constant "dal_skip_list"))
               (req
                  "skip_list"
                  (Skip_list.encoding Pointer_hash.encoding Content.encoding)))
            (fun x -> Some ((), x))
            (fun ((), x) -> x);
        ]

    let equal_history : history -> history -> bool =
      Skip_list.equal Pointer_hash.equal Content.equal

    let encoding = history_encoding

    let equal : t -> t -> bool = equal_history

    let hash cell =
      let current_slot = Skip_list.content cell in
      let back_pointers_hashes = Skip_list.back_pointers cell in
      Data_encoding.Binary.to_bytes_exn Content.encoding current_slot
      :: List.map Pointer_hash.to_bytes back_pointers_hashes
      |> Pointer_hash.hash_bytes

    let pp_history fmt (history : history) =
      let history_hash = hash history in
      Format.fprintf
        fmt
        "@[hash : %a@;%a@]"
        Pointer_hash.pp
        history_hash
        (Skip_list.pp ~pp_content:Content.pp ~pp_ptr:Pointer_hash.pp)
        history

    let pp = pp_history

    module History_cache =
      Bounded_history_repr.Make
        (struct
          let name = "dal_slots_cache"
        end)
        (Pointer_hash)
        (struct
          type t = history

          let encoding = history_encoding

          let pp = pp_history

          let equal = equal_history
        end)

    (* Insert a cell in the skip list [t] and the corresponding association [(hash(t),
       t)] in the given [cache].

       Note that if the given skip list contains the genesis cell, its content is
       reset with the given content. This ensures the invariant that
       there are no gaps in the successive cells of the list. *)
    let add_cell (t, cache) next_cell_content ~number_of_slots =
      let open Result_syntax in
      let prev_cell_ptr = hash t in
      let Header.{published_level; _} =
        Skip_list.content t |> Content.content_id
      in
      if Raw_level_repr.equal published_level genesis_level then
        (* If this is the first real cell of DAL, replace dummy genesis. *)
        return (Skip_list.genesis next_cell_content, cache)
      else
        let* cache = History_cache.remember prev_cell_ptr t cache in
        let* new_head =
          Skip_list.next
            ~prev_cell:t
            ~prev_cell_ptr
            next_cell_content
            ~number_of_slots
        in
        return (new_head, cache)

    (* Given a list [attested_slot_headers] of well-ordered (wrt slots indices)
       (attested) slot headers, this function builds an extension [l] of
       [attested_slot_headers] such that:

       - all elements in [attested_slot_headers] are in [l],

       - for every slot index i in [0, number_of_slots - 1] that doesn't appear
       in [attested_slot_headers], an unattested slot id is inserted in [l],

       - [l] is well sorted wrt. slots indices. *)
    let fill_slot_headers ~number_of_slots ~published_level
        attested_slot_headers =
      let open Result_syntax in
      let module I = Dal_slot_index_repr in
      let* all_indices =
        I.slots_range ~number_of_slots ~lower:0 ~upper:(number_of_slots - 1)
      in
      let mk_unattested index =
        Content.Unattested Header.{published_level; index}
      in
      (* Hypothesis: both lists are sorted in increasing order w.r.t. slots
         indices. *)
      let rec aux indices slots =
        match (indices, slots) with
        | _, [] -> List.map mk_unattested indices |> ok
        | [], _s :: _ -> tzfail Add_element_in_slots_skip_list_violates_ordering
        | i :: indices', s :: slots' ->
            if I.(i = s.Header.id.index) then
              let* res = aux indices' slots' in
              Content.Attested s :: res |> ok
            else if I.(i < s.Header.id.index) then
              let* res = aux indices' slots in
              mk_unattested i :: res |> ok
            else
              (* i > s.Header.id.index *)
              tzfail Add_element_in_slots_skip_list_violates_ordering
      in
      aux all_indices attested_slot_headers

    (* Assuming a [number_of_slots] per L1 level, we will ensure below that we
       insert exactly [number_of_slots] cells in the skip list per level. This
       will simplify the shape of proofs and help bounding the history cache
       required for their generation. *)
    let add_confirmed_slot_headers (t : t) cache published_level
        ~number_of_slots attested_slot_headers =
      let open Result_syntax in
      let* slot_headers =
        fill_slot_headers
          ~number_of_slots
          ~published_level
          attested_slot_headers
      in
      List.fold_left_e (add_cell ~number_of_slots) (t, cache) slot_headers

    let add_confirmed_slot_headers_no_cache =
      let empty_cache = History_cache.empty ~capacity:0L in
      fun t published_level ~number_of_slots slots ->
        let open Result_syntax in
        let+ cell, (_ : History_cache.t) =
          add_confirmed_slot_headers
            t
            empty_cache
            published_level
            ~number_of_slots
            slots
        in
        cell

    (* Dal proofs section *)

    (** An inclusion proof is a sequence (list) of cells from the Dal skip list,
        represented as [c1; c2; ...; cn], that encodes a minimal path from the
        head [c1] (referred to as the "reference" or "snapshot" cell below) to a
        target cell [cn]. Thanks to the back-pointers, it can be demonstrated
        that the successive elements of the sequence are indeed cells of the
        skip list. *)
    type inclusion_proof = history list

    (** (See the documentation in the mli file to understand what we want to
        prove in a refutation game involving Dal and why.)

        A Dal proof is an algebraic datatype with two cases, where we basically
        prove that a Dal page is confirmed on L1 or not. Being 'not confirmed'
        here includes the case where the slot's header is not published and the
        case where the slot's header is published, but the attesters didn't
        confirm the availability of its data.

        To produce a proof representation for a page (see function
        {!produce_proof_repr} below), we assume given:

        - [page_id], identifies the page;

        - [slots_history], a current/recent cell of the slots history skip list.
        Typically, it should be the skip list cell snapshotted when starting the
        refutation game;

       - [get_history], a sufficiently large slots history cache, encoded as a
       function from pointer hashes to their corresponding skip lists cells, to
       navigate back through the successive cells of the skip list. The cache
       should at least contain the cells starting from the published level of
       the page ID for which we want to generate a proof. Indeed, inclusion
       proofs encode paths through skip lists' cells where the head is the
       reference/snapshot cell and the last element is the target cell inserted
       at the level corresponding to the page's published level). Note that, the
       case where the level of the page is far in the past (i.e. the skip list
       was not populated yet) should be handled by the caller ;

        - [page_info], provides information for [page_id]. In case the page is
        supposed to be confirmed, this argument should contain the page's
        content and the proof that the page is part of the (confirmed) slot
        whose ID is given in [page_id]. In case we want to show that the page is
        not confirmed, the value [page_info] should be [None].

      [dal_parameters] is used when verifying that/if the page is part of
      the candidate slot (if any). *)
    type proof_repr =
      | Page_confirmed of {
          target_cell : history;
              (** [target_cell] is a cell whose content contains the slot to
                  which the page belongs to. *)
          inc_proof : inclusion_proof;
              (** [inc_proof] is a (minimal) path in the skip list that proves
                  cells inclusion. The head of the list is the [slots_history]
                  provided to produce the proof. The last cell's content is
                  the slot containing the page identified by [page_id],
                  that is: [target_cell]. *)
          page_data : Page.content;
              (** [page_data] is the content of the page. *)
          page_proof : Page.proof;
              (** [page_proof] is the proof that the page whose content is
                  [page_data] is actually the [page_id.page_index]th page of
                  the slot stored in [target_cell] and identified by
                  [page_id.slot_id]. *)
        }  (** The case where the slot's page is confirmed/attested on L1. *)
      | Page_unconfirmed of {target_cell : history; inc_proof : inclusion_proof}
          (** The case where the slot's page doesn't exist or is not confirmed
              on L1. The fields are similar to {!Page_confirmed} case except
              that we don't have a page data or proof to check.

              As said above, in case the level of the page is far in the past
              (for instance, the skip list was not populated yet or the slots of
              that level are not valid to be imported by the DAL anymore) should
              be handled by the caller. In fact, the [proof_repr] type here only
              covers levels where a new cell has been added to the skip list. *)

    let proof_repr_encoding =
      let open Data_encoding in
      let case_page_confirmed =
        case
          ~title:"confirmed dal page proof representation"
          (Tag 0)
          (obj5
             (req "kind" (constant "confirmed"))
             (req "target_cell" history_encoding)
             (req "inc_proof" (list history_encoding))
             (req "page_data" (bytes Hex))
             (req "page_proof" Page.proof_encoding))
          (function
            | Page_confirmed {target_cell; inc_proof; page_data; page_proof} ->
                Some ((), target_cell, inc_proof, page_data, page_proof)
            | _ -> None)
          (fun ((), target_cell, inc_proof, page_data, page_proof) ->
            Page_confirmed {target_cell; inc_proof; page_data; page_proof})
      and case_page_unconfirmed =
        case
          ~title:"unconfirmed dal page proof representation"
          (Tag 1)
          (obj3
             (req "kind" (constant "unconfirmed"))
             (req "target_cell" history_encoding)
             (req "inc_proof" (list history_encoding)))
          (function
            | Page_unconfirmed {target_cell; inc_proof} ->
                Some ((), target_cell, inc_proof)
            | _ -> None)
          (fun ((), target_cell, inc_proof) ->
            Page_unconfirmed {target_cell; inc_proof})
      in

      union [case_page_confirmed; case_page_unconfirmed]

    (** Proof's type is set to bytes and not a structural datatype because
        when a proof appears in a tezos operation or in an rpc, a user can not
        reasonably understand the proof, thus it eases the work of people decoding
        the proof by only supporting bytes and not the whole structured proof. *)

    type proof = bytes

    (** DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/4084
        DAL proof's encoding should be bounded *)
    let proof_encoding = Data_encoding.(bytes Hex)

    type error += Dal_invalid_proof_serialization

    let () =
      register_error_kind
        `Permanent
        ~id:"Dal_slot_repr.invalid_proof_serialization"
        ~title:"Dal invalid proof serialization"
        ~description:"Error occured during dal proof serialization"
        Data_encoding.unit
        (function Dal_invalid_proof_serialization -> Some () | _ -> None)
        (fun () -> Dal_invalid_proof_serialization)

    let serialize_proof proof =
      let open Result_syntax in
      match Data_encoding.Binary.to_bytes_opt proof_repr_encoding proof with
      | None -> tzfail Dal_invalid_proof_serialization
      | Some serialized_proof -> return serialized_proof

    type error += Dal_invalid_proof_deserialization

    let () =
      register_error_kind
        `Permanent
        ~id:"Dal_slot_repr.invalid_proof_deserialization"
        ~title:"Dal invalid proof deserialization"
        ~description:"Error occured during dal proof deserialization"
        Data_encoding.unit
        (function Dal_invalid_proof_deserialization -> Some () | _ -> None)
        (fun () -> Dal_invalid_proof_deserialization)

    let deserialize_proof proof =
      let open Result_syntax in
      match Data_encoding.Binary.of_bytes_opt proof_repr_encoding proof with
      | None -> tzfail Dal_invalid_proof_deserialization
      | Some deserialized_proof -> return deserialized_proof

    let pp_inclusion_proof = Format.pp_print_list pp_history

    let pp_proof ~serialized fmt p =
      if serialized then Format.pp_print_string fmt (Bytes.to_string p)
      else
        match deserialize_proof p with
        | Error msg -> Error_monad.pp_trace fmt msg
        | Ok proof -> (
            match proof with
            | Page_confirmed {target_cell; inc_proof; page_data; page_proof} ->
                Format.fprintf
                  fmt
                  "Page_confirmed (target_cell=%a, data=%s,@ \
                   inc_proof:[size=%d |@ path=%a]@ page_proof:%a)"
                  pp_history
                  target_cell
                  (Bytes.to_string page_data)
                  (List.length inc_proof)
                  pp_inclusion_proof
                  inc_proof
                  Page.pp_proof
                  page_proof
            | Page_unconfirmed {target_cell; inc_proof} ->
                Format.fprintf
                  fmt
                  "Page_unconfirmed (target_cell = %a | inc_proof:[size=%d@ | \
                   path=%a])"
                  pp_history
                  target_cell
                  (List.length inc_proof)
                  pp_inclusion_proof
                  inc_proof)

    type error +=
      | Dal_proof_error of string
      | Unexpected_page_size of {expected_size : int; page_size : int}

    let () =
      let open Data_encoding in
      register_error_kind
        `Permanent
        ~id:"dal_slot_repr.slots_history.dal_proof_error"
        ~title:"Dal proof error"
        ~description:"Error occurred during Dal proof production or validation"
        ~pp:(fun ppf e -> Format.fprintf ppf "Dal proof error: %s" e)
        (obj1 (req "error" (string Plain)))
        (function Dal_proof_error e -> Some e | _ -> None)
        (fun e -> Dal_proof_error e)

    let () =
      let open Data_encoding in
      register_error_kind
        `Permanent
        ~id:"dal_slot_repr.slots_history.unexpected_page_size"
        ~title:"Unexpected page size"
        ~description:
          "The size of the given page content doesn't match the expected one."
        ~pp:(fun ppf (expected, size) ->
          Format.fprintf
            ppf
            "The size of a Dal page is expected to be %d bytes. The given one \
             has %d"
            expected
            size)
        (obj2 (req "expected_size" int16) (req "page_size" int16))
        (function
          | Unexpected_page_size {expected_size; page_size} ->
              Some (expected_size, page_size)
          | _ -> None)
        (fun (expected_size, page_size) ->
          Unexpected_page_size {expected_size; page_size})

    let dal_proof_error reason = Dal_proof_error reason

    let proof_error reason = error @@ dal_proof_error reason

    let check_page_proof dal_params proof data ({Page.page_index; _} as pid)
        commitment =
      let open Result_syntax in
      let* dal =
        match Dal.make dal_params with
        | Ok dal -> return dal
        | Error (`Fail s) -> proof_error s
      in
      let fail_with_error_msg what =
        Format.kasprintf proof_error "%s (page id=%a)." what Page.pp pid
      in
      match Dal.verify_page dal commitment ~page_index data proof with
      | Ok true -> return_unit
      | Ok false ->
          fail_with_error_msg
            "Wrong page content for the given page index and slot commitment"
      | Error `Segment_index_out_of_range ->
          fail_with_error_msg "Segment_index_out_of_range"
      | Error `Page_length_mismatch ->
          tzfail
          @@ Unexpected_page_size
               {
                 expected_size = dal_params.page_size;
                 page_size = Bytes.length data;
               }

    (** The [produce_proof_repr] function assumes that some invariants hold, such as:
        - The DAL has been activated,
        - The level of [page_id] is after the DAL activation level.

        Under these assumptions, we recall that we maintain an invariant
        ensuring that we a have a cell per slot index in the skip list at every level
        after DAL activation. *)
    let produce_proof_repr dal_params page_id ~page_info ~get_history slots_hist
        =
      let open Lwt_result_syntax in
      let Page.{slot_id = target_slot_id; page_index = _} = page_id in
      (* We first search for the slots attested at level [published_level]. *)
      let*! search_result =
        Skip_list.search ~deref:get_history ~target_slot_id ~cell:slots_hist
      in
      (* The search should necessarily find a cell in the skip list (assuming
         enough cache is given) under the assumptions made when calling
         {!produce_proof_repr}. *)
      match search_result.Skip_list.last_cell with
      | Deref_returned_none ->
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned 'Deref_returned_none': Slots history \
                cache is ill-formed or has too few entries."
      | No_exact_or_lower_ptr ->
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned 'No_exact_or_lower_ptr', while it is \
                initialized with a min elt (slot zero)."
      | Nearest _ ->
          (* This should not happen: there is one cell at each level
             after DAL activation. The case where the page's level is before DAL
             activation level should be handled by the caller
             ({!Sc_refutation_proof.produce} in our case). *)
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned Nearest', while all given levels to \
                produce proofs are supposed to be in the skip list."
      | Found target_cell -> (
          let inc_proof = List.rev search_result.Skip_list.rev_path in
          match (page_info, Skip_list.content target_cell) with
          | Some (page_data, page_proof), Attested {commitment; id = _} ->
              (* The case where the slot to which the page is supposed to belong
                 is found and the page's information are given. *)
              let*? () =
                (* We check the page's proof against the commitment. *)
                check_page_proof
                  dal_params
                  page_proof
                  page_data
                  page_id
                  commitment
              in
              (* All checks succeeded. We return a `Page_confirmed` proof. *)
              return
                ( Page_confirmed {target_cell; inc_proof; page_data; page_proof},
                  Some page_data )
          | None, Unattested _ ->
              (* The slot corresponding to the given page's index is not found in
                 the attested slots of the page's level, and no information is
                 given for that page. So, we produce a proof that the page is not
                 attested. *)
              return (Page_unconfirmed {target_cell; inc_proof}, None)
          | None, Attested _ ->
              (* Mismatch: case where no page information are given, but the
                 slot is attested. *)
              tzfail
              @@ dal_proof_error
                   "The page ID's slot is confirmed, but no page content and \
                    proof are provided."
          | Some _, Unattested _ ->
              (* Mismatch: case where page information are given, but the slot
                 is not attested. *)
              tzfail
              @@ dal_proof_error
                   "The page ID's slot is not confirmed, but page content and \
                    proof are provided.")

    let produce_proof dal_params page_id ~page_info ~get_history slots_hist =
      let open Lwt_result_syntax in
      let* proof_repr, page_data =
        produce_proof_repr dal_params page_id ~page_info ~get_history slots_hist
      in
      let*? serialized_proof = serialize_proof proof_repr in
      return (serialized_proof, page_data)

    (* Given a starting cell [snapshot] and a (final) [target], this function
       checks that the provided [inc_proof] encodes a minimal path from
       [snapshot] to [target]. *)
    let verify_inclusion_proof inc_proof ~src:snapshot ~dest:target =
      let assoc = List.map (fun c -> (hash c, c)) inc_proof in
      let path = List.split assoc |> fst in
      let deref =
        let open Map.Make (Pointer_hash) in
        let map = of_seq (List.to_seq assoc) in
        fun ptr -> find_opt ptr map
      in
      let snapshot_ptr = hash snapshot in
      let target_ptr = hash target in
      error_unless
        (Skip_list.valid_back_path
           ~equal_ptr:Pointer_hash.equal
           ~deref
           ~cell_ptr:snapshot_ptr
           ~target_ptr
           path)
        (dal_proof_error "verify_proof_repr: invalid inclusion Dal proof.")

    let verify_proof_repr dal_params page_id snapshot proof =
      let open Result_syntax in
      let Page.{slot_id = Header.{published_level; index}; page_index = _} =
        page_id
      in
      let* target_cell, inc_proof, page_proof_check =
        match proof with
        | Page_confirmed {target_cell; inc_proof; page_data; page_proof} ->
            let page_proof_check =
              Some
                (fun commitment ->
                  (* We check that the page indeed belongs to the target slot at the
                     given page index. *)
                  let* () =
                    check_page_proof
                      dal_params
                      page_proof
                      page_data
                      page_id
                      commitment
                  in
                  (* If the check succeeds, we return the data/content of the
                     page. *)
                  return page_data)
            in
            return (target_cell, inc_proof, page_proof_check)
        | Page_unconfirmed {target_cell; inc_proof} ->
            return (target_cell, inc_proof, None)
      in
      let cell_content = Skip_list.content target_cell in
      (* We check that the target cell has the same level and index than the
         page we're about to prove. *)
      let cell_id = Content.content_id cell_content in
      let* () =
        error_when
          Raw_level_repr.(cell_id.published_level <> published_level)
          (dal_proof_error "verify_proof_repr: published_level mismatch.")
      in
      let* () =
        error_when
          (not (Dal_slot_index_repr.equal cell_id.index index))
          (dal_proof_error "verify_proof_repr: slot index mismatch.")
      in
      (* We check that the given inclusion proof indeed links our L1 snapshot to
         the target cell. *)
      let* () =
        verify_inclusion_proof inc_proof ~src:snapshot ~dest:target_cell
      in
      match (page_proof_check, cell_content) with
      | None, Unattested _ -> return_none
      | Some page_proof_check, Attested {commitment; _} ->
          let* page_data = page_proof_check commitment in
          return_some page_data
      | Some _, Unattested _ ->
          error
          @@ dal_proof_error
               "verify_proof_repr: the unconfirmation proof contains the \
                target slot."
      | None, Attested _ ->
          error
          @@ dal_proof_error
               "verify_proof_repr: the confirmation proof doesn't contain the \
                attested slot."

    let verify_proof dal_params page_id snapshot serialized_proof =
      let open Result_syntax in
      let* proof_repr = deserialize_proof serialized_proof in
      verify_proof_repr dal_params page_id snapshot proof_repr

    module Internal_for_tests = struct
      type cell_content = Content.t =
        | Unattested of Header.id
        | Attested of Header.t

      let content = Skip_list.content

      let proof_statement_is serialized_proof expected =
        match deserialize_proof serialized_proof with
        | Error _ -> false
        | Ok proof -> (
            match (expected, proof) with
            | `Confirmed, Page_confirmed _ | `Unconfirmed, Page_unconfirmed _ ->
                true
            | _ -> false)
    end
  end

  include V1
end
