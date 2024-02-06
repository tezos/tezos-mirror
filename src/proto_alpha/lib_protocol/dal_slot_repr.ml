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

  let compare_slot_id {published_level; index} s2 =
    let c = Raw_level_repr.compare published_level s2.published_level in
    if Compare.Int.(c <> 0) then c
    else Dal_slot_index_repr.compare index s2.index

  let zero_id =
    {
      (* We don't expect to have any published slot at level
         Raw_level_repr.root. *)
      published_level = Raw_level_repr.root;
      index = Dal_slot_index_repr.zero;
    }

  let zero = {id = zero_id; commitment = Commitment.zero}

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

  (* A leaf of the merkle tree is a slot. *)
  module Leaf = struct
    type t = Header.t

    let to_bytes = Data_encoding.Binary.to_bytes_exn Header.encoding
  end

  module Content_prefix = struct
    let (_prefix : string) = "dash1"

    (* 32 *)
    let b58check_prefix = "\002\224\072\094\219" (* dash1(55) *)

    let size = Some 32

    let name = "dal_skip_list_content"

    let title = "A hash to represent the content of a cell in the skip list"
  end

  module Content_hash = Blake2B.Make (Base58) (Content_prefix)
  module Merkle_list = Merkle_list.Make (Leaf) (Content_hash)

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

  module Skip_list = struct
    include Skip_list.Make (Skip_list_parameters)

    (** All confirmed DAL slots will be stored in a skip list, where only the
        last cell is remembered in the L1 context. The skip list is used in
        the proof phase of a refutation game to verify whether a given slot
        exists (i.e., confirmed) or not in the skip list. The skip list is
        supposed to be sorted, as its 'search' function explicitly uses a given
        `compare` function during the list traversal to quickly (in log(size))
        reach the target if any.

        In our case, we will store one slot per cell in the skip list and
        maintain that the list is well sorted (and without redundancy) w.r.t.
        the [compare_slot_id] function.

        Below, we redefine the [next] function (that allows adding elements
        on top of the list) to enforce that the constructed skip list is
        well-sorted. We also define a wrapper around the search function to
        guarantee that it can only be called with the adequate compare function.
    *)

    let next ~prev_cell ~prev_cell_ptr elt =
      let open Result_syntax in
      let* () =
        error_when
          (Compare.Int.( <= )
             (Header.compare_slot_id
                elt.Header.id
                (content prev_cell).Header.id)
             0)
          Add_element_in_slots_skip_list_violates_ordering
      in
      return @@ next ~prev_cell ~prev_cell_ptr elt

    let search ~deref ~cell ~target_id =
      Lwt.search ~deref ~cell ~compare:(fun slot ->
          Header.compare_slot_id slot.Header.id target_id)
  end

  module V1 = struct
    (* The content of a cell is the hash of all the slot commitments
       represented as a merkle list. *)
    (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/3765
       Decide how to store attested slots in the skip list's content. *)
    type content = Header.t

    (* A pointer to a cell is the hash of its content and all the back
       pointers. *)
    type hash = Pointer_hash.t

    type history = (content, hash) Skip_list.cell

    type t = history

    let history_encoding =
      Skip_list.encoding Pointer_hash.encoding Header.encoding

    let equal_history : history -> history -> bool =
      Skip_list.equal Pointer_hash.equal Header.equal

    let encoding = history_encoding

    let equal : t -> t -> bool = equal_history

    let genesis : t = Skip_list.genesis Header.zero

    let hash cell =
      let current_slot = Skip_list.content cell in
      let back_pointers_hashes = Skip_list.back_pointers cell in
      Data_encoding.Binary.to_bytes_exn Header.encoding current_slot
      :: List.map Pointer_hash.to_bytes back_pointers_hashes
      |> Pointer_hash.hash_bytes

    let pp_history fmt (history : history) =
      let history_hash = hash history in
      Format.fprintf
        fmt
        "@[hash : %a@;%a@]"
        Pointer_hash.pp
        history_hash
        (Skip_list.pp ~pp_content:Header.pp ~pp_ptr:Pointer_hash.pp)
        history

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

    let add_confirmed_slot_header (t, cache) slot_header =
      let open Result_syntax in
      let prev_cell_ptr = hash t in
      let* cache = History_cache.remember prev_cell_ptr t cache in
      let* new_cell = Skip_list.next ~prev_cell:t ~prev_cell_ptr slot_header in
      return (new_cell, cache)

    let add_confirmed_slot_headers (t : t) cache slot_headers =
      List.fold_left_e add_confirmed_slot_header (t, cache) slot_headers

    let add_confirmed_slot_headers_no_cache =
      let open Result_syntax in
      let no_cache = History_cache.empty ~capacity:0L in
      fun t slots ->
        let+ cell, (_ : History_cache.t) =
          List.fold_left_e add_confirmed_slot_header (t, no_cache) slots
        in
        cell

    (* Dal proofs section *)

    (** An inclusion proof, for a page ID, is a list of the slots' history
        skip list's cells that encodes a minimal path:
        - from a starting cell, which serves as a reference. It is usually called
        'snapshot' below,
        - to a final cell, that is either the exact target cell in case the slot
         of the page is confirmed, or a cell whose slot ID is the smallest
         that directly follows the page's slot id, in case the target slot
         is not confirmed.

         Using the starting cell as a trustable starting point (i.e. maintained
         and provided by L1), and combined with the extra information stored in
         the {!proof} type below, one can verify if a slot (and then a page of
         that slot) is confirmed on L1 or not. *)
    type inclusion_proof = history list

    (** (See the documentation in the mli file to understand what we want to
        prove in game refutation involving Dal and why.)

        A Dal proof is an algebraic datatype with two cases, where we basically
        prove that a Dal page is confirmed on L1 or not. Being 'not confirmed'
        here includes the case where the slot's header is not published and the
        case where the slot's header is published, but the attesters didn't
        confirm the availability of its data.

        To produce a proof representation for a page (see function {!produce_proof_repr}
        below), we assume given:

        - [page_id], identifies the page;

        - [slots_history], a current/recent cell of the slots history skip list.
          Typically, it should be the skip list cell snapshotted when starting the
          refutation game;

       - [history_cache], a sufficiently large slots history cache, to navigate
          back through the successive cells of the skip list. Typically,
          the cache should at least contain the cell whose slot ID is [page_id.slot_id]
          in case the page is confirmed, or the cell whose slot ID is immediately
          after [page_id.slot_id] in case of an unconfirmed page. Indeed,
          inclusion proofs encode paths through skip lists' cells where the head
          is the reference/snapshot cell and the last element is the target slot
          in or the nearest upper slot (w.r.t [page_id]'s slot id and to
          skip list elements ordering) ;

        - [page_info], that provides the page's information (the content and
          the slot membership proof) for page_id. In case the page is supposed
          to be confirmed, this argument should contain the page's content and
          the proof that the page is part of the (confirmed) slot whose ID is
          given in [page_id]. In case we want to show that the page is not confirmed,
          the value [page_info] should be [None].

      [dal_parameters] is used when verifying that/if the page is part of
      the candidate slot (if any).


*)
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
                  page_id.slot_id. *)
        }  (** The case where the slot's page is confirmed/attested on L1. *)
      | Page_unconfirmed of {
          prev_cell : history;
              (** [prev_cell] is the cell of the skip list containing a
                  (confirmed) slot, and whose ID is the biggest (w.r.t. to skip
                  list elements ordering), but smaller than [page_id.slot_id]. *)
          next_cell_opt : history option;
              (** [next_cell_opt] is the cell that immediately follows [prev_cell]
                  in the skip list, if [prev_cell] is not the latest element in
                  the list. Otherwise, it's set to [None]. *)
          next_inc_proof : inclusion_proof;
              (** [inc_proof] is a (minimal) path in the skip list that proves
                  cells inclusion. In case, [next_cell_opt] contains some cell
                  'next_cell', the head of the list is the [slots_history]
                  provided to produce the proof, and the last cell is
                  'next_cell'. In case [next_cell_opt] is [None], the list is
                  empty.

                  We maintain the following invariant in case the inclusion
                  proof is not empty:
                  ```
                   (content next_cell).id > page_id.slot_id > (content prev_cell).id AND
                   hash prev_cell = back_pointer next_cell 0 AND
                   Some next_cell = next_cell_opt AND
                   head next_inc_proof = slots_history
                  ```

                  Said differently, `next_cell` and `prev_cell` are two consecutive
                  cells of the skip list whose contents' IDs surround the page's
                  slot ID. Moreover, the head of the list should be equal to
                  the initial (snapshotted) slots_history skip list.

                  The case of an empty inclusion proof happens when the inputs
                  are such that: `page_id.slot_id > (content slots_history).id`.
                  The returned proof statement implies the following property in this case:

                  ```
                  next_cell_opt = None AND prev_cell = slots_history
                  ```
              *)
        }
          (** The case where the slot's page doesn't exist or is not
              confirmed on L1. *)

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
          (obj4
             (req "kind" (constant "unconfirmed"))
             (req "prev_cell" history_encoding)
             (req "next_cell_opt" (option history_encoding))
             (req "next_inc_proof" (list history_encoding)))
          (function
            | Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof} ->
                Some ((), prev_cell, next_cell_opt, next_inc_proof)
            | _ -> None)
          (fun ((), prev_cell, next_cell_opt, next_inc_proof) ->
            Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof})
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

    let pp_history_opt = Format.pp_print_option pp_history

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
            | Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof} ->
                Format.fprintf
                  fmt
                  "Page_unconfirmed (prev_cell = %a | next_cell = %a | \
                   prev_inc_proof:[size=%d@ | path=%a])"
                  pp_history
                  prev_cell
                  pp_history_opt
                  next_cell_opt
                  (List.length next_inc_proof)
                  pp_inclusion_proof
                  next_inc_proof)

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

    let produce_proof_repr dal_params page_id ~page_info ~get_history slots_hist
        =
      let open Lwt_result_syntax in
      let Page.{slot_id; page_index = _} = page_id in
      (* We search for a slot whose ID is equal to target_id. *)
      let*! search_result =
        Skip_list.search ~deref:get_history ~target_id:slot_id ~cell:slots_hist
      in
      match (page_info, search_result.Skip_list.last_cell) with
      | _, Deref_returned_none ->
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned 'Deref_returned_none': Slots history \
                cache is ill-formed or has too few entries."
      | _, No_exact_or_lower_ptr ->
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned 'No_exact_or_lower_ptr', while it is \
                initialized with a min elt (slot zero)."
      | Some (page_data, page_proof), Found target_cell ->
          (* The slot to which the page is supposed to belong is found. *)
          let Header.{id; commitment} = Skip_list.content target_cell in
          (* We check that the slot is not the dummy slot. *)
          let*? () =
            error_when
              Compare.Int.(Header.compare_slot_id id Header.zero.id = 0)
              (dal_proof_error
                 "Skip_list.search returned 'Found <zero_slot>': No existence \
                  proof should be constructed with the slot zero.")
          in
          let*? () =
            check_page_proof dal_params page_proof page_data page_id commitment
          in
          let inc_proof = List.rev search_result.Skip_list.rev_path in
          let*? () =
            error_when
              (List.is_empty inc_proof)
              (dal_proof_error "The inclusion proof cannot be empty")
          in
          (* All checks succeeded. We return a `Page_confirmed` proof. *)
          return
            ( Page_confirmed {inc_proof; target_cell; page_data; page_proof},
              Some page_data )
      | None, Nearest {lower = prev_cell; upper = next_cell_opt} ->
          (* There is no previously confirmed slot in the skip list whose ID
             corresponds to the {published_level; slot_index} information
             given in [page_id]. But, `search` returned a skip list [prev_cell]
             (and possibly [next_cell_opt]) such that:
             - the ID of [prev_cell]'s slot is the biggest immediately smaller than
               the page's information {published_level; slot_index}
             - if not equal to [None], the ID of [next_cell_opt]'s slot is the smallest
               immediately bigger than the page's slot id `slot_id`.
             - if [next_cell_opt] is [None] then, [prev_cell] should be equal to
               the given history_proof cell. *)
          let* next_inc_proof =
            match search_result.Skip_list.rev_path with
            | [] -> assert false (* Not reachable *)
            | prev :: rev_next_inc_proof ->
                let*? () =
                  error_unless
                    (equal_history prev prev_cell)
                    (dal_proof_error
                       "Internal error: search's Nearest result is \
                        inconsistent.")
                in
                return @@ List.rev rev_next_inc_proof
          in
          return
            (Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof}, None)
      | None, Found _ ->
          tzfail
          @@ dal_proof_error
               "The page ID's slot is confirmed, but no page content and proof \
                are provided."
      | Some _, Nearest _ ->
          tzfail
          @@ dal_proof_error
               "The page ID's slot is not confirmed, but page content and \
                proof are provided."

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
      let Page.{slot_id; page_index = _} = page_id in
      match proof with
      | Page_confirmed {target_cell; page_data; page_proof; inc_proof} ->
          (* If the page is supposed to be confirmed, the last cell in
             [inc_proof] should store the slot of the page. *)
          let Header.{id; commitment} = Skip_list.content target_cell in
          let* () =
            error_when
              Compare.Int.(Header.compare_slot_id id Header.zero.id = 0)
              (dal_proof_error
                 "verify_proof_repr: cannot construct a confirmation page \
                  proof with 'zero' as target slot.")
          in
          let* () =
            verify_inclusion_proof inc_proof ~src:snapshot ~dest:target_cell
          in
          (* We check that the page indeed belongs to the target slot at the
             given page index. *)
          let* () =
            check_page_proof dal_params page_proof page_data page_id commitment
          in
          (* If all checks succeed, we return the data/content of the page. *)
          return_some page_data
      | Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof} ->
          (* The page's slot is supposed to be unconfirmed. *)
          let ( < ) a b = Compare.Int.(Header.compare_slot_id a b < 0) in
          (* We retrieve the last cell of the inclusion proof to be able to
             call {!verify_inclusion_proof}. We also do some well-formedness on
             the shape of the inclusion proof (see the case [Page_unconfirmed]
             of type {!proof}). *)
          let* () =
            match next_cell_opt with
            | None ->
                let* () =
                  error_unless
                    (List.is_empty next_inc_proof)
                    (dal_proof_error
                       "verify_proof_repr: invalid next_inc_proof")
                in
                (* In case the inclusion proof has no elements, we check that:
                   - the prev_cell slot's id is smaller than the unconfirmed slot's ID
                   - the snapshot is equal to the [prev_cell] skip list.

                   This way, and since the skip list is sorted wrt.
                   {!compare_slot_id}, we are sure that the skip list whose head
                   is [snapshot] = [prev_cell] cannot contain a slot whose ID is
                   [slot_id]. *)
                error_unless
                  ((Skip_list.content prev_cell).id < slot_id
                  && equal_history snapshot prev_cell)
                  (dal_proof_error "verify_proof_repr: invalid next_inc_proof")
            | Some next_cell ->
                (* In case the inclusion proof has at least one element,
                   we check that:
                   - the [prev_cell] slot's id is smaller than [slot_id]
                   - the [next_cell] slot's id is greater than [slot_id]
                   - the [next_cell] cell is a direct successor of the
                     [prev_cell] cell.
                   - the [next_cell] cell is a predecessor of [snapshot]

                   Since the skip list is sorted wrt. {!compare_slot_id}, and
                   if the call to {!verify_inclusion_proof} succeeds, we are
                   sure that the skip list whose head is [snapshot] cannot
                   contain a slot whose ID is [slot_id]. *)
                let* () =
                  error_unless
                    ((Skip_list.content prev_cell).id < slot_id
                    && slot_id < (Skip_list.content next_cell).id
                    &&
                    let prev_cell_pointer =
                      Skip_list.back_pointer next_cell 0
                    in
                    match prev_cell_pointer with
                    | None -> false
                    | Some prev_ptr ->
                        Pointer_hash.equal prev_ptr (hash prev_cell))
                    (dal_proof_error
                       "verify_proof_repr: invalid next_inc_proof")
                in
                verify_inclusion_proof
                  next_inc_proof
                  ~src:snapshot
                  ~dest:next_cell
          in
          return_none

    let verify_proof dal_params page_id snapshot serialized_proof =
      let open Result_syntax in
      let* proof_repr = deserialize_proof serialized_proof in
      verify_proof_repr dal_params page_id snapshot proof_repr

    module Internal_for_tests = struct
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

module History_v2 = struct
  (* History is represented via a skip list. The content of the cell
     is the hash of a merkle proof. *)

  [@@@ocaml.warning "-a"]

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

  type error += Add_element_in_slots_skip_list_violates_ordering_v2

  let () =
    register_error_kind
      `Temporary
      ~id:"Dal_slot_repr.add_element_in_slots_skip_list_violates_ordering_v2"
      ~title:"Add an element in slots skip list that violates ordering v2"
      ~description:
        "Attempting to add an element on top of the Dal confirmed slots skip \
         list that violates the ordering."
      Data_encoding.unit
      (function
        | Add_element_in_slots_skip_list_violates_ordering_v2 -> Some ()
        | _ -> None)
      (fun () -> Add_element_in_slots_skip_list_violates_ordering_v2)

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
          Add_element_in_slots_skip_list_violates_ordering_v2
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
            ~title:"dal_skip_list_v2"
            (Tag 1)
            (obj2
               (req "kind" (constant "dal_skip_list_v2"))
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
          let name = "dal_slots_cache_v2"
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
        | [], s :: _ ->
            tzfail Add_element_in_slots_skip_list_violates_ordering_v2
        | i :: indices', s :: slots' ->
            if I.(i = s.Header.id.index) then
              let* res = aux indices' slots' in
              Content.Attested s :: res |> ok
            else if I.(i < s.Header.id.index) then
              let* res = aux indices' slots in
              mk_unattested i :: res |> ok
            else
              (* i > s.Header.id.index *)
              tzfail Add_element_in_slots_skip_list_violates_ordering_v2
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

    (*  TODO: will be uncommented incrementally on the next MRs *)

    (*
    (* Dal proofs section *)

    (** An inclusion proof, for a page ID, is a list of the slots' history
        skip list's cells that encodes a minimal path:
        - from a starting cell, which serves as a reference. It is usually called
        'snapshot' below,
        - to a final cell, that is either the exact target cell in case the slot
         of the page is confirmed, or a cell whose slot ID is the smallest
         that directly follows the page's slot id, in case the target slot
         is not confirmed.

         Using the starting cell as a trustable starting point (i.e. maintained
         and provided by L1), and combined with the extra information stored in
         the {!proof} type below, one can verify if a slot (and then a page of
         that slot) is confirmed on L1 or not. *)
    type inclusion_proof = history list

    (** (See the documentation in the mli file to understand what we want to
        prove in game refutation involving Dal and why.)

        A Dal proof is an algebraic datatype with two cases, where we basically
        prove that a Dal page is confirmed on L1 or not. Being 'not confirmed'
        here includes the case where the slot's header is not published and the
        case where the slot's header is published, but the attesters didn't
        confirm the availability of its data.

        To produce a proof representation for a page (see function {!produce_proof_repr}
        below), we assume given:

        - [page_id], identifies the page;

        - [slots_history], a current/recent cell of the slots history skip list.
          Typically, it should be the skip list cell snapshotted when starting the
          refutation game;

       - [history_cache], a sufficiently large slots history cache, to navigate
          back through the successive cells of the skip list. Typically,
          the cache should at least contain the cell whose slot ID is [page_id.slot_id]
          in case the page is confirmed, or the cell whose slot ID is immediately
          after [page_id.slot_id] in case of an unconfirmed page. Indeed,
          inclusion proofs encode paths through skip lists' cells where the head
          is the reference/snapshot cell and the last element is the target slot
          in or the nearest upper slot (w.r.t [page_id]'s slot id and to
          skip list elements ordering) ;

        - [page_info], that provides the page's information (the content and
          the slot membership proof) for page_id. In case the page is supposed
          to be confirmed, this argument should contain the page's content and
          the proof that the page is part of the (confirmed) slot whose ID is
          given in [page_id]. In case we want to show that the page is not confirmed,
          the value [page_info] should be [None].

      [dal_parameters] is used when verifying that/if the page is part of
      the candidate slot (if any).


*)
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
                  page_id.slot_id. *)
        }  (** The case where the slot's page is confirmed/attested on L1. *)
      | Page_unconfirmed of {
          prev_cell : history;
              (** [prev_cell] is the cell of the skip list containing a
                  (confirmed) slot, and whose ID is the biggest (w.r.t. to skip
                  list elements ordering), but smaller than [page_id.slot_id]. *)
          next_cell_opt : history option;
              (** [next_cell_opt] is the cell that immediately follows [prev_cell]
                  in the skip list, if [prev_cell] is not the latest element in
                  the list. Otherwise, it's set to [None]. *)
          next_inc_proof : inclusion_proof;
              (** [inc_proof] is a (minimal) path in the skip list that proves
                  cells inclusion. In case, [next_cell_opt] contains some cell
                  'next_cell', the head of the list is the [slots_history]
                  provided to produce the proof, and the last cell is
                  'next_cell'. In case [next_cell_opt] is [None], the list is
                  empty.

                  We maintain the following invariant in case the inclusion
                  proof is not empty:
                  ```
                   (content next_cell).id > page_id.slot_id > (content prev_cell).id AND
                   hash prev_cell = back_pointer next_cell 0 AND
                   Some next_cell = next_cell_opt AND
                   head next_inc_proof = slots_history
                  ```

                  Said differently, `next_cell` and `prev_cell` are two consecutive
                  cells of the skip list whose contents' IDs surround the page's
                  slot ID. Moreover, the head of the list should be equal to
                  the initial (snapshotted) slots_history skip list.

                  The case of an empty inclusion proof happens when the inputs
                  are such that: `page_id.slot_id > (content slots_history).id`.
                  The returned proof statement implies the following property in this case:

                  ```
                  next_cell_opt = None AND prev_cell = slots_history
                  ```
              *)
        }
          (** The case where the slot's page doesn't exist or is not
              confirmed on L1. *)

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
          (obj4
             (req "kind" (constant "unconfirmed"))
             (req "prev_cell" history_encoding)
             (req "next_cell_opt" (option history_encoding))
             (req "next_inc_proof" (list history_encoding)))
          (function
            | Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof} ->
                Some ((), prev_cell, next_cell_opt, next_inc_proof)
            | _ -> None)
          (fun ((), prev_cell, next_cell_opt, next_inc_proof) ->
            Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof})
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

    type error += Dal_invalid_proof_serialization_v2

    let () =
      register_error_kind
        `Permanent
        ~id:"Dal_slot_repr.invalid_proof_serialization_v2"
        ~title:"Dal invalid proof serialization v2"
        ~description:"Error occured during dal proof serialization"
        Data_encoding.unit
        (function Dal_invalid_proof_serialization_v2 -> Some () | _ -> None)
        (fun () -> Dal_invalid_proof_serialization_v2)

    let serialize_proof proof =
      let open Result_syntax in
      match Data_encoding.Binary.to_bytes_opt proof_repr_encoding proof with
      | None -> tzfail Dal_invalid_proof_serialization_v2
      | Some serialized_proof -> return serialized_proof

    type error += Dal_invalid_proof_deserialization_v2

    let () =
      register_error_kind
        `Permanent
        ~id:"Dal_slot_repr.invalid_proof_deserialization_v2"
        ~title:"Dal invalid proof deserialization v2"
        ~description:"Error occured during dal proof deserialization"
        Data_encoding.unit
        (function Dal_invalid_proof_deserialization_v2 -> Some () | _ -> None)
        (fun () -> Dal_invalid_proof_deserialization_v2)

    let deserialize_proof proof =
      let open Result_syntax in
      match Data_encoding.Binary.of_bytes_opt proof_repr_encoding proof with
      | None -> tzfail Dal_invalid_proof_deserialization_v2
      | Some deserialized_proof -> return deserialized_proof

    let pp_inclusion_proof = Format.pp_print_list pp_history

    let pp_history_opt = Format.pp_print_option pp_history

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
            | Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof} ->
                Format.fprintf
                  fmt
                  "Page_unconfirmed (prev_cell = %a | next_cell = %a | \
                   prev_inc_proof:[size=%d@ | path=%a])"
                  pp_history
                  prev_cell
                  pp_history_opt
                  next_cell_opt
                  (List.length next_inc_proof)
                  pp_inclusion_proof
                  next_inc_proof)

    type error +=
      | Dal_proof_error_v2 of string
      | Unexpected_page_size_v2 of {expected_size : int; page_size : int}

    let () =
      let open Data_encoding in
      register_error_kind
        `Permanent
        ~id:"dal_slot_repr.slots_history.dal_proof_error_v2"
        ~title:"Dal proof error v2"
        ~description:"Error occurred during Dal proof production or validation"
        ~pp:(fun ppf e -> Format.fprintf ppf "Dal proof error: %s" e)
        (obj1 (req "error" (string Plain)))
        (function Dal_proof_error_v2 e -> Some e | _ -> None)
        (fun e -> Dal_proof_error_v2 e)

    let () =
      let open Data_encoding in
      register_error_kind
        `Permanent
        ~id:"dal_slot_repr.slots_history.unexpected_page_size_v2"
        ~title:"Unexpected page size v2"
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
          | Unexpected_page_size_v2 {expected_size; page_size} ->
              Some (expected_size, page_size)
          | _ -> None)
        (fun (expected_size, page_size) ->
          Unexpected_page_size_v2 {expected_size; page_size})

    let dal_proof_error reason = Dal_proof_error_v2 reason

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
          @@ Unexpected_page_size_v2
               {
                 expected_size = dal_params.page_size;
                 page_size = Bytes.length data;
               }

    let produce_proof_repr dal_params page_id ~page_info ~get_history slots_hist
        =
      let open Lwt_result_syntax in
      let Page.{slot_id; page_index = _} = page_id in
      (* We search for a slot whose ID is equal to target_id. *)
      let*! search_result =
        Skip_list.search ~deref:get_history ~target_id:slot_id ~cell:slots_hist
      in
      match (page_info, search_result.Skip_list.last_cell) with
      | _, Deref_returned_none ->
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned 'Deref_returned_none': Slots history \
                cache is ill-formed or has too few entries."
      | _, No_exact_or_lower_ptr ->
          tzfail
          @@ dal_proof_error
               "Skip_list.search returned 'No_exact_or_lower_ptr', while it is \
                initialized with a min elt (slot zero)."
      | Some (page_data, page_proof), Found target_cell ->
          (* The slot to which the page is supposed to belong is found. *)
          let Header.{id; commitment} = Skip_list.content target_cell in
          (* We check that the slot is not the dummy slot. *)
          let*? () =
            error_when
              Compare.Int.(Header.compare_slot_id id Header.zero.id = 0)
              (dal_proof_error
                 "Skip_list.search returned 'Found <zero_slot>': No existence \
                  proof should be constructed with the slot zero.")
          in
          let*? () =
            check_page_proof dal_params page_proof page_data page_id commitment
          in
          let inc_proof = List.rev search_result.Skip_list.rev_path in
          let*? () =
            error_when
              (List.is_empty inc_proof)
              (dal_proof_error "The inclusion proof cannot be empty")
          in
          (* All checks succeeded. We return a `Page_confirmed` proof. *)
          return
            ( Page_confirmed {inc_proof; target_cell; page_data; page_proof},
              Some page_data )
      | None, Nearest {lower = prev_cell; upper = next_cell_opt} ->
          (* There is no previously confirmed slot in the skip list whose ID
             corresponds to the {published_level; slot_index} information
             given in [page_id]. But, `search` returned a skip list [prev_cell]
             (and possibly [next_cell_opt]) such that:
             - the ID of [prev_cell]'s slot is the biggest immediately smaller than
               the page's information {published_level; slot_index}
             - if not equal to [None], the ID of [next_cell_opt]'s slot is the smallest
               immediately bigger than the page's slot id `slot_id`.
             - if [next_cell_opt] is [None] then, [prev_cell] should be equal to
               the given history_proof cell. *)
          let* next_inc_proof =
            match search_result.Skip_list.rev_path with
            | [] -> assert false (* Not reachable *)
            | prev :: rev_next_inc_proof ->
                let*? () =
                  error_unless
                    (equal_history prev prev_cell)
                    (dal_proof_error
                       "Internal error: search's Nearest result is \
                        inconsistent.")
                in
                return @@ List.rev rev_next_inc_proof
          in
          return
            (Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof}, None)
      | None, Found _ ->
          tzfail
          @@ dal_proof_error
               "The page ID's slot is confirmed, but no page content and proof \
                are provided."
      | Some _, Nearest _ ->
          tzfail
          @@ dal_proof_error
               "The page ID's slot is not confirmed, but page content and \
                proof are provided."

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
      let Page.{slot_id; page_index = _} = page_id in
      match proof with
      | Page_confirmed {target_cell; page_data; page_proof; inc_proof} ->
          (* If the page is supposed to be confirmed, the last cell in
             [inc_proof] should store the slot of the page. *)
          let Header.{id; commitment} = Skip_list.content target_cell in
          let* () =
            error_when
              Compare.Int.(Header.compare_slot_id id Header.zero.id = 0)
              (dal_proof_error
                 "verify_proof_repr: cannot construct a confirmation page \
                  proof with 'zero' as target slot.")
          in
          let* () =
            verify_inclusion_proof inc_proof ~src:snapshot ~dest:target_cell
          in
          (* We check that the page indeed belongs to the target slot at the
             given page index. *)
          let* () =
            check_page_proof dal_params page_proof page_data page_id commitment
          in
          (* If all checks succeed, we return the data/content of the page. *)
          return_some page_data
      | Page_unconfirmed {prev_cell; next_cell_opt; next_inc_proof} ->
          (* The page's slot is supposed to be unconfirmed. *)
          let ( < ) a b = Compare.Int.(Header.compare_slot_id a b < 0) in
          (* We retrieve the last cell of the inclusion proof to be able to
             call {!verify_inclusion_proof}. We also do some well-formedness on
             the shape of the inclusion proof (see the case [Page_unconfirmed]
             of type {!proof}). *)
          let* () =
            match next_cell_opt with
            | None ->
                let* () =
                  error_unless
                    (List.is_empty next_inc_proof)
                    (dal_proof_error
                       "verify_proof_repr: invalid next_inc_proof")
                in
                (* In case the inclusion proof has no elements, we check that:
                   - the prev_cell slot's id is smaller than the unconfirmed slot's ID
                   - the snapshot is equal to the [prev_cell] skip list.

                   This way, and since the skip list is sorted wrt.
                   {!compare_slot_id}, we are sure that the skip list whose head
                   is [snapshot] = [prev_cell] cannot contain a slot whose ID is
                   [slot_id]. *)
                error_unless
                  ((Skip_list.content prev_cell).id < slot_id
                  && equal_history snapshot prev_cell)
                  (dal_proof_error "verify_proof_repr: invalid next_inc_proof")
            | Some next_cell ->
                (* In case the inclusion proof has at least one element,
                   we check that:
                   - the [prev_cell] slot's id is smaller than [slot_id]
                   - the [next_cell] slot's id is greater than [slot_id]
                   - the [next_cell] cell is a direct successor of the
                     [prev_cell] cell.
                   - the [next_cell] cell is a predecessor of [snapshot]

                   Since the skip list is sorted wrt. {!compare_slot_id}, and
                   if the call to {!verify_inclusion_proof} succeeds, we are
                   sure that the skip list whose head is [snapshot] cannot
                   contain a slot whose ID is [slot_id]. *)
                let* () =
                  error_unless
                    ((Skip_list.content prev_cell).id < slot_id
                    && slot_id < (Skip_list.content next_cell).id
                    &&
                    let prev_cell_pointer =
                      Skip_list.back_pointer next_cell 0
                    in
                    match prev_cell_pointer with
                    | None -> false
                    | Some prev_ptr ->
                        Pointer_hash.equal prev_ptr (hash prev_cell))
                    (dal_proof_error
                       "verify_proof_repr: invalid next_inc_proof")
                in
                verify_inclusion_proof
                  next_inc_proof
                  ~src:snapshot
                  ~dest:next_cell
          in
          return_none

    let verify_proof dal_params page_id snapshot serialized_proof =
      let open Result_syntax in
      let* proof_repr = deserialize_proof serialized_proof in
      verify_proof_repr dal_params page_id snapshot proof_repr

    module Internal_for_tests = struct
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
  *)
  end

  include V1
end
