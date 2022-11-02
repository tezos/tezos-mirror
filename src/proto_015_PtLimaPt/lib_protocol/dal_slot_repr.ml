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

module Header = struct
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3389

     It is not clear whether the size of the slot associated to the
     commitment should be given here. *)
  type t = Dal.commitment

  let equal = Dal.Commitment.equal

  let encoding = Dal.Commitment.encoding

  let pp ppf commitment =
    Format.fprintf ppf "%s" (Dal.Commitment.to_b58check commitment)

  let zero = Dal.Commitment.zero
end

module Index = struct
  type t = int

  let max_value = 255

  let encoding = Data_encoding.uint8

  let pp = Format.pp_print_int

  let zero = 0

  let of_int slot_index =
    if Compare.Int.(slot_index <= max_value && slot_index >= zero) then
      Some slot_index
    else None

  let to_int slot_index = slot_index [@@ocaml.inline always]

  let compare = Compare.Int.compare

  let equal = Compare.Int.equal
end

type id = {published_level : Raw_level_repr.t; index : Index.t}

type t = {id : id; header : Header.t}

type slot = t

type slot_index = Index.t

let slot_id_equal ({published_level; index} : id) s2 =
  Raw_level_repr.equal published_level s2.published_level
  && Index.equal index s2.index

let slot_equal ({id; header} : t) s2 =
  slot_id_equal id s2.id && Header.equal header s2.header

let compare_slot_id ({published_level; index} : id) s2 =
  let c = Raw_level_repr.compare published_level s2.published_level in
  if Compare.Int.(c <> 0) then c else Index.compare index s2.index

let zero_id =
  {
    (* We don't expect to have any published slot at level
       Raw_level_repr.root. *)
    published_level = Raw_level_repr.root;
    index = Index.zero;
  }

let zero = {id = zero_id; header = Header.zero}

module Slot_index = Index

module Page = struct
  type content = Bytes.t

  module Index = struct
    type t = int

    let zero = 0

    let encoding = Data_encoding.int16

    let pp = Format.pp_print_int

    let compare = Compare.Int.compare

    let equal = Compare.Int.equal
  end

  type t = {slot_index : Slot_index.t; page_index : Index.t}

  let encoding =
    let open Data_encoding in
    conv
      (fun {slot_index; page_index} -> (slot_index, page_index))
      (fun (slot_index, page_index) -> {slot_index; page_index})
      (obj2
         (req "slot_index" Slot_index.encoding)
         (req "page_index" Index.encoding))

  let equal page page' =
    Slot_index.equal page.slot_index page'.slot_index
    && Index.equal page.page_index page'.page_index

  let pp fmt {slot_index; page_index} =
    Format.fprintf
      fmt
      "(slot_index: %a, page_index: %a)"
      Slot_index.pp
      slot_index
      Index.pp
      page_index
end

let slot_encoding =
  let open Data_encoding in
  conv
    (fun {id = {published_level; index}; header} ->
      (published_level, index, header))
    (fun (published_level, index, header) ->
      {id = {published_level; index}; header})
    (obj3
       (req "level" Raw_level_repr.encoding)
       (req "index" Data_encoding.uint8)
       (req "header" Header.encoding))

let pp_slot fmt {id = {published_level; index}; header} =
  Format.fprintf
    fmt
    "published_level: %a index: %a header: %a"
    Raw_level_repr.pp
    published_level
    Format.pp_print_int
    index
    Header.pp
    header

module Slot_market = struct
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3108

     Think harder about this data structure and whether it can be
     optimized. *)

  module Slot_index_map = Map.Make (Index)

  type t = {length : int; slots : slot Slot_index_map.t}

  let init ~length =
    if Compare.Int.(length < 0) then
      invalid_arg "Dal_slot_repr.Slot_market.init: length cannot be negative" ;
    let slots = Slot_index_map.empty in
    {length; slots}

  let length {length; _} = length

  let register t new_slot =
    if not Compare.Int.(0 <= new_slot.id.index && new_slot.id.index < t.length)
    then None
    else
      let has_changed = ref false in
      let update = function
        | None ->
            has_changed := true ;
            Some new_slot
        | Some x -> Some x
      in
      let slots = Slot_index_map.update new_slot.id.index update t.slots in
      let t = {t with slots} in
      Some (t, !has_changed)

  let candidates t =
    t.slots |> Slot_index_map.to_seq |> Seq.map snd |> List.of_seq
end

module Slots_history = struct
  (* History is represented via a skip list. The content of the cell
     is the hash of a merkle proof. *)

  (* A leaf of the merkle tree is a slot. *)
  module Leaf = struct
    type t = slot

    let to_bytes = Data_encoding.Binary.to_bytes_exn slot_encoding
  end

  module Content_prefix = struct
    let _prefix = "dash1"

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
    let _prefix = "dask1"

    (* 32 *)
    let b58check_prefix = "\002\224\072\115\035" (* dask1(55) *)

    let size = Some 32

    let name = "dal_skip_list_pointer"

    let title = "A hash that represents the skip list pointers"
  end

  module Pointer_hash = Blake2B.Make (Base58) (Pointer_prefix)

  module Skip_list_parameters = struct
    let basis = 2
  end

  module Skip_list = struct
    include Skip_list_repr.Make (Skip_list_parameters)

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

    let compare = compare_slot_id

    let compare_lwt a b = Lwt.return @@ compare a b

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

    let next ~prev_cell ~prev_cell_ptr elt =
      let open Tzresult_syntax in
      let* () =
        error_when
          (Compare.Int.( <= ) (compare elt.id (content prev_cell).id) 0)
          Add_element_in_slots_skip_list_violates_ordering
      in
      return @@ next ~prev_cell ~prev_cell_ptr elt

    let search ~deref ~cell ~id_target =
      search ~deref ~cell ~compare:(compare_lwt id_target)

    (* FIXME/DAL: search will be used in refutation proof. But we need to
       introduce it here to explain why we need an ordering on the skip list's
       elements. *)
    let _ = ignore search
  end

  module V1 = struct
    (* The content of a cell is the hash of all the slot headers
       represented as a merkle list. *)
    (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/3765
       Decide how to store attested slots in the skip list's content. *)
    type content = slot

    (* A pointer to a cell is the hash of its content and all the back
       pointers. *)
    type ptr = Pointer_hash.t

    type history = (content, ptr) Skip_list.cell

    type t = history

    let history_encoding =
      Skip_list.encoding Pointer_hash.encoding slot_encoding

    let equal_history : history -> history -> bool =
      Skip_list.equal Pointer_hash.equal slot_equal

    let encoding = history_encoding

    let equal : t -> t -> bool = equal_history

    let genesis : t = Skip_list.genesis (zero : slot)

    let hash_skip_list_cell cell =
      let current_slot = Skip_list.content cell in
      let back_pointers_hashes = Skip_list.back_pointers cell in
      Data_encoding.Binary.to_bytes_exn slot_encoding current_slot
      :: List.map Pointer_hash.to_bytes back_pointers_hashes
      |> Pointer_hash.hash_bytes

    let pp_history fmt (history : history) =
      let history_hash = hash_skip_list_cell history in
      Format.fprintf
        fmt
        "@[hash : %a@;%a@]"
        Pointer_hash.pp
        history_hash
        (Skip_list.pp ~pp_content:pp_slot ~pp_ptr:Pointer_hash.pp)
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

    let add_confirmed_slot (t, cache) slot =
      let open Tzresult_syntax in
      let prev_cell_ptr = hash_skip_list_cell t in
      let* cache = History_cache.remember prev_cell_ptr t cache in
      let* new_cell = Skip_list.next ~prev_cell:t ~prev_cell_ptr slot in
      return (new_cell, cache)

    let add_confirmed_slots (t : t) cache slots =
      List.fold_left_e add_confirmed_slot (t, cache) slots

    let add_confirmed_slots_no_cache =
      let no_cache = History_cache.empty ~capacity:0L in
      fun t slots ->
        List.fold_left_e add_confirmed_slot (t, no_cache) slots >|? fst
  end

  include V1
end

let encoding = slot_encoding

let pp = pp_slot

let equal = slot_equal
