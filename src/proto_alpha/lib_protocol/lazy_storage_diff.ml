(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@momadic-labs.com>                *)
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

module type Next = sig
  type id

  val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

  val incr : Raw_context.t -> (Raw_context.t * id) tzresult Lwt.t
end

module type Total_bytes = sig
  type id

  val init : Raw_context.t -> id -> Z.t -> Raw_context.t tzresult Lwt.t

  val get : Raw_context.t -> id -> Z.t tzresult Lwt.t

  val set : Raw_context.t -> id -> Z.t -> Raw_context.t tzresult Lwt.t
end

module type OPS = sig
  module Id : Lazy_storage_kind.ID

  type alloc

  type updates

  val title : string

  val alloc_encoding : alloc Data_encoding.t

  val updates_encoding : updates Data_encoding.t

  val bytes_size_for_empty : Z.t

  val alloc : Raw_context.t -> id:Id.t -> alloc -> Raw_context.t tzresult Lwt.t

  val apply_updates :
    Raw_context.t -> id:Id.t -> updates -> (Raw_context.t * Z.t) tzresult Lwt.t

  module Next : Next with type id := Id.t

  module Total_bytes : Total_bytes with type id := Id.t

  val copy :
    Raw_context.t -> from:Id.t -> to_:Id.t -> Raw_context.t tzresult Lwt.t

  val remove_rec : Raw_context.t -> Id.t -> Raw_context.t Lwt.t
end

module Big_map = struct
  include Lazy_storage_kind.Big_map

  let bytes_size_for_big_map_key = 65

  let bytes_size_for_empty =
    let bytes_size_for_big_map = 33 in
    Z.of_int bytes_size_for_big_map

  let alloc ctxt ~id {key_type; value_type} =
    (* Annotations are erased to allow sharing on [Copy]. The types from the
       contract code are used, these ones are only used to make sure they are
       compatible during transmissions between contracts, and only need to be
       compatible, annotations nonwhistanding. *)
    let key_type =
      Micheline.strip_locations
        (Script_repr.strip_annotations (Micheline.root key_type))
    in
    let value_type =
      Micheline.strip_locations
        (Script_repr.strip_annotations (Micheline.root value_type))
    in
    Storage.Big_map.Key_type.init ctxt id key_type
    >>=? fun ctxt -> Storage.Big_map.Value_type.init ctxt id value_type

  let apply_update ctxt ~id
      { key = _key_is_shown_only_on_the_receipt_in_print_big_map_diff;
        key_hash;
        value } =
    match value with
    | None ->
        Storage.Big_map.Contents.remove (ctxt, id) key_hash
        >|=? fun (ctxt, freed, existed) ->
        let freed =
          if existed then freed + bytes_size_for_big_map_key else freed
        in
        (ctxt, Z.of_int ~-freed)
    | Some v ->
        Storage.Big_map.Contents.init_set (ctxt, id) key_hash v
        >|=? fun (ctxt, size_diff, existed) ->
        let size_diff =
          if existed then size_diff else size_diff + bytes_size_for_big_map_key
        in
        (ctxt, Z.of_int size_diff)

  let apply_updates ctxt ~id updates =
    fold_left_s
      (fun (ctxt, size) update ->
        apply_update ctxt ~id update
        >|=? fun (ctxt, added_size) -> (ctxt, Z.add size added_size))
      (ctxt, Z.zero)
      updates

  include Storage.Big_map
end

type ('id, 'alloc, 'updates) ops =
  (module OPS
     with type Id.t = 'id
      and type alloc = 'alloc
      and type updates = 'updates)

let get_ops : type i a u. (i, a, u) Lazy_storage_kind.t -> (i, a, u) ops =
  function
  | Big_map ->
      (module Big_map)

module KId = struct
  type t = E : ('id, _, _) Lazy_storage_kind.t * 'id -> t

  module Comparable = struct
    type nonrec t = t

    let compare (E (kind1, id1)) (E (kind2, id2)) =
      match Lazy_storage_kind.compare kind1 kind2 with
      | Lt ->
          -1
      | Gt ->
          1
      | Eq ->
          let (module OPS) = get_ops kind1 in
          OPS.Id.compare id1 id2
  end
end

type ('id, 'alloc) init = Existing | Copy of {src : 'id} | Alloc of 'alloc

type ('id, 'alloc, 'updates) diff =
  | Remove
  | Update of {init : ('id, 'alloc) init; updates : 'updates}

let diff_encoding : type i a u. (i, a, u) ops -> (i, a, u) diff Data_encoding.t
    =
 fun (module OPS) ->
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"update"
        (obj2
           (req "action" (constant "update"))
           (req "updates" OPS.updates_encoding))
        (function
          | Update {init = Existing; updates} -> Some ((), updates) | _ -> None)
        (fun ((), updates) -> Update {init = Existing; updates});
      case
        (Tag 1)
        ~title:"remove"
        (obj1 (req "action" (constant "remove")))
        (function Remove -> Some () | _ -> None)
        (fun () -> Remove);
      case
        (Tag 2)
        ~title:"copy"
        (obj3
           (req "action" (constant "copy"))
           (req "source" OPS.Id.encoding)
           (req "updates" OPS.updates_encoding))
        (function
          | Update {init = Copy {src}; updates} ->
              Some ((), src, updates)
          | _ ->
              None)
        (fun ((), src, updates) -> Update {init = Copy {src}; updates});
      case
        (Tag 3)
        ~title:"alloc"
        (merge_objs
           (obj2
              (req "action" (constant "alloc"))
              (req "updates" OPS.updates_encoding))
           OPS.alloc_encoding)
        (function
          | Update {init = Alloc alloc; updates} ->
              Some (((), updates), alloc)
          | _ ->
              None)
        (fun (((), updates), alloc) -> Update {init = Alloc alloc; updates}) ]

let apply_updates :
    type i a u.
    Raw_context.t ->
    (i, a, u) ops ->
    id:i ->
    u ->
    (Raw_context.t * Z.t) tzresult Lwt.t =
 fun ctxt (module OPS) ~id updates ->
  OPS.apply_updates ctxt ~id updates
  >>=? fun (ctxt, updates_size) ->
  if Z.(equal updates_size zero) then return (ctxt, updates_size)
  else
    OPS.Total_bytes.get ctxt id
    >>=? fun size ->
    OPS.Total_bytes.set ctxt id (Z.add size updates_size)
    >|=? fun ctxt -> (ctxt, updates_size)

let apply_init :
    type i a u.
    Raw_context.t ->
    (i, a, u) ops ->
    id:i ->
    (i, a) init ->
    (Raw_context.t * Z.t) tzresult Lwt.t =
 fun ctxt (module OPS) ~id init ->
  match init with
  | Existing ->
      return (ctxt, Z.zero)
  | Copy {src} ->
      OPS.copy ctxt ~from:src ~to_:id
      >>=? fun ctxt ->
      OPS.Total_bytes.get ctxt src
      >>=? fun copy_size ->
      return (ctxt, Z.add copy_size OPS.bytes_size_for_empty)
  | Alloc alloc ->
      OPS.Total_bytes.init ctxt id Z.zero
      >>=? fun ctxt ->
      OPS.alloc ctxt id alloc
      >>=? fun ctxt -> return (ctxt, OPS.bytes_size_for_empty)

let apply_diff :
    type i a u.
    Raw_context.t ->
    (i, a, u) ops ->
    id:i ->
    (i, a, u) diff ->
    (Raw_context.t * Z.t) tzresult Lwt.t =
 fun ctxt ((module OPS) as ops) ~id diff ->
  match diff with
  | Remove ->
      OPS.Total_bytes.get ctxt id
      >>=? fun size ->
      OPS.remove_rec ctxt id
      >>= fun ctxt -> return (ctxt, Z.neg (Z.add size OPS.bytes_size_for_empty))
  | Update {init; updates} ->
      apply_init ctxt ops ~id init
      >>=? fun (ctxt, init_size) ->
      apply_updates ctxt ops ~id updates
      >>=? fun (ctxt, updates_size) ->
      return (ctxt, Z.add init_size updates_size)

type diffs_item =
  | E : ('i, 'a, 'u) Lazy_storage_kind.t * 'i * ('i, 'a, 'u) diff -> diffs_item

let make :
    type i a u.
    (i, a, u) Lazy_storage_kind.t -> i -> (i, a, u) diff -> diffs_item =
 fun k id diff -> E (k, id, diff)

let make_remove (KId.E (k, id)) = E (k, id, Remove)

let item_encoding =
  let open Data_encoding in
  union
  @@ List.map
       (fun (tag, Lazy_storage_kind.E k) ->
         let ops = get_ops k in
         let (module OPS) = ops in
         let title = OPS.title in
         case
           (Tag tag)
           ~title
           (obj3
              (req "kind" (constant title))
              (req "id" OPS.Id.encoding)
              (req "diff" (diff_encoding ops)))
           (fun (E (kind, id, diff)) ->
             match Lazy_storage_kind.compare k kind with
             | Eq ->
                 Some ((), id, diff)
             | Lt | Gt ->
                 None)
           (fun ((), id, diff) -> E (k, id, diff)))
       Lazy_storage_kind.all

type diffs = diffs_item list

let encoding =
  let open Data_encoding in
  def "lazy_storage_diff" @@ list item_encoding

let apply ctxt diffs =
  fold_left_s
    (fun (ctxt, total_size) (E (k, id, diff)) ->
      let ops = get_ops k in
      apply_diff ctxt ops id diff
      >|=? fun (ctxt, added_size) ->
      let (module OPS) = ops in
      ( ctxt,
        if OPS.Id.is_temp id then total_size else Z.add total_size added_size
      ))
    (ctxt, Z.zero)
    diffs

let fresh :
    type i a u.
    (i, a, u) Lazy_storage_kind.t ->
    temporary:bool ->
    Raw_context.t ->
    (Raw_context.t * i) tzresult Lwt.t =
 fun kind ~temporary ctxt ->
  if temporary then
    return
      (Raw_context.fold_map_temporary_lazy_storage_ids ctxt (fun temp_ids ->
           Lazy_storage_kind.Temp_ids.fresh kind temp_ids))
  else
    let (module OPS) = get_ops kind in
    OPS.Next.incr ctxt

let init ctxt =
  fold_left_s
    (fun ctxt (_tag, Lazy_storage_kind.E k) ->
      let (module OPS) = get_ops k in
      OPS.Next.init ctxt)
    ctxt
    Lazy_storage_kind.all

let cleanup_temporaries ctxt =
  Raw_context.map_temporary_lazy_storage_ids_s ctxt (fun temp_ids ->
      Lwt_list.fold_left_s
        (fun ctxt (_tag, Lazy_storage_kind.E k) ->
          let (module OPS) = get_ops k in
          Lazy_storage_kind.Temp_ids.fold_s k OPS.remove_rec temp_ids ctxt)
        ctxt
        Lazy_storage_kind.all
      >|= fun ctxt -> (ctxt, Lazy_storage_kind.Temp_ids.init))
