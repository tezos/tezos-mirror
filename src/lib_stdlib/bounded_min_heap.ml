(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This heap structure could maybe be improved by maintaining a start
   index. This would reduce the complexity of `pop` to O(1) but would
   requires more complex bubble_<up|down> functions. Also, it could be
   interesting to add `pop_n` and `peek_n`, this might to changes the
   type of `t`, which is why it was not introduced at first to reduce
   the complexity of it. *)

module Make
    (Id : Stdlib.Hashtbl.HashedType)
    (E : sig
      include Stdlib.Set.OrderedType

      val id : t -> Id.t
    end) =
struct
  module Hash_map = Hashtbl.Make (Id)

  (* Invariants :
     - all indices between 0 and size (excluded) have data:
       size =
         Array.fold_left
         (fun count e -> if Option.is_some e then count + 1 else count) 0 data
     - the heap property itself:
        let P(i, j) =
          0 <= i < size /\ 0 <= j < size =>
          data.(j) = None \/ E.compare data.(i) data.(j) <= 0
       in \forall 0 <= i < size, P(i, 2 * i + 1) /\ P(i, 2 * (i + 1))
     - the size of [hash_map] is equal to [size]
     - the hash_map is the map from id to indice:
       \forall 0 <= i < size, hash_map( id data.(i) ) = i
  *)
  type t = {
    mutable size : int;
    data : E.t option array;
    hash_map : int Hash_map.t;
  }

  let create capacity =
    if capacity < 0 || capacity > Sys.max_array_length then
      invalid_arg
        (Format.sprintf "Bounded_min_heap.Make(_).create. capacity %d" capacity) ;
    {
      size = 0;
      data = Array.make capacity None;
      hash_map = Hash_map.create capacity;
    }

  let get_data data i =
    match data.(i) with
    | Some elt -> elt
    | None ->
        (* only used locally to get data we are sure exists*)
        assert false

  let swap i j t =
    let j_elt = t.data.(j) in
    let i_elt = t.data.(i) in
    let j_id = Option.map E.id j_elt in
    let i_id = Option.map E.id i_elt in

    (* replace in heap structure *)
    t.data.(j) <- i_elt ;
    t.data.(i) <- j_elt ;

    (* replace in id -> index structure *)
    Option.iter (fun j_id -> Hash_map.replace t.hash_map j_id i) j_id ;
    Option.iter (fun i_id -> Hash_map.replace t.hash_map i_id j) i_id

  let parent i = (i - 1) / 2

  let left_child i = (2 * i) + 1

  let right_child i = 2 * (i + 1)

  let childrens i = (left_child i, right_child i)

  (* Bubble up the value located at index [i] such until the t
     property is locally fulfilled *)
  let bubble_up i t =
    let rec loop i =
      let p = parent i in
      if E.compare (get_data t.data i) (get_data t.data p) < 0 then (
        swap i p t ;
        loop p)
    in
    loop i

  (* Bubble down the value at index [i] until the t property is
     locally fulfilled, aka the value at index [i] is smaller than the
     one of its two children *)
  let bubble_down i ({size; data; _} as t) =
    let rec loop i =
      let left_index, right_index = childrens i in
      let value = get_data data i in
      if left_index < size then (
        assert (data.(left_index) <> None) ;
        let left_value = get_data data left_index in
        if right_index < size then (
          (* swap the value with the smallest of its two children *)
          assert (data.(right_index) <> None) ;
          let right_value = get_data data right_index in
          if E.compare right_value left_value < 0 then (
            if E.compare value right_value > 0 then (
              swap i right_index t ;
              loop right_index))
          else if E.compare value left_value > 0 then (
            swap i left_index t ;
            loop left_index))
        else if
          E.compare value left_value > 0
          (* swap the value with its left child, since the right one does not exist *)
        then (
          swap i left_index t ;
          loop left_index))
    in
    loop i

  let add t id x pos =
    t.data.(pos) <- Some x ;
    t.size <- pos + 1 ;
    Hash_map.replace t.hash_map id pos ;
    bubble_up pos t

  let insert x t =
    let id = E.id x in
    let exits_elt_i_opt = Hash_map.find_opt t.hash_map id in
    match exits_elt_i_opt with
    (* Element already exists, we replace it if the order is
       inferior. *)
    | Some elt_i ->
        let elt = get_data t.data elt_i in
        if E.compare x elt < 0 then (
          t.data.(elt_i) <- Some x ;
          bubble_up elt_i t) ;
        Ok ()
    | None ->
        let pos = t.size in
        let data = t.data in
        if pos < Array.length data then (
          add t id x pos ;
          Ok ())
        else Error "no space left in heap"

  let remove_index t index =
    t.data.(index) <- None ;
    t.size <- t.size - 1 ;
    if t.size > 0 && index <> t.size then (
      swap index t.size t ;
      bubble_down index t)

  let remove t id =
    (* uses `find_opt` and not `find` because it may failed if called
       with unknown id *)
    match Hash_map.find_opt t.hash_map id with
    | None -> ()
    | Some index ->
        remove_index t index ;
        Hash_map.remove t.hash_map id

  let pop t =
    if t.size = 0 then None
    else
      let min_elem = get_data t.data 0 in
      let id = E.id min_elem in
      remove t id ;
      assert (Hash_map.length t.hash_map = t.size) ;
      Some min_elem

  let peek_min t = if t.size = 0 then None else t.data.(0)

  let elements t =
    let a = Array.init t.size (get_data t.data) in
    Array.sort E.compare a ;
    Array.to_list a

  let length {size; _} = size

  let mem elt t =
    let id = E.id elt in
    Hash_map.mem t.hash_map id

  let find_opt id t =
    let elt_i = Hash_map.find_opt t.hash_map id in
    Option.map (get_data t.data) elt_i

  let clear t =
    Hash_map.clear t.hash_map ;
    Array.fill t.data 0 t.size None ;
    t.size <- 0

  let remove_predicate f (t : t) =
    let rec aux heap_index acc =
      if heap_index >= t.size then acc
      else
        let value = get_data t.data heap_index in
        if f value then (
          let id = E.id value in
          remove t id ;
          assert (Hash_map.length t.hash_map = t.size) ;
          aux heap_index (id :: acc))
        else aux (heap_index + 1) acc
    in
    aux 0 []

  module Internal_for_tests = struct
    let check_heap_invariant ~pp_id ~pp_elt h =
      let get_data_opt i =
        if i < Array.length h.data then h.data.(i) else None
      in
      let is_greater_or_none x i =
        match (x, get_data_opt i) with
        | Some _x, None -> true
        | Some x, Some l -> E.compare x l < 0
        | None, Some _ | None, None -> false (* impossible case *)
      in
      let rec check_invariant_for_all_index i =
        if i = 0 then ()
        else
          let index = i - 1 in
          let data_opt = h.data.(index) in
          let id_opt = Option.map E.id data_opt in
          let index_opt = Option.map (Hash_map.find h.hash_map) id_opt in
          let has_data = Option.is_some data_opt in
          let has_map_index = Option.is_some index_opt in
          let map_index_is_correct =
            Option.value ~default:false
            @@ Option.map (Int.equal index) index_opt
          in
          let left_child_index = left_child index in
          let right_child_index = right_child index in
          let left_child_is_none_or_inf =
            is_greater_or_none data_opt left_child_index
          in
          let right_child_is_none_or_inf =
            is_greater_or_none data_opt right_child_index
          in
          let correct_map_size = Hash_map.length h.hash_map = h.size in
          let is_correct =
            has_data && has_map_index && map_index_is_correct
            && left_child_is_none_or_inf && right_child_is_none_or_inf
            && correct_map_size
          in
          if is_correct then check_invariant_for_all_index (i - 1)
          else
            failwith
              (Format.asprintf
                 "invariant for index %d are invalid:@.has_data: \
                  %b@.has_map_index: %b@.map_index_is_correct: %b@.left_child \
                  : (valid: %b, %d, %a)@.right_child: (valid: %b, %d, \
                  %a)@.correct_map_size: %b@."
                 index
                 has_data
                 has_map_index
                 map_index_is_correct
                 left_child_is_none_or_inf
                 left_child_index
                 Format.(pp_print_option pp_elt)
                 (get_data_opt left_child_index)
                 right_child_is_none_or_inf
                 right_child_index
                 Format.(pp_print_option pp_elt)
                 (get_data_opt right_child_index)
                 correct_map_size)
      in
      Format.printf
        "@.@.HEAP:@.heap: %a@.size: %d@.hash_map: %a@.@."
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
            (pp_print_option pp_elt))
        (h.data |> Array.to_list)
        h.size
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
            (fun fmt (id, index) ->
              Format.fprintf fmt "(%a, %d)" pp_id id index))
        (Hash_map.to_seq h.hash_map |> List.of_seq) ;
      check_invariant_for_all_index h.size ;
      assert (Hash_map.length h.hash_map = h.size)
  end
end
