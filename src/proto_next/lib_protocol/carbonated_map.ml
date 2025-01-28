(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
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

module type S = sig
  type 'a t

  type key

  type context

  val empty : 'a t

  val singleton : key -> 'a -> 'a t

  val size : 'a t -> int

  val find : context -> key -> 'a t -> ('a option * context) tzresult

  val update :
    context ->
    key ->
    (context -> 'a option -> ('a option * context) tzresult) ->
    'a t ->
    ('a t * context) tzresult

  val to_list : context -> 'a t -> ((key * 'a) list * context) tzresult

  val of_list :
    context ->
    merge_overlap:(context -> 'a -> 'a -> ('a * context) tzresult) ->
    (key * 'a) list ->
    ('a t * context) tzresult

  val merge :
    context ->
    merge_overlap:(context -> 'a -> 'a -> ('a * context) tzresult) ->
    'a t ->
    'a t ->
    ('a t * context) tzresult

  val map_e :
    context ->
    (context -> key -> 'a -> ('b * context) tzresult) ->
    'a t ->
    ('b t * context) tzresult

  val fold_e :
    context ->
    (context -> 'state -> key -> 'value -> ('state * context) tzresult) ->
    'state ->
    'value t ->
    ('state * context) tzresult

  val fold_es :
    context ->
    (context -> 'state -> key -> 'value -> ('state * context) tzresult Lwt.t) ->
    'state ->
    'value t ->
    ('state * context) tzresult Lwt.t
end

module type GAS = sig
  type context

  val consume :
    context ->
    Saturation_repr.may_saturate Saturation_repr.t ->
    context tzresult
end

module type COMPARABLE = sig
  include Compare.COMPARABLE

  (** [compare_cost k] returns the cost of comparing the given key [k] with
      another value of the same type. *)
  val compare_cost : t -> Saturation_repr.may_saturate Saturation_repr.t
end

module Make_builder (C : COMPARABLE) = struct
  module M = Map.Make (C)

  type 'a t = {map : 'a M.t; size : int}

  module Make (G : GAS) :
    S with type key = C.t and type context = G.context and type 'a t := 'a t =
  struct
    type key = C.t

    type context = G.context

    let empty = {map = M.empty; size = 0}

    let singleton key value = {map = M.singleton key value; size = 1}

    let size {size; _} = size

    let find_cost ~key ~size =
      Carbonated_map_costs.find_cost
        ~compare_key_cost:(C.compare_cost key)
        ~size

    let update_cost ~key ~size =
      Carbonated_map_costs.update_cost
        ~compare_key_cost:(C.compare_cost key)
        ~size

    let find ctxt key {map; size} =
      let open Result_syntax in
      let+ ctxt = G.consume ctxt (find_cost ~key ~size) in
      (M.find key map, ctxt)

    let update ctxt key f {map; size} =
      let open Result_syntax in
      let find_cost = find_cost ~key ~size in
      let update_cost = update_cost ~key ~size in
      (* Consume gas for looking up the old value *)
      let* ctxt = G.consume ctxt find_cost in
      let old_val_opt = M.find key map in
      (* The call to [f] must also account for gas *)
      let* new_val_opt, ctxt = f ctxt old_val_opt in
      match (old_val_opt, new_val_opt) with
      | Some _, Some new_val ->
          (* Consume gas for adding to the map *)
          let+ ctxt = G.consume ctxt update_cost in
          ({map = M.add key new_val map; size}, ctxt)
      | Some _, None ->
          (* Consume gas for removing from the map *)
          let+ ctxt = G.consume ctxt update_cost in
          ({map = M.remove key map; size = size - 1}, ctxt)
      | None, Some new_val ->
          (* Consume gas for adding to the map *)
          let+ ctxt = G.consume ctxt update_cost in
          ({map = M.add key new_val map; size = size + 1}, ctxt)
      | None, None -> return ({map; size}, ctxt)

    let to_list ctxt {map; size} =
      let open Result_syntax in
      let+ ctxt = G.consume ctxt (Carbonated_map_costs.fold_cost ~size) in
      (M.bindings map, ctxt)

    let add ctxt ~merge_overlap key value {map; size} =
      let open Result_syntax in
      (* Consume gas for looking up the element *)
      let* ctxt = G.consume ctxt (find_cost ~key ~size) in
      (* Consume gas for adding the element *)
      let* ctxt = G.consume ctxt (update_cost ~key ~size) in
      match M.find key map with
      | Some old_val ->
          (* Invoking [merge_overlap] must also account for gas *)
          let+ new_value, ctxt = merge_overlap ctxt old_val value in
          ({map = M.add key new_value map; size}, ctxt)
      | None -> Ok ({map = M.add key value map; size = size + 1}, ctxt)

    let add_key_values_to_map ctxt ~merge_overlap map key_values =
      let accum (map, ctxt) (key, value) =
        add ctxt ~merge_overlap key value map
      in
      (* Gas is paid at each step of the fold. *)
      List.fold_left_e accum (map, ctxt) key_values

    let of_list ctxt ~merge_overlap =
      add_key_values_to_map ctxt ~merge_overlap empty

    let merge ctxt ~merge_overlap map1 {map; size} =
      let open Result_syntax in
      (* To be on the safe side, pay an upfront gas cost for traversing the
         map. Each step of the fold is accounted for separately.
      *)
      let* ctxt = G.consume ctxt (Carbonated_map_costs.fold_cost ~size) in
      M.fold_e
        (fun key value (map, ctxt) -> add ctxt ~merge_overlap key value map)
        map
        (map1, ctxt)

    let fold_e ctxt f empty {map; size} =
      let open Result_syntax in
      let* ctxt = G.consume ctxt (Carbonated_map_costs.fold_cost ~size) in
      M.fold_e
        (fun key value (acc, ctxt) ->
          (* Invoking [f] must also account for gas. *)
          f ctxt acc key value)
        map
        (empty, ctxt)

    let fold_es ctxt f empty {map; size} =
      let open Lwt_result_syntax in
      let*? ctxt = G.consume ctxt (Carbonated_map_costs.fold_cost ~size) in
      M.fold_es
        (fun key value (acc, ctxt) ->
          (* Invoking [f] must also account for gas. *)
          f ctxt acc key value)
        map
        (empty, ctxt)

    let map_e ctxt f {map; size} =
      let open Result_syntax in
      (* We cannot use the standard map function because [f] also meters the gas
         cost at each invocation. *)
      let+ map, ctxt =
        fold_e
          ctxt
          (fun ctxt map key value ->
            (* Invoking [f] must also account for gas. *)
            let* value, ctxt = f ctxt key value in
            (* Consume gas for adding the element. *)
            let+ ctxt = G.consume ctxt (update_cost ~key ~size) in
            (M.add key value map, ctxt))
          M.empty
          {map; size}
      in
      ({map; size}, ctxt)
  end
end

module Make (G : GAS) (C : COMPARABLE) :
  S with type key = C.t and type context = G.context = struct
  module M = Make_builder (C)

  type 'a t = 'a M.t

  include M.Make (G)
end
