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

module type SizedSet = sig
  include TzLwtreslib.Set.S

  type set

  val to_set : t -> set

  val of_set : set -> t
end

module MakeSizedSet (S : TzLwtreslib.Set.S) = struct
  type elt = S.elt

  type t = {cardinal : int; set : S.t}

  let cardinal t = t.cardinal

  let to_set t = t.set

  let of_set set = {cardinal = S.cardinal set; set}

  let empty = {cardinal = 0; set = S.empty}

  let is_empty t = t.cardinal = 0

  let mem x t = S.mem x t.set

  let add x t =
    let nset = S.add x t.set in
    if nset == t.set then t else {cardinal = t.cardinal + 1; set = nset}

  let singleton e = {cardinal = 1; set = S.singleton e}

  let remove x t =
    let nset = S.remove x t.set in
    if nset == t.set then t else {cardinal = t.cardinal - 1; set = nset}

  (** This function is less efficient than {!TzLwtreslib.Set.S.union} which should be considered instead of this function, especially in case it's called several times.
      {!to_set} and {!of_set} can be used for this purpose.*)
  let union t1 t2 = S.union t1.set t2.set |> of_set

  (** This function is less efficient than {!TzLwtreslib.Set.S.inter} which should be considered instead of this function, especially in case it's called several times.
      {!to_set} and {!of_set} can be used for this purpose.*)
  let inter t1 t2 = S.inter t1.set t2.set |> of_set

  let disjoint t1 t2 = S.disjoint t1.set t2.set

  let diff t1 t2 = S.diff t1.set t2.set |> of_set

  let compare t1 t2 = S.compare t1.set t2.set

  let equal t1 t2 = S.equal t1.set t2.set

  let subset t1 t2 = S.subset t1.set t2.set

  let iter f t = S.iter f t.set

  let iter_e f t = S.iter_e f t.set

  let iter_s f t = S.iter_s f t.set

  let iter_p f t = S.iter_p f t.set

  let iter_es f t = S.iter_es f t.set

  let iter_ep f t = S.iter_ep f t.set

  let map f t = {cardinal = t.cardinal; set = S.map f t.set}

  let fold f t a = S.fold f t.set a

  let fold_e f t a = S.fold_e f t.set a

  let fold_s f t a = S.fold_s f t.set a

  let fold_es f t a = S.fold_es f t.set a

  let for_all f t = S.for_all f t.set

  let exists f t = S.exists f t.set

  let filter f t = S.fold (fun x r -> if f x then add x r else r) t.set empty

  let filter_map f t =
    S.fold
      (fun x r -> match f x with Some v -> add v r | None -> r)
      t.set
      empty

  let partition f t =
    let (s1, s2) = S.partition f t.set in
    let n = S.cardinal s1 in
    ({cardinal = n; set = s1}, {cardinal = t.cardinal - n; set = s2})

  let elements t = S.elements t.set

  let min_elt t = S.min_elt t.set

  let min_elt_opt t = S.min_elt_opt t.set

  let max_elt t = S.max_elt t.set

  let max_elt_opt t = S.max_elt_opt t.set

  let choose t = S.choose t.set

  let choose_opt t = S.choose_opt t.set

  let split e t =
    let (l, b, r) = S.split e t.set in
    let n = S.cardinal l in
    if b then
      ({cardinal = n; set = l}, b, {cardinal = t.cardinal - n - 1; set = r})
    else ({cardinal = n; set = l}, b, {cardinal = t.cardinal - n; set = r})

  let find e t = S.find e t.set

  let find_opt e t = S.find_opt e t.set

  let find_first e t = S.find_first e t.set

  let find_first_opt e t = S.find_first_opt e t.set

  let find_last e t = S.find_last e t.set

  let find_last_opt e t = S.find_last_opt e t.set

  let of_list el = {cardinal = List.length el; set = S.of_list el}

  let to_seq_from e t = S.to_seq_from e t.set

  let to_seq t = S.to_seq t.set

  let to_rev_seq t = S.to_seq t.set

  let add_seq seq t = S.add_seq seq t.set |> of_set

  let of_seq seq = S.of_seq seq |> of_set
end

module type SizedMap = sig
  include TzLwtreslib.Map.S

  type 'a map

  val to_map : 'a t -> 'a map

  val of_map : 'a map -> 'a t
end

module MakeSizedMap (M : TzLwtreslib.Map.S) = struct
  type key = M.key

  type 'a t = {cardinal : int; map : 'a M.t}

  let cardinal t = t.cardinal

  let to_map t = t.map

  let of_map map = {cardinal = M.cardinal map; map}

  let empty = {cardinal = 0; map = M.empty}

  let is_empty t = t.cardinal = 0

  let mem x t = M.mem x t.map

  let update key f t =
    let x = M.find_opt key t.map in
    match x with
    | None -> (
        match f None with
        | Some x -> {cardinal = t.cardinal + 1; map = M.add key x t.map}
        | None -> t)
    | Some x -> (
        match f (Some x) with
        | Some x -> {cardinal = t.cardinal; map = M.add key x t.map}
        | None -> {cardinal = t.cardinal - 1; map = M.remove key t.map})

  let add key binding t = update key (fun _ -> Some binding) t

  let singleton key binding = {cardinal = 1; map = M.singleton key binding}

  let remove key t =
    let nt = M.remove key t.map in
    if nt == t.map then t else {cardinal = t.cardinal - 1; map = nt}

  (** This function is less efficient than {!TzLwtreslib.Map.S.merge} which should be considered instead of this function, especially in case it's called several times.
      {!to_map} and {!of_map} can be used for this purpose.*)
  let merge f t1 t2 = M.merge f t1.map t2.map |> of_map

  (** This function is less efficient than {!TzLwtreslib.Map.S.union} which should be considered instead of this function, especially in case it's called several times.
      {!to_map} and {!of_map} can be used for this purpose.*)
  let union f t1 t2 = M.union f t1.map t2.map |> of_map

  let compare f t1 t2 = M.compare f t1.map t2.map

  let equal f t1 t2 = M.equal f t1.map t2.map

  let iter f t = M.iter f t.map

  let iter_e f t = M.iter_e f t.map

  let iter_s f t = M.iter_s f t.map

  let iter_p f t = M.iter_p f t.map

  let iter_es f t = M.iter_es f t.map

  let iter_ep f t = M.iter_ep f t.map

  let fold f t a = M.fold f t.map a

  let fold_e f t a = M.fold_e f t.map a

  let fold_s f t a = M.fold_s f t.map a

  let fold_es f t a = M.fold_es f t.map a

  let for_all f t = M.for_all f t.map

  let exists f t = M.exists f t.map

  let filter f t =
    M.fold (fun k b r -> if f k b then add k b r else r) t.map empty

  let filter_map f t =
    M.fold
      (fun k b r -> match f k b with Some v -> add k v r | None -> r)
      t.map
      empty

  let partition f t =
    let (m1, m2) = M.partition f t.map in
    let n = M.cardinal m1 in
    ({cardinal = n; map = m1}, {cardinal = t.cardinal - n; map = m2})

  let bindings t = M.bindings t.map

  let min_binding t = M.min_binding t.map

  let min_binding_opt t = M.min_binding_opt t.map

  let max_binding t = M.max_binding t.map

  let max_binding_opt t = M.max_binding_opt t.map

  let choose t = M.choose t.map

  let choose_opt t = M.choose_opt t.map

  let split key t =
    let (l, data, r) = M.split key t.map in
    let n = M.cardinal l in
    match data with
    | Some _ ->
        ({cardinal = n; map = l}, data, {cardinal = t.cardinal - n - 1; map = r})
    | None ->
        ({cardinal = n; map = l}, data, {cardinal = t.cardinal - n; map = r})

  let find key t = M.find key t.map

  let find_opt key t = M.find_opt key t.map

  let find_first key t = M.find_first key t.map

  let find_first_opt key t = M.find_first_opt key t.map

  let find_last key t = M.find_last key t.map

  let find_last_opt key t = M.find_last_opt key t.map

  let map f t = {cardinal = t.cardinal; map = M.map f t.map}

  let mapi f t = {cardinal = t.cardinal; map = M.mapi f t.map}

  let to_seq t = M.to_seq t.map

  let to_rev_seq t = M.to_rev_seq t.map

  let to_seq_from e t = M.to_seq_from e t.map

  let add_seq seq t = M.add_seq seq t.map |> of_map

  let of_seq seq = M.of_seq seq |> of_map
end
