(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
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

open Alpha_context
open Script_typed_ir

let list_empty : 'a Script_typed_ir.boxed_list =
  let open Script_typed_ir in
  {elements = []; length = 0}

let list_cons :
    'a -> 'a Script_typed_ir.boxed_list -> 'a Script_typed_ir.boxed_list =
 fun elt l ->
  {length = 1 + l.length; elements = elt :: l.elements}

let compare_address (x, ex) (y, ey) =
  let lres = Contract.compare x y in
  if Compare.Int.(lres = 0) then Compare.String.compare ex ey else lres

type compare_comparable_cont =
  | Compare_comparable :
      'a comparable_ty * 'a * 'a * compare_comparable_cont
      -> compare_comparable_cont
  | Compare_comparable_return : compare_comparable_cont

let compare_comparable : type a. a comparable_ty -> a -> a -> int =
  let rec compare_comparable :
      type a. a comparable_ty -> compare_comparable_cont -> a -> a -> int =
   fun kind k x y ->
    match (kind, x, y) with
    | (Unit_key _, (), ()) -> (apply [@tailcall]) 0 k
    | (Never_key _, _, _) -> .
    | (Signature_key _, x, y) -> (apply [@tailcall]) (Signature.compare x y) k
    | (String_key _, x, y) -> (apply [@tailcall]) (Script_string.compare x y) k
    | (Bool_key _, x, y) -> (apply [@tailcall]) (Compare.Bool.compare x y) k
    | (Mutez_key _, x, y) -> (apply [@tailcall]) (Tez.compare x y) k
    | (Key_hash_key _, x, y) ->
        (apply [@tailcall]) (Signature.Public_key_hash.compare x y) k
    | (Key_key _, x, y) ->
        (apply [@tailcall]) (Signature.Public_key.compare x y) k
    | (Int_key _, x, y) -> (apply [@tailcall]) (Script_int.compare x y) k
    | (Nat_key _, x, y) -> (apply [@tailcall]) (Script_int.compare x y) k
    | (Timestamp_key _, x, y) ->
        (apply [@tailcall]) (Script_timestamp.compare x y) k
    | (Address_key _, x, y) -> (apply [@tailcall]) (compare_address x y) k
    | (Bytes_key _, x, y) -> (apply [@tailcall]) (Compare.Bytes.compare x y) k
    | (Chain_id_key _, x, y) -> (apply [@tailcall]) (Chain_id.compare x y) k
    | (Pair_key ((tl, _), (tr, _), _), (lx, rx), (ly, ry)) ->
        (compare_comparable [@tailcall])
          tl
          (Compare_comparable (tr, rx, ry, k))
          lx
          ly
    | (Union_key ((tl, _), _, _), L x, L y) ->
        (compare_comparable [@tailcall]) tl k x y
    | (Union_key _, L _, R _) -> -1
    | (Union_key _, R _, L _) -> 1
    | (Union_key (_, (tr, _), _), R x, R y) ->
        (compare_comparable [@tailcall]) tr k x y
    | (Option_key _, None, None) -> 0
    | (Option_key _, None, Some _) -> -1
    | (Option_key _, Some _, None) -> 1
    | (Option_key (t, _), Some x, Some y) ->
        (compare_comparable [@tailcall]) t k x y
  and apply ret k =
    match (ret, k) with
    | (0, Compare_comparable (ty, x, y, k)) ->
        (compare_comparable [@tailcall]) ty k x y
    | (0, Compare_comparable_return) -> 0
    | (ret, _) ->
        (* ret <> 0, we perform an early exit *)
        if Compare.Int.(ret > 0) then 1 else -1
  in
  fun t -> compare_comparable t Compare_comparable_return
  [@@coq_axiom_with_reason "non top-level mutually recursive function"]

let empty_set : type a. a comparable_ty -> a set =
 fun ty ->
  let module OPS = Set.Make (struct
    type t = a

    let compare = compare_comparable ty
  end) in
  (module struct
    type elt = a

    let elt_ty = ty

    module OPS = OPS

    let boxed = OPS.empty

    let size = 0
  end)

let set_update : type a. a -> bool -> a set -> a set =
 fun v b (module Box) ->
  (module struct
    type elt = a

    let elt_ty = Box.elt_ty

    module OPS = Box.OPS

    let boxed =
      if b then Box.OPS.add v Box.boxed else Box.OPS.remove v Box.boxed

    let size =
      let mem = Box.OPS.mem v Box.boxed in
      if mem then if b then Box.size else Box.size - 1
      else if b then Box.size + 1
      else Box.size
  end)

let set_mem : type elt. elt -> elt set -> bool =
 fun v (module Box) -> Box.OPS.mem v Box.boxed

let set_fold : type elt acc. (elt -> acc -> acc) -> elt set -> acc -> acc =
 fun f (module Box) -> Box.OPS.fold f Box.boxed

let set_size : type elt. elt set -> Script_int.n Script_int.num =
 fun (module Box) -> Script_int.(abs (of_int Box.size))

let map_key_ty : type a b. (a, b) map -> a comparable_ty =
 fun (module Box) -> Box.key_ty

let empty_map : type a b. a comparable_ty -> (a, b) map =
 fun ty ->
  let module OPS = Map.Make (struct
    type t = a

    let compare = compare_comparable ty
  end) in
  (module struct
    type key = a

    type value = b

    let key_ty = ty

    module OPS = OPS

    let boxed = (OPS.empty, 0)
  end)

let map_get : type key value. key -> (key, value) map -> value option =
 fun k (module Box) -> Box.OPS.find k (fst Box.boxed)

let map_update : type a b. a -> b option -> (a, b) map -> (a, b) map =
 fun k v (module Box) ->
  (module struct
    type key = a

    type value = b

    let key_ty = Box.key_ty

    module OPS = Box.OPS

    let boxed =
      let (map, size) = Box.boxed in
      let contains = Box.OPS.mem k map in
      match v with
      | Some v -> (Box.OPS.add k v map, size + if contains then 0 else 1)
      | None -> (Box.OPS.remove k map, size - if contains then 1 else 0)
  end)

let map_mem : type key value. key -> (key, value) map -> bool =
 fun k (module Box) -> Box.OPS.mem k (fst Box.boxed)

let map_fold :
    type key value acc.
    (key -> value -> acc -> acc) -> (key, value) map -> acc -> acc =
 fun f (module Box) -> Box.OPS.fold f (fst Box.boxed)

let map_size : type key value. (key, value) map -> Script_int.n Script_int.num =
 fun (module Box) -> Script_int.(abs (of_int (snd Box.boxed)))

