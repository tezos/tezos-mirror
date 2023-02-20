(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Michelson types. *)

module Base = struct
  type comparable_tag = Comparable | Maybe_not_comparable

  type t = t_node Hashcons.hash_consed

  and t_node =
    | Unit_t
    | Var_t of int
    | Int_t
    | Nat_t
    | Bool_t
    | String_t
    | Bytes_t
    | Key_hash_t
    | Timestamp_t
    | Mutez_t
    | Key_t
    | Option_t of t
    | Pair_t of t * t
    | Or_t of t * t
    | List_t of t
    | Set_t of t
    | Map_t of t * t
    | Lambda_t of t * t

  module Hashed = struct
    type t = t_node

    let equal (t1 : t) (t2 : t) =
      match (t1, t2) with
      | Var_t v1, Var_t v2 -> v1 = v2
      | Unit_t, Unit_t
      | Int_t, Int_t
      | Nat_t, Nat_t
      | Bool_t, Bool_t
      | String_t, String_t
      | Bytes_t, Bytes_t
      | Key_hash_t, Key_hash_t
      | Timestamp_t, Timestamp_t
      | Mutez_t, Mutez_t
      | Key_t, Key_t ->
          true
      | Option_t ty1, Option_t ty2 -> ty1.tag = ty2.tag
      | Pair_t (l1, r1), Pair_t (l2, r2) -> l1.tag = l2.tag && r1.tag = r2.tag
      | Or_t (l1, r1), Or_t (l2, r2) -> l1.tag = l2.tag && r1.tag = r2.tag
      | List_t ty1, List_t ty2 -> ty1.tag = ty2.tag
      | Set_t ty1, Set_t ty2 -> ty1.tag = ty2.tag
      | Map_t (kty1, vty1), Map_t (kty2, vty2) ->
          kty1.tag = kty2.tag && vty1.tag = vty2.tag
      | Lambda_t (dom1, range1), Lambda_t (dom2, range2) ->
          dom1.tag = dom2.tag && range1.tag = range2.tag
      | _ -> false

    let hash (t : t) = Hashtbl.hash t
  end

  module Table = Hashcons.Make (Hashed)

  let table = Table.create 101

  let rec pp fmtr x =
    match x.Hashcons.node with
    | Unit_t -> Format.pp_print_string fmtr "unit"
    | Var_t v -> Format.fprintf fmtr "%d" v
    | Int_t -> Format.pp_print_string fmtr "int"
    | Nat_t -> Format.pp_print_string fmtr "nat"
    | Bool_t -> Format.pp_print_string fmtr "bool"
    | String_t -> Format.pp_print_string fmtr "string"
    | Bytes_t -> Format.pp_print_string fmtr "bytes"
    | Key_hash_t -> Format.pp_print_string fmtr "key_hash"
    | Timestamp_t -> Format.pp_print_string fmtr "timestamp"
    | Mutez_t -> Format.pp_print_string fmtr "mutez"
    | Key_t -> Format.pp_print_string fmtr "key"
    | Option_t ty -> Format.fprintf fmtr "(option %a)" pp ty
    | List_t ty -> Format.fprintf fmtr "(list %a)" pp ty
    | Pair_t (lty, rty) -> Format.fprintf fmtr "(pair %a %a)" pp lty pp rty
    | Or_t (lty, rty) -> Format.fprintf fmtr "(or %a %a)" pp lty pp rty
    | Set_t ty -> Format.fprintf fmtr "(set %a)" pp ty
    | Map_t (kty, vty) -> Format.fprintf fmtr "(map %a %a)" pp kty pp vty
    | Lambda_t (dom, range) ->
        Format.fprintf fmtr "(lambda %a %a)" pp dom pp range

  let rec vars x acc =
    match x.Hashcons.node with
    | Unit_t | Int_t | Nat_t | Bool_t | String_t | Bytes_t | Key_hash_t
    | Timestamp_t | Mutez_t | Key_t ->
        acc
    | Var_t v -> v :: acc
    | Option_t ty | List_t ty | Set_t ty -> vars ty acc
    | Pair_t (lty, rty) | Or_t (lty, rty) -> vars lty (vars rty acc)
    | Map_t (kty, vty) -> vars kty (vars vty acc)
    | Lambda_t (dom, range) -> vars dom (vars range acc)

  let vars x = vars x []
end

module Stack = struct
  type t = t_node Hashcons.hash_consed

  and t_node = Empty_t | Stack_var_t of int | Item_t of Base.t * t

  module Hashed = struct
    type t = t_node

    let equal (t1 : t) (t2 : t) =
      match (t1, t2) with
      | Empty_t, Empty_t -> true
      | Stack_var_t v1, Stack_var_t v2 -> v1 = v2
      | Item_t (h1, tl1), Item_t (h2, tl2) -> h1 == h2 && tl1 == tl2
      | _ -> false

    let hash (t : t) = Hashtbl.hash t
  end

  module Table = Hashcons.Make (Hashed)

  let table = Table.create 101

  let rec pp fmtr x =
    match x.Hashcons.node with
    | Empty_t -> Format.pp_print_string fmtr "[]"
    | Stack_var_t v -> Format.fprintf fmtr "<%d>" v
    | Item_t (head, tail) -> Format.fprintf fmtr "%a :: %a" Base.pp head pp tail

  let rec vars x =
    match x.Hashcons.node with
    | Empty_t -> None
    | Stack_var_t v -> Some v
    | Item_t (_head, tail) -> vars tail
end

let unit = Base.Table.hashcons Base.table Unit_t

let var x = Base.Table.hashcons Base.table (Var_t x)

let int = Base.Table.hashcons Base.table Int_t

let nat = Base.Table.hashcons Base.table Nat_t

let bool = Base.Table.hashcons Base.table Bool_t

let string = Base.Table.hashcons Base.table String_t

let bytes = Base.Table.hashcons Base.table Bytes_t

let key_hash = Base.Table.hashcons Base.table Key_hash_t

let timestamp = Base.Table.hashcons Base.table Timestamp_t

let mutez = Base.Table.hashcons Base.table Mutez_t

let key = Base.Table.hashcons Base.table Key_t

let option ty = Base.Table.hashcons Base.table (Option_t ty)

let pair lty rty = Base.Table.hashcons Base.table (Pair_t (lty, rty))

let or_ lty rty = Base.Table.hashcons Base.table (Or_t (lty, rty))

let list ty = Base.Table.hashcons Base.table (List_t ty)

let set ty = Base.Table.hashcons Base.table (Set_t ty)

let map kty vty = Base.Table.hashcons Base.table (Map_t (kty, vty))

let lambda dom range = Base.Table.hashcons Base.table (Lambda_t (dom, range))

(* Stack smart constructors *)
let empty = Stack.Table.hashcons Stack.table Empty_t

let stack_var x = Stack.Table.hashcons Stack.table (Stack_var_t x)

let item head tail = Stack.Table.hashcons Stack.table (Item_t (head, tail))
