(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* The head of the list is the root of the namespace *)
type t = string list

type cons = string -> t

let sep = '/'

let root_name = "."

let empty = []

let is_empty = List.is_empty

let is_singleton = function [_] -> true | _ -> false

let to_string t =
  match t with [] -> root_name | _ -> String.concat (Char.escaped sep) t

let basename = function [] -> root_name | t -> Stdlib.List.(rev t |> hd)

let pp fmt t = Format.pp_print_string fmt (to_string t)

let pp_short fmt t = Format.pp_print_string fmt (basename t)

let equal = List.equal String.equal

let compare = List.compare String.compare

let of_string s =
  if String.equal s root_name then [] else String.split_on_char sep s

let append : t -> t -> t = List.append

let ( @ ) = append

let cons (l : t) (a : string) : t =
  l
  @
  match of_string a with
  | [_] -> [a]
  | _ ->
      Format.eprintf "Namespace.cons error: string contains %c: %s@." sep a ;
      exit 1

let encoding = Data_encoding.(list string)

let to_list l = root_name :: l

let name_match (pattern : t) (name : t) =
  let l, leftovers = List.combine_with_leftovers pattern name in
  match leftovers with
  | None | Some (Right _) -> List.for_all (fun (a, b) -> String.equal a b) l
  | _ -> false

module Hashtbl = Hashtbl.MakeSeeded (struct
  type nonrec t = t

  let equal = equal

  let hash = Hashtbl.seeded_hash
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

let root = cons []

let make ns str = cons (ns str)
