(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Micheline

let no_comment : Micheline_printer.location = {comment = None}

let mismatch ~pp expected =
  Micheline_printer.{comment = Some (Format.asprintf "%a" pp expected)}

let repr_node fmt = function
  | Int (_, i) -> Format.fprintf fmt "%d" (Z.to_int i)
  | String (_, s) -> Format.fprintf fmt "\"%s\"" s
  | Bytes (_, _) -> Format.fprintf fmt "Bytes"
  | Prim (_, prim, _, _) -> Format.fprintf fmt "Prim \"%s\"" prim
  | Seq (_, _) -> Format.fprintf fmt "a Seq"

let added = Micheline_printer.{comment = Some "+"}

let removed = Micheline_printer.{comment = Some "-"}

let unexpected_structure_msg exp =
  Micheline_printer.{comment = Some (Format.asprintf "%a" repr_node exp)}

let pp_print_bytes fmt bytes =
  Format.pp_print_string fmt (Bytes.to_string bytes)

let rec replace_location :
          'a.
          Micheline_printer.location ->
          ('a, string) node ->
          Micheline_printer.node =
 fun loc -> function
  | Int (_, i) -> Int (loc, i)
  | String (_, s) -> String (loc, s)
  | Bytes (_, b) -> Bytes (loc, b)
  | Prim (_, p, args, annots) ->
      Prim (loc, p, List.map (replace_location no_comment) args, annots)
  | Seq (_, es) -> Seq (loc, List.map (replace_location no_comment) es)

let rec diff_loop prev current =
  match (prev, current) with
  | (Int (_, a), Int (_, e)) ->
      if Z.equal e a then (false, Int (no_comment, a))
      else (true, Int (mismatch ~pp:Z.pp_print e, a))
  | (String (_, a), String (_, e)) ->
      if String.equal e a then (false, String (no_comment, a))
      else (true, String (mismatch ~pp:Format.pp_print_string e, a))
  | (Bytes (_, a), Bytes (_, e)) ->
      if Bytes.equal e a then (false, Bytes (no_comment, a))
      else (true, Bytes (mismatch ~pp:pp_print_bytes e, a))
  | (Prim (_, ap, aargs, annots), Prim (_, ep, eargs, _)) ->
      let subresults = diff_args aargs eargs in
      let prim_eq = String.equal ep ap in
      let comment =
        if prim_eq then no_comment else mismatch ~pp:Format.pp_print_string ep
      in
      ( (not prim_eq) || List.exists fst subresults,
        Prim (comment, ap, List.map snd subresults, annots) )
  | (Seq (_, aexprs), Seq (_, eexprs)) ->
      let subresults = diff_args aexprs eexprs in
      (List.exists fst subresults, Seq (no_comment, List.map snd subresults))
  | (act, exp) -> (true, replace_location (unexpected_structure_msg exp) act)

and diff_args actual expected =
  match (actual, expected) with
  | ([], []) -> []
  | (ac :: acs, ex :: exs) -> diff_loop ac ex :: diff_args acs exs
  | ([], exs) -> List.map (fun e -> (true, replace_location added e)) exs
  | (acs, []) -> List.map (fun e -> (true, replace_location removed e)) acs

let diff ~prev ~current () =
  match diff_loop prev current with (true, d) -> Some d | (false, _) -> None
