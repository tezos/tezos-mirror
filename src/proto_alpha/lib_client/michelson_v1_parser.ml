(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Tezos_micheline
open Micheline_parser
open Micheline

type 'prim parser_result = {
  source : string;
  unexpanded : string canonical;
  expanded : 'prim canonical;
  expansion_table : (int * (Micheline_parser.location * int list)) list;
  unexpansion_table : (int * int) list;
}

type parsed = Michelson_v1_primitives.prim parser_result

let compare_parsed = Stdlib.compare

(* Unexpanded toplevel expression should be a sequence *)
let expand_all source ast errors =
  let unexpanded, loc_table = extract_locations ast in
  let expanded, expansion_errors =
    Michelson_v1_macros.expand_rec (root unexpanded)
  in
  let expanded, unexpansion_table = extract_locations expanded in
  let expansion_table =
    let sorted =
      List.sort (fun (_, a) (_, b) -> Stdlib.compare a b) unexpansion_table
    in
    let grouped =
      let rec group = function
        | acc, [] -> acc
        | [], (u, e) :: r -> group ([(e, [u])], r)
        | ((pe, us) :: racc as acc), (u, e) :: r ->
            if e = pe then group ((e, u :: us) :: racc, r)
            else group ((e, [u]) :: acc, r)
      in
      group ([], sorted)
    in
    match
      List.map2
        ~when_different_lengths:()
        (fun (l, ploc) (l', elocs) ->
          assert (l = l') ;
          (l, (ploc, elocs)))
        (List.sort Stdlib.compare loc_table)
        (List.sort Stdlib.compare grouped)
    with
    | Ok v -> v
    | Error () -> invalid_arg "Michelson_v1_parser.expand_all"
  in
  ( {source; unexpanded; expanded; expansion_table; unexpansion_table},
    errors @ expansion_errors )

let expand_all_and_recognize_prims source ast errors =
  let parsed, errors = expand_all source ast errors in
  match Michelson_v1_primitives.prims_of_strings parsed.expanded with
  | Ok expanded -> ({parsed with expanded}, errors)
  | Error errs ->
      let errs = Environment.wrap_tztrace errs in
      let expanded = Micheline.strip_locations (Seq ((), [])) in
      ({parsed with expanded}, errors @ errs)

type micheline_parser = Toplevel | Expression

type 'prim prim_type =
  | Michelson_prim : Michelson_v1_primitives.prim prim_type
  | String : string prim_type

let parse (type prim) micheline_parser (prim_type : prim prim_type) ?check
    source =
  let tokens, lexing_errors = Micheline_parser.tokenize source in
  let ast, parsing_errors =
    match micheline_parser with
    | Toplevel ->
        let asts, parsing_errors =
          Micheline_parser.parse_toplevel ?check tokens
        in
        let start = min_point asts and stop = max_point asts in
        (Seq ({start; stop}, asts), parsing_errors)
    | Expression -> Micheline_parser.parse_expression ?check tokens
  in
  let expand :
      string ->
      (location, string) Micheline.node ->
      error trace ->
      prim parser_result Micheline_parser.parsing_result =
    match prim_type with
    | Michelson_prim -> expand_all_and_recognize_prims
    | String -> expand_all
  in
  expand source ast (lexing_errors @ parsing_errors)

let parse_toplevel = parse Toplevel Michelson_prim

let expand_toplevel = parse Toplevel String

let parse_expression = parse Expression Michelson_prim

let expand_expression = parse Expression String

let expand_all_and_recognize_prims ~source ~original =
  expand_all_and_recognize_prims source original []
