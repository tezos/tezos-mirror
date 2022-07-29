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

open Tezos_micheline
open Micheline

let no_comment : Micheline_printer.location = {comment = None}

let nullary prim = Prim (no_comment, prim, [], [])

let unary prim arg = Prim (no_comment, prim, [arg], [])

let binary prim l r = Prim (no_comment, prim, [l; r], [])

let unit = nullary "Unit"

let none = nullary "None"

let some = unary "Some"

let num n = Int (no_comment, Z.of_int n)

let tez t = Int (no_comment, Z.of_int64 @@ Tez.mutez_int64 t)

let str s = String (no_comment, s)

let tuple args = Prim (no_comment, "Pair", args, [])

let left = unary "Left"

let right = unary "Right"

let optional constr = function None -> none | Some x -> some (constr x)

let rec pairs = function
  | [] -> Prim (no_comment, "Pair", [], [])
  | [x] -> x
  | x :: xs -> Prim (no_comment, "Pair", [x; pairs xs], [])

let pair = binary "Pair"

let timestamp t = String (no_comment, Ptime.to_rfc3339 ~tz_offset_s:0 t)

let list xs = Seq (no_comment, xs)

module Types = struct
  let unit = nullary "unit"

  let nat = nullary "nat"

  let key = nullary "key"

  let key_hash = nullary "key_hash"

  let signature = nullary "signature"

  let mutez = nullary "mutez"

  let address = nullary "address"

  let contract = unary "contract"

  let option = unary "option"

  let list = unary "list"

  let pair = binary "pair"

  let tuple elems = Prim (no_comment, "Pair", elems, [])

  let either = binary "or" (* "or" is unfortunately a keyword *)
end

let to_string expr = Format.asprintf "%a" Micheline_printer.print_expr expr

let encoding =
  Micheline_encoding.canonical_encoding
    ~variant:"tezt-michelson-repr"
    Data_encoding.string

let parse code =
  let tokens, errors = Micheline_parser.tokenize code in
  let* () =
    if List.compare_length_with errors 0 >= 0 then Lwt.return ()
    else Test.fail "Couldn't tokenize Micheline!"
  in
  let expr, errors = Micheline_parser.parse_expression tokens in
  if List.compare_length_with errors 0 >= 0 then Lwt.return expr
  else Test.fail "Couldn't parse Micheline!"
