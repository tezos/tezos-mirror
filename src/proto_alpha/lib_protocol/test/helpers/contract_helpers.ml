(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Initializes 2 addresses to do only operations plus one that will be
    used to bake. *)
let init () =
  Context.init 3 >|=? fun (b, contracts) ->
  let (src0, src1, src2) =
    match contracts with
    | src0 :: src1 :: src2 :: _ -> (src0, src1, src2)
    | _ -> assert false
  in
  let baker =
    match Alpha_context.Contract.is_implicit src0 with
    | Some v -> v
    | None -> assert false
  in
  (b, baker, src1, src2)

(** Parses a Michelson contract from string. *)
let toplevel_from_string str =
  let (ast, errs) = Michelson_v1_parser.parse_toplevel ~check:true str in
  match errs with [] -> ast.expanded | _ -> Stdlib.failwith "parse toplevel"

(** Parses a Michelson expression from string, useful for call parameters. *)
let expression_from_string str =
  let (ast, errs) = Michelson_v1_parser.parse_expression ~check:true str in
  match errs with [] -> ast.expanded | _ -> Stdlib.failwith "parse expression"

(** Returns a block in which the contract is originated. *)
let originate_contract file storage src b baker =
  let load_file f =
    let ic = open_in f in
    let res = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    res
  in
  let contract_string = load_file file in
  let code = toplevel_from_string contract_string in
  let storage = expression_from_string storage in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  Op.origination (B b) src ~fee:(Test_tez.Tez.of_int 10) ~script
  >>=? fun (operation, dst) ->
  Incremental.begin_construction ~policy:Block.(By_account baker) b
  >>=? fun incr ->
  Incremental.add_operation incr operation >>=? fun incr ->
  Incremental.finalize_block incr >|=? fun b -> (dst, b)
