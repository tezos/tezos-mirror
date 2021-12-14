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
open Alpha_context
open Error_monad_operators

(** Initializes 2 addresses to do only operations plus one that will be
    used to bake. *)
let init () =
  Context.init ~consensus_threshold:0 3 >|=? fun (b, contracts) ->
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

(** Returns a block in which the contract is originated. *)
let originate_contract file storage src b baker =
  let load_file f =
    let ic = open_in f in
    let res = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    res
  in
  let contract_string = load_file file in
  let code = Expr.toplevel_from_string contract_string in
  let storage = Expr.from_string storage in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  Op.origination (B b) src ~fee:(Test_tez.of_int 10) ~script
  >>=? fun (operation, dst) ->
  Incremental.begin_construction ~policy:Block.(By_account baker) b
  >>=? fun incr ->
  Incremental.add_operation incr operation >>=? fun incr ->
  Incremental.finalize_block incr >|=? fun b -> (dst, b)

let default_source = Contract.implicit_contract Signature.Public_key_hash.zero

let default_step_constants =
  Script_interpreter.
    {
      source = default_source;
      payer = default_source;
      self = default_source;
      amount = Tez.zero;
      chain_id = Chain_id.zero;
      now = Script_timestamp.of_zint Z.zero;
      level = Script_int.zero_n;
    }

(** Helper function that parses and types a script, its initial storage and
   parameters from strings. It then executes the typed script with the storage
   and parameter and returns the result. *)
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let parameter_expr = Expr.from_string parameter in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Script_interpreter.execute
    ctx
    Readable
    step_constants
    ~script
    ~cached_script:None
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false
  >>=?? fun res -> return res
