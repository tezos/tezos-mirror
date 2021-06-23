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

open Tezos_error_monad.Error_monad

let rng_state = Random.State.make [|42; 987897; 54120|]

let base_type_to_michelson_type (typ : Type.Base.t) =
  let typ = Mikhailsky.map_var (fun _ -> Mikhailsky.unit_ty) typ in
  Mikhailsky.to_michelson typ

(* Convert a Mikhailsky stack to a list of Micheline-encoded types *)
let rec stack_type_to_michelson_type_list (typ : Type.Stack.t) =
  let node = typ.node in
  match node with
  | Type.Stack.Stack_var_t _ ->
      Stdlib.failwith "stack_type_to_michelson_type_list: bug found"
  | Type.Stack.Empty_t -> []
  | Type.Stack.Item_t (ty, tl) ->
      base_type_to_michelson_type ty :: stack_type_to_michelson_type_list tl

(* Convert a Micheline-encoded type to its internal GADT format. *)
let michelson_type_to_ex_ty (typ : Protocol.Alpha_context.Script.expr)
    (ctxt : Protocol.Alpha_context.t) =
  Protocol.Script_ir_translator.parse_ty
    ctxt
    ~legacy:false
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:false
    ~allow_ticket:false
    (Micheline.root typ)
  |> Protocol.Environment.wrap_tzresult
  |> function
  | Ok t -> t
  | Error errs ->
      Format.eprintf "%a@." pp_print_error errs ;
      raise (Failure "Test_helpers.michelson_type_to_ex_ty: error")

let base_type_to_ex_ty ty =
  michelson_type_to_ex_ty (base_type_to_michelson_type ty)

(* Convert a list of Micheline-encoded Michelson types to the
     internal GADT format. *)
let rec michelson_type_list_to_ex_stack_ty
    (stack_ty : Protocol.Alpha_context.Script.expr list) ctxt =
  let open Protocol.Script_ir_translator in
  match stack_ty with
  | [] -> (Ex_stack_ty Protocol.Script_typed_ir.Bot_t, ctxt)
  | hd :: tl -> (
      let (ex_ty, ctxt) = michelson_type_to_ex_ty hd ctxt in
      match ex_ty with
      | Ex_ty ty -> (
          let (ex_stack_ty, ctxt) =
            michelson_type_list_to_ex_stack_ty tl ctxt
          in
          match ex_stack_ty with
          | Ex_stack_ty tl -> (Ex_stack_ty (Item_t (ty, tl, None)), ctxt)))

module Alpha_test_helpers = Tezos_010_PtGRANAD_test_helpers

let typecheck_by_tezos =
  let open Alpha_test_helpers in
  let context_init_memory ~rng_state =
    Context.init
      ~rng_state
      ~initial_balances:
        [
          4_000_000_000_000L;
          4_000_000_000_000L;
          4_000_000_000_000L;
          4_000_000_000_000L;
          4_000_000_000_000L;
        ]
      5
    >>=? fun (block, _accounts) ->
    Incremental.begin_construction
      ~priority:0
      ~timestamp:(Tezos_base.Time.Protocol.add block.header.shell.timestamp 30L)
      block
    >>=? fun vs ->
    let ctxt = Incremental.alpha_ctxt vs in
    (* Required for eg Create_contract *)
    return
    @@ Protocol.Alpha_context.Contract.init_origination_nonce
         ctxt
         Tezos_crypto.Operation_hash.zero
  in
  fun bef node ->
    Result.get_ok
      (Lwt_main.run
         ( context_init_memory ~rng_state >>=? fun ctxt ->
           let stack = stack_type_to_michelson_type_list bef in
           let (bef, ctxt) = michelson_type_list_to_ex_stack_ty stack ctxt in
           let (Protocol.Script_ir_translator.Ex_stack_ty bef) = bef in
           Protocol.Script_ir_translator.parse_instr
             Protocol.Script_ir_translator.Lambda
             ctxt
             ~legacy:false
             (Micheline.root node)
             bef
           >|= Protocol.Environment.wrap_tzresult
           >>=? fun _ -> return_unit ))
