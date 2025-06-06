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

let rng_state = Random.State.make [|42; 987897; 54120|]

let print_script_expr fmtr (expr : Protocol.Script_repr.expr) =
  Micheline_printer.print_expr
    fmtr
    (Micheline_printer.printable
       Protocol.Michelson_v1_primitives.string_of_prim
       expr)

let print_script_expr_list fmtr (exprs : Protocol.Script_repr.expr list) =
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr " :: ")
    print_script_expr
    fmtr
    exprs

let typecheck_by_tezos =
  let open Lwt_result_wrap_syntax in
  let context_init_memory ~rng_state =
    let* block, _accounts =
      Context.init_n
        ~rng_state
        ~bootstrap_balances:
          [
            4_000_000_000_000L;
            4_000_000_000_000L;
            4_000_000_000_000L;
            4_000_000_000_000L;
            4_000_000_000_000L;
          ]
        5
        ()
    in
    let* csts = Context.get_constants (B block) in
    let minimal_block_delay =
      Protocol.Alpha_context.Period.to_seconds
        csts.parametric.minimal_block_delay
    in
    let* vs =
      Incremental.begin_construction
        ~timestamp:
          (Tezos_base.Time.Protocol.add
             block.header.shell.timestamp
             minimal_block_delay)
        block
    in
    let ctxt = Incremental.alpha_ctxt vs in
    (* Required for eg Create_contract *)
    return
    @@ Protocol.Alpha_context.Origination_nonce.init
         ctxt
         Tezos_crypto.Hashed.Operation_hash.zero
  in
  fun bef node ->
    Stdlib.Result.get_ok
      (Lwt_main.run
         (let* ctxt = context_init_memory ~rng_state in
          let (Protocol.Script_ir_translator.Ex_stack_ty bef) =
            Type_helpers.michelson_type_list_to_ex_stack_ty bef ctxt
          in
          let*@ (_ :
                  _ Protocol.Script_ir_translator.judgement
                  * Protocol.Alpha_context.t) =
            Protocol.Script_ir_translator.parse_instr
              Protocol.Script_tc_context.data
              ctxt
              ~elab_conf:
                (Protocol.Script_ir_translator_config.make ~legacy:false ())
              (Micheline.root node)
              bef
          in
          return_unit))
