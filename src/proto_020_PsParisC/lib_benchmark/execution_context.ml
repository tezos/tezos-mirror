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

open Protocol

type context = Alpha_context.context * Script_interpreter.step_constants

let initial_balance = 4_000_000_000_000L

let context_init_memory ?dal ~rng_state () =
  let open Lwt_result_wrap_syntax in
  let dal_enable = Option.is_some dal in
  let* block, accounts =
    Context.init_n
      ~rng_state
      ~dal_enable
      ?dal
      ~bootstrap_balances:
        [
          initial_balance;
          initial_balance;
          initial_balance;
          initial_balance;
          initial_balance;
        ]
      5
      ()
  in
  match accounts with
  | [bs1; bs2; bs3; bs4; bs5] ->
      return (`Mem_block (block, (bs1, bs2, bs3, bs4, bs5)))
  | _ -> assert false

let context_init ?dal ~rng_state () = context_init_memory ?dal ~rng_state ()

let make ?dal ~rng_state () =
  let open Lwt_result_wrap_syntax in
  let* context = context_init_memory ?dal ~rng_state () in
  let amount = Alpha_context.Tez.one in
  let chain_id = Tezos_crypto.Hashed.Chain_id.zero in
  let now = Script_timestamp.of_zint Z.zero in
  let level = Script_int.zero_n in
  let open Script_interpreter in
  let* block, step_constants =
    match context with
    | `Mem_block (block, (bs1, _, _, _, _)) ->
        let sender = Alpha_context.Destination.Contract bs1 in
        let payer = Contract_helpers.default_payer in
        let self = Contract_helpers.default_self in
        let step_constants =
          {
            sender;
            payer;
            self;
            amount;
            balance = Alpha_context.Tez.of_mutez_exn initial_balance;
            chain_id;
            now;
            level;
          }
        in
        return (block, step_constants)
  in
  let* csts = Context.get_constants (B block) in
  let minimal_block_delay =
    Protocol.Alpha_context.Period.to_seconds csts.parametric.minimal_block_delay
  in
  let* vs =
    Incremental.begin_construction
      ~timestamp:
        (Time.Protocol.add block.header.shell.timestamp minimal_block_delay)
      block
  in
  let ctxt = Incremental.alpha_ctxt vs in
  let ctxt =
    (* Required for eg Create_contract *)
    Protocol.Alpha_context.Origination_nonce.init
      ctxt
      Tezos_crypto.Hashed.Operation_hash.zero
  in
  return (ctxt, step_constants)
