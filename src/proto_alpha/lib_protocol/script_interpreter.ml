(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(*

  This module implements an interpreter for Michelson. It takes the
   form of a [step] function that interprets script instructions in a
   dedicated abstract machine.

  The interpreter is written in a small-step style: an execution
   [step] only interprets a single instruction by updating the
   configuration of a dedicated abstract machine.

  This abstract machine has two components:

  - a stack to control which instructions must be executed ; and

  - a stack of values where instructions get their inputs and put
   their outputs.

  In addition, the machine has access to effectful primitives to
   interact with the execution environment (e.g. the Tezos
   node). These primitives live in the [Lwt+State+Error] monad. Hence,
   this interpreter produces a computation in the [Lwt+State+Error]
   monad.

  This interpreter enjoys the following properties:

  - The interpreter is tail-recursive, hence it is robust to stack
   overflow. This property is checked by the compiler thanks to the
   [@ocaml.tailcall] annotation of each recursive call.

  - The interpreter is type-preserving. Thanks to GADTs, the typing
   rules of Michelson are statically checked by the OCaml typechecker:
   a Michelson program cannot go wrong.

  - The interpreter is tagless. Thanks to GADTs, the exact shape of
   the stack is known statically so the interpreter does not have to
   check that the input stack has the shape expected by the
   instruction to be executed.

  Outline
  =======

  This file is organized as follows:

  1. Runtime errors: The standard incantations to register the errors
   that can be produced by this module's functions.

  2. Gas accounting: The function [cost_of_instr] assigns a gas
   consumption to an instruction and a stack of values according to
   the cost model. This function is used in the interpretation
   loop. Several auxiliary functions are given to deal with gas
   accounting.

  3. Logging: One can instrument the interpreter with logging
   functions.

  4. Interpretation loop: This is the main functionality of this
   module, aka the [step] function.

  5. Interface functions: This part of the module builds high-level
   functions on top of the more basic [step] function.

  Implementation details are explained along the file.

*)

open Alpha_context
open Script
open Script_typed_ir
open Script_ir_translator
module S = Saturation_repr

(* ---- Run-time errors -----------------------------------------------------*)

type error +=
  | Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error : Contract.t * Script.expr -> error

type error += Bad_contract_parameter of Contract.t (* `Permanent *)

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

type error += Michelson_too_many_recursive_calls

let () =
  let open Data_encoding in
  let trace_encoding =
    list
    @@ obj3
         (req "location" Script.location_encoding)
         (req "gas" Gas.encoding)
         (req
            "stack"
            (list (obj2 (req "item" Script.expr_encoding) (opt "annot" string))))
  in
  (* Reject *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.script_rejected"
    ~title:"Script failed"
    ~description:"A FAILWITH instruction was reached"
    (obj3
       (req "location" Script.location_encoding)
       (req "with" Script.expr_encoding)
       (opt "trace" trace_encoding))
    (function Reject (loc, v, trace) -> Some (loc, v, trace) | _ -> None)
    (fun (loc, v, trace) -> Reject (loc, v, trace)) ;
  (* Overflow *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.script_overflow"
    ~title:"Script failed (overflow error)"
    ~description:
      "A FAIL instruction was reached due to the detection of an overflow"
    (obj2
       (req "location" Script.location_encoding)
       (opt "trace" trace_encoding))
    (function Overflow (loc, trace) -> Some (loc, trace) | _ -> None)
    (fun (loc, trace) -> Overflow (loc, trace)) ;
  (* Runtime contract error *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.runtime_error"
    ~title:"Script runtime error"
    ~description:"Toplevel error for all runtime script errors"
    (obj2
       (req "contract_handle" Contract.encoding)
       (req "contract_code" Script.expr_encoding))
    (function
      | Runtime_contract_error (contract, expr) ->
          Some (contract, expr)
      | _ ->
          None)
    (fun (contract, expr) -> Runtime_contract_error (contract, expr)) ;
  (* Bad contract parameter *)
  register_error_kind
    `Permanent
    ~id:"michelson_v1.bad_contract_parameter"
    ~title:"Contract supplied an invalid parameter"
    ~description:
      "Either no parameter was supplied to a contract with a non-unit \
       parameter type, a non-unit parameter was passed to an account, or a \
       parameter was supplied of the wrong type"
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Bad_contract_parameter c -> Some c | _ -> None)
    (fun c -> Bad_contract_parameter c) ;
  (* Cannot serialize failure *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_failure"
    ~title:"Not enough gas to serialize argument of FAILWITH"
    ~description:
      "Argument of FAILWITH was too big to be serialized with the provided gas"
    Data_encoding.empty
    (function Cannot_serialize_failure -> Some () | _ -> None)
    (fun () -> Cannot_serialize_failure) ;
  (* Cannot serialize storage *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_storage"
    ~title:"Not enough gas to serialize execution storage"
    ~description:
      "The returned storage was too big to be serialized with the provided gas"
    Data_encoding.empty
    (function Cannot_serialize_storage -> Some () | _ -> None)
    (fun () -> Cannot_serialize_storage)

(*

   Computing the cost of Michelson instructions
   ============================================

   The function [cost_of_instr] provides a cost model for Michelson
   instructions. It is used by the interpreter to track the
   consumption of gas. This consumption may depend on the values
   on the stack.

 *)

module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let cost_of_instr : type a s r f. (a, s, r, f) kinstr -> a -> s -> Gas.cost =
 fun i accu stack ->
  match i with
  | IList_map _ ->
      let list = accu in
      Interp_costs.list_map list
  | IList_iter _ ->
      let list = accu in
      Interp_costs.list_iter list
  | ISet_iter _ ->
      let set = accu in
      Interp_costs.set_iter set
  | ISet_mem _ ->
      let v = accu and (set, _) = stack in
      Interp_costs.set_mem v set
  | ISet_update _ ->
      let v = accu and (_, (set, _)) = stack in
      Interp_costs.set_update v set
  | IMap_map _ ->
      let map = accu in
      Interp_costs.map_map map
  | IMap_iter _ ->
      let map = accu in
      Interp_costs.map_iter map
  | IMap_mem _ ->
      let v = accu and (map, _) = stack in
      Interp_costs.map_mem v map
  | IMap_get _ ->
      let v = accu and (map, _) = stack in
      Interp_costs.map_get v map
  | IMap_update _ ->
      let k = accu and (_, (map, _)) = stack in
      Interp_costs.map_update k map
  | IMap_get_and_update _ ->
      let k = accu and (_, (map, _)) = stack in
      Interp_costs.map_get_and_update k map
  | IBig_map_mem _ ->
      let (map, _) = stack in
      Interp_costs.big_map_mem map.diff
  | IBig_map_get _ ->
      let (map, _) = stack in
      Interp_costs.big_map_get map.diff
  | IBig_map_update _ ->
      let (_, (map, _)) = stack in
      Interp_costs.big_map_update map.diff
  | IBig_map_get_and_update _ ->
      let (_, (map, _)) = stack in
      Interp_costs.big_map_get_and_update map.diff
  | IAdd_seconds_to_timestamp _ ->
      let n = accu and (t, _) = stack in
      Interp_costs.add_seconds_timestamp n t
  | IAdd_timestamp_to_seconds _ ->
      let t = accu and (n, _) = stack in
      Interp_costs.add_seconds_timestamp n t
  | ISub_timestamp_seconds _ ->
      let t = accu and (n, _) = stack in
      Interp_costs.sub_seconds_timestamp n t
  | IDiff_timestamps _ ->
      let t1 = accu and (t2, _) = stack in
      Interp_costs.diff_timestamps t1 t2
  | IConcat_string_pair _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.concat_string_pair x y
  | IConcat_string _ ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | ISlice_string _ ->
      let _offset = accu in
      let (_length, (s, _)) = stack in
      Interp_costs.slice_string s
  | IConcat_bytes_pair _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.concat_bytes_pair x y
  | IConcat_bytes _ ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | ISlice_bytes _ ->
      let (_, (s, _)) = stack in
      Interp_costs.slice_bytes s
  | IMul_teznat _ ->
      let (n, _) = stack in
      Interp_costs.mul_teznat n
  | IMul_nattez _ ->
      let n = accu in
      Interp_costs.mul_teznat n
  | IAbs_int _ ->
      let x = accu in
      Interp_costs.abs_int x
  | INeg_int _ ->
      let x = accu in
      Interp_costs.neg_int x
  | INeg_nat _ ->
      let x = accu in
      Interp_costs.neg_nat x
  | IAdd_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | IAdd_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | IAdd_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | IAdd_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_bigint x y
  | ISub_int _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.sub_bigint x y
  | IMul_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | IMul_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | IMul_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | IMul_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_bigint x y
  | IEdiv_teznat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_teznat x y
  | IEdiv_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | IEdiv_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | IEdiv_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | IEdiv_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_bigint x y
  | ILsl_nat _ ->
      let x = accu in
      Interp_costs.lsl_nat x
  | ILsr_nat _ ->
      let x = accu in
      Interp_costs.lsr_nat x
  | IOr_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.or_nat x y
  | IAnd_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.and_nat x y
  | IAnd_int_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.and_nat x y
  | IXor_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.xor_nat x y
  | INot_int _ ->
      let x = accu in
      Interp_costs.not_nat x
  | INot_nat _ ->
      let x = accu in
      Interp_costs.not_nat x
  | ICompare (_, ty, _) ->
      let a = accu and (b, _) = stack in
      Interp_costs.compare ty a b
  | ICheck_signature _ ->
      let key = accu and (_, (message, _)) = stack in
      Interp_costs.check_signature key message
  | IHash_key _ ->
      let pk = accu in
      Interp_costs.hash_key pk
  | IBlake2b _ ->
      let bytes = accu in
      Interp_costs.blake2b bytes
  | ISha256 _ ->
      let bytes = accu in
      Interp_costs.sha256 bytes
  | ISha512 _ ->
      let bytes = accu in
      Interp_costs.sha512 bytes
  | IKeccak _ ->
      let bytes = accu in
      Interp_costs.keccak bytes
  | ISha3 _ ->
      let bytes = accu in
      Interp_costs.sha3 bytes
  | IPairing_check_bls12_381 _ ->
      let pairs = accu in
      Interp_costs.pairing_check_bls12_381 pairs
  | ISapling_verify_update _ ->
      let tx = accu in
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | ISplit_ticket _ ->
      let ticket = accu and ((amount_a, amount_b), _) = stack in
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | IJoin_tickets (_, ty, _) ->
      let (ticket_a, ticket_b) = accu in
      Interp_costs.join_tickets ty ticket_a ticket_b
  | IHalt _ ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | IDrop _ ->
      Interp_costs.drop
  | IDup _ ->
      Interp_costs.dup
  | ISwap _ ->
      Interp_costs.swap
  | IConst _ ->
      Interp_costs.push
  | ICons_some _ ->
      Interp_costs.cons_some
  | ICons_none _ ->
      Interp_costs.cons_none
  | IIf_none _ ->
      Interp_costs.if_none
  | ICons_pair _ ->
      Interp_costs.cons_pair
  | IUnpair _ ->
      Interp_costs.unpair
  | ICar _ ->
      Interp_costs.car
  | ICdr _ ->
      Interp_costs.cdr
  | ICons_left _ ->
      Interp_costs.cons_left
  | ICons_right _ ->
      Interp_costs.cons_right
  | IIf_left _ ->
      Interp_costs.if_left
  | ICons_list _ ->
      Interp_costs.cons_list
  | INil _ ->
      Interp_costs.nil
  | IIf_cons _ ->
      Interp_costs.if_cons
  | IList_size _ ->
      Interp_costs.list_size
  | IEmpty_set _ ->
      Interp_costs.empty_set
  | ISet_size _ ->
      Interp_costs.set_size
  | IEmpty_map _ ->
      Interp_costs.empty_map
  | IMap_size _ ->
      Interp_costs.map_size
  | IEmpty_big_map _ ->
      Interp_costs.empty_map
  | IString_size _ ->
      Interp_costs.string_size
  | IBytes_size _ ->
      Interp_costs.bytes_size
  | IAdd_tez _ ->
      Interp_costs.add_tez
  | ISub_tez _ ->
      Interp_costs.sub_tez
  | IOr _ ->
      Interp_costs.bool_or
  | IAnd _ ->
      Interp_costs.bool_and
  | IXor _ ->
      Interp_costs.bool_xor
  | INot _ ->
      Interp_costs.bool_not
  | IIs_nat _ ->
      Interp_costs.is_nat
  | IInt_nat _ ->
      Interp_costs.int_nat
  | IInt_bls12_381_fr _ ->
      Interp_costs.int_bls12_381_fr
  | IEdiv_tez _ ->
      Interp_costs.ediv_tez
  | IIf _ ->
      Interp_costs.if_
  | ILoop _ ->
      Interp_costs.loop
  | ILoop_left _ ->
      Interp_costs.loop_left
  | IDip _ ->
      Interp_costs.dip
  | IExec _ ->
      Interp_costs.exec
  | IApply _ ->
      Interp_costs.apply
  | ILambda _ ->
      Interp_costs.push
  | IFailwith _ ->
      Gas.free
  | INop _ ->
      Interp_costs.nop
  | IEq _ ->
      Interp_costs.eq
  | INeq _ ->
      Interp_costs.neq
  | ILt _ ->
      Interp_costs.neq
  | ILe _ ->
      Interp_costs.neq
  | IGt _ ->
      Interp_costs.neq
  | IGe _ ->
      Interp_costs.neq
  | IPack _ ->
      Gas.free
  | IUnpack _ ->
      Gas.free
  | IAddress _ ->
      Interp_costs.address
  | IContract _ ->
      Interp_costs.contract
  | ITransfer_tokens _ ->
      Interp_costs.transfer_tokens
  | IImplicit_account _ ->
      Interp_costs.implicit_account
  | ISet_delegate _ ->
      Interp_costs.set_delegate
  | IBalance _ ->
      Interp_costs.balance
  | ILevel _ ->
      Interp_costs.level
  | INow _ ->
      Interp_costs.now
  | ISapling_empty_state _ ->
      Interp_costs.sapling_empty_state
  | ISource _ ->
      Interp_costs.source
  | ISender _ ->
      Interp_costs.source
  | ISelf _ ->
      Interp_costs.self
  | ISelf_address _ ->
      Interp_costs.self
  | IAmount _ ->
      Interp_costs.amount
  | IDig (_, n, _, _) ->
      Interp_costs.dign n
  | IDug (_, n, _, _) ->
      Interp_costs.dugn n
  | IDipn (_, n, _, _, _) ->
      Interp_costs.dipn n
  | IDropn (_, n, _, _) ->
      Interp_costs.dropn n
  | IChainId _ ->
      Interp_costs.chain_id
  | ICreate_contract _ ->
      Interp_costs.create_contract
  | INever _ -> (
    match accu with _ -> . )
  | IVoting_power _ ->
      Interp_costs.voting_power
  | ITotal_voting_power _ ->
      Interp_costs.total_voting_power
  | IAdd_bls12_381_g1 _ ->
      Interp_costs.add_bls12_381_g1
  | IAdd_bls12_381_g2 _ ->
      Interp_costs.add_bls12_381_g2
  | IAdd_bls12_381_fr _ ->
      Interp_costs.add_bls12_381_fr
  | IMul_bls12_381_g1 _ ->
      Interp_costs.mul_bls12_381_g1
  | IMul_bls12_381_g2 _ ->
      Interp_costs.mul_bls12_381_g2
  | IMul_bls12_381_fr _ ->
      Interp_costs.mul_bls12_381_fr
  | INeg_bls12_381_g1 _ ->
      Interp_costs.neg_bls12_381_g1
  | INeg_bls12_381_g2 _ ->
      Interp_costs.neg_bls12_381_g2
  | INeg_bls12_381_fr _ ->
      Interp_costs.neg_bls12_381_fr
  | IMul_bls12_381_fr_z _ ->
      Interp_costs.mul_bls12_381_fr_z
  | IMul_bls12_381_z_fr _ ->
      Interp_costs.mul_bls12_381_fr_z
  | IDup_n (_, n, _, _) ->
      Interp_costs.dupn n
  | IComb (_, n, _, _) ->
      Interp_costs.comb n
  | IUncomb (_, n, _, _) ->
      Interp_costs.uncomb n
  | IComb_get (_, n, _, _) ->
      Interp_costs.comb_get n
  | IComb_set (_, n, _, _) ->
      Interp_costs.comb_set n
  | ITicket _ ->
      Interp_costs.ticket
  | IRead_ticket _ ->
      Interp_costs.read_ticket
  | ILog _ ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
 [@@ocaml.inline always]

let cost_of_control : type a s r f. (a, s, r, f) continuation -> Gas.cost =
 fun ks ->
  match ks with
  | KLog _ ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KNil ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KCons (_, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KReturn _ ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KUndip (_, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KLoop_in (_, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KLoop_in_left (_, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KIter (_, _, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KList_enter_body (_, _, _, _, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KList_exit_body (_, _, _, _, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KMap_enter_body (_, _, _, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free
  | KMap_exit_body (_, _, _, _, _) ->
      (* FIXME: This will be fixed when the new cost model is defined. *)
      Gas.free

(*

   Gas update and check for gas exhaustion
   =======================================

   Each instruction has a cost. The runtime subtracts this cost
   from an amount of gas made available for the script execution.

   Updating the gas counter is a critical aspect to Michelson
   execution because it is done at each execution step.

   For this reason, the interpreter must read and update the
   gas counter as quickly as possible. Hence, the gas counter
   should be stored in a machine register. To motivate the
   OCaml compiler to make that choice, we represent the gas
   counter as a local parameter of the execution [step]
   function.

*)

type local_gas_counter = int

(*

   The gas counter stored in the context is desynchronized with the
   [local_gas_counter] used in the interpretation loop. When we have
   to call a gas-consuming function which lives outside the
   interpreter, we must update the context so that it carries an
   up-to-date gas counter. Similarly, when we return from such a
   function, the [local_gas_counter] must be updated as well.

   To statically track these points where the context's gas counter
   must be updated, we introduce a type for outdated contexts. The
   [step] function carries an [outdated_context]. When an external
   function needs a [context], the typechecker points out the need for
   a conversion: this forces us to either call [update_context], or
   better, when this is possible, the function
   [use_gas_counter_in_ctxt].

*)
type outdated_context = OutDatedContext of context [@@unboxed]

let update_context local_gas_counter = function
  | OutDatedContext ctxt ->
      Gas.update_gas_counter ctxt (Saturation_repr.safe_int local_gas_counter)
  [@@ocaml.inline always]

let update_local_gas_counter ctxt =
  (Gas.gas_counter ctxt :> int)
  [@@ocaml.inline always]

let outdated ctxt = OutDatedContext ctxt [@@ocaml.inline always]

let context_from_outdated_context (OutDatedContext ctxt) =
  ctxt
  [@@ocaml.inline always]

let use_gas_counter_in_ctxt ctxt local_gas_counter f =
  let ctxt = update_context local_gas_counter ctxt in
  f ctxt
  >>=? fun (y, ctxt) -> return (y, outdated ctxt, update_local_gas_counter ctxt)
  [@@ocaml.inline always]

(*

   [step] calls [consume] at the beginning of each execution step.

   [consume'] is used in the implementation of [IConcat_string]
   and [IConcat_bytes] because in that special cases, the cost
   is expressed with respect to a non-constant-time computation
   on the inputs.

*)

let update_and_check gas_counter cost =
  let gas_counter = gas_counter - cost in
  if Compare.Int.(gas_counter < 0) then None else Some gas_counter
  [@@ocaml.inline always]

let consume local_gas_counter k accu stack =
  let cost = cost_of_instr k accu stack in
  update_and_check local_gas_counter (cost :> int)
  [@@ocaml.inline always]

let consume' ctxt local_gas_counter cost =
  match update_and_check local_gas_counter cost with
  | None ->
      Gas.gas_exhausted_error (update_context local_gas_counter ctxt)
  | Some local_gas_counter ->
      Ok local_gas_counter
  [@@ocaml.inline always]

let consume_control local_gas_counter ks =
  let cost = cost_of_control ks in
  update_and_check local_gas_counter (cost :> int)
  [@@ocaml.inline always]

let log_entry (logger : logger) ctxt gas k accu stack =
  let kinfo = kinfo_of_kinstr k in
  let ctxt = update_context gas ctxt in
  logger.log_entry k ctxt kinfo.iloc kinfo.kstack_ty (accu, stack)

let log_exit (logger : logger) ctxt gas kinfo_prev k accu stack =
  let ctxt = update_context gas ctxt in
  let kinfo = kinfo_of_kinstr k in
  logger.log_exit k ctxt kinfo_prev.iloc kinfo.kstack_ty (accu, stack)

let log_control (logger : logger) ks = logger.log_control ks

let get_log (logger : logger option) =
  match logger with
  | None ->
      Lwt.return (Ok None)
  | Some logger ->
      logger.get_log ()
  [@@ocaml.inline always]

(*

  Interpretation loop
  ===================

*)

(*

    The interpreter is parameterized by a small set of values.

*)
type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

let rec interp_stack_prefix_preserving_operation :
    type a s b t c u d w result.
    (a -> s -> (b * t) * result) ->
    (a, s, b, t, c, u, d, w) stack_prefix_preservation_witness ->
    c ->
    u ->
    (d * w) * result =
 fun f n accu stk ->
  match (n, stk) with
  | (KPrefix (_, n), rest) ->
      interp_stack_prefix_preserving_operation f n (fst rest) (snd rest)
      |> fun ((v, rest'), result) -> ((accu, (v, rest')), result)
  | (KRest, v) ->
      f accu v

(*

   As announced earlier, the step function produces a computation in
   the [Lwt+State+Error] monad. The [State] monad is implemented by
   having the [context] passed as input and returned updated as
   output. The [Error] monad is represented by the [tzresult] type
   constructor.

   The [step] function is actually defined as an internal
   tail-recursive routine of the toplevel [step]. It monitors the gas
   level before executing the instruction under focus, once this is
   done, it recursively calls itself on the continuation held by the
   current instruction.

   For each pure instruction (i.e. that is not monadic), the
   interpretation simply updates the input arguments of the [step]
   function. Since these arguments are (most likely) stored in
   hardware registers and since the tail-recursive calls are compiled
   into direct jumps, this interpretation technique offers good
   performances while saving safety thanks to a rich typing.

   For each impure instruction, the interpreter makes use of monadic
   bindings to compose monadic primitives with the [step] function.
   Again, we make sure that the recursive calls to [step] are tail
   calls by annotating them with [@ocaml.tailcall].

*)
let rec next :
    type a s r f.
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, r, f) continuation ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun ((ctxt, _) as g) gas ks0 accu stack ->
  match ks0 with
  | KLog (ks, logger) ->
      log logger g gas ks accu stack
  | KNil ->
      Lwt.return (Ok (accu, stack, ctxt, gas))
  | KCons (k, ks) ->
      (step [@ocaml.tailcall]) g gas k ks accu stack
  | KLoop_in (ki, ks') -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas ->
        let (accu', stack') = stack in
        if accu then (step [@ocaml.tailcall]) g gas ki ks0 accu' stack'
        else (next [@ocaml.tailcall]) g gas ks' accu' stack' )
  | KReturn (stack', ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas ->
        next g gas ks accu stack' )
  | KLoop_in_left (ki, ks') -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas -> (
      match accu with
      | L v ->
          (step [@ocaml.tailcall]) g gas ki ks0 v stack
      | R v ->
          (next [@ocaml.tailcall]) g gas ks' v stack ) )
  | KUndip (x, ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas ->
        next g gas ks x (accu, stack) )
  | KIter (body, xs, ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas -> (
      match xs with
      | [] ->
          next g gas ks accu stack
      | x :: xs ->
          let ks = KIter (body, xs, ks) in
          (step [@ocaml.tailcall]) g gas body ks x (accu, stack) ) )
  | KList_enter_body (body, xs, ys, len, ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas -> (
      match xs with
      | [] ->
          let ys = {elements = List.rev ys; length = len} in
          next g gas ks ys (accu, stack)
      | x :: xs ->
          let ks = KList_exit_body (body, xs, ys, len, ks) in
          (step [@ocaml.tailcall]) g gas body ks x (accu, stack) ) )
  | KList_exit_body (body, xs, ys, len, ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas ->
        let ks = KList_enter_body (body, xs, accu :: ys, len, ks) in
        let (accu, stack) = stack in
        next g gas ks accu stack )
  | KMap_enter_body (body, xs, ys, ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas -> (
      match xs with
      | [] ->
          next g gas ks ys (accu, stack)
      | (xk, xv) :: xs ->
          let ks = KMap_exit_body (body, xs, ys, xk, ks) in
          let res = (xk, xv) in
          let stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas body ks res stack ) )
  | KMap_exit_body (body, xs, ys, yk, ks) -> (
    match consume_control gas ks0 with
    | None ->
        Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
    | Some gas ->
        let ys = map_update yk (Some accu) ys in
        let ks = KMap_enter_body (body, xs, ys, ks) in
        let (accu, stack) = stack in
        next g gas ks accu stack )

and step :
    type a s b t r f.
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, b, t) kinstr ->
    (b, t, r, f) continuation ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun ((ctxt, sc) as g) gas i ks accu stack ->
  match consume gas i accu stack with
  | None ->
      Lwt.return (Gas.gas_exhausted_error (update_context gas ctxt))
  | Some gas -> (
    match i with
    | ILog (_, event, logger, k) ->
        ( match event with
        | LogEntry ->
            log_entry logger ctxt gas k accu stack
        | LogExit prev_kinfo ->
            log_exit logger ctxt gas prev_kinfo k accu stack ) ;
        let k = log_next_kinstr logger k in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IHalt _ ->
        next g gas ks accu stack
    (* stack ops *)
    | IDrop (_, k) ->
        let (accu, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IDup (_, k) ->
        (step [@ocaml.tailcall]) g gas k ks accu (accu, stack)
    | ISwap (_, k) ->
        let (top, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks top (accu, stack)
    | IConst (_, v, k) ->
        (step [@ocaml.tailcall]) g gas k ks v (accu, stack)
    (* options *)
    | ICons_some (_, k) ->
        (step [@ocaml.tailcall]) g gas k ks (Some accu) stack
    | ICons_none (_, _, k) ->
        (step [@ocaml.tailcall]) g gas k ks None (accu, stack)
    | IIf_none (_, bt, bf) -> (
      match accu with
      | None ->
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas bt ks accu stack
      | Some v ->
          (step [@ocaml.tailcall]) g gas bf ks v stack )
    (* pairs *)
    | ICons_pair (_, k) ->
        let (b, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks (accu, b) stack
    | IUnpair (_, k) ->
        let (a, b) = accu in
        (step [@ocaml.tailcall]) g gas k ks a (b, stack)
    | ICar (_, k) ->
        let (a, _) = accu in
        (step [@ocaml.tailcall]) g gas k ks a stack
    | ICdr (_, k) ->
        let (_, b) = accu in
        (step [@ocaml.tailcall]) g gas k ks b stack
    (* unions *)
    | ICons_left (_, k) ->
        (step [@ocaml.tailcall]) g gas k ks (L accu) stack
    | ICons_right (_, k) ->
        (step [@ocaml.tailcall]) g gas k ks (R accu) stack
    | IIf_left (_, bl, br) -> (
      match accu with
      | L v ->
          (step [@ocaml.tailcall]) g gas bl ks v stack
      | R v ->
          (step [@ocaml.tailcall]) g gas br ks v stack )
    (* lists *)
    | ICons_list (_, k) ->
        let (tl, stack) = stack in
        let accu = list_cons accu tl in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | INil (_, k) ->
        let stack = (accu, stack) in
        let accu = list_empty in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IIf_cons (_, bc, bn) -> (
      match accu.elements with
      | [] ->
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas bn ks accu stack
      | hd :: tl ->
          let tl = {elements = tl; length = accu.length - 1} in
          (step [@ocaml.tailcall]) g gas bc ks hd (tl, stack) )
    | IList_map (_, body, k) ->
        let xs = accu.elements in
        let ys = [] in
        let len = accu.length in
        let ks = KList_enter_body (body, xs, ys, len, KCons (k, ks)) in
        let (accu, stack) = stack in
        (next [@ocaml.tailcall]) g gas ks accu stack
    | IList_size (_, k) ->
        let list = accu in
        let len = Script_int.(abs (of_int list.length)) in
        (step [@ocaml.tailcall]) g gas k ks len stack
    | IList_iter (_, body, k) ->
        let xs = accu.elements in
        let ks = KIter (body, xs, KCons (k, ks)) in
        let (accu, stack) = stack in
        (next [@ocaml.tailcall]) g gas ks accu stack
    (* sets *)
    | IEmpty_set (_, ty, k) ->
        let res = empty_set ty in
        let stack = (accu, stack) in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | ISet_iter (_, body, k) ->
        let set = accu in
        let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
        let ks = KIter (body, l, KCons (k, ks)) in
        let (accu, stack) = stack in
        (next [@ocaml.tailcall]) g gas ks accu stack
    | ISet_mem (_, k) ->
        let (set, stack) = stack in
        let res = set_mem accu set in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | ISet_update (_, k) ->
        let (presence, (set, stack)) = stack in
        let res = set_update accu presence set in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | ISet_size (_, k) ->
        let res = set_size accu in
        (step [@ocaml.tailcall]) g gas k ks res stack
    (* maps *)
    | IEmpty_map (_, ty, _, k) ->
        let res = empty_map ty and stack = (accu, stack) in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMap_map (_, body, k) ->
        let map = accu in
        let xs = List.rev (map_fold (fun k v a -> (k, v) :: a) map []) in
        let ys = empty_map (map_key_ty map) in
        let ks = KMap_enter_body (body, xs, ys, KCons (k, ks)) in
        let (accu, stack) = stack in
        (next [@ocaml.tailcall]) g gas ks accu stack
    | IMap_iter (_, body, k) ->
        let map = accu in
        let l = List.rev (map_fold (fun k v a -> (k, v) :: a) map []) in
        let ks = KIter (body, l, KCons (k, ks)) in
        let (accu, stack) = stack in
        (next [@ocaml.tailcall]) g gas ks accu stack
    | IMap_mem (_, k) ->
        let (map, stack) = stack in
        let res = map_mem accu map in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMap_get (_, k) ->
        let (map, stack) = stack in
        let res = map_get accu map in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMap_update (_, k) ->
        let (v, (map, stack)) = stack in
        let key = accu in
        let res = map_update key v map in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMap_get_and_update (_, k) ->
        let key = accu in
        let (v, (map, rest)) = stack in
        let map' = map_update key v map in
        let v' = map_get key map in
        (step [@ocaml.tailcall]) g gas k ks v' (map', rest)
    | IMap_size (_, k) ->
        let res = map_size accu in
        (step [@ocaml.tailcall]) g gas k ks res stack
    (* Big map operations *)
    | IEmpty_big_map (_, tk, tv, k) ->
        let ebm = Script_ir_translator.empty_big_map tk tv in
        (step [@ocaml.tailcall]) g gas k ks ebm (accu, stack)
    | IBig_map_mem (_, k) ->
        let (map, stack) = stack in
        let key = accu in
        ( use_gas_counter_in_ctxt ctxt gas
        @@ fun ctxt -> Script_ir_translator.big_map_mem ctxt key map )
        >>=? fun (res, ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
    | IBig_map_get (_, k) ->
        let (map, stack) = stack in
        let key = accu in
        ( use_gas_counter_in_ctxt ctxt gas
        @@ fun ctxt -> Script_ir_translator.big_map_get ctxt key map )
        >>=? fun (res, ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
    | IBig_map_update (_, k) ->
        let key = accu in
        let (maybe_value, (map, stack)) = stack in
        ( use_gas_counter_in_ctxt ctxt gas
        @@ fun ctxt ->
        Script_ir_translator.big_map_update ctxt key maybe_value map )
        >>=? fun (big_map, ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks big_map stack
    | IBig_map_get_and_update (_, k) ->
        let key = accu in
        let (v, (map, stack)) = stack in
        ( use_gas_counter_in_ctxt ctxt gas
        @@ fun ctxt ->
        Script_ir_translator.big_map_get_and_update ctxt key v map )
        >>=? fun ((v', map'), ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks v' (map', stack)
    (* timestamp operations *)
    | IAdd_seconds_to_timestamp (_, k) ->
        let n = accu in
        let (t, stack) = stack in
        let result = Script_timestamp.add_delta t n in
        (step [@ocaml.tailcall]) g gas k ks result stack
    | IAdd_timestamp_to_seconds (_, k) ->
        let t = accu in
        let (n, stack) = stack in
        let result = Script_timestamp.add_delta t n in
        (step [@ocaml.tailcall]) g gas k ks result stack
    | ISub_timestamp_seconds (_, k) ->
        let t = accu in
        let (s, stack) = stack in
        let result = Script_timestamp.sub_delta t s in
        (step [@ocaml.tailcall]) g gas k ks result stack
    | IDiff_timestamps (_, k) ->
        let t1 = accu in
        let (t2, stack) = stack in
        let result = Script_timestamp.diff t1 t2 in
        (step [@ocaml.tailcall]) g gas k ks result stack
    (* string operations *)
    | IConcat_string_pair (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        let s = String.concat "" [x; y] in
        (step [@ocaml.tailcall]) g gas k ks s stack
    | IConcat_string (_, k) ->
        let ss = accu in
        (* The cost for this fold_left has been paid upfront *)
        let total_length =
          List.fold_left
            (fun acc s -> S.add acc (S.safe_int (String.length s)))
            (S.zero |> S.may_saturate)
            accu.elements
        in
        consume' ctxt gas (Interp_costs.concat_string total_length :> int)
        >>?= fun gas ->
        let s = String.concat "" ss.elements in
        (step [@ocaml.tailcall]) g gas k ks s stack
    | ISlice_string (_, k) ->
        let offset = accu and (length, (s, stack)) = stack in
        let s_length = Z.of_int (String.length s) in
        let offset = Script_int.to_zint offset in
        let length = Script_int.to_zint length in
        if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
        then
          let s = String.sub s (Z.to_int offset) (Z.to_int length) in
          (step [@ocaml.tailcall]) g gas k ks (Some s) stack
        else (step [@ocaml.tailcall]) g gas k ks None stack
    | IString_size (_, k) ->
        let s = accu in
        let result = Script_int.(abs (of_int (String.length s))) in
        (step [@ocaml.tailcall]) g gas k ks result stack
    (* bytes operations *)
    | IConcat_bytes_pair (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        let s = Bytes.cat x y in
        (step [@ocaml.tailcall]) g gas k ks s stack
    | IConcat_bytes (_, k) ->
        let ss = accu in
        (* The cost for this fold_left has been paid upfront *)
        let total_length =
          List.fold_left
            (fun acc s -> S.add acc (S.safe_int (Bytes.length s)))
            (S.zero |> S.may_saturate)
            accu.elements
        in
        consume' ctxt gas (Interp_costs.concat_string total_length :> int)
        >>?= fun gas ->
        let s = Bytes.concat Bytes.empty ss.elements in
        (step [@ocaml.tailcall]) g gas k ks s stack
    | ISlice_bytes (_, k) ->
        let offset = accu and (length, (s, stack)) = stack in
        let s_length = Z.of_int (Bytes.length s) in
        let offset = Script_int.to_zint offset in
        let length = Script_int.to_zint length in
        if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
        then
          let s = Bytes.sub s (Z.to_int offset) (Z.to_int length) in
          (step [@ocaml.tailcall]) g gas k ks (Some s) stack
        else (step [@ocaml.tailcall]) g gas k ks None stack
    | IBytes_size (_, k) ->
        let s = accu in
        let result = Script_int.(abs (of_int (Bytes.length s))) in
        (step [@ocaml.tailcall]) g gas k ks result stack
    (* currency operations *)
    | IAdd_tez (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        Tez.(x +? y)
        >>?= fun res -> (step [@ocaml.tailcall]) g gas k ks res stack
    | ISub_tez (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        Tez.(x -? y)
        >>?= fun res -> (step [@ocaml.tailcall]) g gas k ks res stack
    | IMul_teznat (kinfo, logger, k) -> (
        let x = accu in
        let (y, stack) = stack in
        match Script_int.to_int64 y with
        | None ->
            get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
        | Some y ->
            Tez.(x *? y)
            >>?= fun res -> (step [@ocaml.tailcall]) g gas k ks res stack )
    | IMul_nattez (kinfo, logger, k) -> (
        let y = accu in
        let (x, stack) = stack in
        match Script_int.to_int64 y with
        | None ->
            get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
        | Some y ->
            Tez.(x *? y)
            >>?= fun res -> (step [@ocaml.tailcall]) g gas k ks res stack )
    (* boolean operations *)
    | IOr (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks (x || y) stack
    | IAnd (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks (x && y) stack
    | IXor (_, k) ->
        let x = accu in
        let (y, stack) = stack in
        let res = Compare.Bool.(x <> y) in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | INot (_, k) ->
        let x = accu in
        (step [@ocaml.tailcall]) g gas k ks (not x) stack
    (* integer operations *)
    | IIs_nat (_, k) ->
        let x = accu in
        let res = Script_int.is_nat x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAbs_int (_, k) ->
        let x = accu in
        let res = Script_int.abs x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IInt_nat (_, k) ->
        let x = accu in
        let res = Script_int.int x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | INeg_int (_, k) ->
        let x = accu in
        let res = Script_int.neg x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | INeg_nat (_, k) ->
        let x = accu in
        let res = Script_int.neg x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAdd_intint (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.add x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAdd_intnat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.add x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAdd_natint (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.add x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAdd_natnat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.add_n x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | ISub_int (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.sub x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMul_intint (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.mul x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMul_intnat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.mul x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMul_natint (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.mul x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMul_natnat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.mul_n x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IEdiv_teznat (_, k) ->
        let x = accu and (y, stack) = stack in
        let x = Script_int.of_int64 (Tez.to_mutez x) in
        let result =
          match Script_int.ediv x y with
          | None ->
              None
          | Some (q, r) -> (
            match (Script_int.to_int64 q, Script_int.to_int64 r) with
            | (Some q, Some r) -> (
              match (Tez.of_mutez q, Tez.of_mutez r) with
              | (Some q, Some r) ->
                  Some (q, r)
              (* Cannot overflow *)
              | _ ->
                  assert false )
            (* Cannot overflow *)
            | _ ->
                assert false )
        in
        (step [@ocaml.tailcall]) g gas k ks result stack
    | IEdiv_tez (_, k) ->
        let x = accu and (y, stack) = stack in
        let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
        let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
        let result =
          match Script_int.ediv_n x y with
          | None ->
              None
          | Some (q, r) -> (
            match Script_int.to_int64 r with
            | None ->
                assert false (* Cannot overflow *)
            | Some r -> (
              match Tez.of_mutez r with
              | None ->
                  assert false (* Cannot overflow *)
              | Some r ->
                  Some (q, r) ) )
        in
        (step [@ocaml.tailcall]) g gas k ks result stack
    | IEdiv_intint (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.ediv x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IEdiv_intnat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.ediv x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IEdiv_natint (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.ediv x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IEdiv_natnat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.ediv_n x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | ILsl_nat (kinfo, logger, k) -> (
        let x = accu and (y, stack) = stack in
        match Script_int.shift_left_n x y with
        | None ->
            get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
        | Some x ->
            (step [@ocaml.tailcall]) g gas k ks x stack )
    | ILsr_nat (kinfo, logger, k) -> (
        let x = accu and (y, stack) = stack in
        match Script_int.shift_right_n x y with
        | None ->
            get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
        | Some r ->
            (step [@ocaml.tailcall]) g gas k ks r stack )
    | IOr_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.logor x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAnd_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.logand x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IAnd_int_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.logand x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IXor_nat (_, k) ->
        let x = accu and (y, stack) = stack in
        let res = Script_int.logxor x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | INot_int (_, k) ->
        let x = accu in
        let res = Script_int.lognot x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | INot_nat (_, k) ->
        let x = accu in
        let res = Script_int.lognot x in
        (step [@ocaml.tailcall]) g gas k ks res stack
    (* control *)
    | IIf (_, bt, bf) ->
        let (res, stack) = stack in
        if accu then (step [@ocaml.tailcall]) g gas bt ks res stack
        else (step [@ocaml.tailcall]) g gas bf ks res stack
    | ILoop (_, body, k) ->
        let ks = KLoop_in (body, KCons (k, ks)) in
        (next [@ocaml.tailcall]) g gas ks accu stack
    | ILoop_left (_, bl, br) ->
        let ks = KLoop_in_left (bl, KCons (br, ks)) in
        (next [@ocaml.tailcall]) g gas ks accu stack
    | IDip (_, _, b, k) ->
        let ign = accu in
        let ks = KUndip (ign, KCons (k, ks)) in
        let (accu, stack) = stack in
        (step [@ocaml.tailcall]) g gas b ks accu stack
    | IExec (_, logger, k) ->
        let arg = accu and (code, stack) = stack in
        let (Lam (code, _)) = code in
        let code =
          match logger with
          | None ->
              code.kinstr
          | Some logger ->
              log_kinstr logger code.kinstr
        in
        let ks = KReturn (stack, KCons (k, ks)) in
        (step [@ocaml.tailcall]) g gas code ks arg ((), ())
    | IApply (_, capture_ty, k) ->
        let capture = accu in
        let (lam, stack) = stack in
        apply ctxt gas capture_ty capture lam
        >>=? fun (lam', ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks lam' stack
    | ILambda (_, lam, k) ->
        (step [@ocaml.tailcall]) g gas k ks lam (accu, stack)
    | IFailwith (_, kloc, tv, logger, _) ->
        let v = accu in
        let ctxt = update_context gas ctxt in
        trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
        >>=? fun (v, _ctxt) ->
        let v = Micheline.strip_locations v in
        get_log logger >>=? fun log -> fail (Reject (kloc, v, log))
    | INop (_, k) ->
        (step [@ocaml.tailcall]) g gas k ks accu stack
    (* comparison *)
    | ICompare (_, ty, k) ->
        let a = accu in
        let (b, stack) = stack in
        let r =
          Script_int.of_int @@ Script_ir_translator.compare_comparable ty a b
        in
        (step [@ocaml.tailcall]) g gas k ks r stack
    (* comparators *)
    | IEq (_, k) ->
        let a = accu in
        let a = Script_int.compare a Script_int.zero in
        let a = Compare.Int.(a = 0) in
        (step [@ocaml.tailcall]) g gas k ks a stack
    | INeq (_, k) ->
        let a = accu in
        let a = Script_int.compare a Script_int.zero in
        let a = Compare.Int.(a <> 0) in
        (step [@ocaml.tailcall]) g gas k ks a stack
    | ILt (_, k) ->
        let a = accu in
        let a = Script_int.compare a Script_int.zero in
        let a = Compare.Int.(a < 0) in
        (step [@ocaml.tailcall]) g gas k ks a stack
    | ILe (_, k) ->
        let a = accu in
        let a = Script_int.compare a Script_int.zero in
        let a = Compare.Int.(a <= 0) in
        (step [@ocaml.tailcall]) g gas k ks a stack
    | IGt (_, k) ->
        let a = accu in
        let a = Script_int.compare a Script_int.zero in
        let a = Compare.Int.(a > 0) in
        (step [@ocaml.tailcall]) g gas k ks a stack
    | IGe (_, k) ->
        let a = accu in
        let a = Script_int.compare a Script_int.zero in
        let a = Compare.Int.(a >= 0) in
        (step [@ocaml.tailcall]) g gas k ks a stack
    (* packing *)
    | IPack (_, ty, k) ->
        let value = accu in
        ( use_gas_counter_in_ctxt ctxt gas
        @@ fun ctxt -> Script_ir_translator.pack_data ctxt ty value )
        >>=? fun (bytes, ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks bytes stack
    | IUnpack (_, ty, k) ->
        let bytes = accu in
        (use_gas_counter_in_ctxt ctxt gas @@ fun ctxt -> unpack ctxt ~ty ~bytes)
        >>=? fun (opt, ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks opt stack
    | IAddress (_, k) ->
        let (_, address) = accu in
        (step [@ocaml.tailcall]) g gas k ks address stack
    | IContract (kinfo, t, entrypoint, k) -> (
        let contract = accu in
        match (contract, entrypoint) with
        | ((contract, "default"), entrypoint)
        | ((contract, entrypoint), "default") ->
            let ctxt = update_context gas ctxt in
            Script_ir_translator.parse_contract_for_script
              ctxt
              kinfo.iloc
              t
              contract
              ~entrypoint
            >>=? fun (ctxt, maybe_contract) ->
            let gas = update_local_gas_counter ctxt in
            let ctxt = outdated ctxt in
            let accu = maybe_contract in
            (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
        | _ ->
            (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack )
    | ITransfer_tokens (_, k) ->
        let p = accu in
        let (amount, ((tp, (destination, entrypoint)), stack)) = stack in
        transfer (ctxt, sc) gas amount tp p destination entrypoint
        >>=? fun (accu, ctxt, gas) ->
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
    | IImplicit_account (_, k) ->
        let key = accu in
        let contract = Contract.implicit_contract key in
        let res = (Unit_t None, (contract, "default")) in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | ICreate_contract
        (_, storage_type, param_type, Lam (_, code), root_name, k) ->
        (* Removed the instruction's arguments manager, spendable and delegatable *)
        let delegate = accu in
        let (credit, (init, stack)) = stack in
        create_contract
          g
          gas
          storage_type
          param_type
          code
          root_name
          delegate
          credit
          init
        >>=? fun (res, contract, ctxt, gas) ->
        let stack = ((contract, "default"), stack) in
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
    | ISet_delegate (_, k) ->
        let delegate = accu in
        let operation = Delegation delegate in
        let ctxt = update_context gas ctxt in
        fresh_internal_nonce ctxt
        >>?= fun (ctxt, nonce) ->
        let res =
          (Internal_operation {source = sc.self; operation; nonce}, None)
        in
        let gas = update_local_gas_counter ctxt in
        let ctxt = outdated ctxt in
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
    | IBalance (_, k) ->
        let ctxt = update_context gas ctxt in
        Contract.get_balance_carbonated ctxt sc.self
        >>=? fun (ctxt, balance) ->
        let gas = update_local_gas_counter ctxt in
        let ctxt = outdated ctxt in
        let g = (ctxt, sc) in
        (step [@ocaml.tailcall]) g gas k ks balance (accu, stack)
    | ILevel (_, k) ->
        let level =
          (Level.current (context_from_outdated_context ctxt)).level
          |> Raw_level.to_int32 |> Script_int.of_int32 |> Script_int.abs
        in
        (step [@ocaml.tailcall]) g gas k ks level (accu, stack)
    | INow (_, k) ->
        let now = Script_timestamp.now (context_from_outdated_context ctxt) in
        (step [@ocaml.tailcall]) g gas k ks now (accu, stack)
    | ICheck_signature (_, k) ->
        let key = accu and (signature, (message, stack)) = stack in
        let res = Signature.check key signature message in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IHash_key (_, k) ->
        let key = accu in
        let res = Signature.Public_key.hash key in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IBlake2b (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.blake2b bytes in
        (step [@ocaml.tailcall]) g gas k ks hash stack
    | ISha256 (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.sha256 bytes in
        (step [@ocaml.tailcall]) g gas k ks hash stack
    | ISha512 (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.sha512 bytes in
        (step [@ocaml.tailcall]) g gas k ks hash stack
    | ISource (_, k) ->
        let res = (sc.payer, "default") in
        (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
    | ISender (_, k) ->
        let res = (sc.source, "default") in
        (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
    | ISelf (_, ty, entrypoint, k) ->
        let res = (ty, (sc.self, entrypoint)) in
        (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
    | ISelf_address (_, k) ->
        let res = (sc.self, "default") in
        (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
    | IAmount (_, k) ->
        let accu = sc.amount and stack = (accu, stack) in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IDig (_, _n, n', k) ->
        let ((accu, stack), x) =
          interp_stack_prefix_preserving_operation
            (fun v stack -> (stack, v))
            n'
            accu
            stack
        in
        let accu = x and stack = (accu, stack) in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IDug (_, _n, n', k) ->
        let v = accu in
        let (accu, stack) = stack in
        let ((accu, stack), ()) =
          interp_stack_prefix_preserving_operation
            (fun accu stack -> ((v, (accu, stack)), ()))
            n'
            accu
            stack
        in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IDipn (_, _n, n', b, k) ->
        let (accu, stack, restore_prefix) = kundip n' accu stack k in
        let ks = KCons (restore_prefix, ks) in
        (step [@ocaml.tailcall]) g gas b ks accu stack
    | IDropn (_, _n, n', k) ->
        let stack =
          let rec aux :
              type a s b t.
              (b, t, b, t, a, s, a, s) stack_prefix_preservation_witness ->
              a ->
              s ->
              b * t =
           fun w accu stack ->
            match w with
            | KRest ->
                (accu, stack)
            | KPrefix (_, w) ->
                let (accu, stack) = stack in
                aux w accu stack
          in
          aux n' accu stack
        in
        let (accu, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | ISapling_empty_state (_, memo_size, k) ->
        let state = Sapling.empty_state ~memo_size () in
        (step [@ocaml.tailcall]) g gas k ks state (accu, stack)
    | ISapling_verify_update (_, k) -> (
        let transaction = accu in
        let (state, stack) = stack in
        let address = Contract.to_b58check sc.self in
        let chain_id = Chain_id.to_b58check sc.chain_id in
        let anti_replay = address ^ chain_id in
        let ctxt = update_context gas ctxt in
        Sapling.verify_update ctxt state transaction anti_replay
        >>=? fun (ctxt, balance_state_opt) ->
        let gas = update_local_gas_counter ctxt in
        let ctxt = outdated ctxt in
        match balance_state_opt with
        | Some (balance, state) ->
            let state = Some (Script_int.of_int64 balance, state) in
            (step [@ocaml.tailcall]) (ctxt, sc) gas k ks state stack
        | None ->
            (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack )
    | IChainId (_, k) ->
        let accu = sc.chain_id and stack = (accu, stack) in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | INever _ -> (
      match accu with _ -> . )
    | IVoting_power (_, k) ->
        let key_hash = accu in
        let ctxt = update_context gas ctxt in
        Vote.get_voting_power ctxt key_hash
        >>=? fun (ctxt, rolls) ->
        let power = Script_int.(abs (of_int32 rolls)) in
        let gas = update_local_gas_counter ctxt in
        let ctxt = outdated ctxt in
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks power stack
    | ITotal_voting_power (_, k) ->
        let ctxt = update_context gas ctxt in
        Vote.get_total_voting_power ctxt
        >>=? fun (ctxt, rolls) ->
        let power = Script_int.(abs (of_int32 rolls)) in
        let gas = update_local_gas_counter ctxt in
        let ctxt = outdated ctxt in
        let g = (ctxt, sc) in
        (step [@ocaml.tailcall]) g gas k ks power (accu, stack)
    | IKeccak (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.keccak256 bytes in
        (step [@ocaml.tailcall]) g gas k ks hash stack
    | ISha3 (_, k) ->
        let bytes = accu in
        let hash = Raw_hashes.sha3_256 bytes in
        (step [@ocaml.tailcall]) g gas k ks hash stack
    | IAdd_bls12_381_g1 (_, k) ->
        let x = accu and (y, stack) = stack in
        let accu = Bls12_381.G1.add x y in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IAdd_bls12_381_g2 (_, k) ->
        let x = accu and (y, stack) = stack in
        let accu = Bls12_381.G2.add x y in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IAdd_bls12_381_fr (_, k) ->
        let x = accu and (y, stack) = stack in
        let accu = Bls12_381.Fr.add x y in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IMul_bls12_381_g1 (_, k) ->
        let x = accu and (y, stack) = stack in
        let accu = Bls12_381.G1.mul x y in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IMul_bls12_381_g2 (_, k) ->
        let x = accu and (y, stack) = stack in
        let accu = Bls12_381.G2.mul x y in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IMul_bls12_381_fr (_, k) ->
        let x = accu and (y, stack) = stack in
        let accu = Bls12_381.Fr.mul x y in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IMul_bls12_381_fr_z (_, k) ->
        let x = accu and (y, stack) = stack in
        let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
        let res = Bls12_381.Fr.mul x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IMul_bls12_381_z_fr (_, k) ->
        let y = accu and (x, stack) = stack in
        let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
        let res = Bls12_381.Fr.mul x y in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | IInt_bls12_381_fr (_, k) ->
        let x = accu in
        let res = Script_int.of_zint (Bls12_381.Fr.to_z x) in
        (step [@ocaml.tailcall]) g gas k ks res stack
    | INeg_bls12_381_g1 (_, k) ->
        let x = accu in
        let accu = Bls12_381.G1.negate x in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | INeg_bls12_381_g2 (_, k) ->
        let x = accu in
        let accu = Bls12_381.G2.negate x in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | INeg_bls12_381_fr (_, k) ->
        let x = accu in
        let accu = Bls12_381.Fr.negate x in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IPairing_check_bls12_381 (_, k) ->
        let pairs = accu in
        let check =
          match pairs.elements with
          | [] ->
              true
          | pairs ->
              Bls12_381.(
                miller_loop pairs |> final_exponentiation_opt
                |> Option.map Gt.(eq one))
              |> Option.value ~default:false
        in
        (step [@ocaml.tailcall]) g gas k ks check stack
    | IComb (_, _, witness, k) ->
        let rec aux :
            type before after.
            (before, after) comb_gadt_witness -> before -> after =
         fun witness stack ->
          match (witness, stack) with
          | (Comb_one, stack) ->
              stack
          | (Comb_succ witness', (a, tl)) ->
              let (b, tl') = aux witness' tl in
              ((a, b), tl')
        in
        let stack = aux witness (accu, stack) in
        let (accu, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IUncomb (_, _, witness, k) ->
        let rec aux :
            type before after.
            (before, after) uncomb_gadt_witness -> before -> after =
         fun witness stack ->
          match (witness, stack) with
          | (Uncomb_one, stack) ->
              stack
          | (Uncomb_succ witness', ((a, b), tl)) ->
              (a, aux witness' (b, tl))
        in
        let stack = aux witness (accu, stack) in
        let (accu, stack) = stack in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IComb_get (_, _, witness, k) ->
        let comb = accu in
        let rec aux :
            type before after.
            (before, after) comb_get_gadt_witness -> before -> after =
         fun witness comb ->
          match (witness, comb) with
          | (Comb_get_zero, v) ->
              v
          | (Comb_get_one, (a, _)) ->
              a
          | (Comb_get_plus_two witness', (_, b)) ->
              aux witness' b
        in
        let accu = aux witness comb in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IComb_set (_, _, witness, k) ->
        let value = accu and (comb, stack) = stack in
        let rec aux :
            type value before after.
            (value, before, after) comb_set_gadt_witness ->
            value ->
            before ->
            after =
         fun witness value item ->
          match (witness, item) with
          | (Comb_set_zero, _) ->
              value
          | (Comb_set_one, (_hd, tl)) ->
              (value, tl)
          | (Comb_set_plus_two witness', (hd, tl)) ->
              (hd, aux witness' value tl)
        in
        let accu = aux witness value comb in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IDup_n (_, _, witness, k) ->
        let rec aux :
            type before after.
            (before, after) dup_n_gadt_witness -> before -> after =
         fun witness stack ->
          match (witness, stack) with
          | (Dup_n_zero, (a, _)) ->
              a
          | (Dup_n_succ witness', (_, tl)) ->
              aux witness' tl
        in
        let stack = (accu, stack) in
        let accu = aux witness stack in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    (* Tickets *)
    | ITicket (_, k) ->
        let contents = accu and (amount, stack) = stack in
        let ticketer = (sc.self, "default") in
        let accu = {ticketer; contents; amount} in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | IRead_ticket (_, k) ->
        let {ticketer; contents; amount} = accu in
        let stack = (accu, stack) in
        let accu = (ticketer, (contents, amount)) in
        (step [@ocaml.tailcall]) g gas k ks accu stack
    | ISplit_ticket (_, k) ->
        let ticket = accu and ((amount_a, amount_b), stack) = stack in
        let result =
          if
            Compare.Int.(
              Script_int.(compare (add_n amount_a amount_b) ticket.amount) = 0)
          then
            Some
              ({ticket with amount = amount_a}, {ticket with amount = amount_b})
          else None
        in
        (step [@ocaml.tailcall]) g gas k ks result stack
    | IJoin_tickets (_, contents_ty, k) ->
        let (ticket_a, ticket_b) = accu in
        let result =
          if
            Compare.Int.(
              compare_address ticket_a.ticketer ticket_b.ticketer = 0
              && compare_comparable
                   contents_ty
                   ticket_a.contents
                   ticket_b.contents
                 = 0)
          then
            Some
              {
                ticketer = ticket_a.ticketer;
                contents = ticket_a.contents;
                amount = Script_int.add_n ticket_a.amount ticket_b.amount;
              }
          else None
        in
        (step [@ocaml.tailcall]) g gas k ks result stack )

(*

   The following function inserts a logging instruction and modifies
   the continuation to continue the logging process in the next
   execution steps.

   This on-the-fly instrumentation of the execution allows zero-cost
   logging since logging instructions are only introduced if an
   initial logging continuation is pushed at the beginning of the
   evaluation.

*)
and log :
    type a s r f.
    logger ->
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, r, f) continuation ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger g gas ks0 accu stack ->
  log_control logger ks0 ;
  let enable_log ki = log_kinstr logger ki in
  match ks0 with
  | KCons (ki, ks') ->
      let log = enable_log ki in
      let ks = KLog (ks', logger) in
      (step [@ocaml.tailcall]) g gas log ks accu stack
  | KNil ->
      let ks = ks0 in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KLoop_in (ki, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KLoop_in (enable_log ki, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KReturn (stack', ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KReturn (stack', ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KLoop_in_left (ki, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KLoop_in_left (enable_log ki, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KUndip (x, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KUndip (x, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KIter (body, xs, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KIter (enable_log body, xs, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KList_enter_body (body, xs, ys, len, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KList_enter_body (body, xs, ys, len, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KList_exit_body (body, xs, ys, len, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KList_exit_body (body, xs, ys, len, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KMap_enter_body (body, xs, ys, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KMap_enter_body (body, xs, ys, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KMap_exit_body (body, xs, ys, yk, ks') ->
      let ks' = KLog (ks', logger) in
      let ks = KMap_exit_body (body, xs, ys, yk, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KLog (ks', _) ->
      (* This case should never happen. *)
      (next [@ocaml.tailcall]) g gas ks' accu stack

(*

  The following function pops n elements from the stack
  and push their reintroduction in the continuations stack.

 *)
and kundip :
    type a s e z c u d w b t.
    (a, s, e, z, c, u, d, w) stack_prefix_preservation_witness ->
    c ->
    u ->
    (d, w, b, t) kinstr ->
    a * s * (e, z, b, t) kinstr =
 fun w accu stack k ->
  match w with
  | KPrefix (kinfo, w) ->
      let k = IConst (kinfo, accu, k) in
      let (accu, stack) = stack in
      kundip w accu stack k
  | KRest ->
      (accu, stack, k)

(** [apply ctxt gas ty v lam] specializes [lam] by fixing its first
    formal argument to [v]. The type of [v] is represented by [ty]. *)
and apply :
    type a b c.
    outdated_context ->
    local_gas_counter ->
    a ty ->
    a ->
    (a * b, c) lambda ->
    ((b, c) lambda * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun ctxt gas capture_ty capture lam ->
  let (Lam (descr, expr)) = lam in
  let (Item_t (full_arg_ty, _, _)) = descr.kbef in
  let ctxt = update_context gas ctxt in
  unparse_data ctxt Optimized capture_ty capture
  >>=? fun (const_expr, ctxt) ->
  unparse_ty ctxt capture_ty
  >>?= fun (ty_expr, ctxt) ->
  match full_arg_ty with
  | Pair_t ((capture_ty, _, _), (arg_ty, _, _), _) ->
      let arg_stack_ty = Item_t (arg_ty, Bot_t, None) in
      let full_descr =
        {
          kloc = descr.kloc;
          kbef = arg_stack_ty;
          kaft = descr.kaft;
          kinstr =
            (let kinfo_const = {iloc = descr.kloc; kstack_ty = arg_stack_ty} in
             let kinfo_pair =
               {
                 iloc = descr.kloc;
                 kstack_ty = Item_t (capture_ty, arg_stack_ty, None);
               }
             in
             IConst
               (kinfo_const, capture, ICons_pair (kinfo_pair, descr.kinstr)));
        }
      in
      let full_expr =
        Micheline.Seq
          ( 0,
            [ Prim (0, I_PUSH, [ty_expr; const_expr], []);
              Prim (0, I_PAIR, [], []);
              expr ] )
      in
      let lam' = Lam (full_descr, full_expr) in
      let gas = update_local_gas_counter ctxt in
      return (lam', outdated ctxt, gas)
  | _ ->
      assert false

(** [transfer (ctxt, sc) gas tez tp p destination entrypoint]
    creates an operation that transfers an amount of [tez] to
    a contract determined by [(destination, entrypoint)]
    instantiated with argument [p] of type [tp]. *)
and transfer :
    type a.
    outdated_context * step_constants ->
    local_gas_counter ->
    Tez.t ->
    a ty ->
    a ->
    Contract.t ->
    string ->
    (operation * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun (ctxt, sc) gas amount tp p destination entrypoint ->
  let ctxt = update_context gas ctxt in
  collect_lazy_storage ctxt tp p
  >>?= fun (to_duplicate, ctxt) ->
  let to_update = no_lazy_storage_id in
  extract_lazy_storage_diff
    ctxt
    Optimized
    tp
    p
    ~to_duplicate
    ~to_update
    ~temporary:true
  >>=? fun (p, lazy_storage_diff, ctxt) ->
  unparse_data ctxt Optimized tp p
  >>=? fun (p, ctxt) ->
  Gas.consume ctxt (Script.strip_locations_cost p)
  >>?= fun ctxt ->
  let operation =
    Transaction
      {
        amount;
        destination;
        entrypoint;
        parameters = Script.lazy_expr (Micheline.strip_locations p);
      }
  in
  fresh_internal_nonce ctxt
  >>?= fun (ctxt, nonce) ->
  let iop = {source = sc.self; operation; nonce} in
  let res = (Internal_operation iop, lazy_storage_diff) in
  let gas = update_local_gas_counter ctxt in
  let ctxt = outdated ctxt in
  return (res, ctxt, gas)

(** [create_contract (ctxt, sc) gas storage_ty param_ty code root_name
    delegate credit init] creates an origination operation for a
    contract represented by [code], with some [root_name], some initial
    [credit] (taken to contract being executed), and an initial storage
    [init] of type [storage_ty]. The type of the new contract argument
    is [param_ty]. *)
and create_contract :
    type a b.
    outdated_context * step_constants ->
    local_gas_counter ->
    a ty ->
    b ty ->
    node ->
    field_annot option ->
    public_key_hash option ->
    Tez.t ->
    a ->
    (operation * Contract.t * outdated_context * local_gas_counter) tzresult
    Lwt.t =
 fun (ctxt, sc) gas storage_type param_type code root_name delegate credit init ->
  let ctxt = update_context gas ctxt in
  unparse_ty ctxt param_type
  >>?= fun (unparsed_param_type, ctxt) ->
  let unparsed_param_type =
    Script_ir_translator.add_field_annot root_name None unparsed_param_type
  in
  unparse_ty ctxt storage_type
  >>?= fun (unparsed_storage_type, ctxt) ->
  let code =
    Micheline.strip_locations
      (Seq
         ( 0,
           [ Prim (0, K_parameter, [unparsed_param_type], []);
             Prim (0, K_storage, [unparsed_storage_type], []);
             Prim (0, K_code, [code], []) ] ))
  in
  collect_lazy_storage ctxt storage_type init
  >>?= fun (to_duplicate, ctxt) ->
  let to_update = no_lazy_storage_id in
  extract_lazy_storage_diff
    ctxt
    Optimized
    storage_type
    init
    ~to_duplicate
    ~to_update
    ~temporary:true
  >>=? fun (init, lazy_storage_diff, ctxt) ->
  unparse_data ctxt Optimized storage_type init
  >>=? fun (storage, ctxt) ->
  Gas.consume ctxt (Script.strip_locations_cost storage)
  >>?= fun ctxt ->
  let storage = Micheline.strip_locations storage in
  Contract.fresh_contract_from_current_nonce ctxt
  >>?= fun (ctxt, contract) ->
  let operation =
    Origination
      {
        credit;
        delegate;
        preorigination = Some contract;
        script =
          {code = Script.lazy_expr code; storage = Script.lazy_expr storage};
      }
  in
  fresh_internal_nonce ctxt
  >>?= fun (ctxt, nonce) ->
  let res =
    (Internal_operation {source = sc.self; operation; nonce}, lazy_storage_diff)
  in
  let gas = update_local_gas_counter ctxt in
  let ctxt = outdated ctxt in
  return (res, contract, ctxt, gas)

and unpack :
    type a.
    context -> ty:a ty -> bytes:bytes -> (a option * context) tzresult Lwt.t =
 fun ctxt ~ty ~bytes ->
  Gas.check_enough ctxt (Script.serialized_cost bytes)
  >>?= fun () ->
  if
    Compare.Int.(Bytes.length bytes >= 1)
    && Compare.Int.(TzEndian.get_uint8 bytes 0 = 0x05)
  then
    let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
    match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
    | None ->
        Lwt.return
          ( Gas.consume ctxt (Interp_costs.unpack_failed bytes)
          >|? fun ctxt -> (None, ctxt) )
    | Some expr -> (
        Gas.consume ctxt (Script.deserialized_cost expr)
        >>?= fun ctxt ->
        parse_data
          ctxt
          ~legacy:false
          ~allow_forged:false
          ty
          (Micheline.root expr)
        >|= function
        | Ok (value, ctxt) ->
            ok (Some value, ctxt)
        | Error _ignored ->
            Gas.consume ctxt (Interp_costs.unpack_failed bytes)
            >|? fun ctxt -> (None, ctxt) )
  else return (None, ctxt)

and log_kinstr :
    type a s r f. logger -> (a, s, r, f) kinstr -> (a, s, r, f) kinstr =
 fun logger i ->
  let set_logger_for_trace_dependent_kinstr :
      type a s r f. (a, s, r, f) kinstr -> (a, s, r, f) kinstr = function
    | IFailwith (kinfo, kloc, tv, _, k) ->
        IFailwith (kinfo, kloc, tv, Some logger, k)
    | IExec (kinfo, _, k) ->
        IExec (kinfo, Some logger, k)
    | IMul_teznat (kinfo, _, k) ->
        IMul_teznat (kinfo, Some logger, k)
    | IMul_nattez (kinfo, _, k) ->
        IMul_nattez (kinfo, Some logger, k)
    | ILsr_nat (kinfo, _, k) ->
        ILsr_nat (kinfo, Some logger, k)
    | ILsl_nat (kinfo, _, k) ->
        ILsl_nat (kinfo, Some logger, k)
    | i ->
        i
  in
  ILog
    ( kinfo_of_kinstr i,
      LogEntry,
      logger,
      set_logger_for_trace_dependent_kinstr i )

and log_next_kinstr :
    type a s r f. logger -> (a, s, r, f) kinstr -> (a, s, r, f) kinstr =
 fun logger i ->
  let apply k =
    ILog
      ( kinfo_of_kinstr k,
        LogExit (kinfo_of_kinstr i),
        logger,
        log_kinstr logger k )
  in
  kinstr_rewritek i {apply}

let run_descr logger (ctxt, sc) descr accu stack =
  let gas = (Gas.gas_counter ctxt :> int) in
  ( match logger with
  | None ->
      step (outdated ctxt, sc) gas descr.kinstr KNil accu stack
  | Some logger ->
      let log =
        ILog (kinfo_of_kinstr descr.kinstr, LogEntry, logger, descr.kinstr)
      in
      step (outdated ctxt, sc) gas log KNil accu stack )
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let step_descr log_now logger g descr accu stack =
  ( if log_now then
    match logger with
    | None ->
        ()
    | Some logger ->
        let kinfo = kinfo_of_kinstr descr.kinstr in
        let ctxt = fst g in
        logger.log_interp descr.kinstr ctxt kinfo.iloc descr.kbef (accu, stack)
  ) ;
  run_descr logger g descr accu stack

let interp logger g (Lam (code, _)) arg =
  step_descr true logger g code arg ((), ())
  >|=? fun (ret, _, ctxt) -> (ret, ctxt)

let kstep logger ctxt step_constants kinstr accu stack =
  let gas = (Gas.gas_counter ctxt :> int) in
  let kinstr =
    match logger with
    | None ->
        kinstr
    | Some logger ->
        ILog (kinfo_of_kinstr kinstr, LogEntry, logger, kinstr)
  in
  step (outdated ctxt, step_constants) gas kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let step logger ctxt step_constants descr stack =
  step_descr false logger (ctxt, step_constants) descr stack

(*

   High-level functions
   ====================

*)
let execute logger ctxt mode step_constants ~entrypoint ~internal
    unparsed_script arg :
    ( Script.expr
    * packed_internal_operation list
    * context
    * Lazy_storage.diffs option )
    tzresult
    Lwt.t =
  parse_script ctxt unparsed_script ~legacy:true ~allow_forged_in_storage:true
  >>=? fun (Ex_script {code; arg_type; storage; storage_type; root_name}, ctxt) ->
  record_trace
    (Bad_contract_parameter step_constants.self)
    (find_entrypoint arg_type ~root_name entrypoint)
  >>?= fun (box, _) ->
  trace
    (Bad_contract_parameter step_constants.self)
    (parse_data ctxt ~legacy:false ~allow_forged:internal arg_type (box arg))
  >>=? fun (arg, ctxt) ->
  Script.force_decode_in_context ctxt unparsed_script.code
  >>?= fun (script_code, ctxt) ->
  Script_ir_translator.collect_lazy_storage ctxt arg_type arg
  >>?= fun (to_duplicate, ctxt) ->
  Script_ir_translator.collect_lazy_storage ctxt storage_type storage
  >>?= fun (to_update, ctxt) ->
  trace
    (Runtime_contract_error (step_constants.self, script_code))
    (interp logger (ctxt, step_constants) code (arg, storage))
  >>=? fun ((ops, storage), ctxt) ->
  Script_ir_translator.extract_lazy_storage_diff
    ctxt
    mode
    ~temporary:false
    ~to_duplicate
    ~to_update
    storage_type
    storage
  >>=? fun (storage, lazy_storage_diff, ctxt) ->
  trace
    Cannot_serialize_storage
    ( unparse_data ctxt mode storage_type storage
    >>=? fun (storage, ctxt) ->
    Lwt.return
      ( Gas.consume ctxt (Script.strip_locations_cost storage)
      >>? fun ctxt -> ok (Micheline.strip_locations storage, ctxt) ) )
  >|=? fun (storage, ctxt) ->
  let (ops, op_diffs) = List.split ops.elements in
  let lazy_storage_diff =
    match
      List.flatten
        (List.map (Option.value ~default:[]) (op_diffs @ [lazy_storage_diff]))
    with
    | [] ->
        None
    | diff ->
        Some diff
  in
  (storage, ops, ctxt, lazy_storage_diff)

type execution_result = {
  ctxt : context;
  storage : Script.expr;
  lazy_storage_diff : Lazy_storage.diffs option;
  operations : packed_internal_operation list;
}

let execute ?logger ctxt mode step_constants ~script ~entrypoint ~parameter
    ~internal =
  execute
    logger
    ctxt
    mode
    step_constants
    ~entrypoint
    ~internal
    script
    (Micheline.root parameter)
  >|=? fun (storage, operations, ctxt, lazy_storage_diff) ->
  {ctxt; storage; lazy_storage_diff; operations}

(*

   We export the internals definitions for tool that requires
   a white-box view on the interpreter, typically snoop, the
   gas model inference engine.

*)
module Internals = struct
  type nonrec local_gas_counter = local_gas_counter

  type nonrec outdated_context = outdated_context =
    | OutDatedContext of Alpha_context.t
  [@@unboxed]

  let next logger g gas ks accu stack =
    let ks =
      match logger with None -> ks | Some logger -> KLog (ks, logger)
    in
    next g gas ks accu stack
end
