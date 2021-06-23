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

(*

   This module provides auxiliary definitions used in the interpreter.

   These are internal private definitions. Do not rely on them outside
   the interpreter.

*)

open Alpha_context
open Script
open Script_typed_ir
open Script_ir_translator

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
      Interp_costs.add_timestamp_seconds t n
  | ISub_timestamp_seconds _ ->
      let t = accu and (n, _) = stack in
      Interp_costs.sub_timestamp_seconds t n
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
  | IMul_teznat _ -> Interp_costs.mul_teznat
  | IMul_nattez _ -> Interp_costs.mul_nattez
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
      Interp_costs.add_intint x y
  | IAdd_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_intnat x y
  | IAdd_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_natint x y
  | IAdd_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.add_natnat x y
  | ISub_int _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.sub_int x y
  | IMul_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_intint x y
  | IMul_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_intnat x y
  | IMul_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_natint x y
  | IMul_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.mul_natnat x y
  | IEdiv_teznat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_teznat x y
  | IEdiv_intint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_intint x y
  | IEdiv_intnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_intnat x y
  | IEdiv_natint _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_natint x y
  | IEdiv_natnat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.ediv_natnat x y
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
      Interp_costs.and_int_nat x y
  | IXor_nat _ ->
      let x = accu and (y, _) = stack in
      Interp_costs.xor_nat x y
  | INot_int _ ->
      let x = accu in
      Interp_costs.not_int x
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
  | IHalt _ -> Interp_costs.halt
  | IDrop _ -> Interp_costs.drop
  | IDup _ -> Interp_costs.dup
  | ISwap _ -> Interp_costs.swap
  | IConst _ -> Interp_costs.const
  | ICons_some _ -> Interp_costs.cons_some
  | ICons_none _ -> Interp_costs.cons_none
  | IIf_none _ -> Interp_costs.if_none
  | ICons_pair _ -> Interp_costs.cons_pair
  | IUnpair _ -> Interp_costs.unpair
  | ICar _ -> Interp_costs.car
  | ICdr _ -> Interp_costs.cdr
  | ICons_left _ -> Interp_costs.cons_left
  | ICons_right _ -> Interp_costs.cons_right
  | IIf_left _ -> Interp_costs.if_left
  | ICons_list _ -> Interp_costs.cons_list
  | INil _ -> Interp_costs.nil
  | IIf_cons _ -> Interp_costs.if_cons
  | IList_size _ -> Interp_costs.list_size
  | IEmpty_set _ -> Interp_costs.empty_set
  | ISet_size _ -> Interp_costs.set_size
  | IEmpty_map _ -> Interp_costs.empty_map
  | IMap_size _ -> Interp_costs.map_size
  | IEmpty_big_map _ -> Interp_costs.empty_big_map
  | IString_size _ -> Interp_costs.string_size
  | IBytes_size _ -> Interp_costs.bytes_size
  | IAdd_tez _ -> Interp_costs.add_tez
  | ISub_tez _ -> Interp_costs.sub_tez
  | IOr _ -> Interp_costs.bool_or
  | IAnd _ -> Interp_costs.bool_and
  | IXor _ -> Interp_costs.bool_xor
  | INot _ -> Interp_costs.bool_not
  | IIs_nat _ -> Interp_costs.is_nat
  | IInt_nat _ -> Interp_costs.int_nat
  | IInt_bls12_381_fr _ -> Interp_costs.int_bls12_381_fr
  | IEdiv_tez _ -> Interp_costs.ediv_tez
  | IIf _ -> Interp_costs.if_
  | ILoop _ -> Interp_costs.loop
  | ILoop_left _ -> Interp_costs.loop_left
  | IDip _ -> Interp_costs.dip
  | IExec _ -> Interp_costs.exec
  | IApply _ -> Interp_costs.apply
  | ILambda _ -> Interp_costs.lambda
  | IFailwith _ -> Gas.free
  | IEq _ -> Interp_costs.eq
  | INeq _ -> Interp_costs.neq
  | ILt _ -> Interp_costs.lt
  | ILe _ -> Interp_costs.le
  | IGt _ -> Interp_costs.gt
  | IGe _ -> Interp_costs.ge
  | IPack _ -> Gas.free
  | IUnpack _ ->
      let b = accu in
      Interp_costs.unpack b
  | IAddress _ -> Interp_costs.address
  | IContract _ -> Interp_costs.contract
  | ITransfer_tokens _ -> Interp_costs.transfer_tokens
  | IView _ -> Interp_costs.view
  | IImplicit_account _ -> Interp_costs.implicit_account
  | ISet_delegate _ -> Interp_costs.set_delegate
  | IBalance _ -> Interp_costs.balance
  | ILevel _ -> Interp_costs.level
  | INow _ -> Interp_costs.now
  | ISapling_empty_state _ -> Interp_costs.sapling_empty_state
  | ISource _ -> Interp_costs.source
  | ISender _ -> Interp_costs.sender
  | ISelf _ -> Interp_costs.self
  | ISelf_address _ -> Interp_costs.self_address
  | IAmount _ -> Interp_costs.amount
  | IDig (_, n, _, _) -> Interp_costs.dign n
  | IDug (_, n, _, _) -> Interp_costs.dugn n
  | IDipn (_, n, _, _, _) -> Interp_costs.dipn n
  | IDropn (_, n, _, _) -> Interp_costs.dropn n
  | IChainId _ -> Interp_costs.chain_id
  | ICreate_contract _ -> Interp_costs.create_contract
  | INever _ -> ( match accu with _ -> .)
  | IVoting_power _ -> Interp_costs.voting_power
  | ITotal_voting_power _ -> Interp_costs.total_voting_power
  | IAdd_bls12_381_g1 _ -> Interp_costs.add_bls12_381_g1
  | IAdd_bls12_381_g2 _ -> Interp_costs.add_bls12_381_g2
  | IAdd_bls12_381_fr _ -> Interp_costs.add_bls12_381_fr
  | IMul_bls12_381_g1 _ -> Interp_costs.mul_bls12_381_g1
  | IMul_bls12_381_g2 _ -> Interp_costs.mul_bls12_381_g2
  | IMul_bls12_381_fr _ -> Interp_costs.mul_bls12_381_fr
  | INeg_bls12_381_g1 _ -> Interp_costs.neg_bls12_381_g1
  | INeg_bls12_381_g2 _ -> Interp_costs.neg_bls12_381_g2
  | INeg_bls12_381_fr _ -> Interp_costs.neg_bls12_381_fr
  | IMul_bls12_381_fr_z _ ->
      let z = accu in
      Interp_costs.mul_bls12_381_fr_z z
  | IMul_bls12_381_z_fr _ ->
      let (z, _) = stack in
      Interp_costs.mul_bls12_381_z_fr z
  | IDup_n (_, n, _, _) -> Interp_costs.dupn n
  | IComb (_, n, _, _) -> Interp_costs.comb n
  | IUncomb (_, n, _, _) -> Interp_costs.uncomb n
  | IComb_get (_, n, _, _) -> Interp_costs.comb_get n
  | IComb_set (_, n, _, _) -> Interp_costs.comb_set n
  | ITicket _ -> Interp_costs.ticket
  | IRead_ticket _ -> Interp_costs.read_ticket
  | IOpen_chest _ ->
      let _chest_key = accu and (chest, (time, _)) = stack in
      Interp_costs.open_chest chest (Alpha_context.Script_int.to_zint time)
  | ILog _ -> Gas.free
 [@@ocaml.inline always]
 [@@coq_axiom_with_reason "unreachable expression `.` not handled"]

let cost_of_control : type a s r f. (a, s, r, f) continuation -> Gas.cost =
 fun ks ->
  match ks with
  | KLog _ -> Gas.free
  | KNil -> Interp_costs.Control.nil
  | KCons (_, _) -> Interp_costs.Control.cons
  | KReturn _ -> Interp_costs.Control.return
  | KUndip (_, _) -> Interp_costs.Control.undip
  | KLoop_in (_, _) -> Interp_costs.Control.loop_in
  | KLoop_in_left (_, _) -> Interp_costs.Control.loop_in_left
  | KIter (_, _, _) -> Interp_costs.Control.iter
  | KList_enter_body (_, xs, _, len, _) ->
      Interp_costs.Control.list_enter_body xs len
  | KList_exit_body (_, _, _, _, _) -> Interp_costs.Control.list_exit_body
  | KMap_enter_body (_, _, _, _) -> Interp_costs.Control.map_enter_body
  | KMap_exit_body (_, _, map, key, _) ->
      Interp_costs.Control.map_exit_body key map

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
      Gas.update_remaining_operation_gas
        ctxt
        (Saturation_repr.safe_int local_gas_counter)
  [@@ocaml.inline always]

let update_local_gas_counter ctxt =
  (Gas.remaining_operation_gas ctxt :> int)
  [@@ocaml.inline always]

let outdated ctxt = OutDatedContext ctxt [@@ocaml.inline always]

let context_from_outdated_context (OutDatedContext ctxt) =
  ctxt
  [@@ocaml.inline always]

let use_gas_counter_in_ctxt ctxt local_gas_counter f =
  let ctxt = update_context local_gas_counter ctxt in
  f ctxt >>=? fun (y, ctxt) ->
  return (y, outdated ctxt, update_local_gas_counter ctxt)
  [@@ocaml.inline always]

(*

   [step] calls [consume] at the beginning of each execution step.

   [consume'] is used in the implementation of [IConcat_string]
   and [IConcat_bytes] because in that special cases, the cost
   is expressed with respect to a non-constant-time computation
   on the inputs.

*)

let update_and_check gas_counter (cost : Gas.cost) =
  let gas_counter = gas_counter - (cost :> int) in
  if Compare.Int.(gas_counter < 0) then None else Some gas_counter
  [@@ocaml.inline always]

let consume local_gas_counter k accu stack =
  let cost = cost_of_instr k accu stack in
  update_and_check local_gas_counter cost
  [@@ocaml.inline always]

let consume' ctxt local_gas_counter cost =
  match update_and_check local_gas_counter cost with
  | None -> Gas.gas_exhausted_error (update_context local_gas_counter ctxt)
  | Some local_gas_counter -> Ok local_gas_counter
  [@@ocaml.inline always]

let consume_control local_gas_counter ks =
  let cost = cost_of_control ks in
  update_and_check local_gas_counter cost
  [@@ocaml.inline always]

(*

   Auxiliary functions used by the instrumentation
   ===============================================

*)

let log_entry logger ctxt gas k accu stack =
  let kinfo = kinfo_of_kinstr k in
  let ctxt = update_context gas ctxt in
  logger.log_entry k ctxt kinfo.iloc kinfo.kstack_ty (accu, stack)

let log_exit logger ctxt gas kinfo_prev k accu stack =
  let kinfo = kinfo_of_kinstr k in
  let ctxt = update_context gas ctxt in
  logger.log_exit k ctxt kinfo_prev.iloc kinfo.kstack_ty (accu, stack)

let log_control logger ks = logger.log_control ks

let get_log = function
  | None -> Lwt.return (Ok None)
  | Some logger -> logger.get_log ()
  [@@ocaml.inline always]

(* [log_kinstr logger i] emits an instruction to instrument the
   execution of [i] with [logger]. *)
let log_kinstr logger i = ILog (kinfo_of_kinstr i, LogEntry, logger, i)

(* [log_next_kinstr logger i] instruments the next instruction of [i]
   with the [logger].

   Notice that the instrumentation breaks the sharing of continuations
   that is normally enforced between branches of conditionals. This
   has a performance cost. Anyway, the instrumentation allocates many
   new [ILog] instructions and [KLog] continuations which makes
   the execution of instrumented code significantly slower than
   non-instrumented code. "Zero-cost logging" means that the normal
   non-instrumented execution is not impacted by the ability to
   instrument it, not that the logging itself has no cost.

*)
let log_next_kinstr logger i =
  let apply k =
    ILog
      ( kinfo_of_kinstr k,
        LogExit (kinfo_of_kinstr i),
        logger,
        log_kinstr logger k )
  in
  kinstr_rewritek i {apply}

(* We pass the identity function when no instrumentation is needed. *)
let id x = x [@@inline]

(*

    Interpreter parameters
    ======================

*)
type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

(*

   Auxiliary functions used by the interpretation loop
   ===================================================

*)

(* The following function pops n elements from the stack
   and push their reintroduction in the continuations stack. *)
let rec kundip :
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
  | KRest -> (accu, stack, k)

(* [apply ctxt gas ty v lam] specializes [lam] by fixing its first
   formal argument to [v]. The type of [v] is represented by [ty]. *)
let apply ctxt gas capture_ty capture lam =
  let (Lam (descr, expr)) = lam in
  let (Item_t (full_arg_ty, _, _)) = descr.kbef in
  let ctxt = update_context gas ctxt in
  unparse_data ctxt Optimized capture_ty capture >>=? fun (const_expr, ctxt) ->
  unparse_ty ctxt capture_ty >>?= fun (ty_expr, ctxt) ->
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
             IConst (kinfo_const, capture, ICons_pair (kinfo_pair, descr.kinstr)));
        }
      in
      let full_expr =
        Micheline.Seq
          ( 0,
            [
              Prim (0, I_PUSH, [ty_expr; const_expr], []);
              Prim (0, I_PAIR, [], []);
              expr;
            ] )
      in
      let lam' = Lam (full_descr, full_expr) in
      let gas = update_local_gas_counter ctxt in
      return (lam', outdated ctxt, gas)
  | _ -> assert false

(* [transfer (ctxt, sc) gas tez tp p destination entrypoint]
   creates an operation that transfers an amount of [tez] to
   a contract determined by [(destination, entrypoint)]
   instantiated with argument [p] of type [tp]. *)
let transfer (ctxt, sc) gas amount tp p destination entrypoint =
  let ctxt = update_context gas ctxt in
  collect_lazy_storage ctxt tp p >>?= fun (to_duplicate, ctxt) ->
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
  unparse_data ctxt Optimized tp p >>=? fun (p, ctxt) ->
  Gas.consume ctxt (Script.strip_locations_cost p) >>?= fun ctxt ->
  let operation =
    Transaction
      {
        amount;
        destination;
        entrypoint;
        parameters = Script.lazy_expr (Micheline.strip_locations p);
      }
  in
  fresh_internal_nonce ctxt >>?= fun (ctxt, nonce) ->
  let iop = {source = sc.self; operation; nonce} in
  let res = (Internal_operation iop, lazy_storage_diff) in
  let gas = update_local_gas_counter ctxt in
  let ctxt = outdated ctxt in
  return (res, ctxt, gas)

(* [create_contract (ctxt, sc) gas storage_ty param_ty code root_name
   delegate credit init] creates an origination operation for a
   contract represented by [code], with some [root_name], some initial
   [credit] (taken to contract being executed), and an initial storage
   [init] of type [storage_ty]. The type of the new contract argument
   is [param_ty]. *)
let create_contract (ctxt, sc) gas storage_type param_type code root_name
    delegate credit init =
  let ctxt = update_context gas ctxt in
  unparse_ty ctxt param_type >>?= fun (unparsed_param_type, ctxt) ->
  let unparsed_param_type =
    Script_ir_translator.add_field_annot root_name None unparsed_param_type
  in
  unparse_ty ctxt storage_type >>?= fun (unparsed_storage_type, ctxt) ->
  let code =
    Micheline.strip_locations
      (Seq
         ( 0,
           [
             Prim (0, K_parameter, [unparsed_param_type], []);
             Prim (0, K_storage, [unparsed_storage_type], []);
             Prim (0, K_code, [code], []);
           ] ))
  in
  collect_lazy_storage ctxt storage_type init >>?= fun (to_duplicate, ctxt) ->
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
  unparse_data ctxt Optimized storage_type init >>=? fun (storage, ctxt) ->
  Gas.consume ctxt (Script.strip_locations_cost storage) >>?= fun ctxt ->
  let storage = Micheline.strip_locations storage in
  Contract.fresh_contract_from_current_nonce ctxt >>?= fun (ctxt, contract) ->
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
  fresh_internal_nonce ctxt >>?= fun (ctxt, nonce) ->
  let res =
    (Internal_operation {source = sc.self; operation; nonce}, lazy_storage_diff)
  in
  let gas = update_local_gas_counter ctxt in
  let ctxt = outdated ctxt in
  return (res, contract, ctxt, gas)

(* [unpack ctxt ty bytes] deserialize [bytes] into a value of type [ty]. *)
let unpack ctxt ~ty ~bytes =
  Gas.consume
    ctxt
    (Script.deserialization_cost_estimated_from_bytes (Bytes.length bytes))
  >>?= fun ctxt ->
  if
    Compare.Int.(Bytes.length bytes >= 1)
    && Compare.Int.(TzEndian.get_uint8 bytes 0 = 0x05)
  then
    let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
    match Data_encoding.Binary.of_bytes_opt Script.expr_encoding bytes with
    | None ->
        Lwt.return
          ( Gas.consume ctxt (Interp_costs.unpack_failed bytes) >|? fun ctxt ->
            (None, ctxt) )
    | Some expr -> (
        parse_data
          ctxt
          ~legacy:false
          ~allow_forged:false
          ty
          (Micheline.root expr)
        >|= function
        | Ok (value, ctxt) -> ok (Some value, ctxt)
        | Error _ignored ->
            Gas.consume ctxt (Interp_costs.unpack_failed bytes) >|? fun ctxt ->
            (None, ctxt))
  else return (None, ctxt)

(* [interp_stack_prefix_preserving_operation f w accu stack] applies
   a well-typed operation [f] under some prefix of the A-stack
   exploiting [w] to justify that the shape of the stack is
   preserved. *)
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
  | (KRest, v) -> f accu v

(*

   Some auxiliary functions have complex types and must be annotated
   because of GADTs and polymorphic recursion.

   To improve readibility, we introduce their types as abbreviations:

*)

type ('a, 's, 'b, 't, 'r, 'f) step_type =
  outdated_context * step_constants ->
  local_gas_counter ->
  ('a, 's, 'b, 't) kinstr ->
  ('b, 't, 'r, 'f) continuation ->
  'a ->
  's ->
  ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'm, 'n, 'o) kmap_exit_type =
  (('c, 'd, 'e, 'f) continuation -> ('a, 'b, 'g, 'h) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('m * 'n, 'c * 'd, 'o, 'c * 'd) kinstr * ('m * 'n) list * ('m, 'o) map * 'm ->
  (('m, 'o) map, 'c * 'd, 'e, 'f) continuation ->
  'o ->
  'a * 'b ->
  ('g * 'h * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'j, 'k) kmap_enter_type =
  (('a, 'b * 'c, 'd, 'e) continuation -> ('a, 'b * 'c, 'd, 'e) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('j * 'k, 'b * 'c, 'a, 'b * 'c) kinstr * ('j * 'k) list * ('j, 'a) map ->
  (('j, 'a) map, 'b * 'c, 'd, 'e) continuation ->
  'b ->
  'c ->
  ('d * 'e * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'i, 'j) klist_exit_type =
  (('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('i, 'a * 'b, 'j, 'a * 'b) kinstr * 'i list * 'j list * local_gas_counter ->
  ('j boxed_list, 'a * 'b, 'c, 'd) continuation ->
  'j ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'j) klist_enter_type =
  (('b, 'a * 'c, 'd, 'e) continuation -> ('b, 'a * 'c, 'd, 'e) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('j, 'a * 'c, 'b, 'a * 'c) kinstr * 'j list * 'b list * local_gas_counter ->
  ('b boxed_list, 'a * 'c, 'd, 'e) continuation ->
  'a ->
  'c ->
  ('d * 'e * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) kloop_in_left_type =
  outdated_context * step_constants ->
  local_gas_counter ->
  ('c, 'd, 'e, 'f) continuation ->
  ('a, 'g, 'c, 'd) kinstr ->
  ('b, 'g, 'e, 'f) continuation ->
  ('a, 'b) union ->
  'g ->
  ('e * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'r, 'f, 's) kloop_in_type =
  outdated_context * step_constants ->
  local_gas_counter ->
  ('b, 'c, 'r, 'f) continuation ->
  ('a, 's, 'b, 'c) kinstr ->
  ('a, 's, 'r, 'f) continuation ->
  bool ->
  'a * 's ->
  ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 's, 'r, 'f) kiter_type =
  (('a, 's, 'r, 'f) continuation -> ('a, 's, 'r, 'f) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('b, 'a * 's, 'a, 's) kinstr * 'b list ->
  ('a, 's, 'r, 'f) continuation ->
  'a ->
  's ->
  ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) ilist_map_type =
  (('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e, 'a * 'b, 'f, 'a * 'b) kinstr * ('f boxed_list, 'a * 'b, 'g, 'h) kinstr ->
  ('g, 'h, 'c, 'd) continuation ->
  'e boxed_list ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) ilist_iter_type =
  (('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e, 'a * 'b, 'a, 'b) kinstr * ('a, 'b, 'f, 'g) kinstr ->
  ('f, 'g, 'c, 'd) continuation ->
  'e boxed_list ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) iset_iter_type =
  (('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e, 'a * 'b, 'a, 'b) kinstr * ('a, 'b, 'f, 'g) kinstr ->
  ('f, 'g, 'c, 'd) continuation ->
  'e set ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) imap_map_type =
  (('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e * 'f, 'a * 'b, 'g, 'a * 'b) kinstr
  * (('e, 'g) map, 'a * 'b, 'h, 'i) kinstr ->
  ('h, 'i, 'c, 'd) continuation ->
  ('e, 'f) map ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) imap_iter_type =
  (('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation) ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e * 'f, 'a * 'b, 'a, 'b) kinstr * ('a, 'b, 'g, 'h) kinstr ->
  ('g, 'h, 'c, 'd) continuation ->
  ('e, 'f) map ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) imul_teznat_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  (Tez.t, 'a) kinfo * (Tez.t, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Tez.t ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) imul_nattez_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  (Script_int.n Script_int.num, 'a) kinfo * (Tez.t, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Script_int.n Script_int.num ->
  Tez.t * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) ilsl_nat_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  (Script_int.n Script_int.num, 'a) kinfo
  * (Script_int.n Script_int.num, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Script_int.n Script_int.num ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) ilsr_nat_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  (Script_int.n Script_int.num, 'a) kinfo
  * (Script_int.n Script_int.num, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Script_int.n Script_int.num ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b) ifailwith_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  int ->
  'a ty ->
  'a ->
  ('b, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) iexec_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  'g ->
  ('g, 'a) lambda * 'b ->
  ('e * 'f * outdated_context * local_gas_counter) tzresult Lwt.t
