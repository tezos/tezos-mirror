(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
open Local_gas_counter

type error += Rollup_invalid_transaction_amount | Rollup_invalid_entrypoint

let () =
  register_error_kind
    `Permanent
    ~id:"operation.rollup_invalid_transaction_amount"
    ~title:"Transaction amount to a rollup must be zero"
    ~description:
      "Because rollups are outside of the delegation mechanism of Tezos, they \
       cannot own Tez, and therefore transactions targeting a rollup must have \
       its amount field set to zero."
    ~pp:(fun ppf () ->
      Format.pp_print_string ppf "Transaction amount to a rollup must be zero.")
    Data_encoding.unit
    (function Rollup_invalid_transaction_amount -> Some () | _ -> None)
    (fun () -> Rollup_invalid_transaction_amount) ;
  register_error_kind
    `Permanent
    ~id:"operation.rollup_invalid_entrypoint"
    ~title:"Only the default entrypoint is allowed for rollups"
    ~description:"Rollups only support transactions to the default entrypoint."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "Rollups only support transactions to the default entrypoint.")
    Data_encoding.unit
    (function Rollup_invalid_entrypoint -> Some () | _ -> None)
    (fun () -> Rollup_invalid_entrypoint)

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
      let v = accu and set, _ = stack in
      Interp_costs.set_mem v set
  | ISet_update _ ->
      let v = accu and _, (set, _) = stack in
      Interp_costs.set_update v set
  | IMap_map _ ->
      let map = accu in
      Interp_costs.map_map map
  | IMap_iter _ ->
      let map = accu in
      Interp_costs.map_iter map
  | IMap_mem _ ->
      let v = accu and map, _ = stack in
      Interp_costs.map_mem v map
  | IMap_get _ ->
      let v = accu and map, _ = stack in
      Interp_costs.map_get v map
  | IMap_update _ ->
      let k = accu and _, (map, _) = stack in
      Interp_costs.map_update k map
  | IMap_get_and_update _ ->
      let k = accu and _, (map, _) = stack in
      Interp_costs.map_get_and_update k map
  | IBig_map_mem _ ->
      let Big_map map, _ = stack in
      Interp_costs.big_map_mem map.diff
  | IBig_map_get _ ->
      let Big_map map, _ = stack in
      Interp_costs.big_map_get map.diff
  | IBig_map_update _ ->
      let _, (Big_map map, _) = stack in
      Interp_costs.big_map_update map.diff
  | IBig_map_get_and_update _ ->
      let _, (Big_map map, _) = stack in
      Interp_costs.big_map_get_and_update map.diff
  | IAdd_seconds_to_timestamp _ ->
      let n = accu and t, _ = stack in
      Interp_costs.add_seconds_timestamp n t
  | IAdd_timestamp_to_seconds _ ->
      let t = accu and n, _ = stack in
      Interp_costs.add_timestamp_seconds t n
  | ISub_timestamp_seconds _ ->
      let t = accu and n, _ = stack in
      Interp_costs.sub_timestamp_seconds t n
  | IDiff_timestamps _ ->
      let t1 = accu and t2, _ = stack in
      Interp_costs.diff_timestamps t1 t2
  | IConcat_string_pair _ ->
      let x = accu and y, _ = stack in
      Interp_costs.concat_string_pair x y
  | IConcat_string _ ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | ISlice_string _ ->
      let (_offset : Script_int.n Script_int.num) = accu in
      let _length, (s, _) = stack in
      Interp_costs.slice_string s
  | IConcat_bytes_pair _ ->
      let x = accu and y, _ = stack in
      Interp_costs.concat_bytes_pair x y
  | IConcat_bytes _ ->
      let ss = accu in
      Interp_costs.concat_string_precheck ss
  | ISlice_bytes _ ->
      let _, (s, _) = stack in
      Interp_costs.slice_bytes s
  | IBytes_nat _ ->
      let n = accu in
      Interp_costs.bytes_nat n
  | INat_bytes _ ->
      let b = accu in
      Interp_costs.nat_bytes b
  | IBytes_int _ ->
      let n = accu in
      Interp_costs.bytes_int n
  | IInt_bytes _ ->
      let b = accu in
      Interp_costs.int_bytes b
  | IMul_teznat _ -> Interp_costs.mul_teznat
  | IMul_nattez _ -> Interp_costs.mul_nattez
  | IAbs_int _ ->
      let x = accu in
      Interp_costs.abs_int x
  | INeg _ ->
      let x = accu in
      Interp_costs.neg x
  | IAdd_int _ ->
      let x = accu and y, _ = stack in
      Interp_costs.add_int x y
  | IAdd_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.add_nat x y
  | ISub_int _ ->
      let x = accu and y, _ = stack in
      Interp_costs.sub_int x y
  | IMul_int _ ->
      let x = accu and y, _ = stack in
      Interp_costs.mul_int x y
  | IMul_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.mul_nat x y
  | IEdiv_teznat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.ediv_teznat x y
  | IEdiv_int _ ->
      let x = accu and y, _ = stack in
      Interp_costs.ediv_int x y
  | IEdiv_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.ediv_nat x y
  | ILsl_nat _ ->
      let x = accu in
      Interp_costs.lsl_nat x
  | ILsl_bytes _ ->
      let x = accu in
      let y, _ = stack in
      Interp_costs.lsl_bytes x y
  | ILsr_nat _ ->
      let x = accu in
      Interp_costs.lsr_nat x
  | ILsr_bytes _ ->
      let x = accu in
      let y, _ = stack in
      Interp_costs.lsr_bytes x y
  | IOr_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.or_nat x y
  | IOr_bytes _ ->
      let x = accu and y, _ = stack in
      Interp_costs.or_bytes x y
  | IAnd_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.and_nat x y
  | IAnd_int_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.and_int_nat x y
  | IAnd_bytes _ ->
      let x = accu and y, _ = stack in
      Interp_costs.and_bytes x y
  | IXor_nat _ ->
      let x = accu and y, _ = stack in
      Interp_costs.xor_nat x y
  | IXor_bytes _ ->
      let x = accu and y, _ = stack in
      Interp_costs.xor_bytes x y
  | INot_int _ ->
      let x = accu in
      Interp_costs.not_int x
  | INot_bytes _ ->
      let x = accu in
      Interp_costs.not_bytes x
  | ICompare (_, ty, _) ->
      let a = accu and b, _ = stack in
      Interp_costs.compare ty a b
  | ICheck_signature _ ->
      let key = accu and _, (message, _) = stack in
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
      let inputs = Gas_input_size.sapling_transaction_inputs tx in
      let outputs = Gas_input_size.sapling_transaction_outputs tx in
      let bound_data = Gas_input_size.sapling_transaction_bound_data tx in
      Interp_costs.sapling_verify_update ~inputs ~outputs ~bound_data
  | ISapling_verify_update_deprecated _ ->
      let tx = accu in
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update_deprecated ~inputs ~outputs
  | ISplit_ticket _ ->
      let (amount_a, amount_b), _ = stack in
      Interp_costs.split_ticket amount_a amount_b
  | IJoin_tickets (_, ty, _) ->
      let ticket_a, ticket_b = accu in
      Interp_costs.join_tickets ty ticket_a ticket_b
  | IHalt _ -> Interp_costs.halt
  | IDrop _ -> Interp_costs.drop
  | IDup _ -> Interp_costs.dup
  | ISwap _ -> Interp_costs.swap
  | IPush _ -> Interp_costs.push
  | IUnit _ -> Interp_costs.unit
  | ICons_some _ -> Interp_costs.cons_some
  | ICons_none _ -> Interp_costs.cons_none
  | IIf_none _ -> Interp_costs.if_none
  | IOpt_map _ -> Interp_costs.opt_map
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
  | ISub_tez_legacy _ -> Interp_costs.sub_tez_legacy
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
  | IApply _ -> (
      let l, _ = stack in
      match l with
      | Lam _ -> Interp_costs.apply ~rec_flag:false
      | LamRec _ -> Interp_costs.apply ~rec_flag:true)
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
  | IMin_block_time _ -> Interp_costs.min_block_time
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
      let z, _ = stack in
      Interp_costs.mul_bls12_381_z_fr z
  | IDup_n (_, n, _, _) -> Interp_costs.dupn n
  | IComb (_, n, _, _) -> Interp_costs.comb n
  | IUncomb (_, n, _, _) -> Interp_costs.uncomb n
  | IComb_get (_, n, _, _) -> Interp_costs.comb_get n
  | IComb_set (_, n, _, _) -> Interp_costs.comb_set n
  | ITicket _ | ITicket_deprecated _ -> Interp_costs.ticket
  | IRead_ticket _ -> Interp_costs.read_ticket
  | IOpen_chest _ ->
      let (_chest_key : Script_timelock.chest_key) = accu
      and chest, (time, _) = stack in
      Interp_costs.open_chest ~chest ~time:(Script_int.to_zint time)
  | IEmit _ -> Interp_costs.emit
  | ILog _ -> Gas.free
 [@@ocaml.inline always]

let cost_of_control : type a s r f. (a, s, r, f) continuation -> Gas.cost =
 fun ks ->
  match ks with
  | KLog _ -> Gas.free
  | KNil -> Interp_costs.Control.nil
  | KCons (_, _) -> Interp_costs.Control.cons
  | KReturn _ -> Interp_costs.Control.return
  | KMap_head (_, _) -> Interp_costs.Control.map_head
  | KUndip (_, _, _) -> Interp_costs.Control.undip
  | KLoop_in (_, _) -> Interp_costs.Control.loop_in
  | KLoop_in_left (_, _) -> Interp_costs.Control.loop_in_left
  | KIter (_, _, _, _) -> Interp_costs.Control.iter
  | KList_enter_body (_, xs, _, _, len, _) ->
      Interp_costs.Control.list_enter_body xs len
  | KList_exit_body (_, _, _, _, _, _) -> Interp_costs.Control.list_exit_body
  | KMap_enter_body (_, _, map, _, _) -> Interp_costs.Control.map_enter_body map
  | KMap_exit_body (_, _, map, key, _, _) ->
      Interp_costs.Control.map_exit_body key map
  | KView_exit (_, _) -> Interp_costs.Control.view_exit

(*

   [step] calls [consume_instr] at the beginning of each execution step.

   [Local_gas_counter.consume] is used in the implementation of
   [IConcat_string] and [IConcat_bytes] because in that special cases, the
   cost is expressed with respect to a non-constant-time computation on the
   inputs.

*)

let consume_instr local_gas_counter k accu stack =
  let cost = cost_of_instr k accu stack in
  consume_opt local_gas_counter cost
  [@@ocaml.inline always]

let consume_control local_gas_counter ks =
  let cost = cost_of_control ks in
  consume_opt local_gas_counter cost
  [@@ocaml.inline always]

let get_log = function None -> return_none | Some logger -> logger.get_log ()
  [@@ocaml.inline always]

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
    (d, w, b, t) continuation ->
    a * s * (e, z, b, t) continuation =
 fun w accu stack ks ->
  match w with
  | KPrefix (_loc, ty, w) ->
      let ks = KUndip (accu, Some ty, ks) in
      let accu, stack = stack in
      kundip w accu stack ks
  | KRest -> (accu, stack, ks)

(* [apply ctxt gas ty v lam] specializes [lam] by fixing its first
   formal argument to [v]. The type of [v] is represented by [ty]. *)
let apply ctxt gas capture_ty capture lam =
  let open Lwt_result_syntax in
  let loc = Micheline.dummy_location in
  let ctxt = update_context gas ctxt in
  let*? ty_expr, ctxt = Script_ir_unparser.unparse_ty ~loc ctxt capture_ty in
  let* const_expr, ctxt = unparse_data ctxt Optimized capture_ty capture in
  let make_expr expr =
    Micheline.(
      Seq
        ( loc,
          Prim (loc, I_PUSH, [ty_expr; Micheline.root const_expr], [])
          :: Prim (loc, I_PAIR, [], [])
          :: expr ))
  in
  let lam' =
    match lam with
    | LamRec (descr, expr) -> (
        let (Item_t (full_arg_ty, Item_t (Lambda_t (_, _, _), Bot_t))) =
          descr.kbef
        in
        let (Item_t (ret_ty, Bot_t)) = descr.kaft in
        let*? arg_ty_expr, ctxt =
          Script_ir_unparser.unparse_ty ~loc ctxt full_arg_ty
        in
        let*? ret_ty_expr, ctxt =
          Script_ir_unparser.unparse_ty ~loc ctxt ret_ty
        in
        match full_arg_ty with
        | Pair_t (capture_ty, arg_ty, _, _) ->
            let arg_stack_ty = Item_t (arg_ty, Bot_t) in
            (* To avoid duplicating the recursive lambda [lam], we
               return a regular lambda that builds the tuple of
               parameters and applies it to `lam`. Since `lam` is
               recursive it will push itself on top of the stack at
               execution time. *)
            let full_descr =
              {
                kloc = descr.kloc;
                kbef = arg_stack_ty;
                kaft = descr.kaft;
                kinstr =
                  IPush
                    ( descr.kloc,
                      capture_ty,
                      capture,
                      ICons_pair
                        ( descr.kloc,
                          ILambda
                            ( descr.kloc,
                              lam,
                              ISwap
                                ( descr.kloc,
                                  IExec
                                    ( descr.kloc,
                                      Some descr.kaft,
                                      IHalt descr.kloc ) ) ) ) );
              }
            in
            let full_expr =
              make_expr
                Micheline.
                  [
                    Prim
                      (loc, I_LAMBDA_REC, [arg_ty_expr; ret_ty_expr; expr], []);
                    Prim (loc, I_SWAP, [], []);
                    Prim (loc, I_EXEC, [], []);
                  ]
            in
            return (Lam (full_descr, full_expr), ctxt))
    | Lam (descr, expr) -> (
        let (Item_t (full_arg_ty, Bot_t)) = descr.kbef in
        match full_arg_ty with
        | Pair_t (capture_ty, arg_ty, _, _) ->
            let arg_stack_ty = Item_t (arg_ty, Bot_t) in
            let full_descr =
              {
                kloc = descr.kloc;
                kbef = arg_stack_ty;
                kaft = descr.kaft;
                kinstr =
                  IPush
                    ( descr.kloc,
                      capture_ty,
                      capture,
                      ICons_pair (descr.kloc, descr.kinstr) );
              }
            in
            let full_expr = make_expr [expr] in
            return (Lam (full_descr, full_expr), ctxt))
  in
  let* lam', ctxt = lam' in
  let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
  return (lam', ctxt, gas)

let make_transaction_to_sc_rollup ctxt ~destination ~amount ~entrypoint
    ~parameters_ty ~parameters =
  let open Lwt_result_syntax in
  let*? () =
    error_unless Tez.(amount = zero) Rollup_invalid_transaction_amount
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4023
     We currently don't support entrypoints as the entrypoint information
     for L1 to L2 messages is not propagated to the rollup. *)
  let*? () =
    error_unless (Entrypoint.is_default entrypoint) Rollup_invalid_entrypoint
  in
  let+ unparsed_parameters, ctxt =
    unparse_data ctxt Optimized parameters_ty parameters
  in
  ( Transaction_to_sc_rollup
      {destination; entrypoint; parameters_ty; parameters; unparsed_parameters},
    ctxt )

(** [emit_event] generates an internal operation that will effect an event emission
    if the contract code returns this successfully. *)
let emit_event (type t tc) (ctxt, sc) gas ~(event_type : (t, tc) ty)
    ~unparsed_ty ~tag ~(event_data : t) =
  let open Lwt_result_syntax in
  let ctxt = update_context gas ctxt in
  (* No need to take care of lazy storage as only packable types are allowed *)
  let lazy_storage_diff = None in
  let* unparsed_data, ctxt =
    unparse_data ctxt Optimized event_type event_data
  in
  let*? ctxt, nonce = fresh_internal_nonce ctxt in
  let operation = Event {ty = unparsed_ty; tag; unparsed_data} in
  let iop =
    {
      sender = Destination.Contract (Contract.Originated sc.self);
      operation;
      nonce;
    }
  in
  let res = {piop = Internal_operation iop; lazy_storage_diff} in
  let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
  return (res, ctxt, gas)

let make_transaction_to_zk_rollup (type t) ctxt ~destination ~amount
    ~(parameters_ty : ((t ticket, bytes) pair, _) ty) ~parameters =
  let open Lwt_result_syntax in
  let*? () =
    error_unless Tez.(amount = zero) Rollup_invalid_transaction_amount
  in
  let+ unparsed_parameters, ctxt =
    unparse_data ctxt Optimized parameters_ty parameters
  in
  ( Transaction_to_zk_rollup
      {destination; parameters_ty; parameters; unparsed_parameters},
    ctxt )

(* [transfer (ctxt, sc) gas tez parameters_ty parameters destination entrypoint]
   creates an operation that transfers an amount of [tez] to a destination and
   an entrypoint instantiated with argument [parameters] of type
   [parameters_ty]. *)
let transfer (type t) (ctxt, sc) gas amount location
    (typed_contract : t typed_contract) (parameters : t) =
  let open Lwt_result_syntax in
  let ctxt = update_context gas ctxt in
  let* operation, lazy_storage_diff, ctxt =
    match typed_contract with
    | Typed_implicit destination ->
        let () = parameters in
        return (Transaction_to_implicit {destination; amount}, None, ctxt)
    | Typed_implicit_with_ticket {destination; ticket_ty} ->
        let* unparsed_ticket, ctxt =
          unparse_data ctxt Optimized ticket_ty parameters
        in
        return
          ( Transaction_to_implicit_with_ticket
              {
                destination;
                amount;
                ticket_ty;
                ticket = parameters;
                unparsed_ticket = Script.lazy_expr unparsed_ticket;
              },
            None,
            ctxt )
    | Typed_originated
        {arg_ty = parameters_ty; contract_hash = destination; entrypoint} ->
        let*? to_duplicate, ctxt =
          collect_lazy_storage ctxt parameters_ty parameters
        in
        let to_update = no_lazy_storage_id in
        let* parameters, lazy_storage_diff, ctxt =
          extract_lazy_storage_diff
            ctxt
            Optimized
            parameters_ty
            parameters
            ~to_duplicate
            ~to_update
            ~temporary:true
        in
        let+ unparsed_parameters, ctxt =
          unparse_data ctxt Optimized parameters_ty parameters
        in
        ( Transaction_to_smart_contract
            {
              destination;
              amount;
              entrypoint;
              location;
              parameters_ty;
              parameters;
              unparsed_parameters;
            },
          lazy_storage_diff,
          ctxt )
    | Typed_sc_rollup
        {arg_ty = parameters_ty; sc_rollup = destination; entrypoint} ->
        let+ operation, ctxt =
          make_transaction_to_sc_rollup
            ctxt
            ~destination
            ~amount
            ~entrypoint
            ~parameters_ty
            ~parameters
        in
        (operation, None, ctxt)
    | Typed_zk_rollup {arg_ty = parameters_ty; zk_rollup = destination} ->
        let+ operation, ctxt =
          make_transaction_to_zk_rollup
            ctxt
            ~destination
            ~amount
            ~parameters_ty
            ~parameters
        in
        (operation, None, ctxt)
  in
  let*? ctxt, nonce = fresh_internal_nonce ctxt in
  let iop =
    {
      sender = Destination.Contract (Contract.Originated sc.self);
      operation;
      nonce;
    }
  in
  let res = {piop = Internal_operation iop; lazy_storage_diff} in
  let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
  return (res, ctxt, gas)

(** [create_contract (ctxt, sc) gas storage_ty code delegate credit init]
    creates an origination operation for a contract represented by [code], some
    initial [credit] (withdrawn from the contract being executed), and an
    initial storage [init] of type [storage_ty]. *)
let create_contract (ctxt, sc) gas storage_type code delegate credit init =
  let open Lwt_result_syntax in
  let ctxt = update_context gas ctxt in
  let*? to_duplicate, ctxt = collect_lazy_storage ctxt storage_type init in
  let to_update = no_lazy_storage_id in
  let* init, lazy_storage_diff, ctxt =
    extract_lazy_storage_diff
      ctxt
      Optimized
      storage_type
      init
      ~to_duplicate
      ~to_update
      ~temporary:true
  in
  let* unparsed_storage, ctxt = unparse_data ctxt Optimized storage_type init in
  let*? ctxt, preorigination =
    Contract.fresh_contract_from_current_nonce ctxt
  in
  let operation =
    Origination
      {
        credit;
        delegate;
        code;
        unparsed_storage;
        preorigination;
        storage_type;
        storage = init;
      }
  in
  let*? ctxt, nonce = fresh_internal_nonce ctxt in
  let sender = Destination.Contract (Contract.Originated sc.self) in
  let piop = Internal_operation {sender; operation; nonce} in
  let res = {piop; lazy_storage_diff} in
  let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
  return (res, preorigination, ctxt, gas)

(* [unpack ctxt ty bytes] deserialize [bytes] into a value of type [ty]. *)
let unpack ctxt ~ty ~bytes =
  let open Lwt_result_syntax in
  let*? ctxt =
    Gas.consume
      ctxt
      (Script.deserialization_cost_estimated_from_bytes (Bytes.length bytes))
  in
  if
    Compare.Int.(Bytes.length bytes >= 1)
    && Compare.Int.(TzEndian.get_uint8 bytes 0 = 0x05)
  then
    let str = Bytes.sub_string bytes 1 (Bytes.length bytes - 1) in
    match Data_encoding.Binary.of_string_opt Script.expr_encoding str with
    | None ->
        let*? ctxt = Gas.consume ctxt (Interp_costs.unpack_failed str) in
        return (None, ctxt)
    | Some expr -> (
        let*! value_opt =
          parse_data
            ctxt
            ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
            ~allow_forged:false
            ty
            (Micheline.root expr)
        in
        match value_opt with
        | Ok (value, ctxt) -> return (Some value, ctxt)
        | Error _ignored ->
            let*? ctxt = Gas.consume ctxt (Interp_costs.unpack_failed str) in
            return (None, ctxt))
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
  | KPrefix (_, _, n), rest ->
      interp_stack_prefix_preserving_operation f n (fst rest) (snd rest)
      |> fun ((v, rest'), result) -> ((accu, (v, rest')), result)
  | KRest, v -> f accu v

(*

   Some auxiliary functions have complex types and must be annotated
   because of GADTs and polymorphic recursion.

   To improve readibility, we introduce their types as abbreviations:

 *)

(* A function of this type either introduces type-preserving
   instrumentation of a continuation for the purposes of logging
   or returns given continuation unchanged. *)
type ('a, 'b, 'c, 'd) cont_instrumentation =
  ('a, 'b, 'c, 'd) continuation -> ('a, 'b, 'c, 'd) continuation

let id x = x

type ('a, 'b, 'c, 'e, 'f, 'm, 'n, 'o) kmap_exit_type =
  ('a, 'b, 'e, 'f) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('m * 'n, 'a * 'b, 'o, 'a * 'b) kinstr ->
  ('m * 'n) list ->
  (('m, 'o) map, 'c) ty option ->
  ('m, 'o) map ->
  'm ->
  (('m, 'o) map, 'a * 'b, 'e, 'f) continuation ->
  'o ->
  'a * 'b ->
  ('e * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'j, 'k) kmap_enter_type =
  ('a, 'b * 'c, 'd, 'e) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('j * 'k, 'b * 'c, 'a, 'b * 'c) kinstr ->
  ('j * 'k) list ->
  (('j, 'a) map, 'f) ty option ->
  ('j, 'a) map ->
  (('j, 'a) map, 'b * 'c, 'd, 'e) continuation ->
  'b ->
  'c ->
  ('d * 'e * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'i, 'j) klist_exit_type =
  ('a, 'b, 'c, 'd) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('i, 'a * 'b, 'j, 'a * 'b) kinstr ->
  'i list ->
  'j Script_list.t ->
  ('j Script_list.t, 'e) ty option ->
  int ->
  ('j Script_list.t, 'a * 'b, 'c, 'd) continuation ->
  'j ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'j) klist_enter_type =
  ('b, 'a * 'c, 'd, 'e) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('j, 'a * 'c, 'b, 'a * 'c) kinstr ->
  'j list ->
  'b Script_list.t ->
  ('b Script_list.t, 'f) ty option ->
  int ->
  ('b Script_list.t, 'a * 'c, 'd, 'e) continuation ->
  'a ->
  'c ->
  ('d * 'e * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) kloop_in_left_type =
  outdated_context * step_constants ->
  local_gas_counter ->
  ('c, 'd, 'e, 'f) continuation ->
  ('a, 'g, 'c, 'd) kinstr ->
  ('b, 'g, 'e, 'f) continuation ->
  ('a, 'b) or_ ->
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

type ('a, 'b, 's, 'r, 'f, 'c) kiter_type =
  ('a, 's, 'r, 'f) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('b, 'a * 's, 'a, 's) kinstr ->
  ('b, 'c) ty option ->
  'b list ->
  ('a, 's, 'r, 'f) continuation ->
  'a ->
  's ->
  ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) ilist_map_type =
  ('a, 'b, 'c, 'd) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e, 'a * 'b, 'f, 'a * 'b) kinstr ->
  ('f Script_list.t, 'a * 'b, 'g, 'h) kinstr ->
  ('g, 'h, 'c, 'd) continuation ->
  ('f Script_list.t, 'i) ty option ->
  'e Script_list.t ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'cmp) ilist_iter_type =
  ('a, 'b, 'c, 'd) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e, 'a * 'b, 'a, 'b) kinstr ->
  ('e, 'cmp) ty option ->
  ('a, 'b, 'f, 'g) kinstr ->
  ('f, 'g, 'c, 'd) continuation ->
  'e Script_list.t ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) iset_iter_type =
  ('a, 'b, 'c, 'd) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e, 'a * 'b, 'a, 'b) kinstr ->
  'e comparable_ty option ->
  ('a, 'b, 'f, 'g) kinstr ->
  ('f, 'g, 'c, 'd) continuation ->
  'e set ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) imap_map_type =
  ('a, 'b, 'c, 'd) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e * 'f, 'a * 'b, 'g, 'a * 'b) kinstr ->
  (('e, 'g) map, 'a * 'b, 'h, 'i) kinstr ->
  ('h, 'i, 'c, 'd) continuation ->
  (('e, 'g) map, 'j) ty option ->
  ('e, 'f) map ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'cmp) imap_iter_type =
  ('a, 'b, 'c, 'd) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('e * 'f, 'a * 'b, 'a, 'b) kinstr ->
  ('e * 'f, 'cmp) ty option ->
  ('a, 'b, 'g, 'h) kinstr ->
  ('g, 'h, 'c, 'd) continuation ->
  ('e, 'f) map ->
  'a * 'b ->
  ('c * 'd * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) imul_teznat_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  Script.location ->
  (Tez.t, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Tez.t ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) imul_nattez_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  Script.location ->
  (Tez.t, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Script_int.n Script_int.num ->
  Tez.t * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) ilsl_nat_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  Script.location ->
  (Script_int.n Script_int.num, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Script_int.n Script_int.num ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) ilsr_nat_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  Script.location ->
  (Script_int.n Script_int.num, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  Script_int.n Script_int.num ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f) ilsl_bytes_type =
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  Script.location ->
  (bytes, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  bytes ->
  Script_int.n Script_int.num * 'b ->
  ('e * 'f * outdated_context * local_gas_counter, error trace) result Lwt.t

type ifailwith_type = {
  ifailwith :
    'a 'ac 'b.
    logger option ->
    outdated_context * step_constants ->
    local_gas_counter ->
    Script.location ->
    ('a, 'ac) ty ->
    'a ->
    ('b, error trace) result Lwt.t;
}
[@@unboxed]

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) iexec_type =
  ('a, end_of_stack, 'e, 'f) cont_instrumentation ->
  logger option ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('a, 'b) stack_ty option ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  'g ->
  ('g, 'a) lambda * 'b ->
  ('e * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

type ('a, 'b, 'c, 'd, 'e, 'f, 'i, 'o) iview_type =
  ('o, end_of_stack, 'e, 'f) cont_instrumentation ->
  outdated_context * step_constants ->
  local_gas_counter ->
  ('i, 'o) view_signature ->
  ('a, 'b) stack_ty option ->
  ('o option, 'a * 'b, 'c, 'd) kinstr ->
  ('c, 'd, 'e, 'f) continuation ->
  'i ->
  address * ('a * 'b) ->
  ('e * 'f * outdated_context * local_gas_counter) tzresult Lwt.t
