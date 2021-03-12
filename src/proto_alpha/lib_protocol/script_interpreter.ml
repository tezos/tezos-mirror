(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Alpha_context
open Script
open Script_typed_ir
open Script_ir_translator
module S = Saturation_repr

(* ---- Run-time errors -----------------------------------------------------*)

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

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
    (fun () -> Cannot_serialize_storage) ;
  (* Michelson Stack Overflow *)
  register_error_kind
    `Permanent
    ~id:"michelson_v1.interp_too_many_recursive_calls"
    ~title:"Too many recursive calls during interpretation"
    ~description:
      "Too many recursive calls were needed for interpretation of a Michelson \
       script"
    Data_encoding.empty
    (function Michelson_too_many_recursive_calls -> Some () | _ -> None)
    (fun () -> Michelson_too_many_recursive_calls)

(* ---- interpreter ---------------------------------------------------------*)

module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let rec interp_stack_prefix_preserving_operation :
    type fbef bef faft aft result.
    (fbef -> (faft * result) tzresult Lwt.t) ->
    (fbef, faft, bef, aft) stack_prefix_preservation_witness ->
    bef ->
    (aft * result) tzresult Lwt.t =
 fun f n stk ->
  match (n, stk) with
  | ( Prefix
        (Prefix
          (Prefix
            (Prefix
              (Prefix
                (Prefix
                  (Prefix
                    (Prefix
                      (Prefix
                        (Prefix
                          (Prefix
                            (Prefix (Prefix (Prefix (Prefix (Prefix n))))))))))))))),
      ( v0,
        ( v1,
          ( v2,
            ( v3,
              ( v4,
                ( v5,
                  ( v6,
                    (v7, (v8, (v9, (va, (vb, (vc, (vd, (ve, (vf, rest)))))))))
                  ) ) ) ) ) ) ) ) ->
      interp_stack_prefix_preserving_operation f n rest
      >|=? fun (rest', result) ->
      ( ( v0,
          ( v1,
            ( v2,
              ( v3,
                ( v4,
                  ( v5,
                    ( v6,
                      ( v7,
                        (v8, (v9, (va, (vb, (vc, (vd, (ve, (vf, rest'))))))))
                      ) ) ) ) ) ) ) ),
        result )
  | (Prefix (Prefix (Prefix (Prefix n))), (v0, (v1, (v2, (v3, rest))))) ->
      interp_stack_prefix_preserving_operation f n rest
      >|=? fun (rest', result) -> ((v0, (v1, (v2, (v3, rest')))), result)
  | (Prefix n, (v, rest)) ->
      interp_stack_prefix_preserving_operation f n rest
      >|=? fun (rest', result) -> ((v, rest'), result)
  | (Rest, v) ->
      f v

type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

module type STEP_LOGGER = sig
  val log_interp :
    context -> ('bef, 'aft) Script_typed_ir.descr -> 'bef -> unit

  val log_entry : context -> ('bef, 'aft) Script_typed_ir.descr -> 'bef -> unit

  val log_exit : context -> ('bef, 'aft) Script_typed_ir.descr -> 'aft -> unit

  val get_log : unit -> execution_trace option tzresult Lwt.t
end

type logger = (module STEP_LOGGER)

module No_trace : STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = return_none
end

let cost_of_instr : type b a. (b, a) descr -> b -> Gas.cost =
 fun descr stack ->
  match (descr.instr, stack) with
  | (Drop, _) ->
      Interp_costs.drop
  | (Dup, _) ->
      Interp_costs.dup
  | (Swap, _) ->
      Interp_costs.swap
  | (Const _, _) ->
      Interp_costs.push
  | (Cons_some, _) ->
      Interp_costs.cons_some
  | (Cons_none _, _) ->
      Interp_costs.cons_none
  | (If_none _, _) ->
      Interp_costs.if_none
  | (Cons_pair, _) ->
      Interp_costs.cons_pair
  | (Unpair, _) ->
      Interp_costs.unpair
  | (Car, _) ->
      Interp_costs.car
  | (Cdr, _) ->
      Interp_costs.cdr
  | (Cons_left, _) ->
      Interp_costs.cons_left
  | (Cons_right, _) ->
      Interp_costs.cons_right
  | (If_left _, _) ->
      Interp_costs.if_left
  | (Cons_list, _) ->
      Interp_costs.cons_list
  | (Nil, _) ->
      Interp_costs.nil
  | (If_cons _, _) ->
      Interp_costs.if_cons
  | (List_map _, (list, _)) ->
      Interp_costs.list_map list
  | (List_size, _) ->
      Interp_costs.list_size
  | (List_iter _, (l, _)) ->
      Interp_costs.list_iter l
  | (Empty_set _, _) ->
      Interp_costs.empty_set
  | (Set_iter _, (set, _)) ->
      Interp_costs.set_iter set
  | (Set_mem, (v, (set, _))) ->
      Interp_costs.set_mem v set
  | (Set_update, (v, (_, (set, _)))) ->
      Interp_costs.set_update v set
  | (Set_size, _) ->
      Interp_costs.set_size
  | (Empty_map _, _) ->
      Interp_costs.empty_map
  | (Map_map _, (map, _)) ->
      Interp_costs.map_map map
  | (Map_iter _, (map, _)) ->
      Interp_costs.map_iter map
  | (Map_mem, (v, (map, _rest))) ->
      Interp_costs.map_mem v map
  | (Map_get, (v, (map, _rest))) ->
      Interp_costs.map_get v map
  | (Map_update, (k, (_, (map, _)))) ->
      Interp_costs.map_update k map
  | (Map_get_and_update, (k, (_, (map, _)))) ->
      Interp_costs.map_get_and_update k map
  | (Map_size, _) ->
      Interp_costs.map_size
  | (Empty_big_map _, _) ->
      Interp_costs.empty_map
  | (Big_map_mem, (_, (map, _))) ->
      Interp_costs.big_map_mem map.diff
  | (Big_map_get, (_, (map, _))) ->
      Interp_costs.big_map_get map.diff
  | (Big_map_update, (_, (_, (map, _)))) ->
      Interp_costs.big_map_update map.diff
  | (Big_map_get_and_update, (_, (_, (map, _)))) ->
      Interp_costs.big_map_get_and_update map.diff
  | (Add_seconds_to_timestamp, (n, (t, _))) ->
      Interp_costs.add_seconds_timestamp n t
  | (Add_timestamp_to_seconds, (t, (n, _))) ->
      Interp_costs.add_seconds_timestamp n t
  | (Sub_timestamp_seconds, (t, (n, _))) ->
      Interp_costs.sub_seconds_timestamp n t
  | (Diff_timestamps, (t1, (t2, _))) ->
      Interp_costs.diff_timestamps t1 t2
  | (Concat_string_pair, (x, (y, _))) ->
      Interp_costs.concat_string_pair x y
  | (Concat_string, (ss, _)) ->
      Interp_costs.concat_string_precheck ss
  | (Slice_string, (_offset, (_length, (s, _)))) ->
      Interp_costs.slice_string s
  | (String_size, _) ->
      Interp_costs.string_size
  | (Concat_bytes_pair, (x, (y, _))) ->
      Interp_costs.concat_bytes_pair x y
  | (Concat_bytes, (ss, _)) ->
      Interp_costs.concat_string_precheck ss
  | (Slice_bytes, (_offset, (_length, (s, _)))) ->
      Interp_costs.slice_bytes s
  | (Bytes_size, _) ->
      Interp_costs.bytes_size
  | (Add_tez, _) ->
      Interp_costs.add_tez
  | (Sub_tez, _) ->
      Interp_costs.sub_tez
  | (Mul_teznat, (_, (n, _))) ->
      Interp_costs.mul_teznat n
  | (Mul_nattez, (n, (_, _))) ->
      Interp_costs.mul_teznat n
  | (Or, _) ->
      Interp_costs.bool_or
  | (And, _) ->
      Interp_costs.bool_and
  | (Xor, _) ->
      Interp_costs.bool_xor
  | (Not, _) ->
      Interp_costs.bool_not
  | (Is_nat, _) ->
      Interp_costs.is_nat
  | (Abs_int, (x, _)) ->
      Interp_costs.abs_int x
  | (Int_nat, _) ->
      Interp_costs.int_nat
  | (Neg_int, (x, _)) ->
      Interp_costs.neg_int x
  | (Neg_nat, (x, _)) ->
      Interp_costs.neg_nat x
  | (Add_intint, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Add_intnat, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Add_natint, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Add_natnat, (x, (y, _))) ->
      Interp_costs.add_bigint x y
  | (Sub_int, (x, (y, _))) ->
      Interp_costs.sub_bigint x y
  | (Mul_intint, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Mul_intnat, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Mul_natint, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Mul_natnat, (x, (y, _))) ->
      Interp_costs.mul_bigint x y
  | (Ediv_teznat, (x, (y, _))) ->
      Interp_costs.ediv_teznat x y
  | (Ediv_tez, _) ->
      Interp_costs.ediv_tez
  | (Ediv_intint, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Ediv_intnat, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Ediv_natint, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Ediv_natnat, (x, (y, _))) ->
      Interp_costs.ediv_bigint x y
  | (Lsl_nat, (x, _)) ->
      Interp_costs.lsl_nat x
  | (Lsr_nat, (x, _)) ->
      Interp_costs.lsr_nat x
  | (Or_nat, (x, (y, _))) ->
      Interp_costs.or_nat x y
  | (And_nat, (x, (y, _))) ->
      Interp_costs.and_nat x y
  | (And_int_nat, (x, (y, _))) ->
      Interp_costs.and_nat x y
  | (Xor_nat, (x, (y, _))) ->
      Interp_costs.xor_nat x y
  | (Not_int, (x, _)) ->
      Interp_costs.not_nat x
  | (Not_nat, (x, _)) ->
      Interp_costs.not_nat x
  | (Seq _, _) ->
      Interp_costs.seq
  | (If _, _) ->
      Interp_costs.if_
  | (Loop _, _) ->
      Interp_costs.loop
  | (Loop_left _, _) ->
      Interp_costs.loop_left
  | (Dip _, _) ->
      Interp_costs.dip
  | (Exec, _) ->
      Interp_costs.exec
  | (Apply _, _) ->
      Interp_costs.apply
  | (Lambda _, _) ->
      Interp_costs.push
  | (Failwith _, _) ->
      Gas.free
  | (Nop, _) ->
      Interp_costs.nop
  | (Compare ty, (a, (b, _))) ->
      Interp_costs.compare ty a b
  | (Eq, _) ->
      Interp_costs.neq
  | (Neq, _) ->
      Interp_costs.neq
  | (Lt, _) ->
      Interp_costs.neq
  | (Le, _) ->
      Interp_costs.neq
  | (Gt, _) ->
      Interp_costs.neq
  | (Ge, _) ->
      Interp_costs.neq
  | (Pack _, _) ->
      Gas.free
  | (Unpack _, _) ->
      Gas.free
  | (Address, _) ->
      Interp_costs.address
  | (Contract _, _) ->
      Interp_costs.contract
  | (Transfer_tokens, _) ->
      Interp_costs.transfer_tokens
  | (Implicit_account, _) ->
      Interp_costs.implicit_account
  | (Set_delegate, _) ->
      Interp_costs.set_delegate
  | (Balance, _) ->
      Interp_costs.balance
  | (Level, _) ->
      Interp_costs.level
  | (Now, _) ->
      Interp_costs.now
  | (Check_signature, (key, (_, (message, _)))) ->
      Interp_costs.check_signature key message
  | (Hash_key, (pk, _)) ->
      Interp_costs.hash_key pk
  | (Blake2b, (bytes, _)) ->
      Interp_costs.blake2b bytes
  | (Sha256, (bytes, _)) ->
      Interp_costs.sha256 bytes
  | (Sha512, (bytes, _)) ->
      Interp_costs.sha512 bytes
  | (Source, _) ->
      Interp_costs.source
  | (Sender, _) ->
      Interp_costs.source
  | (Self _, _) ->
      Interp_costs.self
  | (Self_address, _) ->
      Interp_costs.self
  | (Amount, _) ->
      Interp_costs.amount
  | (Dig (n, _), _) ->
      Interp_costs.dign n
  | (Dug (n, _), _) ->
      Interp_costs.dugn n
  | (Dipn (n, _, _), _) ->
      Interp_costs.dipn n
  | (Dropn (n, _), _) ->
      Interp_costs.dropn n
  | (ChainId, _) ->
      Interp_costs.chain_id
  | (Create_contract _, _) ->
      Interp_costs.create_contract
  | (Never, (_, _)) ->
      .
  | (Voting_power, _) ->
      Interp_costs.voting_power
  | (Total_voting_power, _) ->
      Interp_costs.total_voting_power
  | (Keccak, (bytes, _)) ->
      Interp_costs.keccak bytes
  | (Sha3, (bytes, _)) ->
      Interp_costs.sha3 bytes
  | (Add_bls12_381_g1, _) ->
      Interp_costs.add_bls12_381_g1
  | (Add_bls12_381_g2, _) ->
      Interp_costs.add_bls12_381_g2
  | (Add_bls12_381_fr, _) ->
      Interp_costs.add_bls12_381_fr
  | (Mul_bls12_381_g1, _) ->
      Interp_costs.mul_bls12_381_g1
  | (Mul_bls12_381_g2, _) ->
      Interp_costs.mul_bls12_381_g2
  | (Mul_bls12_381_fr, _) ->
      Interp_costs.mul_bls12_381_fr
  | (Mul_bls12_381_fr_z, _) ->
      Interp_costs.mul_bls12_381_fr_z
  | (Mul_bls12_381_z_fr, _) ->
      Interp_costs.mul_bls12_381_fr_z
  | (Int_bls12_381_fr, _) ->
      Interp_costs.int_bls12_381_fr
  | (Neg_bls12_381_g1, _) ->
      Interp_costs.neg_bls12_381_g1
  | (Neg_bls12_381_g2, _) ->
      Interp_costs.neg_bls12_381_g2
  | (Neg_bls12_381_fr, _) ->
      Interp_costs.neg_bls12_381_fr
  | (Pairing_check_bls12_381, (pairs, _)) ->
      Interp_costs.pairing_check_bls12_381 pairs
  | (Comb (n, _), _) ->
      Interp_costs.comb n
  | (Uncomb (n, _), _) ->
      Interp_costs.uncomb n
  | (Comb_get (n, _), _) ->
      Interp_costs.comb_get n
  | (Comb_set (n, _), _) ->
      Interp_costs.comb_set n
  | (Dup_n (n, _), _) ->
      Interp_costs.dupn n
  | (Sapling_empty_state _, _) ->
      Interp_costs.sapling_empty_state
  | (Sapling_verify_update, (tx, _)) ->
      let inputs = List.length tx.inputs in
      let outputs = List.length tx.outputs in
      Interp_costs.sapling_verify_update ~inputs ~outputs
  | (Ticket, _) ->
      Interp_costs.ticket
  | (Read_ticket, _) ->
      Interp_costs.read_ticket
  | (Split_ticket, (ticket, ((amount_a, amount_b), _))) ->
      Interp_costs.split_ticket ticket.amount amount_a amount_b
  | (Join_tickets ty, ((ticket_a, ticket_b), _)) ->
      Interp_costs.join_tickets ty ticket_a ticket_b

let unpack ctxt ~ty ~bytes =
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

let rec step_bounded :
    type b a.
    logger ->
    stack_depth:int ->
    context ->
    step_constants ->
    (b, a) descr ->
    b ->
    (a * context) tzresult Lwt.t =
 fun logger ~stack_depth ctxt step_constants ({instr; loc; _} as descr) stack ->
  let gas = cost_of_instr descr stack in
  Gas.consume ctxt gas
  >>?= fun ctxt ->
  let module Log = (val logger) in
  Log.log_entry ctxt descr stack ;
  let logged_return : a * context -> (a * context) tzresult Lwt.t =
   fun (ret, ctxt) ->
    Log.log_exit ctxt descr ret ;
    return (ret, ctxt)
  in
  let non_terminal_recursion ~ctxt ?(stack_depth = stack_depth + 1) descr stack
      =
    if Compare.Int.(stack_depth >= 10_000) then
      fail Michelson_too_many_recursive_calls
    else step_bounded logger ~stack_depth ctxt step_constants descr stack
  in
  match (instr, stack) with
  (* stack ops *)
  | (Drop, (_, rest)) ->
      logged_return (rest, ctxt)
  | (Dup, (v, rest)) ->
      logged_return ((v, (v, rest)), ctxt)
  | (Swap, (vi, (vo, rest))) ->
      logged_return ((vo, (vi, rest)), ctxt)
  | (Const v, rest) ->
      logged_return ((v, rest), ctxt)
  (* options *)
  | (Cons_some, (v, rest)) ->
      logged_return ((Some v, rest), ctxt)
  | (Cons_none _, rest) ->
      logged_return ((None, rest), ctxt)
  | (If_none (bt, _), (None, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bt rest
  | (If_none (_, bf), (Some v, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bf (v, rest)
  (* pairs *)
  | (Cons_pair, (a, (b, rest))) ->
      logged_return (((a, b), rest), ctxt)
  | (Unpair, ((a, b), rest)) ->
      logged_return ((a, (b, rest)), ctxt)
  | (Car, ((a, _), rest)) ->
      logged_return ((a, rest), ctxt)
  | (Cdr, ((_, b), rest)) ->
      logged_return ((b, rest), ctxt)
  (* unions *)
  | (Cons_left, (v, rest)) ->
      logged_return ((L v, rest), ctxt)
  | (Cons_right, (v, rest)) ->
      logged_return ((R v, rest), ctxt)
  | (If_left (bt, _), (L v, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bt (v, rest)
  | (If_left (_, bf), (R v, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bf (v, rest)
  (* lists *)
  | (Cons_list, (hd, (tl, rest))) ->
      logged_return ((list_cons hd tl, rest), ctxt)
  | (Nil, rest) ->
      logged_return ((list_empty, rest), ctxt)
  | (If_cons (_, bf), ({elements = []; _}, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bf rest
  | (If_cons (bt, _), ({elements = hd :: tl; length}, rest)) ->
      let tl = {elements = tl; length = length - 1} in
      step_bounded logger ~stack_depth ctxt step_constants bt (hd, (tl, rest))
  | (List_map body, (list, rest)) ->
      let rec loop rest ctxt l acc =
        match l with
        | [] ->
            let result = {elements = List.rev acc; length = list.length} in
            return ((result, rest), ctxt)
        | hd :: tl ->
            non_terminal_recursion ~ctxt body (hd, rest)
            >>=? fun ((hd, rest), ctxt) -> loop rest ctxt tl (hd :: acc)
      in
      loop rest ctxt list.elements []
      >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (List_size, (list, rest)) ->
      logged_return ((Script_int.(abs (of_int list.length)), rest), ctxt)
  | (List_iter body, (l, init)) ->
      let rec loop ctxt l stack =
        match l with
        | [] ->
            return (stack, ctxt)
        | hd :: tl ->
            non_terminal_recursion ~ctxt body (hd, stack)
            >>=? fun (stack, ctxt) -> loop ctxt tl stack
      in
      loop ctxt l.elements init
      >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  (* sets *)
  | (Empty_set t, rest) ->
      logged_return ((empty_set t, rest), ctxt)
  | (Set_iter body, (set, init)) ->
      let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
      let rec loop ctxt l stack =
        match l with
        | [] ->
            return (stack, ctxt)
        | hd :: tl ->
            non_terminal_recursion ~ctxt body (hd, stack)
            >>=? fun (stack, ctxt) -> loop ctxt tl stack
      in
      loop ctxt l init >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (Set_mem, (v, (set, rest))) ->
      logged_return ((set_mem v set, rest), ctxt)
  | (Set_update, (v, (presence, (set, rest)))) ->
      logged_return ((set_update v presence set, rest), ctxt)
  | (Set_size, (set, rest)) ->
      logged_return ((set_size set, rest), ctxt)
  (* maps *)
  | (Empty_map (t, _), rest) ->
      logged_return ((empty_map t, rest), ctxt)
  | (Map_map body, (map, rest)) ->
      let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
      let rec loop rest ctxt l acc =
        match l with
        | [] ->
            return ((acc, rest), ctxt)
        | ((k, _) as hd) :: tl ->
            non_terminal_recursion ~ctxt body (hd, rest)
            >>=? fun ((hd, rest), ctxt) ->
            loop rest ctxt tl (map_update k (Some hd) acc)
      in
      loop rest ctxt l (empty_map (map_key_ty map))
      >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (Map_iter body, (map, init)) ->
      let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
      let rec loop ctxt l stack =
        match l with
        | [] ->
            return (stack, ctxt)
        | hd :: tl ->
            non_terminal_recursion ~ctxt body (hd, stack)
            >>=? fun (stack, ctxt) -> loop ctxt tl stack
      in
      loop ctxt l init >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (Map_mem, (v, (map, rest))) ->
      logged_return ((map_mem v map, rest), ctxt)
  | (Map_get, (v, (map, rest))) ->
      logged_return ((map_get v map, rest), ctxt)
  | (Map_update, (k, (v, (map, rest)))) ->
      logged_return ((map_update k v map, rest), ctxt)
  | (Map_get_and_update, (k, (v, (map, rest)))) ->
      let map' = map_update k v map in
      let v' = map_get k map in
      logged_return ((v', (map', rest)), ctxt)
  | (Map_size, (map, rest)) ->
      logged_return ((map_size map, rest), ctxt)
  (* Big map operations *)
  | (Empty_big_map (tk, tv), rest) ->
      logged_return ((Script_ir_translator.empty_big_map tk tv, rest), ctxt)
  | (Big_map_mem, (key, (map, rest))) ->
      Script_ir_translator.big_map_mem ctxt key map
      >>=? fun (res, ctxt) -> logged_return ((res, rest), ctxt)
  | (Big_map_get, (key, (map, rest))) ->
      Script_ir_translator.big_map_get ctxt key map
      >>=? fun (res, ctxt) -> logged_return ((res, rest), ctxt)
  | (Big_map_update, (key, (maybe_value, (map, rest)))) ->
      Script_ir_translator.big_map_update ctxt key maybe_value map
      >>=? fun (res, ctxt) -> logged_return ((res, rest), ctxt)
  | (Big_map_get_and_update, (k, (v, (map, rest)))) ->
      Script_ir_translator.big_map_get_and_update ctxt k v map
      >>=? fun (v', map', ctxt) -> logged_return ((v', (map', rest)), ctxt)
  (* timestamp operations *)
  | (Add_seconds_to_timestamp, (n, (t, rest))) ->
      let result = Script_timestamp.add_delta t n in
      logged_return ((result, rest), ctxt)
  | (Add_timestamp_to_seconds, (t, (n, rest))) ->
      let result = Script_timestamp.add_delta t n in
      logged_return ((result, rest), ctxt)
  | (Sub_timestamp_seconds, (t, (s, rest))) ->
      let result = Script_timestamp.sub_delta t s in
      logged_return ((result, rest), ctxt)
  | (Diff_timestamps, (t1, (t2, rest))) ->
      let result = Script_timestamp.diff t1 t2 in
      logged_return ((result, rest), ctxt)
  (* string operations *)
  | (Concat_string_pair, (x, (y, rest))) ->
      let s = String.concat "" [x; y] in
      logged_return ((s, rest), ctxt)
  | (Concat_string, (ss, rest)) ->
      (* The cost for this fold_left has been paid upfront *)
      let total_length =
        List.fold_left
          (fun acc s -> S.add acc (S.safe_int (String.length s)))
          S.zero
          ss.elements
      in
      Gas.consume ctxt (Interp_costs.concat_string total_length)
      >>?= fun ctxt ->
      let s = String.concat "" ss.elements in
      logged_return ((s, rest), ctxt)
  | (Slice_string, (offset, (length, (s, rest)))) ->
      let s_length = Z.of_int (String.length s) in
      let offset = Script_int.to_zint offset in
      let length = Script_int.to_zint length in
      if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
        logged_return
          ( (Some (String.sub s (Z.to_int offset) (Z.to_int length)), rest),
            ctxt )
      else logged_return ((None, rest), ctxt)
  | (String_size, (s, rest)) ->
      logged_return ((Script_int.(abs (of_int (String.length s))), rest), ctxt)
  (* bytes operations *)
  | (Concat_bytes_pair, (x, (y, rest))) ->
      let s = Bytes.cat x y in
      logged_return ((s, rest), ctxt)
  | (Concat_bytes, (ss, rest)) ->
      (* The cost for this fold_left has been paid upfront *)
      let total_length =
        List.fold_left
          (fun acc s -> S.add acc (S.safe_int (Bytes.length s)))
          S.zero
          ss.elements
      in
      Gas.consume ctxt (Interp_costs.concat_string total_length)
      >>?= fun ctxt ->
      let s = Bytes.concat Bytes.empty ss.elements in
      logged_return ((s, rest), ctxt)
  | (Slice_bytes, (offset, (length, (s, rest)))) ->
      let s_length = Z.of_int (Bytes.length s) in
      let offset = Script_int.to_zint offset in
      let length = Script_int.to_zint length in
      if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
        logged_return
          ((Some (Bytes.sub s (Z.to_int offset) (Z.to_int length)), rest), ctxt)
      else logged_return ((None, rest), ctxt)
  | (Bytes_size, (s, rest)) ->
      logged_return ((Script_int.(abs (of_int (Bytes.length s))), rest), ctxt)
  (* currency operations *)
  | (Add_tez, (x, (y, rest))) ->
      Tez.(x +? y) >>?= fun res -> logged_return ((res, rest), ctxt)
  | (Sub_tez, (x, (y, rest))) ->
      Tez.(x -? y) >>?= fun res -> logged_return ((res, rest), ctxt)
  | (Mul_teznat, (x, (y, rest))) -> (
    match Script_int.to_int64 y with
    | None ->
        Log.get_log () >>=? fun log -> fail (Overflow (loc, log))
    | Some y ->
        Tez.(x *? y) >>?= fun res -> logged_return ((res, rest), ctxt) )
  | (Mul_nattez, (y, (x, rest))) -> (
    match Script_int.to_int64 y with
    | None ->
        Log.get_log () >>=? fun log -> fail (Overflow (loc, log))
    | Some y ->
        Tez.(x *? y) >>?= fun res -> logged_return ((res, rest), ctxt) )
  (* boolean operations *)
  | (Or, (x, (y, rest))) ->
      logged_return ((x || y, rest), ctxt)
  | (And, (x, (y, rest))) ->
      logged_return ((x && y, rest), ctxt)
  | (Xor, (x, (y, rest))) ->
      logged_return ((Compare.Bool.(x <> y), rest), ctxt)
  | (Not, (x, rest)) ->
      logged_return ((not x, rest), ctxt)
  (* integer operations *)
  | (Is_nat, (x, rest)) ->
      logged_return ((Script_int.is_nat x, rest), ctxt)
  | (Abs_int, (x, rest)) ->
      logged_return ((Script_int.abs x, rest), ctxt)
  | (Int_nat, (x, rest)) ->
      logged_return ((Script_int.int x, rest), ctxt)
  | (Neg_int, (x, rest)) ->
      logged_return ((Script_int.neg x, rest), ctxt)
  | (Neg_nat, (x, rest)) ->
      logged_return ((Script_int.neg x, rest), ctxt)
  | (Add_intint, (x, (y, rest))) ->
      logged_return ((Script_int.add x y, rest), ctxt)
  | (Add_intnat, (x, (y, rest))) ->
      logged_return ((Script_int.add x y, rest), ctxt)
  | (Add_natint, (x, (y, rest))) ->
      logged_return ((Script_int.add x y, rest), ctxt)
  | (Add_natnat, (x, (y, rest))) ->
      logged_return ((Script_int.add_n x y, rest), ctxt)
  | (Sub_int, (x, (y, rest))) ->
      logged_return ((Script_int.sub x y, rest), ctxt)
  | (Mul_intint, (x, (y, rest))) ->
      logged_return ((Script_int.mul x y, rest), ctxt)
  | (Mul_intnat, (x, (y, rest))) ->
      logged_return ((Script_int.mul x y, rest), ctxt)
  | (Mul_natint, (x, (y, rest))) ->
      logged_return ((Script_int.mul x y, rest), ctxt)
  | (Mul_natnat, (x, (y, rest))) ->
      logged_return ((Script_int.mul_n x y, rest), ctxt)
  | (Ediv_teznat, (x, (y, rest))) ->
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
      logged_return ((result, rest), ctxt)
  | (Ediv_tez, (x, (y, rest))) ->
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
      logged_return ((result, rest), ctxt)
  | (Ediv_intint, (x, (y, rest))) ->
      logged_return ((Script_int.ediv x y, rest), ctxt)
  | (Ediv_intnat, (x, (y, rest))) ->
      logged_return ((Script_int.ediv x y, rest), ctxt)
  | (Ediv_natint, (x, (y, rest))) ->
      logged_return ((Script_int.ediv x y, rest), ctxt)
  | (Ediv_natnat, (x, (y, rest))) ->
      logged_return ((Script_int.ediv_n x y, rest), ctxt)
  | (Lsl_nat, (x, (y, rest))) -> (
    match Script_int.shift_left_n x y with
    | None ->
        Log.get_log () >>=? fun log -> fail (Overflow (loc, log))
    | Some x ->
        logged_return ((x, rest), ctxt) )
  | (Lsr_nat, (x, (y, rest))) -> (
    match Script_int.shift_right_n x y with
    | None ->
        Log.get_log () >>=? fun log -> fail (Overflow (loc, log))
    | Some r ->
        logged_return ((r, rest), ctxt) )
  | (Or_nat, (x, (y, rest))) ->
      logged_return ((Script_int.logor x y, rest), ctxt)
  | (And_nat, (x, (y, rest))) ->
      logged_return ((Script_int.logand x y, rest), ctxt)
  | (And_int_nat, (x, (y, rest))) ->
      logged_return ((Script_int.logand x y, rest), ctxt)
  | (Xor_nat, (x, (y, rest))) ->
      logged_return ((Script_int.logxor x y, rest), ctxt)
  | (Not_int, (x, rest)) ->
      logged_return ((Script_int.lognot x, rest), ctxt)
  | (Not_nat, (x, rest)) ->
      logged_return ((Script_int.lognot x, rest), ctxt)
  (* control *)
  | (Seq (hd, tl), stack) ->
      non_terminal_recursion ~ctxt hd stack
      >>=? fun (trans, ctxt) ->
      step_bounded logger ~stack_depth ctxt step_constants tl trans
  | (If (bt, _), (true, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bt rest
  | (If (_, bf), (false, rest)) ->
      step_bounded logger ~stack_depth ctxt step_constants bf rest
  | (Loop body, (true, rest)) ->
      non_terminal_recursion ~ctxt body rest
      >>=? fun (trans, ctxt) ->
      step_bounded logger ~stack_depth ctxt step_constants descr trans
  | (Loop _, (false, rest)) ->
      logged_return (rest, ctxt)
  | (Loop_left body, (L v, rest)) ->
      non_terminal_recursion ~ctxt body (v, rest)
      >>=? fun (trans, ctxt) ->
      step_bounded logger ~stack_depth ctxt step_constants descr trans
  | (Loop_left _, (R v, rest)) ->
      logged_return ((v, rest), ctxt)
  | (Dip b, (ign, rest)) ->
      non_terminal_recursion ~ctxt b rest
      >>=? fun (res, ctxt) -> logged_return ((ign, res), ctxt)
  | (Exec, (arg, (Lam (code, _), rest))) ->
      Log.log_interp ctxt code (arg, ()) ;
      non_terminal_recursion ~ctxt code (arg, ())
      >>=? fun ((res, ()), ctxt) -> logged_return ((res, rest), ctxt)
  | (Apply capture_ty, (capture, (lam, rest))) -> (
      let (Lam (descr, expr)) = lam in
      let (Item_t (full_arg_ty, _, _)) = descr.bef in
      unparse_data ctxt Optimized capture_ty capture
      >>=? fun (const_expr, ctxt) ->
      unparse_ty ctxt capture_ty
      >>?= fun (ty_expr, ctxt) ->
      match full_arg_ty with
      | Pair_t ((capture_ty, _, _), (arg_ty, _, _), _) ->
          let arg_stack_ty = Item_t (arg_ty, Empty_t, None) in
          let const_descr =
            ( {
                loc = descr.loc;
                bef = arg_stack_ty;
                aft = Item_t (capture_ty, arg_stack_ty, None);
                instr = Const capture;
              }
              : (_, _) descr )
          in
          let pair_descr =
            ( {
                loc = descr.loc;
                bef = Item_t (capture_ty, arg_stack_ty, None);
                aft = Item_t (full_arg_ty, Empty_t, None);
                instr = Cons_pair;
              }
              : (_, _) descr )
          in
          let seq_descr =
            ( {
                loc = descr.loc;
                bef = arg_stack_ty;
                aft = Item_t (full_arg_ty, Empty_t, None);
                instr = Seq (const_descr, pair_descr);
              }
              : (_, _) descr )
          in
          let full_descr =
            ( {
                loc = descr.loc;
                bef = arg_stack_ty;
                aft = descr.aft;
                instr = Seq (seq_descr, descr);
              }
              : (_, _) descr )
          in
          let full_expr =
            Micheline.Seq
              ( 0,
                [ Prim (0, I_PUSH, [ty_expr; const_expr], []);
                  Prim (0, I_PAIR, [], []);
                  expr ] )
          in
          let lam' = Lam (full_descr, full_expr) in
          logged_return ((lam', rest), ctxt)
      | _ ->
          assert false )
  | (Lambda lam, rest) ->
      logged_return ((lam, rest), ctxt)
  | (Failwith tv, (v, _)) ->
      trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
      >>=? fun (v, _ctxt) ->
      let v = Micheline.strip_locations v in
      Log.get_log () >>=? fun log -> fail (Reject (loc, v, log))
  | (Nop, stack) ->
      logged_return (stack, ctxt)
  (* comparison *)
  | (Compare ty, (a, (b, rest))) ->
      logged_return
        ( ( Script_int.of_int @@ Script_ir_translator.compare_comparable ty a b,
            rest ),
          ctxt )
  (* comparators *)
  | (Eq, (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres = 0) in
      logged_return ((cmpres, rest), ctxt)
  | (Neq, (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres <> 0) in
      logged_return ((cmpres, rest), ctxt)
  | (Lt, (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres < 0) in
      logged_return ((cmpres, rest), ctxt)
  | (Le, (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres <= 0) in
      logged_return ((cmpres, rest), ctxt)
  | (Gt, (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres > 0) in
      logged_return ((cmpres, rest), ctxt)
  | (Ge, (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres >= 0) in
      logged_return ((cmpres, rest), ctxt)
  (* packing *)
  | (Pack t, (value, rest)) ->
      Script_ir_translator.pack_data ctxt t value
      >>=? fun (bytes, ctxt) -> logged_return ((bytes, rest), ctxt)
  | (Unpack ty, (bytes, rest)) ->
      unpack ctxt ~ty ~bytes
      >>=? fun (opt, ctxt) -> logged_return ((opt, rest), ctxt)
  (* protocol *)
  | (Address, ((_, address), rest)) ->
      logged_return ((address, rest), ctxt)
  | (Contract (t, entrypoint), (contract, rest)) -> (
    match (contract, entrypoint) with
    | ((contract, "default"), entrypoint) | ((contract, entrypoint), "default")
      ->
        Script_ir_translator.parse_contract_for_script
          ctxt
          loc
          t
          contract
          ~entrypoint
        >>=? fun (ctxt, maybe_contract) ->
        logged_return ((maybe_contract, rest), ctxt)
    | _ ->
        logged_return ((None, rest), ctxt) )
  | (Transfer_tokens, (p, (amount, ((tp, (destination, entrypoint)), rest))))
    ->
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
      logged_return
        ( ( ( Internal_operation
                {source = step_constants.self; operation; nonce},
              lazy_storage_diff ),
            rest ),
          ctxt )
  | (Implicit_account, (key, rest)) ->
      let contract = Contract.implicit_contract key in
      logged_return (((Unit_t None, (contract, "default")), rest), ctxt)
  | ( Create_contract (storage_type, param_type, Lam (_, code), root_name),
      (* Removed the instruction's arguments manager, spendable and delegatable *)
    (delegate, (credit, (init, rest))) ) ->
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
              {
                code = Script.lazy_expr code;
                storage = Script.lazy_expr storage;
              };
          }
      in
      fresh_internal_nonce ctxt
      >>?= fun (ctxt, nonce) ->
      logged_return
        ( ( ( Internal_operation
                {source = step_constants.self; operation; nonce},
              lazy_storage_diff ),
            ((contract, "default"), rest) ),
          ctxt )
  | (Set_delegate, (delegate, rest)) ->
      let operation = Delegation delegate in
      fresh_internal_nonce ctxt
      >>?= fun (ctxt, nonce) ->
      logged_return
        ( ( ( Internal_operation
                {source = step_constants.self; operation; nonce},
              None ),
            rest ),
          ctxt )
  | (Balance, rest) ->
      Contract.get_balance_carbonated ctxt step_constants.self
      >>=? fun (ctxt, balance) -> logged_return ((balance, rest), ctxt)
  | (Level, rest) ->
      let level =
        (Level.current ctxt).level |> Raw_level.to_int32 |> Script_int.of_int32
        |> Script_int.abs
      in
      logged_return ((level, rest), ctxt)
  | (Now, rest) ->
      let now = Script_timestamp.now ctxt in
      logged_return ((now, rest), ctxt)
  | (Check_signature, (key, (signature, (message, rest)))) ->
      let res = Signature.check key signature message in
      logged_return ((res, rest), ctxt)
  | (Hash_key, (key, rest)) ->
      logged_return ((Signature.Public_key.hash key, rest), ctxt)
  | (Blake2b, (bytes, rest)) ->
      let hash = Raw_hashes.blake2b bytes in
      logged_return ((hash, rest), ctxt)
  | (Sha256, (bytes, rest)) ->
      let hash = Raw_hashes.sha256 bytes in
      logged_return ((hash, rest), ctxt)
  | (Sha512, (bytes, rest)) ->
      let hash = Raw_hashes.sha512 bytes in
      logged_return ((hash, rest), ctxt)
  | (Source, rest) ->
      logged_return (((step_constants.payer, "default"), rest), ctxt)
  | (Sender, rest) ->
      logged_return (((step_constants.source, "default"), rest), ctxt)
  | (Self (t, entrypoint), rest) ->
      logged_return (((t, (step_constants.self, entrypoint)), rest), ctxt)
  | (Self_address, rest) ->
      logged_return (((step_constants.self, "default"), rest), ctxt)
  | (Amount, rest) ->
      logged_return ((step_constants.amount, rest), ctxt)
  | (Dig (_n, n'), stack) ->
      interp_stack_prefix_preserving_operation
        (fun (v, rest) -> return (rest, v))
        n'
        stack
      >>=? fun (aft, x) -> logged_return ((x, aft), ctxt)
  | (Dug (_n, n'), (v, rest)) ->
      interp_stack_prefix_preserving_operation
        (fun stk -> return ((v, stk), ()))
        n'
        rest
      >>=? fun (aft, ()) -> logged_return (aft, ctxt)
  | (Dipn (n, n', b), stack) ->
      interp_stack_prefix_preserving_operation
        (fun stk ->
          non_terminal_recursion
            ~ctxt
            b
            stk
            (* This is a cheap upper bound of the number recursive calls to
               `interp_stack_prefix_preserving_operation`, which does
               ((n / 16) + log2 (n % 16)) iterations *)
            ~stack_depth:(stack_depth + 4 + (n / 16)))
        n'
        stack
      >>=? fun (aft, ctxt') -> logged_return (aft, ctxt')
  | (Dropn (_n, n'), stack) ->
      interp_stack_prefix_preserving_operation
        (fun stk -> return (stk, stk))
        n'
        stack
      >>=? fun (_, rest) -> logged_return (rest, ctxt)
  | (Sapling_empty_state {memo_size}, stack) ->
      logged_return ((Sapling.empty_state ~memo_size (), stack), ctxt)
  | (Sapling_verify_update, (transaction, (state, rest))) -> (
      let address = Contract.to_b58check step_constants.self in
      let chain_id = Chain_id.to_b58check step_constants.chain_id in
      let anti_replay = address ^ chain_id in
      Sapling.verify_update ctxt state transaction anti_replay
      >>=? fun (ctxt, balance_state_opt) ->
      match balance_state_opt with
      | Some (balance, state) ->
          logged_return
            ((Some (Script_int.of_int64 balance, state), rest), ctxt)
      | None ->
          logged_return ((None, rest), ctxt) )
  | (ChainId, rest) ->
      logged_return ((step_constants.chain_id, rest), ctxt)
  | (Never, (_, _)) ->
      .
  | (Voting_power, (key_hash, rest)) ->
      Vote.get_voting_power ctxt key_hash
      >>=? fun (ctxt, rolls) ->
      logged_return ((Script_int.(abs (of_int32 rolls)), rest), ctxt)
  | (Total_voting_power, rest) ->
      Vote.get_total_voting_power ctxt
      >>=? fun (ctxt, rolls) ->
      logged_return ((Script_int.(abs (of_int32 rolls)), rest), ctxt)
  | (Keccak, (bytes, rest)) ->
      let hash = Raw_hashes.keccak256 bytes in
      logged_return ((hash, rest), ctxt)
  | (Sha3, (bytes, rest)) ->
      let hash = Raw_hashes.sha3_256 bytes in
      logged_return ((hash, rest), ctxt)
  | (Add_bls12_381_g1, (x, (y, rest))) ->
      logged_return ((Bls12_381.G1.add x y, rest), ctxt)
  | (Add_bls12_381_g2, (x, (y, rest))) ->
      logged_return ((Bls12_381.G2.add x y, rest), ctxt)
  | (Add_bls12_381_fr, (x, (y, rest))) ->
      logged_return ((Bls12_381.Fr.add x y, rest), ctxt)
  | (Mul_bls12_381_g1, (x, (y, rest))) ->
      logged_return ((Bls12_381.G1.mul x y, rest), ctxt)
  | (Mul_bls12_381_g2, (x, (y, rest))) ->
      logged_return ((Bls12_381.G2.mul x y, rest), ctxt)
  | (Mul_bls12_381_fr, (x, (y, rest))) ->
      logged_return ((Bls12_381.Fr.mul x y, rest), ctxt)
  | (Mul_bls12_381_fr_z, (x, (y, rest))) ->
      let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
      let res = (Bls12_381.Fr.mul x y, rest) in
      logged_return (res, ctxt)
  | (Mul_bls12_381_z_fr, (y, (x, rest))) ->
      let x = Bls12_381.Fr.of_z (Script_int.to_zint x) in
      let res = (Bls12_381.Fr.mul x y, rest) in
      logged_return (res, ctxt)
  | (Int_bls12_381_fr, (x, rest)) ->
      logged_return ((Script_int.of_zint (Bls12_381.Fr.to_z x), rest), ctxt)
  | (Neg_bls12_381_g1, (x, rest)) ->
      logged_return ((Bls12_381.G1.negate x, rest), ctxt)
  | (Neg_bls12_381_g2, (x, rest)) ->
      logged_return ((Bls12_381.G2.negate x, rest), ctxt)
  | (Neg_bls12_381_fr, (x, rest)) ->
      logged_return ((Bls12_381.Fr.negate x, rest), ctxt)
  | (Pairing_check_bls12_381, (pairs, rest)) ->
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
      logged_return ((check, rest), ctxt)
  | (Comb (_, witness), stack) ->
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
      logged_return (aux witness stack, ctxt)
  | (Uncomb (_, witness), stack) ->
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
      logged_return (aux witness stack, ctxt)
  | (Comb_get (_, witness), (comb, stack)) ->
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
      logged_return ((aux witness comb, stack), ctxt)
  | (Comb_set (_, witness), (value, (comb, stack))) ->
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
      logged_return ((aux witness value comb, stack), ctxt)
  | (Dup_n (_, witness), stack) ->
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
      logged_return ((aux witness stack, stack), ctxt)
  (* Tickets *)
  | (Ticket, (contents, (amount, rest))) ->
      let ticketer = (step_constants.self, "default") in
      logged_return (({ticketer; contents; amount}, rest), ctxt)
  | (Read_ticket, (({ticketer; contents; amount}, _) as stack)) ->
      logged_return (((ticketer, (contents, amount)), stack), ctxt)
  | (Split_ticket, (ticket, ((amount_a, amount_b), rest))) ->
      let result =
        if
          Compare.Int.(
            Script_int.(compare (add_n amount_a amount_b) ticket.amount) = 0)
        then
          Some
            ({ticket with amount = amount_a}, {ticket with amount = amount_b})
        else None
      in
      logged_return ((result, rest), ctxt)
  | (Join_tickets contents_ty, ((ticket_a, ticket_b), rest)) ->
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
      logged_return ((result, rest), ctxt)

let step :
    type b a.
    logger ->
    context ->
    step_constants ->
    (b, a) descr ->
    b ->
    (a * context) tzresult Lwt.t =
  step_bounded ~stack_depth:0

let interp :
    type p r.
    logger ->
    context ->
    step_constants ->
    (p, r) lambda ->
    p ->
    (r * context) tzresult Lwt.t =
 fun logger ctxt step_constants (Lam (code, _)) arg ->
  let stack = (arg, ()) in
  let module Log = (val logger) in
  Log.log_interp ctxt code stack ;
  step logger ctxt step_constants code stack
  >|=? fun ((ret, ()), ctxt) -> (ret, ctxt)

(* ---- contract handling ---------------------------------------------------*)
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
    (interp logger ctxt step_constants code (arg, storage))
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

let execute ?(logger = (module No_trace : STEP_LOGGER)) ctxt mode
    step_constants ~script ~entrypoint ~parameter ~internal =
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
