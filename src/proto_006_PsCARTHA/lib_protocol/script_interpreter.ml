(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* ---- Run-time errors -----------------------------------------------------*)

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

type error +=
  | Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error : Contract.t * Script.expr -> error

type error += Bad_contract_parameter of Contract.t (* `Permanent *)

type error += Cannot_serialize_log

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

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
  (* Cannot serialize log *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_log"
    ~title:"Not enough gas to serialize execution trace"
    ~description:
      "Execution trace with stacks was to big to be serialized with the \
       provided gas"
    Data_encoding.empty
    (function Cannot_serialize_log -> Some () | _ -> None)
    (fun () -> Cannot_serialize_log) ;
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

(* ---- interpreter ---------------------------------------------------------*)

type 'tys stack =
  | Item : 'ty * 'rest stack -> ('ty * 'rest) stack
  | Empty : end_of_stack stack

let unparse_stack ctxt (stack, stack_ty) =
  (* We drop the gas limit as this function is only used for debugging/errors. *)
  let ctxt = Gas.set_unlimited ctxt in
  let rec unparse_stack :
      type a.
      a stack * a stack_ty -> (Script.expr * string option) list tzresult Lwt.t
      = function
    | (Empty, Empty_t) ->
        return_nil
    | (Item (v, rest), Item_t (ty, rest_ty, annot)) ->
        unparse_data ctxt Readable ty v
        >>=? fun (data, _ctxt) ->
        unparse_stack (rest, rest_ty)
        >>=? fun rest ->
        let annot =
          match Script_ir_annot.unparse_var_annot annot with
          | [] ->
              None
          | [a] ->
              Some a
          | _ ->
              assert false
        in
        let data = Micheline.strip_locations data in
        return ((data, annot) :: rest)
  in
  unparse_stack (stack, stack_ty)

module Interp_costs = Michelson_v1_gas.Cost_of.Interpreter

let rec interp_stack_prefix_preserving_operation :
    type fbef bef faft aft result.
    (fbef stack -> (faft stack * result) tzresult Lwt.t) ->
    (fbef, faft, bef, aft) stack_prefix_preservation_witness ->
    bef stack ->
    (aft stack * result) tzresult Lwt.t =
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
      Item
        ( v0,
          Item
            ( v1,
              Item
                ( v2,
                  Item
                    ( v3,
                      Item
                        ( v4,
                          Item
                            ( v5,
                              Item
                                ( v6,
                                  Item
                                    ( v7,
                                      Item
                                        ( v8,
                                          Item
                                            ( v9,
                                              Item
                                                ( va,
                                                  Item
                                                    ( vb,
                                                      Item
                                                        ( vc,
                                                          Item
                                                            ( vd,
                                                              Item
                                                                ( ve,
                                                                  Item
                                                                    (vf, rest)
                                                                ) ) ) ) ) ) )
                                    ) ) ) ) ) ) ) ) ) ->
      interp_stack_prefix_preserving_operation f n rest
      >>=? fun (rest', result) ->
      return
        ( Item
            ( v0,
              Item
                ( v1,
                  Item
                    ( v2,
                      Item
                        ( v3,
                          Item
                            ( v4,
                              Item
                                ( v5,
                                  Item
                                    ( v6,
                                      Item
                                        ( v7,
                                          Item
                                            ( v8,
                                              Item
                                                ( v9,
                                                  Item
                                                    ( va,
                                                      Item
                                                        ( vb,
                                                          Item
                                                            ( vc,
                                                              Item
                                                                ( vd,
                                                                  Item
                                                                    ( ve,
                                                                      Item
                                                                        ( vf,
                                                                          rest'
                                                                        ) ) )
                                                            ) ) ) ) ) ) ) ) )
                        ) ) ) ),
          result )
  | ( Prefix (Prefix (Prefix (Prefix n))),
      Item (v0, Item (v1, Item (v2, Item (v3, rest)))) ) ->
      interp_stack_prefix_preserving_operation f n rest
      >>=? fun (rest', result) ->
      return (Item (v0, Item (v1, Item (v2, Item (v3, rest')))), result)
  | (Prefix n, Item (v, rest)) ->
      interp_stack_prefix_preserving_operation f n rest
      >>=? fun (rest', result) -> return (Item (v, rest'), result)
  | (Rest, v) ->
      f v

type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

let rec step :
    type b a.
    ?log:execution_trace ref ->
    context ->
    step_constants ->
    (b, a) descr ->
    b stack ->
    (a stack * context) tzresult Lwt.t =
 fun ?log ctxt step_constants ({instr; loc; _} as descr) stack ->
  Lwt.return (Gas.consume ctxt Interp_costs.cycle)
  >>=? fun ctxt ->
  let logged_return :
      type a b.
      (b, a) descr -> a stack * context -> (a stack * context) tzresult Lwt.t =
   fun descr (ret, ctxt) ->
    match log with
    | None ->
        return (ret, ctxt)
    | Some log ->
        trace Cannot_serialize_log (unparse_stack ctxt (ret, descr.aft))
        >>=? fun stack ->
        log := (descr.loc, Gas.level ctxt, stack) :: !log ;
        return (ret, ctxt)
  in
  let get_log (log : execution_trace ref option) =
    Option.map ~f:(fun l -> List.rev !l) log
  in
  let consume_gas_terop :
      type ret arg1 arg2 arg3 rest.
      (_ * (_ * (_ * rest)), ret * rest) descr ->
      (arg1 -> arg2 -> arg3 -> ret) * arg1 * arg2 * arg3 ->
      (arg1 -> arg2 -> arg3 -> Gas.cost) ->
      rest stack ->
      ((ret * rest) stack * context) tzresult Lwt.t =
   fun descr (op, x1, x2, x3) cost_func rest ->
    Lwt.return (Gas.consume ctxt (cost_func x1 x2 x3))
    >>=? fun ctxt -> logged_return descr (Item (op x1 x2 x3, rest), ctxt)
  in
  let consume_gas_binop :
      type ret arg1 arg2 rest.
      (_ * (_ * rest), ret * rest) descr ->
      (arg1 -> arg2 -> ret) * arg1 * arg2 ->
      (arg1 -> arg2 -> Gas.cost) ->
      rest stack ->
      context ->
      ((ret * rest) stack * context) tzresult Lwt.t =
   fun descr (op, x1, x2) cost_func rest ctxt ->
    Lwt.return (Gas.consume ctxt (cost_func x1 x2))
    >>=? fun ctxt -> logged_return descr (Item (op x1 x2, rest), ctxt)
  in
  let consume_gas_unop :
      type ret arg rest.
      (_ * rest, ret * rest) descr ->
      (arg -> ret) * arg ->
      (arg -> Gas.cost) ->
      rest stack ->
      context ->
      ((ret * rest) stack * context) tzresult Lwt.t =
   fun descr (op, arg) cost_func rest ctxt ->
    Lwt.return (Gas.consume ctxt (cost_func arg))
    >>=? fun ctxt -> logged_return descr (Item (op arg, rest), ctxt)
  in
  let logged_return : a stack * context -> (a stack * context) tzresult Lwt.t =
    logged_return descr
  in
  match (instr, stack) with
  (* stack ops *)
  | (Drop, Item (_, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.stack_op)
      >>=? fun ctxt -> logged_return (rest, ctxt)
  | (Dup, Item (v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.stack_op)
      >>=? fun ctxt -> logged_return (Item (v, Item (v, rest)), ctxt)
  | (Swap, Item (vi, Item (vo, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.stack_op)
      >>=? fun ctxt -> logged_return (Item (vo, Item (vi, rest)), ctxt)
  | (Const v, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.push)
      >>=? fun ctxt -> logged_return (Item (v, rest), ctxt)
  (* options *)
  | (Cons_some, Item (v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.wrap)
      >>=? fun ctxt -> logged_return (Item (Some v, rest), ctxt)
  | (Cons_none _, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.variant_no_data)
      >>=? fun ctxt -> logged_return (Item (None, rest), ctxt)
  | (If_none (bt, _), Item (None, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bt rest
  | (If_none (_, bf), Item (Some v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bf (Item (v, rest))
  (* pairs *)
  | (Cons_pair, Item (a, Item (b, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.pair)
      >>=? fun ctxt -> logged_return (Item ((a, b), rest), ctxt)
  | (Car, Item ((a, _), rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.pair_access)
      >>=? fun ctxt -> logged_return (Item (a, rest), ctxt)
  | (Cdr, Item ((_, b), rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.pair_access)
      >>=? fun ctxt -> logged_return (Item (b, rest), ctxt)
  (* unions *)
  | (Left, Item (v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.wrap)
      >>=? fun ctxt -> logged_return (Item (L v, rest), ctxt)
  | (Right, Item (v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.wrap)
      >>=? fun ctxt -> logged_return (Item (R v, rest), ctxt)
  | (If_left (bt, _), Item (L v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bt (Item (v, rest))
  | (If_left (_, bf), Item (R v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bf (Item (v, rest))
  (* lists *)
  | (Cons_list, Item (hd, Item (tl, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.cons)
      >>=? fun ctxt -> logged_return (Item (hd :: tl, rest), ctxt)
  | (Nil, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.variant_no_data)
      >>=? fun ctxt -> logged_return (Item ([], rest), ctxt)
  | (If_cons (_, bf), Item ([], rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bf rest
  | (If_cons (bt, _), Item (hd :: tl, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt ->
      step ?log ctxt step_constants bt (Item (hd, Item (tl, rest)))
  | (List_map body, Item (l, rest)) ->
      let rec loop rest ctxt l acc =
        Lwt.return (Gas.consume ctxt Interp_costs.loop_map)
        >>=? fun ctxt ->
        match l with
        | [] ->
            return (Item (List.rev acc, rest), ctxt)
        | hd :: tl ->
            step ?log ctxt step_constants body (Item (hd, rest))
            >>=? fun (Item (hd, rest), ctxt) -> loop rest ctxt tl (hd :: acc)
      in
      loop rest ctxt l [] >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (List_size, Item (list, rest)) ->
      Lwt.return
        (List.fold_left
           (fun acc _ ->
             acc
             >>? fun (size, ctxt) ->
             Gas.consume ctxt Interp_costs.loop_size
             >>? fun ctxt -> ok (size + 1 (* FIXME: overflow *), ctxt))
           (ok (0, ctxt))
           list)
      >>=? fun (len, ctxt) ->
      logged_return (Item (Script_int.(abs (of_int len)), rest), ctxt)
  | (List_iter body, Item (l, init)) ->
      let rec loop ctxt l stack =
        Lwt.return (Gas.consume ctxt Interp_costs.loop_iter)
        >>=? fun ctxt ->
        match l with
        | [] ->
            return (stack, ctxt)
        | hd :: tl ->
            step ?log ctxt step_constants body (Item (hd, stack))
            >>=? fun (stack, ctxt) -> loop ctxt tl stack
      in
      loop ctxt l init >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  (* sets *)
  | (Empty_set t, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.empty_set)
      >>=? fun ctxt -> logged_return (Item (empty_set t, rest), ctxt)
  | (Set_iter body, Item (set, init)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.set_to_list set))
      >>=? fun ctxt ->
      let l = List.rev (set_fold (fun e acc -> e :: acc) set []) in
      let rec loop ctxt l stack =
        Lwt.return (Gas.consume ctxt Interp_costs.loop_iter)
        >>=? fun ctxt ->
        match l with
        | [] ->
            return (stack, ctxt)
        | hd :: tl ->
            step ?log ctxt step_constants body (Item (hd, stack))
            >>=? fun (stack, ctxt) -> loop ctxt tl stack
      in
      loop ctxt l init >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (Set_mem, Item (v, Item (set, rest))) ->
      consume_gas_binop descr (set_mem, v, set) Interp_costs.set_mem rest ctxt
  | (Set_update, Item (v, Item (presence, Item (set, rest)))) ->
      consume_gas_terop
        descr
        (set_update, v, presence, set)
        Interp_costs.set_update
        rest
  | (Set_size, Item (set, rest)) ->
      consume_gas_unop
        descr
        (set_size, set)
        (fun _ -> Interp_costs.set_size)
        rest
        ctxt
  (* maps *)
  | (Empty_map (t, _), rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.empty_map)
      >>=? fun ctxt -> logged_return (Item (empty_map t, rest), ctxt)
  | (Map_map body, Item (map, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map))
      >>=? fun ctxt ->
      let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
      let rec loop rest ctxt l acc =
        Lwt.return (Gas.consume ctxt Interp_costs.loop_map)
        >>=? fun ctxt ->
        match l with
        | [] ->
            return (Item (acc, rest), ctxt)
        | ((k, _) as hd) :: tl ->
            step ?log ctxt step_constants body (Item (hd, rest))
            >>=? fun (Item (hd, rest), ctxt) ->
            loop rest ctxt tl (map_update k (Some hd) acc)
      in
      loop rest ctxt l (empty_map (map_key_ty map))
      >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (Map_iter body, Item (map, init)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.map_to_list map))
      >>=? fun ctxt ->
      let l = List.rev (map_fold (fun k v acc -> (k, v) :: acc) map []) in
      let rec loop ctxt l stack =
        Lwt.return (Gas.consume ctxt Interp_costs.loop_iter)
        >>=? fun ctxt ->
        match l with
        | [] ->
            return (stack, ctxt)
        | hd :: tl ->
            step ?log ctxt step_constants body (Item (hd, stack))
            >>=? fun (stack, ctxt) -> loop ctxt tl stack
      in
      loop ctxt l init >>=? fun (res, ctxt) -> logged_return (res, ctxt)
  | (Map_mem, Item (v, Item (map, rest))) ->
      consume_gas_binop descr (map_mem, v, map) Interp_costs.map_mem rest ctxt
  | (Map_get, Item (v, Item (map, rest))) ->
      consume_gas_binop descr (map_get, v, map) Interp_costs.map_get rest ctxt
  | (Map_update, Item (k, Item (v, Item (map, rest)))) ->
      consume_gas_terop
        descr
        (map_update, k, v, map)
        Interp_costs.map_update
        rest
  | (Map_size, Item (map, rest)) ->
      consume_gas_unop
        descr
        (map_size, map)
        (fun _ -> Interp_costs.map_size)
        rest
        ctxt
  (* Big map operations *)
  | (Empty_big_map (tk, tv), rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.empty_map)
      >>=? fun ctxt ->
      logged_return
        (Item (Script_ir_translator.empty_big_map tk tv, rest), ctxt)
  | (Big_map_mem, Item (key, Item (map, rest))) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.map_mem key map.diff))
      >>=? fun ctxt ->
      Script_ir_translator.big_map_mem ctxt key map
      >>=? fun (res, ctxt) -> logged_return (Item (res, rest), ctxt)
  | (Big_map_get, Item (key, Item (map, rest))) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.map_get key map.diff))
      >>=? fun ctxt ->
      Script_ir_translator.big_map_get ctxt key map
      >>=? fun (res, ctxt) -> logged_return (Item (res, rest), ctxt)
  | (Big_map_update, Item (key, Item (maybe_value, Item (map, rest)))) ->
      consume_gas_terop
        descr
        (Script_ir_translator.big_map_update, key, maybe_value, map)
        (fun k v m -> Interp_costs.map_update k (Some v) m.diff)
        rest
  (* timestamp operations *)
  | (Add_seconds_to_timestamp, Item (n, Item (t, rest))) ->
      consume_gas_binop
        descr
        (Script_timestamp.add_delta, t, n)
        Interp_costs.add_timestamp
        rest
        ctxt
  | (Add_timestamp_to_seconds, Item (t, Item (n, rest))) ->
      consume_gas_binop
        descr
        (Script_timestamp.add_delta, t, n)
        Interp_costs.add_timestamp
        rest
        ctxt
  | (Sub_timestamp_seconds, Item (t, Item (s, rest))) ->
      consume_gas_binop
        descr
        (Script_timestamp.sub_delta, t, s)
        Interp_costs.sub_timestamp
        rest
        ctxt
  | (Diff_timestamps, Item (t1, Item (t2, rest))) ->
      consume_gas_binop
        descr
        (Script_timestamp.diff, t1, t2)
        Interp_costs.diff_timestamps
        rest
        ctxt
  (* string operations *)
  | (Concat_string_pair, Item (x, Item (y, rest))) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.concat_string [x; y]))
      >>=? fun ctxt ->
      let s = String.concat "" [x; y] in
      logged_return (Item (s, rest), ctxt)
  | (Concat_string, Item (ss, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.concat_string ss))
      >>=? fun ctxt ->
      let s = String.concat "" ss in
      logged_return (Item (s, rest), ctxt)
  | (Slice_string, Item (offset, Item (length, Item (s, rest)))) ->
      let s_length = Z.of_int (String.length s) in
      let offset = Script_int.to_zint offset in
      let length = Script_int.to_zint length in
      if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
        Lwt.return
          (Gas.consume ctxt (Interp_costs.slice_string (Z.to_int length)))
        >>=? fun ctxt ->
        logged_return
          ( Item (Some (String.sub s (Z.to_int offset) (Z.to_int length)), rest),
            ctxt )
      else
        Lwt.return (Gas.consume ctxt (Interp_costs.slice_string 0))
        >>=? fun ctxt -> logged_return (Item (None, rest), ctxt)
  | (String_size, Item (s, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.push)
      >>=? fun ctxt ->
      logged_return
        (Item (Script_int.(abs (of_int (String.length s))), rest), ctxt)
  (* bytes operations *)
  | (Concat_bytes_pair, Item (x, Item (y, rest))) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.concat_bytes [x; y]))
      >>=? fun ctxt ->
      let s = MBytes.concat "" [x; y] in
      logged_return (Item (s, rest), ctxt)
  | (Concat_bytes, Item (ss, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.concat_bytes ss))
      >>=? fun ctxt ->
      let s = MBytes.concat "" ss in
      logged_return (Item (s, rest), ctxt)
  | (Slice_bytes, Item (offset, Item (length, Item (s, rest)))) ->
      let s_length = Z.of_int (MBytes.length s) in
      let offset = Script_int.to_zint offset in
      let length = Script_int.to_zint length in
      if Compare.Z.(offset < s_length && Z.add offset length <= s_length) then
        Lwt.return
          (Gas.consume ctxt (Interp_costs.slice_string (Z.to_int length)))
        >>=? fun ctxt ->
        logged_return
          ( Item (Some (MBytes.sub s (Z.to_int offset) (Z.to_int length)), rest),
            ctxt )
      else
        Lwt.return (Gas.consume ctxt (Interp_costs.slice_string 0))
        >>=? fun ctxt -> logged_return (Item (None, rest), ctxt)
  | (Bytes_size, Item (s, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.push)
      >>=? fun ctxt ->
      logged_return
        (Item (Script_int.(abs (of_int (MBytes.length s))), rest), ctxt)
  (* currency operations *)
  | (Add_tez, Item (x, Item (y, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.int64_op)
      >>=? fun ctxt ->
      Lwt.return Tez.(x +? y)
      >>=? fun res -> logged_return (Item (res, rest), ctxt)
  | (Sub_tez, Item (x, Item (y, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.int64_op)
      >>=? fun ctxt ->
      Lwt.return Tez.(x -? y)
      >>=? fun res -> logged_return (Item (res, rest), ctxt)
  | (Mul_teznat, Item (x, Item (y, rest))) -> (
      Lwt.return (Gas.consume ctxt Interp_costs.int64_op)
      >>=? fun ctxt ->
      Lwt.return (Gas.consume ctxt Interp_costs.z_to_int64)
      >>=? fun ctxt ->
      match Script_int.to_int64 y with
      | None ->
          fail (Overflow (loc, get_log log))
      | Some y ->
          Lwt.return Tez.(x *? y)
          >>=? fun res -> logged_return (Item (res, rest), ctxt) )
  | (Mul_nattez, Item (y, Item (x, rest))) -> (
      Lwt.return (Gas.consume ctxt Interp_costs.int64_op)
      >>=? fun ctxt ->
      Lwt.return (Gas.consume ctxt Interp_costs.z_to_int64)
      >>=? fun ctxt ->
      match Script_int.to_int64 y with
      | None ->
          fail (Overflow (loc, get_log log))
      | Some y ->
          Lwt.return Tez.(x *? y)
          >>=? fun res -> logged_return (Item (res, rest), ctxt) )
  (* boolean operations *)
  | (Or, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (( || ), x, y) Interp_costs.bool_binop rest ctxt
  | (And, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (( && ), x, y) Interp_costs.bool_binop rest ctxt
  | (Xor, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Compare.Bool.( <> ), x, y)
        Interp_costs.bool_binop
        rest
        ctxt
  | (Not, Item (x, rest)) ->
      consume_gas_unop descr (not, x) Interp_costs.bool_unop rest ctxt
  (* integer operations *)
  | (Is_nat, Item (x, rest)) ->
      consume_gas_unop descr (Script_int.is_nat, x) Interp_costs.abs rest ctxt
  | (Abs_int, Item (x, rest)) ->
      consume_gas_unop descr (Script_int.abs, x) Interp_costs.abs rest ctxt
  | (Int_nat, Item (x, rest)) ->
      consume_gas_unop descr (Script_int.int, x) Interp_costs.int rest ctxt
  | (Neg_int, Item (x, rest)) ->
      consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest ctxt
  | (Neg_nat, Item (x, rest)) ->
      consume_gas_unop descr (Script_int.neg, x) Interp_costs.neg rest ctxt
  | (Add_intint, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
  | (Add_intnat, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
  | (Add_natint, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.add, x, y) Interp_costs.add rest ctxt
  | (Add_natnat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.add_n, x, y)
        Interp_costs.add
        rest
        ctxt
  | (Sub_int, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.sub, x, y) Interp_costs.sub rest ctxt
  | (Mul_intint, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
  | (Mul_intnat, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
  | (Mul_natint, Item (x, Item (y, rest))) ->
      consume_gas_binop descr (Script_int.mul, x, y) Interp_costs.mul rest ctxt
  | (Mul_natnat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.mul_n, x, y)
        Interp_costs.mul
        rest
        ctxt
  | (Ediv_teznat, Item (x, Item (y, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z)
      >>=? fun ctxt ->
      let x = Script_int.of_int64 (Tez.to_mutez x) in
      consume_gas_binop
        descr
        ( (fun x y ->
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
                  assert false )),
          x,
          y )
        Interp_costs.div
        rest
        ctxt
  | (Ediv_tez, Item (x, Item (y, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z)
      >>=? fun ctxt ->
      Lwt.return (Gas.consume ctxt Interp_costs.int64_to_z)
      >>=? fun ctxt ->
      let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
      let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
      consume_gas_binop
        descr
        ( (fun x y ->
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
                    Some (q, r) ) )),
          x,
          y )
        Interp_costs.div
        rest
        ctxt
  | (Ediv_intint, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.ediv, x, y)
        Interp_costs.div
        rest
        ctxt
  | (Ediv_intnat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.ediv, x, y)
        Interp_costs.div
        rest
        ctxt
  | (Ediv_natint, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.ediv, x, y)
        Interp_costs.div
        rest
        ctxt
  | (Ediv_natnat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.ediv_n, x, y)
        Interp_costs.div
        rest
        ctxt
  | (Lsl_nat, Item (x, Item (y, rest))) -> (
      Lwt.return (Gas.consume ctxt (Interp_costs.shift_left x y))
      >>=? fun ctxt ->
      match Script_int.shift_left_n x y with
      | None ->
          fail (Overflow (loc, get_log log))
      | Some x ->
          logged_return (Item (x, rest), ctxt) )
  | (Lsr_nat, Item (x, Item (y, rest))) -> (
      Lwt.return (Gas.consume ctxt (Interp_costs.shift_right x y))
      >>=? fun ctxt ->
      match Script_int.shift_right_n x y with
      | None ->
          fail (Overflow (loc, get_log log))
      | Some r ->
          logged_return (Item (r, rest), ctxt) )
  | (Or_nat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.logor, x, y)
        Interp_costs.logor
        rest
        ctxt
  | (And_nat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.logand, x, y)
        Interp_costs.logand
        rest
        ctxt
  | (And_int_nat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.logand, x, y)
        Interp_costs.logand
        rest
        ctxt
  | (Xor_nat, Item (x, Item (y, rest))) ->
      consume_gas_binop
        descr
        (Script_int.logxor, x, y)
        Interp_costs.logxor
        rest
        ctxt
  | (Not_int, Item (x, rest)) ->
      consume_gas_unop
        descr
        (Script_int.lognot, x)
        Interp_costs.lognot
        rest
        ctxt
  | (Not_nat, Item (x, rest)) ->
      consume_gas_unop
        descr
        (Script_int.lognot, x)
        Interp_costs.lognot
        rest
        ctxt
  (* control *)
  | (Seq (hd, tl), stack) ->
      step ?log ctxt step_constants hd stack
      >>=? fun (trans, ctxt) -> step ?log ctxt step_constants tl trans
  | (If (bt, _), Item (true, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bt rest
  | (If (_, bf), Item (false, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.branch)
      >>=? fun ctxt -> step ?log ctxt step_constants bf rest
  | (Loop body, Item (true, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle)
      >>=? fun ctxt ->
      step ?log ctxt step_constants body rest
      >>=? fun (trans, ctxt) -> step ?log ctxt step_constants descr trans
  | (Loop _, Item (false, rest)) ->
      logged_return (rest, ctxt)
  | (Loop_left body, Item (L v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle)
      >>=? fun ctxt ->
      step ?log ctxt step_constants body (Item (v, rest))
      >>=? fun (trans, ctxt) -> step ?log ctxt step_constants descr trans
  | (Loop_left _, Item (R v, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.loop_cycle)
      >>=? fun ctxt -> logged_return (Item (v, rest), ctxt)
  | (Dip b, Item (ign, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.stack_op)
      >>=? fun ctxt ->
      step ?log ctxt step_constants b rest
      >>=? fun (res, ctxt) -> logged_return (Item (ign, res), ctxt)
  | (Exec, Item (arg, Item (lam, rest))) ->
      Lwt.return (Gas.consume ctxt Interp_costs.exec)
      >>=? fun ctxt ->
      interp ?log ctxt step_constants lam arg
      >>=? fun (res, ctxt) -> logged_return (Item (res, rest), ctxt)
  | (Apply capture_ty, Item (capture, Item (lam, rest))) -> (
      Lwt.return (Gas.consume ctxt Interp_costs.apply)
      >>=? fun ctxt ->
      let (Lam (descr, expr)) = lam in
      let (Item_t (full_arg_ty, _, _)) = descr.bef in
      unparse_data ctxt Optimized capture_ty capture
      >>=? fun (const_expr, ctxt) ->
      unparse_ty ctxt capture_ty
      >>=? fun (ty_expr, ctxt) ->
      match full_arg_ty with
      | Pair_t ((capture_ty, _, _), (arg_ty, _, _), _, _) ->
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
          logged_return (Item (lam', rest), ctxt)
      | _ ->
          assert false )
  | (Lambda lam, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.push)
      >>=? fun ctxt -> logged_return (Item (lam, rest), ctxt)
  | (Failwith tv, Item (v, _)) ->
      trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
      >>=? fun (v, _ctxt) ->
      let v = Micheline.strip_locations v in
      fail (Reject (loc, v, get_log log))
  | (Nop, stack) ->
      logged_return (stack, ctxt)
  (* comparison *)
  | (Compare ty, Item (a, Item (b, rest))) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.compare ty a b))
      >>=? fun ctxt ->
      logged_return
        ( Item
            ( Script_int.of_int
              @@ Script_ir_translator.compare_comparable ty a b,
              rest ),
          ctxt )
  (* comparators *)
  | (Eq, Item (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres = 0) in
      Lwt.return (Gas.consume ctxt Interp_costs.compare_res)
      >>=? fun ctxt -> logged_return (Item (cmpres, rest), ctxt)
  | (Neq, Item (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres <> 0) in
      Lwt.return (Gas.consume ctxt Interp_costs.compare_res)
      >>=? fun ctxt -> logged_return (Item (cmpres, rest), ctxt)
  | (Lt, Item (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres < 0) in
      Lwt.return (Gas.consume ctxt Interp_costs.compare_res)
      >>=? fun ctxt -> logged_return (Item (cmpres, rest), ctxt)
  | (Le, Item (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres <= 0) in
      Lwt.return (Gas.consume ctxt Interp_costs.compare_res)
      >>=? fun ctxt -> logged_return (Item (cmpres, rest), ctxt)
  | (Gt, Item (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres > 0) in
      Lwt.return (Gas.consume ctxt Interp_costs.compare_res)
      >>=? fun ctxt -> logged_return (Item (cmpres, rest), ctxt)
  | (Ge, Item (cmpres, rest)) ->
      let cmpres = Script_int.compare cmpres Script_int.zero in
      let cmpres = Compare.Int.(cmpres >= 0) in
      Lwt.return (Gas.consume ctxt Interp_costs.compare_res)
      >>=? fun ctxt -> logged_return (Item (cmpres, rest), ctxt)
  (* packing *)
  | (Pack t, Item (value, rest)) ->
      Script_ir_translator.pack_data ctxt t value
      >>=? fun (bytes, ctxt) -> logged_return (Item (bytes, rest), ctxt)
  | (Unpack t, Item (bytes, rest)) ->
      Lwt.return (Gas.check_enough ctxt (Script.serialized_cost bytes))
      >>=? fun () ->
      if
        Compare.Int.(MBytes.length bytes >= 1)
        && Compare.Int.(MBytes.get_uint8 bytes 0 = 0x05)
      then
        let bytes = MBytes.sub bytes 1 (MBytes.length bytes - 1) in
        match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
        | None ->
            Lwt.return (Gas.consume ctxt (Interp_costs.unpack_failed bytes))
            >>=? fun ctxt -> logged_return (Item (None, rest), ctxt)
        | Some expr -> (
            Lwt.return (Gas.consume ctxt (Script.deserialized_cost expr))
            >>=? fun ctxt ->
            parse_data ctxt ~legacy:false t (Micheline.root expr)
            >>= function
            | Ok (value, ctxt) ->
                logged_return (Item (Some value, rest), ctxt)
            | Error _ignored ->
                Lwt.return
                  (Gas.consume ctxt (Interp_costs.unpack_failed bytes))
                >>=? fun ctxt -> logged_return (Item (None, rest), ctxt) )
      else logged_return (Item (None, rest), ctxt)
  (* protocol *)
  | (Address, Item ((_, address), rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.address)
      >>=? fun ctxt -> logged_return (Item (address, rest), ctxt)
  | (Contract (t, entrypoint), Item (contract, rest)) -> (
      Lwt.return (Gas.consume ctxt Interp_costs.contract)
      >>=? fun ctxt ->
      match (contract, entrypoint) with
      | ((contract, "default"), entrypoint)
      | ((contract, entrypoint), "default") ->
          Script_ir_translator.parse_contract_for_script
            ~legacy:false
            ctxt
            loc
            t
            contract
            ~entrypoint
          >>=? fun (ctxt, maybe_contract) ->
          logged_return (Item (maybe_contract, rest), ctxt)
      | _ ->
          logged_return (Item (None, rest), ctxt) )
  | ( Transfer_tokens,
      Item (p, Item (amount, Item ((tp, (destination, entrypoint)), rest))) )
    ->
      Lwt.return (Gas.consume ctxt Interp_costs.transfer)
      >>=? fun ctxt ->
      collect_big_maps ctxt tp p
      >>=? fun (to_duplicate, ctxt) ->
      let to_update = no_big_map_id in
      extract_big_map_diff
        ctxt
        Optimized
        tp
        p
        ~to_duplicate
        ~to_update
        ~temporary:true
      >>=? fun (p, big_map_diff, ctxt) ->
      unparse_data ctxt Optimized tp p
      >>=? fun (p, ctxt) ->
      let operation =
        Transaction
          {
            amount;
            destination;
            entrypoint;
            parameters = Script.lazy_expr (Micheline.strip_locations p);
          }
      in
      Lwt.return (fresh_internal_nonce ctxt)
      >>=? fun (ctxt, nonce) ->
      logged_return
        ( Item
            ( ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                big_map_diff ),
              rest ),
          ctxt )
  | ( Create_account,
      Item (manager, Item (delegate, Item (_delegatable, Item (credit, rest))))
    ) ->
      Lwt.return (Gas.consume ctxt Interp_costs.create_account)
      >>=? fun ctxt ->
      Contract.fresh_contract_from_current_nonce ctxt
      >>=? fun (ctxt, contract) ->
      (* store in optimized binary representation - as unparsed with [Optimized]. *)
      let manager_bytes =
        Data_encoding.Binary.to_bytes_exn
          Signature.Public_key_hash.encoding
          manager
      in
      let storage =
        Script_repr.lazy_expr @@ Micheline.strip_locations
        @@ Micheline.Bytes (0, manager_bytes)
      in
      let script = {code = Legacy_support.manager_script_code; storage} in
      let operation =
        Origination {credit; delegate; preorigination = Some contract; script}
      in
      Lwt.return (fresh_internal_nonce ctxt)
      >>=? fun (ctxt, nonce) ->
      logged_return
        ( Item
            ( ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                None ),
              Item ((contract, "default"), rest) ),
          ctxt )
  | (Implicit_account, Item (key, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.implicit_account)
      >>=? fun ctxt ->
      let contract = Contract.implicit_contract key in
      logged_return (Item ((Unit_t None, (contract, "default")), rest), ctxt)
  | ( Create_contract (storage_type, param_type, Lam (_, code), root_name),
      Item
        ( manager,
          Item
            ( delegate,
              Item
                ( spendable,
                  Item (delegatable, Item (credit, Item (init, rest))) ) ) ) )
    ->
      Lwt.return (Gas.consume ctxt Interp_costs.create_contract)
      >>=? fun ctxt ->
      unparse_ty ctxt param_type
      >>=? fun (unparsed_param_type, ctxt) ->
      let unparsed_param_type =
        Script_ir_translator.add_field_annot
          (Option.map ~f:(fun n -> `Field_annot n) root_name)
          None
          unparsed_param_type
      in
      unparse_ty ctxt storage_type
      >>=? fun (unparsed_storage_type, ctxt) ->
      let code =
        Script.lazy_expr
        @@ Micheline.strip_locations
             (Seq
                ( 0,
                  [ Prim (0, K_parameter, [unparsed_param_type], []);
                    Prim (0, K_storage, [unparsed_storage_type], []);
                    Prim (0, K_code, [code], []) ] ))
      in
      collect_big_maps ctxt storage_type init
      >>=? fun (to_duplicate, ctxt) ->
      let to_update = no_big_map_id in
      extract_big_map_diff
        ctxt
        Optimized
        storage_type
        init
        ~to_duplicate
        ~to_update
        ~temporary:true
      >>=? fun (init, big_map_diff, ctxt) ->
      unparse_data ctxt Optimized storage_type init
      >>=? fun (storage, ctxt) ->
      let storage = Script.lazy_expr @@ Micheline.strip_locations storage in
      ( if spendable then
        Legacy_support.add_do
          ~manager_pkh:manager
          ~script_code:code
          ~script_storage:storage
      else if delegatable then
        Legacy_support.add_set_delegate
          ~manager_pkh:manager
          ~script_code:code
          ~script_storage:storage
      else if Legacy_support.has_default_entrypoint code then
        Legacy_support.add_root_entrypoint code
        >>=? fun code -> return (code, storage)
      else return (code, storage) )
      >>=? fun (code, storage) ->
      Contract.fresh_contract_from_current_nonce ctxt
      >>=? fun (ctxt, contract) ->
      let operation =
        Origination
          {
            credit;
            delegate;
            preorigination = Some contract;
            script = {code; storage};
          }
      in
      Lwt.return (fresh_internal_nonce ctxt)
      >>=? fun (ctxt, nonce) ->
      logged_return
        ( Item
            ( ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                big_map_diff ),
              Item ((contract, "default"), rest) ),
          ctxt )
  | ( Create_contract_2 (storage_type, param_type, Lam (_, code), root_name),
      (* Removed the instruction's arguments manager, spendable and delegatable *)
    Item (delegate, Item (credit, Item (init, rest))) ) ->
      Lwt.return (Gas.consume ctxt Interp_costs.create_contract)
      >>=? fun ctxt ->
      unparse_ty ctxt param_type
      >>=? fun (unparsed_param_type, ctxt) ->
      let unparsed_param_type =
        Script_ir_translator.add_field_annot
          (Option.map ~f:(fun n -> `Field_annot n) root_name)
          None
          unparsed_param_type
      in
      unparse_ty ctxt storage_type
      >>=? fun (unparsed_storage_type, ctxt) ->
      let code =
        Micheline.strip_locations
          (Seq
             ( 0,
               [ Prim (0, K_parameter, [unparsed_param_type], []);
                 Prim (0, K_storage, [unparsed_storage_type], []);
                 Prim (0, K_code, [code], []) ] ))
      in
      collect_big_maps ctxt storage_type init
      >>=? fun (to_duplicate, ctxt) ->
      let to_update = no_big_map_id in
      extract_big_map_diff
        ctxt
        Optimized
        storage_type
        init
        ~to_duplicate
        ~to_update
        ~temporary:true
      >>=? fun (init, big_map_diff, ctxt) ->
      unparse_data ctxt Optimized storage_type init
      >>=? fun (storage, ctxt) ->
      let storage = Micheline.strip_locations storage in
      Contract.fresh_contract_from_current_nonce ctxt
      >>=? fun (ctxt, contract) ->
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
      Lwt.return (fresh_internal_nonce ctxt)
      >>=? fun (ctxt, nonce) ->
      logged_return
        ( Item
            ( ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                big_map_diff ),
              Item ((contract, "default"), rest) ),
          ctxt )
  | (Set_delegate, Item (delegate, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.create_account)
      >>=? fun ctxt ->
      let operation = Delegation delegate in
      Lwt.return (fresh_internal_nonce ctxt)
      >>=? fun (ctxt, nonce) ->
      logged_return
        ( Item
            ( ( Internal_operation
                  {source = step_constants.self; operation; nonce},
                None ),
              rest ),
          ctxt )
  | (Balance, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.balance)
      >>=? fun ctxt ->
      Contract.get_balance ctxt step_constants.self
      >>=? fun balance -> logged_return (Item (balance, rest), ctxt)
  | (Now, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.now)
      >>=? fun ctxt ->
      let now = Script_timestamp.now ctxt in
      logged_return (Item (now, rest), ctxt)
  | (Check_signature, Item (key, Item (signature, Item (message, rest)))) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.check_signature key message))
      >>=? fun ctxt ->
      let res = Signature.check key signature message in
      logged_return (Item (res, rest), ctxt)
  | (Hash_key, Item (key, rest)) ->
      Lwt.return (Gas.consume ctxt Interp_costs.hash_key)
      >>=? fun ctxt ->
      logged_return (Item (Signature.Public_key.hash key, rest), ctxt)
  | (Blake2b, Item (bytes, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.hash_blake2b bytes))
      >>=? fun ctxt ->
      let hash = Raw_hashes.blake2b bytes in
      logged_return (Item (hash, rest), ctxt)
  | (Sha256, Item (bytes, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.hash_sha256 bytes))
      >>=? fun ctxt ->
      let hash = Raw_hashes.sha256 bytes in
      logged_return (Item (hash, rest), ctxt)
  | (Sha512, Item (bytes, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.hash_sha512 bytes))
      >>=? fun ctxt ->
      let hash = Raw_hashes.sha512 bytes in
      logged_return (Item (hash, rest), ctxt)
  | (Steps_to_quota, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.steps_to_quota)
      >>=? fun ctxt ->
      let steps =
        match Gas.level ctxt with
        | Limited {remaining} ->
            remaining
        | Unaccounted ->
            Z.of_string "99999999"
      in
      logged_return (Item (Script_int.(abs (of_zint steps)), rest), ctxt)
  | (Source, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.source)
      >>=? fun ctxt ->
      logged_return (Item ((step_constants.payer, "default"), rest), ctxt)
  | (Sender, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.source)
      >>=? fun ctxt ->
      logged_return (Item ((step_constants.source, "default"), rest), ctxt)
  | (Self (t, entrypoint), rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.self)
      >>=? fun ctxt ->
      logged_return (Item ((t, (step_constants.self, entrypoint)), rest), ctxt)
  | (Amount, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.amount)
      >>=? fun ctxt -> logged_return (Item (step_constants.amount, rest), ctxt)
  | (Dig (n, n'), stack) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.stack_n_op n))
      >>=? fun ctxt ->
      interp_stack_prefix_preserving_operation
        (fun (Item (v, rest)) -> return (rest, v))
        n'
        stack
      >>=? fun (aft, x) -> logged_return (Item (x, aft), ctxt)
  | (Dug (n, n'), Item (v, rest)) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.stack_n_op n))
      >>=? fun ctxt ->
      interp_stack_prefix_preserving_operation
        (fun stk -> return (Item (v, stk), ()))
        n'
        rest
      >>=? fun (aft, ()) -> logged_return (aft, ctxt)
  | (Dipn (n, n', b), stack) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.stack_n_op n))
      >>=? fun ctxt ->
      interp_stack_prefix_preserving_operation
        (fun stk ->
          step ?log ctxt step_constants b stk
          >>=? fun (res, ctxt') -> return (res, ctxt'))
        n'
        stack
      >>=? fun (aft, ctxt') -> logged_return (aft, ctxt')
  | (Dropn (n, n'), stack) ->
      Lwt.return (Gas.consume ctxt (Interp_costs.stack_n_op n))
      >>=? fun ctxt ->
      interp_stack_prefix_preserving_operation
        (fun stk -> return (stk, stk))
        n'
        stack
      >>=? fun (_, rest) -> logged_return (rest, ctxt)
  | (ChainId, rest) ->
      Lwt.return (Gas.consume ctxt Interp_costs.chain_id)
      >>=? fun ctxt ->
      logged_return (Item (step_constants.chain_id, rest), ctxt)

and interp :
    type p r.
    ?log:execution_trace ref ->
    context ->
    step_constants ->
    (p, r) lambda ->
    p ->
    (r * context) tzresult Lwt.t =
 fun ?log ctxt step_constants (Lam (code, _)) arg ->
  let stack = Item (arg, Empty) in
  ( match log with
  | None ->
      return_unit
  | Some log ->
      trace Cannot_serialize_log (unparse_stack ctxt (stack, code.bef))
      >>=? fun stack ->
      log := (code.loc, Gas.level ctxt, stack) :: !log ;
      return_unit )
  >>=? fun () ->
  step ?log ctxt step_constants code stack
  >>=? fun (Item (ret, Empty), ctxt) -> return (ret, ctxt)

(* ---- contract handling ---------------------------------------------------*)
and execute ?log ctxt mode step_constants ~entrypoint unparsed_script arg :
    ( Script.expr
    * packed_internal_operation list
    * context
    * Contract.big_map_diff option )
    tzresult
    Lwt.t =
  parse_script ctxt unparsed_script ~legacy:true
  >>=? fun (Ex_script {code; arg_type; storage; storage_type; root_name}, ctxt) ->
  trace
    (Bad_contract_parameter step_constants.self)
    (Lwt.return (find_entrypoint arg_type ~root_name entrypoint))
  >>=? fun (box, _) ->
  trace
    (Bad_contract_parameter step_constants.self)
    (parse_data ctxt ~legacy:false arg_type (box arg))
  >>=? fun (arg, ctxt) ->
  Script.force_decode ctxt unparsed_script.code
  >>=? fun (script_code, ctxt) ->
  Script_ir_translator.collect_big_maps ctxt arg_type arg
  >>=? fun (to_duplicate, ctxt) ->
  Script_ir_translator.collect_big_maps ctxt storage_type storage
  >>=? fun (to_update, ctxt) ->
  trace
    (Runtime_contract_error (step_constants.self, script_code))
    (interp ?log ctxt step_constants code (arg, storage))
  >>=? fun ((ops, storage), ctxt) ->
  Script_ir_translator.extract_big_map_diff
    ctxt
    mode
    ~temporary:false
    ~to_duplicate
    ~to_update
    storage_type
    storage
  >>=? fun (storage, big_map_diff, ctxt) ->
  trace Cannot_serialize_storage (unparse_data ctxt mode storage_type storage)
  >>=? fun (storage, ctxt) ->
  let (ops, op_diffs) = List.split ops in
  let big_map_diff =
    match
      List.flatten
        (List.map (Option.unopt ~default:[]) (op_diffs @ [big_map_diff]))
    with
    | [] ->
        None
    | diff ->
        Some diff
  in
  return (Micheline.strip_locations storage, ops, ctxt, big_map_diff)

type execution_result = {
  ctxt : context;
  storage : Script.expr;
  big_map_diff : Contract.big_map_diff option;
  operations : packed_internal_operation list;
}

let trace ctxt mode step_constants ~script ~entrypoint ~parameter =
  let log = ref [] in
  execute
    ~log
    ctxt
    mode
    step_constants
    ~entrypoint
    script
    (Micheline.root parameter)
  >>=? fun (storage, operations, ctxt, big_map_diff) ->
  let trace = List.rev !log in
  return ({ctxt; storage; big_map_diff; operations}, trace)

let execute ctxt mode step_constants ~script ~entrypoint ~parameter =
  execute
    ctxt
    mode
    step_constants
    ~entrypoint
    script
    (Micheline.root parameter)
  >>=? fun (storage, operations, ctxt, big_map_diff) ->
  return {ctxt; storage; big_map_diff; operations}
