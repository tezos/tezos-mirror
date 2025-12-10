(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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

  1. Definition of runtime errors.

  2. Interpretation loop: This is the main functionality of this
   module, aka the [step] function.

  3. Interface functions: This part of the module builds high-level
   functions on top of the more basic [step] function.

  Auxiliary definitions can be found in {!Script_interpreter_defs}.

  Implementation details are explained along the file.

*)

open Alpha_context
open Script_typed_ir
open Script_ir_translator
open Local_gas_counter
open Script_interpreter_defs
module S = Saturation_repr

type step_constants = Script_typed_ir.step_constants = {
  sender : Destination.t;
  payer : Signature.public_key_hash;
  self : Contract_hash.t;
  amount : Tez.t;
  balance : Tez.t;
  chain_id : Chain_id.t;
  now : Script_timestamp.t;
  level : Script_int.n Script_int.num;
}

(* ---- Run-time errors -----------------------------------------------------*)

type error += Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error of Contract_hash.t

type error += Bad_contract_parameter of Contract.t (* `Permanent *)

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

type error += Michelson_too_many_recursive_calls

let () =
  let open Data_encoding in
  let trace_encoding : Script_typed_ir.execution_trace encoding =
    list
    @@ obj3
         (req "location" Script.location_encoding)
         (req "gas" Gas.Arith.z_fp_encoding)
         (req "stack" (list Script.expr_encoding))
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
      "While interpreting a Michelson script, an overflow was detected"
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
       (req "contract_handle" Contract.originated_encoding)
       (req "contract_code" (constant "Deprecated")))
    (function
      | Runtime_contract_error contract -> Some (contract, ()) | _ -> None)
    (fun (contract, ()) -> Runtime_contract_error contract) ;
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

  Interpretation loop
  ===================

*)

(*

   As announced earlier, the [step] function produces a computation in
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

   The [step] function is actually based on several mutually
   recursive functions that can be separated in two groups: the first
   group focuses on the evaluation of continuations while the second
   group is about evaluating the instructions.

*)

module Raw = struct
  (*

    Evaluation of continuations
    ===========================

    As explained in [Script_typed_ir], there are several kinds of
    continuations, each having a specific evaluation rules. The
    following group of functions starts with a list of evaluation
    rules for continuations that generate fresh continuations. This
    group ends with the definition of [next], which dispatches
    evaluation rules depending on the continuation at stake.

   Some of these functions generate fresh continuations. As such, they
   expect a constructor [instrument] which inserts a [KLog] if the
   evaluation is logged.

 *)
  let rec kmap_exit : type a b c e f m n o.
      (a, b, c, e, f, m, n, o) kmap_exit_type =
   fun instrument g gas body xs ty ys yk ks accu stack ->
    let ys = Script_map.update yk (Some accu) ys in
    let ks = instrument @@ KMap_enter_body (body, xs, ys, ty, ks) in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and kmap_enter : type a b c d f i j k.
      (a, b, c, d, f, i, j, k) kmap_enter_type =
   fun instrument g gas body xs ty ys ks accu stack ->
    match xs with
    | [] -> (next [@ocaml.tailcall]) g gas ks ys (accu, stack)
    | (xk, xv) :: xs ->
        let ks = instrument @@ KMap_exit_body (body, xs, ys, xk, ty, ks) in
        let res = (xk, xv) in
        let stack = (accu, stack) in
        (step [@ocaml.tailcall]) g gas body ks res stack
  [@@inline]

  and klist_exit : type a b c d e i j. (a, b, c, d, e, i, j) klist_exit_type =
   fun instrument g gas body xs ys ty len ks accu stack ->
    let ys = Script_list.cons accu ys in
    let ks = instrument @@ KList_enter_body (body, xs, ys, ty, len, ks) in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and klist_enter : type a b c d e f j. (a, b, c, d, e, f, j) klist_enter_type =
   fun instrument g gas body xs ys ty len ks' accu stack ->
    match xs with
    | [] ->
        let ys = Script_list.rev ys in
        (next [@ocaml.tailcall]) g gas ks' ys (accu, stack)
    | x :: xs ->
        let ks = instrument @@ KList_exit_body (body, xs, ys, ty, len, ks') in
        (step [@ocaml.tailcall]) g gas body ks x (accu, stack)
  [@@inline]

  and kloop_in_left : type a b c d e f g.
      (a, b, c, d, e, f, g) kloop_in_left_type =
   fun g gas ks0 ki ks' accu stack ->
    match accu with
    | L v -> (step [@ocaml.tailcall]) g gas ki ks0 v stack
    | R v -> (next [@ocaml.tailcall]) g gas ks' v stack
  [@@inline]

  and kloop_in : type a b c r f s. (a, b, c, r, f, s) kloop_in_type =
   fun g gas ks0 ki ks' accu stack ->
    let accu', stack' = stack in
    if accu then (step [@ocaml.tailcall]) g gas ki ks0 accu' stack'
    else (next [@ocaml.tailcall]) g gas ks' accu' stack'
  [@@inline]

  and kiter : type a b s r f c. (a, b, s, r, f, c) kiter_type =
   fun instrument g gas body ty xs ks accu stack ->
    match xs with
    | [] -> (next [@ocaml.tailcall]) g gas ks accu stack
    | x :: xs ->
        let ks = instrument @@ KIter (body, ty, xs, ks) in
        (step [@ocaml.tailcall]) g gas body ks x (accu, stack)
  [@@inline]

  and next : type a s r f.
      outdated_context * step_constants ->
      local_gas_counter ->
      (a, s, r, f) continuation ->
      a ->
      s ->
      (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
   fun ((ctxt, _) as g) gas ks0 accu stack ->
    match consume_control gas ks0 with
    | None -> tzfail Gas.Operation_quota_exceeded
    | Some gas -> (
        match ks0 with
        | KLog (ks, sty, logger) ->
            (logger.klog [@ocaml.tailcall]) logger g gas sty ks0 ks accu stack
        | KNil -> Lwt.return (Ok (accu, stack, ctxt, gas))
        | KCons (k, ks) -> (step [@ocaml.tailcall]) g gas k ks accu stack
        | KLoop_in (ki, ks') ->
            (kloop_in [@ocaml.tailcall]) g gas ks0 ki ks' accu stack
        | KReturn (stack', _, ks) ->
            (next [@ocaml.tailcall]) g gas ks accu stack'
        | KMap_head (f, ks) -> (next [@ocaml.tailcall]) g gas ks (f accu) stack
        | KLoop_in_left (ki, ks') ->
            (kloop_in_left [@ocaml.tailcall]) g gas ks0 ki ks' accu stack
        | KUndip (x, _, ks) -> (next [@ocaml.tailcall]) g gas ks x (accu, stack)
        | KIter (body, ty, xs, ks) ->
            (kiter [@ocaml.tailcall]) id g gas body ty xs ks accu stack
        | KList_enter_body (body, xs, ys, ty, len, ks) ->
            (klist_enter [@ocaml.tailcall])
              id
              g
              gas
              body
              xs
              ys
              ty
              len
              ks
              accu
              stack
        | KList_exit_body (body, xs, ys, ty, len, ks) ->
            (klist_exit [@ocaml.tailcall])
              id
              g
              gas
              body
              xs
              ys
              ty
              len
              ks
              accu
              stack
        | KMap_enter_body (body, xs, ys, ty, ks) ->
            (kmap_enter [@ocaml.tailcall]) id g gas body xs ty ys ks accu stack
        | KMap_exit_body (body, xs, ys, yk, ty, ks) ->
            (kmap_exit [@ocaml.tailcall])
              id
              g
              gas
              body
              xs
              ty
              ys
              yk
              ks
              accu
              stack
        | KView_exit (orig_step_constants, ks) ->
            let g = (fst g, orig_step_constants) in
            (next [@ocaml.tailcall]) g gas ks accu stack)

  (*

   Evaluation of instructions
   ==========================

   The following functions define evaluation rules for instructions that
   generate fresh continuations. As such, they expect a constructor
   [instrument] which inserts a [KLog] if the evaluation is logged.

   The [step] function is taking care of the evaluation of the other
   instructions.

*)
  and ilist_map : type a b c d e f g h i.
      (a, b, c, d, e, f, g, h, i) ilist_map_type =
   fun instrument g gas body k ks ty accu stack ->
    let xs = accu.elements in
    let ys = Script_list.empty in
    let len = accu.length in
    let ks =
      instrument @@ KList_enter_body (body, xs, ys, ty, len, KCons (k, ks))
    in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and ilist_iter : type a b c d e f g cmp.
      (a, b, c, d, e, f, g, cmp) ilist_iter_type =
   fun instrument g gas body ty k ks accu stack ->
    let xs = accu.elements in
    let ks = instrument @@ KIter (body, ty, xs, KCons (k, ks)) in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and iset_iter : type a b c d e f g. (a, b, c, d, e, f, g) iset_iter_type =
   fun instrument g gas body ty k ks accu stack ->
    let set = accu in
    let l = List.rev (Script_set.fold (fun e acc -> e :: acc) set []) in
    let ks = instrument @@ KIter (body, ty, l, KCons (k, ks)) in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and imap_map : type a b c d e f g h i j.
      (a, b, c, d, e, f, g, h, i, j) imap_map_type =
   fun instrument g gas body k ks ty accu stack ->
    let map = accu in
    let xs = List.rev (Script_map.fold (fun k v a -> (k, v) :: a) map []) in
    let ys = Script_map.empty_from map in
    let ks = instrument @@ KMap_enter_body (body, xs, ys, ty, KCons (k, ks)) in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and imap_iter : type a b c d e f g h cmp.
      (a, b, c, d, e, f, g, h, cmp) imap_iter_type =
   fun instrument g gas body ty k ks accu stack ->
    let map = accu in
    let l = List.rev (Script_map.fold (fun k v a -> (k, v) :: a) map []) in
    let ks = instrument @@ KIter (body, ty, l, KCons (k, ks)) in
    let accu, stack = stack in
    (next [@ocaml.tailcall]) g gas ks accu stack
  [@@inline]

  and imul_teznat : type a b c d e f. (a, b, c, d, e, f) imul_teznat_type =
    let open Lwt_result_syntax in
    fun logger g gas loc k ks accu stack ->
      let x = accu in
      let y, stack = stack in
      match Script_int.to_int64 y with
      | None ->
          let* log = get_log logger in
          tzfail (Overflow (loc, log))
      | Some y ->
          let*? res = Tez.(x *? y) in
          (step [@ocaml.tailcall]) g gas k ks res stack

  and imul_nattez : type a b c d e f. (a, b, c, d, e, f) imul_nattez_type =
    let open Lwt_result_syntax in
    fun logger g gas loc k ks accu stack ->
      let y = accu in
      let x, stack = stack in
      match Script_int.to_int64 y with
      | None ->
          let* log = get_log logger in
          tzfail (Overflow (loc, log))
      | Some y ->
          let*? res = Tez.(x *? y) in
          (step [@ocaml.tailcall]) g gas k ks res stack

  and ilsl_nat : type a b c d e f. (a, b, c, d, e, f) ilsl_nat_type =
    let open Lwt_result_syntax in
    fun logger g gas loc k ks accu stack ->
      let x = accu and y, stack = stack in
      match Script_int.shift_left_n x y with
      | None ->
          let* log = get_log logger in
          tzfail (Overflow (loc, log))
      | Some x -> (step [@ocaml.tailcall]) g gas k ks x stack

  and ilsr_nat : type a b c d e f. (a, b, c, d, e, f) ilsr_nat_type =
    let open Lwt_result_syntax in
    fun logger g gas loc k ks accu stack ->
      let x = accu and y, stack = stack in
      match Script_int.shift_right_n x y with
      | None ->
          let* log = get_log logger in
          tzfail (Overflow (loc, log))
      | Some r -> (step [@ocaml.tailcall]) g gas k ks r stack

  and ilsl_bytes : type a b c d e f. (a, b, c, d, e, f) ilsl_bytes_type =
    let open Lwt_result_syntax in
    fun logger g gas loc k ks accu stack ->
      let x = accu and y, stack = stack in
      match Script_bytes.bytes_lsl x y with
      | None ->
          let* log = get_log logger in
          tzfail (Overflow (loc, log))
      | Some res -> (step [@ocaml.tailcall]) g gas k ks res stack

  and ifailwith : ifailwith_type =
    let open Lwt_result_syntax in
    {
      ifailwith =
        (fun logger (ctxt, _) gas kloc tv accu ->
          let v = accu in
          let ctxt = update_context gas ctxt in
          let* v, _ctxt =
            trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
          in
          let* log = get_log logger in
          tzfail (Reject (kloc, v, log)));
    }

  and iexec : type a b c d e f g. (a, b, c, d, e, f, g) iexec_type =
   fun instrument logger g gas cont_sty k ks accu stack ->
    let arg = accu and code, stack = stack in
    let log_code b =
      let body =
        match logger with
        | None -> b.kinstr
        | Some logger -> logger.log_kinstr logger b.kbef b.kinstr
      in
      let ks = instrument @@ KReturn (stack, cont_sty, KCons (k, ks)) in
      (body, ks)
    in
    match code with
    | Lam (body, _) ->
        let body, ks = log_code body in
        (step [@ocaml.tailcall]) g gas body ks arg (EmptyCell, EmptyCell)
    | LamRec (body, _) ->
        let body, ks = log_code body in
        (step [@ocaml.tailcall]) g gas body ks arg (code, (EmptyCell, EmptyCell))

  and iview : type a b c d e f i o. (a, b, c, d, e, f, i, o) iview_type =
    let open Lwt_result_syntax in
    fun instrument
        (ctxt, sc)
        gas
        (View_signature {name; input_ty; output_ty})
        stack_ty
        k
        ks
        accu
        stack
      ->
      let input = accu in
      let addr, stack = stack in
      let ctxt = update_context gas ctxt in
      let return_none ctxt =
        let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
        (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack
      in
      let legacy = Script_ir_translator_config.make ~legacy:true () in
      match addr.destination with
      | Contract (Implicit _) | Sc_rollup _ | Zk_rollup _ ->
          (return_none [@ocaml.tailcall]) ctxt
      | Contract (Originated contract_hash as c) -> (
          let* ctxt, script_opt = Contract.get_script ctxt contract_hash in
          match script_opt with
          | None -> (return_none [@ocaml.tailcall]) ctxt
          | Some script -> (
              let* ( Ex_script
                       (Script {storage; storage_type; implementation; _}),
                     ctxt ) =
                parse_script
                  ~elab_conf:legacy
                  ~allow_forged_tickets_in_storage:true
                  ~allow_forged_lazy_storage_id_in_storage:true
                  ctxt
                  script
              in
              match implementation with
              | Native {kind} -> (
                  let*? views = Script_native.get_views kind in
                  let*? ctxt =
                    Gas.consume ctxt (Interp_costs.native_view_get name views)
                  in
                  match Script_map.get name views with
                  | None -> (return_none [@ocaml.tailcall]) ctxt
                  | Some
                      (Ex_view
                         {
                           name = _;
                           ty =
                             {
                               input_ty = Ty_ex_c input_ty';
                               output_ty = Ty_ex_c output_ty';
                             };
                           implementation;
                         }) -> (
                      let io_ty =
                        let open Gas_monad.Syntax in
                        let* out_eq =
                          ty_eq ~error_details:Fast output_ty' output_ty
                        in
                        let+ in_eq =
                          ty_eq ~error_details:Fast input_ty input_ty'
                        in
                        (out_eq, in_eq)
                      in
                      let*? eq, ctxt = Gas_monad.run ctxt io_ty in
                      match eq with
                      | Error Inconsistent_types_fast ->
                          (return_none [@ocaml.tailcall]) ctxt
                      | Ok (Eq, Eq) ->
                          let* ctxt, balance =
                            Contract.get_balance_carbonated ctxt c
                          in
                          let step_constants =
                            {
                              sender =
                                Destination.Contract
                                  (Contract.Originated sc.self);
                              self = contract_hash;
                              amount = Tez.zero;
                              balance;
                              (* The following remain unchanged, but let's
                               list them anyway, so that we don't forget
                               to update something added later. *)
                              payer = sc.payer;
                              chain_id = sc.chain_id;
                              now = sc.now;
                              level = sc.level;
                            }
                          in
                          let* result, ctxt =
                            implementation (ctxt, step_constants) input storage
                          in
                          let gas, ctxt =
                            local_gas_counter_and_outdated_context ctxt
                          in
                          (step [@ocaml.tailcall])
                            (ctxt, sc)
                            gas
                            (ICons_some (kinstr_location k, k))
                            ks
                            result
                            stack))
              | Lambda {views; _} -> (
                  let*? ctxt =
                    Gas.consume ctxt (Interp_costs.view_get name views)
                  in
                  match Script_map.get name views with
                  | None -> (return_none [@ocaml.tailcall]) ctxt
                  | Some view -> (
                      let view_result =
                        Script_ir_translator.parse_view
                          ctxt
                          ~elab_conf:legacy
                          storage_type
                          view
                      in
                      let* ( Typed_view
                               {
                                 input_ty = input_ty';
                                 output_ty = output_ty';
                                 kinstr;
                                 original_code_expr = _;
                               },
                             ctxt ) =
                        trace_eval
                          (fun () ->
                            Script_tc_errors.Ill_typed_contract
                              (Micheline.strip_locations view.view_code, []))
                          view_result
                      in
                      let io_ty =
                        let open Gas_monad.Syntax in
                        let* out_eq =
                          ty_eq ~error_details:Fast output_ty' output_ty
                        in
                        let+ in_eq =
                          ty_eq ~error_details:Fast input_ty input_ty'
                        in
                        (out_eq, in_eq)
                      in
                      let*? eq, ctxt = Gas_monad.run ctxt io_ty in
                      match eq with
                      | Error Inconsistent_types_fast ->
                          (return_none [@ocaml.tailcall]) ctxt
                      | Ok (Eq, Eq) ->
                          let kcons =
                            KCons (ICons_some (kinstr_location k, k), ks)
                          in
                          let* ctxt, balance =
                            Contract.get_balance_carbonated ctxt c
                          in
                          let gas, ctxt =
                            local_gas_counter_and_outdated_context ctxt
                          in
                          let sty =
                            Option.map (fun t -> Item_t (output_ty, t)) stack_ty
                          in
                          (step [@ocaml.tailcall])
                            ( ctxt,
                              {
                                sender =
                                  Destination.Contract
                                    (Contract.Originated sc.self);
                                self = contract_hash;
                                amount = Tez.zero;
                                balance;
                                (* The following remain unchanged, but let's
                               list them anyway, so that we don't forget
                               to update something added later. *)
                                payer = sc.payer;
                                chain_id = sc.chain_id;
                                now = sc.now;
                                level = sc.level;
                              } )
                            gas
                            kinstr
                            (instrument
                            @@ KView_exit (sc, KReturn (stack, sty, kcons)))
                            (input, storage)
                            (EmptyCell, EmptyCell)))))

  and step : type a s b t r f. (a, s, b, t, r, f) step_type =
    let open Lwt_result_syntax in
    fun ((ctxt, sc) as g) gas i ks accu stack ->
      match consume_instr gas i accu stack with
      | None -> tzfail Gas.Operation_quota_exceeded
      | Some gas -> (
          match i with
          | ILog (_, sty, event, logger, k) ->
              (logger.ilog [@ocaml.tailcall])
                logger
                event
                sty
                g
                gas
                k
                ks
                accu
                stack
          | IHalt _ -> (next [@ocaml.tailcall]) g gas ks accu stack
          (* stack ops *)
          | IDrop (_, k) ->
              let accu, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IDup (_, k) -> (step [@ocaml.tailcall]) g gas k ks accu (accu, stack)
          | ISwap (_, k) ->
              let top, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks top (accu, stack)
          | IPush (_, _ty, v, k) ->
              (step [@ocaml.tailcall]) g gas k ks v (accu, stack)
          | IUnit (_, k) -> (step [@ocaml.tailcall]) g gas k ks () (accu, stack)
          (* options *)
          | ICons_some (_, k) ->
              (step [@ocaml.tailcall]) g gas k ks (Some accu) stack
          | ICons_none (_, _ty, k) ->
              (step [@ocaml.tailcall]) g gas k ks None (accu, stack)
          | IIf_none {branch_if_none; branch_if_some; k; _} -> (
              match accu with
              | None ->
                  let accu, stack = stack in
                  (step [@ocaml.tailcall])
                    g
                    gas
                    branch_if_none
                    (KCons (k, ks))
                    accu
                    stack
              | Some v ->
                  (step [@ocaml.tailcall])
                    g
                    gas
                    branch_if_some
                    (KCons (k, ks))
                    v
                    stack)
          | IOpt_map {body; k; loc = _} -> (
              match accu with
              | None -> (step [@ocaml.tailcall]) g gas k ks None stack
              | Some v ->
                  let ks' = KMap_head (Option.some, KCons (k, ks)) in
                  (step [@ocaml.tailcall]) g gas body ks' v stack)
          (* pairs *)
          | ICons_pair (_, k) ->
              let b, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks (accu, b) stack
          | IUnpair (_, k) ->
              let a, b = accu in
              (step [@ocaml.tailcall]) g gas k ks a (b, stack)
          | ICar (_, k) ->
              let a, _ = accu in
              (step [@ocaml.tailcall]) g gas k ks a stack
          | ICdr (_, k) ->
              let _, b = accu in
              (step [@ocaml.tailcall]) g gas k ks b stack
          (* ors *)
          | ICons_left (_, _tyb, k) ->
              (step [@ocaml.tailcall]) g gas k ks (L accu) stack
          | ICons_right (_, _tya, k) ->
              (step [@ocaml.tailcall]) g gas k ks (R accu) stack
          | IIf_left {branch_if_left; branch_if_right; k; _} -> (
              match accu with
              | L v ->
                  (step [@ocaml.tailcall])
                    g
                    gas
                    branch_if_left
                    (KCons (k, ks))
                    v
                    stack
              | R v ->
                  (step [@ocaml.tailcall])
                    g
                    gas
                    branch_if_right
                    (KCons (k, ks))
                    v
                    stack)
          (* lists *)
          | ICons_list (_, k) ->
              let tl, stack = stack in
              let accu = Script_list.cons accu tl in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | INil (_, _ty, k) ->
              let stack = (accu, stack) in
              let accu = Script_list.empty in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IIf_cons {branch_if_cons; branch_if_nil; k; _} -> (
              match Script_list.uncons accu with
              | None ->
                  let accu, stack = stack in
                  (step [@ocaml.tailcall])
                    g
                    gas
                    branch_if_nil
                    (KCons (k, ks))
                    accu
                    stack
              | Some (hd, tl) ->
                  (step [@ocaml.tailcall])
                    g
                    gas
                    branch_if_cons
                    (KCons (k, ks))
                    hd
                    (tl, stack))
          | IList_map (_, body, ty, k) ->
              (ilist_map [@ocaml.tailcall]) id g gas body k ks ty accu stack
          | IList_size (_, k) ->
              let list = accu in
              let len = Script_int.(abs (of_int list.length)) in
              (step [@ocaml.tailcall]) g gas k ks len stack
          | IList_iter (_, ty, body, k) ->
              (ilist_iter [@ocaml.tailcall]) id g gas body ty k ks accu stack
          (* sets *)
          | IEmpty_set (_, ty, k) ->
              let res = Script_set.empty ty in
              let stack = (accu, stack) in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ISet_iter (_, ty, body, k) ->
              (iset_iter [@ocaml.tailcall]) id g gas body ty k ks accu stack
          | ISet_mem (_, k) ->
              let set, stack = stack in
              let res = Script_set.mem accu set in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ISet_update (_, k) ->
              let presence, (set, stack) = stack in
              let res = Script_set.update accu presence set in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ISet_size (_, k) ->
              let res = Script_set.size accu in
              (step [@ocaml.tailcall]) g gas k ks res stack
          (* maps *)
          | IEmpty_map (_, kty, _vty, k) ->
              let res = Script_map.empty kty and stack = (accu, stack) in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMap_map (_, ty, body, k) ->
              (imap_map [@ocaml.tailcall]) id g gas body k ks ty accu stack
          | IMap_iter (_, kvty, body, k) ->
              (imap_iter [@ocaml.tailcall]) id g gas body kvty k ks accu stack
          | IMap_mem (_, k) ->
              let map, stack = stack in
              let res = Script_map.mem accu map in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMap_get (_, k) ->
              let map, stack = stack in
              let res = Script_map.get accu map in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMap_update (_, k) ->
              let v, (map, stack) = stack in
              let key = accu in
              let res = Script_map.update key v map in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMap_get_and_update (_, k) ->
              let key = accu in
              let v, (map, rest) = stack in
              let map' = Script_map.update key v map in
              let v' = Script_map.get key map in
              (step [@ocaml.tailcall]) g gas k ks v' (map', rest)
          | IMap_size (_, k) ->
              let res = Script_map.size accu in
              (step [@ocaml.tailcall]) g gas k ks res stack
          (* Big map operations *)
          | IEmpty_big_map (_, tk, tv, k) ->
              let ebm = Script_big_map.empty tk tv in
              (step [@ocaml.tailcall]) g gas k ks ebm (accu, stack)
          | IBig_map_mem (_, k) ->
              let map, stack = stack in
              let key = accu in
              let* res, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_big_map.mem ctxt key map
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
          | IBig_map_get (_, k) ->
              let map, stack = stack in
              let key = accu in
              let* res, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_big_map.get ctxt key map
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
          | IBig_map_update (_, k) ->
              let key = accu in
              let maybe_value, (map, stack) = stack in
              let* big_map, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_big_map.update ctxt key maybe_value map
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks big_map stack
          | IBig_map_get_and_update (_, k) ->
              let key = accu in
              let v, (map, stack) = stack in
              let* (v', map'), ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_big_map.get_and_update ctxt key v map
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks v' (map', stack)
          (* timestamp operations *)
          | IAdd_seconds_to_timestamp (_, k) ->
              let n = accu in
              let t, stack = stack in
              let result = Script_timestamp.add_delta t n in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IAdd_timestamp_to_seconds (_, k) ->
              let t = accu in
              let n, stack = stack in
              let result = Script_timestamp.add_delta t n in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | ISub_timestamp_seconds (_, k) ->
              let t = accu in
              let s, stack = stack in
              let result = Script_timestamp.sub_delta t s in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IDiff_timestamps (_, k) ->
              let t1 = accu in
              let t2, stack = stack in
              let result = Script_timestamp.diff t1 t2 in
              (step [@ocaml.tailcall]) g gas k ks result stack
          (* string operations *)
          | IConcat_string_pair (_, k) ->
              let x = accu in
              let y, stack = stack in
              let s = Script_string.concat_pair x y in
              (step [@ocaml.tailcall]) g gas k ks s stack
          | IConcat_string (_, k) ->
              let ss = accu in
              (* The cost for this fold_left has been paid upfront *)
              let total_length =
                List.fold_left
                  (fun acc s -> S.add acc (S.safe_int (Script_string.length s)))
                  S.zero
                  ss.elements
              in
              let*? gas =
                consume gas (Interp_costs.concat_string total_length)
              in
              let s = Script_string.concat ss.elements in
              (step [@ocaml.tailcall]) g gas k ks s stack
          | ISlice_string (_, k) ->
              let offset = accu and length, (s, stack) = stack in
              let s_length = Z.of_int (Script_string.length s) in
              let offset = Script_int.to_zint offset in
              let length = Script_int.to_zint length in
              if
                Compare.Z.(offset < s_length && Z.add offset length <= s_length)
              then
                let s =
                  Script_string.sub s (Z.to_int offset) (Z.to_int length)
                in
                (step [@ocaml.tailcall]) g gas k ks (Some s) stack
              else (step [@ocaml.tailcall]) g gas k ks None stack
          | IString_size (_, k) ->
              let s = accu in
              let result = Script_int.(abs (of_int (Script_string.length s))) in
              (step [@ocaml.tailcall]) g gas k ks result stack
          (* bytes operations *)
          | IConcat_bytes_pair (_, k) ->
              let x = accu in
              let y, stack = stack in
              let s = Bytes.cat x y in
              (step [@ocaml.tailcall]) g gas k ks s stack
          | IConcat_bytes (_, k) ->
              let ss = accu in
              (* The cost for this fold_left has been paid upfront *)
              let total_length =
                List.fold_left
                  (fun acc s -> S.add acc (S.safe_int (Bytes.length s)))
                  S.zero
                  ss.elements
              in
              let*? gas =
                consume gas (Interp_costs.concat_string total_length)
              in
              let s = Bytes.concat Bytes.empty ss.elements in
              (step [@ocaml.tailcall]) g gas k ks s stack
          | ISlice_bytes (_, k) ->
              let offset = accu and length, (s, stack) = stack in
              let s_length = Z.of_int (Bytes.length s) in
              let offset = Script_int.to_zint offset in
              let length = Script_int.to_zint length in
              if
                Compare.Z.(offset < s_length && Z.add offset length <= s_length)
              then
                let s = Bytes.sub s (Z.to_int offset) (Z.to_int length) in
                (step [@ocaml.tailcall]) g gas k ks (Some s) stack
              else (step [@ocaml.tailcall]) g gas k ks None stack
          | IBytes_size (_, k) ->
              let s = accu in
              let result = Script_int.(abs (of_int (Bytes.length s))) in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | ILsl_bytes (loc, k) -> ilsl_bytes None g gas loc k ks accu stack
          | ILsr_bytes (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_bytes.bytes_lsr x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IOr_bytes (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_bytes.bytes_or x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IAnd_bytes (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_bytes.bytes_and x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IXor_bytes (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_bytes.bytes_xor x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | INot_bytes (_, k) ->
              let x = accu in
              let res = Script_bytes.bytes_not x in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IBytes_nat (_, k) ->
              let n = accu in
              let result = Script_bytes.bytes_of_nat_be n in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | INat_bytes (_, k) ->
              let s = accu in
              let result = Script_bytes.nat_of_bytes_be s in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IBytes_int (_, k) ->
              let n = accu in
              let result = Script_bytes.bytes_of_int_be n in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IInt_bytes (_, k) ->
              let s = accu in
              let result = Script_bytes.int_of_bytes_be s in
              (step [@ocaml.tailcall]) g gas k ks result stack
          (* currency operations *)
          | IAdd_tez (_, k) ->
              let x = accu in
              let y, stack = stack in
              let*? res = Tez.(x +? y) in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ISub_tez (_, k) ->
              let x = accu in
              let y, stack = stack in
              let res = Tez.sub_opt x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ISub_tez_legacy (_, k) ->
              let x = accu in
              let y, stack = stack in
              let*? res = Tez.(x -? y) in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMul_teznat (loc, k) -> imul_teznat None g gas loc k ks accu stack
          | IMul_nattez (loc, k) -> imul_nattez None g gas loc k ks accu stack
          (* boolean operations *)
          | IOr (_, k) ->
              let x = accu in
              let y, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks (x || y) stack
          | IAnd (_, k) ->
              let x = accu in
              let y, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks (x && y) stack
          | IXor (_, k) ->
              let x = accu in
              let y, stack = stack in
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
          | INeg (_, k) ->
              let x = accu in
              let res = Script_int.neg x in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IAdd_int (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.add x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IAdd_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.add_n x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ISub_int (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.sub x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMul_int (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.mul x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMul_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.mul_n x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IEdiv_teznat (_, k) ->
              let x = accu and y, stack = stack in
              let x = Script_int.of_int64 (Tez.to_mutez x) in
              let result =
                match Script_int.ediv x y with
                | None -> None
                | Some (q, r) -> (
                    match (Script_int.to_int64 q, Script_int.to_int64 r) with
                    | Some q, Some r -> (
                        match (Tez.of_mutez q, Tez.of_mutez r) with
                        | Some q, Some r -> Some (q, r)
                        (* Cannot overflow *)
                        | _ -> assert false)
                    (* Cannot overflow *)
                    | _ -> assert false)
              in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IEdiv_tez (_, k) ->
              let x = accu and y, stack = stack in
              let x = Script_int.abs (Script_int.of_int64 (Tez.to_mutez x)) in
              let y = Script_int.abs (Script_int.of_int64 (Tez.to_mutez y)) in
              let result =
                match Script_int.ediv_n x y with
                | None -> None
                | Some (q, r) -> (
                    match Script_int.to_int64 r with
                    | None -> assert false (* Cannot overflow *)
                    | Some r -> (
                        match Tez.of_mutez r with
                        | None -> assert false (* Cannot overflow *)
                        | Some r -> Some (q, r)))
              in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IEdiv_int (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.ediv x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IEdiv_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.ediv_n x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | ILsl_nat (loc, k) -> ilsl_nat None g gas loc k ks accu stack
          | ILsr_nat (loc, k) -> ilsr_nat None g gas loc k ks accu stack
          | IOr_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.logor x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IAnd_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.logand x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IAnd_int_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.logand x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IXor_nat (_, k) ->
              let x = accu and y, stack = stack in
              let res = Script_int.logxor x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | INot_int (_, k) ->
              let x = accu in
              let res = Script_int.lognot x in
              (step [@ocaml.tailcall]) g gas k ks res stack
          (* control *)
          | IIf {branch_if_true; branch_if_false; k; _} ->
              let res, stack = stack in
              if accu then
                (step [@ocaml.tailcall])
                  g
                  gas
                  branch_if_true
                  (KCons (k, ks))
                  res
                  stack
              else
                (step [@ocaml.tailcall])
                  g
                  gas
                  branch_if_false
                  (KCons (k, ks))
                  res
                  stack
          | ILoop (_, body, k) ->
              let ks = KLoop_in (body, KCons (k, ks)) in
              (next [@ocaml.tailcall]) g gas ks accu stack
          | ILoop_left (_, bl, br) ->
              let ks = KLoop_in_left (bl, KCons (br, ks)) in
              (next [@ocaml.tailcall]) g gas ks accu stack
          | IDip (_, b, ty, k) ->
              let ign = accu in
              let ks = KUndip (ign, ty, KCons (k, ks)) in
              let accu, stack = stack in
              (step [@ocaml.tailcall]) g gas b ks accu stack
          | IExec (_, sty, k) -> iexec id None g gas sty k ks accu stack
          | IApply (_, capture_ty, k) ->
              let capture = accu in
              let lam, stack = stack in
              let* lam', ctxt, gas = apply ctxt gas capture_ty capture lam in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks lam' stack
          | ILambda (_, lam, k) ->
              (step [@ocaml.tailcall]) g gas k ks lam (accu, stack)
          | IFailwith (kloc, tv) ->
              let {ifailwith} = ifailwith in
              ifailwith None g gas kloc tv accu
          (* comparison *)
          | ICompare (_, ty, k) ->
              let a = accu in
              let b, stack = stack in
              let r =
                Script_int.of_int @@ Script_comparable.compare_comparable ty a b
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
              let* bytes, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_ir_translator.pack_data ctxt ty value
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks bytes stack
          | IUnpack (_, ty, k) ->
              let bytes = accu in
              let* opt, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                unpack ctxt ~ty ~bytes
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks opt stack
          | IAddress (_, k) ->
              let typed_contract = accu in
              let destination = Typed_contract.destination typed_contract in
              let entrypoint = Typed_contract.entrypoint typed_contract in
              let address = {destination; entrypoint} in
              (step [@ocaml.tailcall]) g gas k ks address stack
          | IContract (loc, t, entrypoint, k) -> (
              let addr = accu in
              let entrypoint_opt =
                if Entrypoint.is_default addr.entrypoint then Some entrypoint
                else if Entrypoint.is_default entrypoint then
                  Some addr.entrypoint
                else (* both entrypoints are non-default *) None
              in
              match entrypoint_opt with
              | Some entrypoint ->
                  let ctxt = update_context gas ctxt in
                  let* ctxt, maybe_contract =
                    Script_ir_translator.parse_contract_for_script
                      ctxt
                      loc
                      t
                      addr.destination
                      ~entrypoint
                  in
                  let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
                  let accu = maybe_contract in
                  (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
              | None -> (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack)
          | ITransfer_tokens (loc, k) ->
              let p = accu in
              let amount, (typed_contract, stack) = stack in
              let* accu, ctxt, gas =
                transfer (ctxt, sc) gas amount loc typed_contract p
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
          | IImplicit_account (_, k) ->
              let key = accu in
              let res = Typed_implicit key in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IIs_implicit_account (_, k) ->
              let (address : address) = accu in
              let res =
                match address.destination with
                | Contract (Implicit pkh) -> Some pkh
                | _ -> None
              in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IIndex_address (_, k) ->
              let (address : address) = accu in
              let* res, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_address_registry.index ctxt address.destination
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
          | IGet_address_index (_, k) ->
              let (address : address) = accu in
              let* res, ctxt, gas =
                use_gas_counter_in_context ctxt gas @@ fun ctxt ->
                Script_address_registry.get ctxt address.destination
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
          | IView (_, view_signature, stack_ty, k) ->
              (iview [@ocaml.tailcall])
                id
                g
                gas
                view_signature
                stack_ty
                k
                ks
                accu
                stack
          | ICreate_contract {storage_type; code; k; loc = _} ->
              (* Removed the instruction's arguments manager, spendable and delegatable *)
              let delegate = accu in
              let credit, (init, stack) = stack in
              let* res, contract, ctxt, gas =
                create_contract g gas storage_type code delegate credit init
              in
              let destination = Destination.Contract (Originated contract) in
              let stack =
                ({destination; entrypoint = Entrypoint.default}, stack)
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
          | ISet_delegate (_, k) ->
              let delegate = accu in
              let operation = Delegation delegate in
              let ctxt = update_context gas ctxt in
              let*? ctxt, nonce = fresh_internal_nonce ctxt in
              let piop =
                Internal_operation
                  {
                    sender = Destination.Contract (Contract.Originated sc.self);
                    operation;
                    nonce;
                  }
              in
              let res = {piop; lazy_storage_diff = None} in
              let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
          | IBalance (_, k) ->
              let ctxt = update_context gas ctxt in
              let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
              let g = (ctxt, sc) in
              (step [@ocaml.tailcall]) g gas k ks sc.balance (accu, stack)
          | ILevel (_, k) ->
              (step [@ocaml.tailcall]) g gas k ks sc.level (accu, stack)
          | INow (_, k) ->
              (step [@ocaml.tailcall]) g gas k ks sc.now (accu, stack)
          | IMin_block_time (_, k) ->
              let ctxt = update_context gas ctxt in
              let min_block_time =
                Alpha_context.Constants.minimal_block_delay ctxt
                |> Period.to_seconds |> Script_int.of_int64
                (* Realistically the block delay is never negative. *)
                |> Script_int.abs
              in
              let new_stack = (accu, stack) in
              (step [@ocaml.tailcall]) g gas k ks min_block_time new_stack
          | ICheck_signature (_, k) ->
              let key = accu and signature, (message, stack) = stack in
              let res = Script_signature.check key signature message in
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
              let destination : Destination.t = Contract (Implicit sc.payer) in
              let res = {destination; entrypoint = Entrypoint.default} in
              (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
          | ISender (_, k) ->
              let destination : Destination.t = sc.sender in
              let res = {destination; entrypoint = Entrypoint.default} in
              (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
          | ISelf (_, ty, entrypoint, k) ->
              let res =
                Typed_originated
                  {arg_ty = ty; contract_hash = sc.self; entrypoint}
              in
              (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
          | ISelf_address (_, k) ->
              let destination : Destination.t = Contract (Originated sc.self) in
              let res = {destination; entrypoint = Entrypoint.default} in
              (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
          | IAmount (_, k) ->
              let accu = sc.amount and stack = (accu, stack) in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IDig (_, _n, n', k) ->
              let (accu, stack), x =
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
              let accu, stack = stack in
              let (accu, stack), () =
                interp_stack_prefix_preserving_operation
                  (fun accu stack -> ((v, (accu, stack)), ()))
                  n'
                  accu
                  stack
              in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IDipn (_, _n, n', b, k) ->
              let accu, stack, ks = kundip n' accu stack (KCons (k, ks)) in
              (step [@ocaml.tailcall]) g gas b ks accu stack
          | IDropn (_, _n, n', k) ->
              let stack =
                let rec aux : type a s b t.
                    (b, t, b, t, a, s, a, s) stack_prefix_preservation_witness ->
                    a ->
                    s ->
                    b * t =
                 fun w accu stack ->
                  match w with
                  | KRest -> (accu, stack)
                  | KPrefix (_, _ty, w) ->
                      let accu, stack = stack in
                      aux w accu stack
                in
                aux n' accu stack
              in
              let accu, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | ISapling_empty_state (_, memo_size, k) ->
              let state = Sapling.empty_state ~memo_size () in
              (step [@ocaml.tailcall]) g gas k ks state (accu, stack)
          | ISapling_verify_update (_, k) -> (
              let transaction = accu in
              let state, stack = stack in
              let address = Contract_hash.to_b58check sc.self in
              let sc_chain_id = Script_chain_id.make sc.chain_id in
              let chain_id = Script_chain_id.to_b58check sc_chain_id in
              let anti_replay = address ^ chain_id in
              let ctxt = update_context gas ctxt in
              let* ctxt, balance_state_opt =
                Sapling.verify_update ctxt state transaction anti_replay
              in
              let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
              match balance_state_opt with
              | Some (balance, state) ->
                  let state =
                    Some
                      ( Bytes.of_string transaction.bound_data,
                        (Script_int.of_int64 balance, state) )
                  in
                  (step [@ocaml.tailcall]) (ctxt, sc) gas k ks state stack
              | None -> (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack)
          | ISapling_verify_update_deprecated (_, k) -> (
              let transaction = accu in
              let state, stack = stack in
              let address = Contract_hash.to_b58check sc.self in
              let sc_chain_id = Script_chain_id.make sc.chain_id in
              let chain_id = Script_chain_id.to_b58check sc_chain_id in
              let anti_replay = address ^ chain_id in
              let ctxt = update_context gas ctxt in
              let* ctxt, balance_state_opt =
                Sapling.Legacy.verify_update ctxt state transaction anti_replay
              in
              let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
              match balance_state_opt with
              | Some (balance, state) ->
                  let state = Some (Script_int.of_int64 balance, state) in
                  (step [@ocaml.tailcall]) (ctxt, sc) gas k ks state stack
              | None -> (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack)
          | IChainId (_, k) ->
              let accu = Script_chain_id.make sc.chain_id
              and stack = (accu, stack) in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | INever _ -> ( match accu with _ -> .)
          | IVoting_power (_, k) ->
              let key_hash = accu in
              let ctxt = update_context gas ctxt in
              let* ctxt, power = Vote.get_voting_power ctxt key_hash in
              let power = Script_int.(abs (of_int64 power)) in
              let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks power stack
          | ITotal_voting_power (_, k) ->
              let ctxt = update_context gas ctxt in
              let* ctxt, power = Vote.get_total_voting_power ctxt in
              let power = Script_int.(abs (of_int64 power)) in
              let gas, ctxt = local_gas_counter_and_outdated_context ctxt in
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
              let x = accu and y, stack = stack in
              let accu = Script_bls.G1.add x y in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IAdd_bls12_381_g2 (_, k) ->
              let x = accu and y, stack = stack in
              let accu = Script_bls.G2.add x y in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IAdd_bls12_381_fr (_, k) ->
              let x = accu and y, stack = stack in
              let accu = Script_bls.Fr.add x y in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IMul_bls12_381_g1 (_, k) ->
              let x = accu and y, stack = stack in
              let accu = Script_bls.G1.mul x y in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IMul_bls12_381_g2 (_, k) ->
              let x = accu and y, stack = stack in
              let accu = Script_bls.G2.mul x y in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IMul_bls12_381_fr (_, k) ->
              let x = accu and y, stack = stack in
              let accu = Script_bls.Fr.mul x y in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IMul_bls12_381_fr_z (_, k) ->
              let x = accu and y, stack = stack in
              let x = Script_bls.Fr.of_z (Script_int.to_zint x) in
              let res = Script_bls.Fr.mul x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IMul_bls12_381_z_fr (_, k) ->
              let y = accu and x, stack = stack in
              let x = Script_bls.Fr.of_z (Script_int.to_zint x) in
              let res = Script_bls.Fr.mul x y in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | IInt_bls12_381_fr (_, k) ->
              let x = accu in
              let res = Script_int.of_zint (Script_bls.Fr.to_z x) in
              (step [@ocaml.tailcall]) g gas k ks res stack
          | INeg_bls12_381_g1 (_, k) ->
              let x = accu in
              let accu = Script_bls.G1.negate x in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | INeg_bls12_381_g2 (_, k) ->
              let x = accu in
              let accu = Script_bls.G2.negate x in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | INeg_bls12_381_fr (_, k) ->
              let x = accu in
              let accu = Script_bls.Fr.negate x in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IPairing_check_bls12_381 (_, k) ->
              let pairs = accu in
              let check = Script_bls.pairing_check pairs.elements in
              (step [@ocaml.tailcall]) g gas k ks check stack
          | IComb (_, _, witness, k) ->
              let rec aux : type a b s c d t.
                  (a, b, s, c, d, t) comb_gadt_witness ->
                  a * (b * s) ->
                  c * (d * t) =
               fun witness stack ->
                match (witness, stack) with
                | Comb_one, stack -> stack
                | Comb_succ witness', (a, tl) ->
                    let b, tl' = aux witness' tl in
                    ((a, b), tl')
              in
              let stack = aux witness (accu, stack) in
              let accu, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IUncomb (_, _, witness, k) ->
              let rec aux : type a b s c d t.
                  (a, b, s, c, d, t) uncomb_gadt_witness ->
                  a * (b * s) ->
                  c * (d * t) =
               fun witness stack ->
                match (witness, stack) with
                | Uncomb_one, stack -> stack
                | Uncomb_succ witness', ((a, b), tl) -> (a, aux witness' (b, tl))
              in
              let stack = aux witness (accu, stack) in
              let accu, stack = stack in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IComb_get (_, _, witness, k) ->
              let comb = accu in
              let rec aux : type before after.
                  (before, after) comb_get_gadt_witness -> before -> after =
               fun witness comb ->
                match (witness, comb) with
                | Comb_get_zero, v -> v
                | Comb_get_one, (a, _) -> a
                | Comb_get_plus_two witness', (_, b) -> aux witness' b
              in
              let accu = aux witness comb in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IComb_set (_, _, witness, k) ->
              let value = accu and comb, stack = stack in
              let rec aux : type value before after.
                  (value, before, after) comb_set_gadt_witness ->
                  value ->
                  before ->
                  after =
               fun witness value item ->
                match (witness, item) with
                | Comb_set_zero, _ -> value
                | Comb_set_one, (_hd, tl) -> (value, tl)
                | Comb_set_plus_two witness', (hd, tl) ->
                    (hd, aux witness' value tl)
              in
              let accu = aux witness value comb in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IDup_n (_, _, witness, k) ->
              let rec aux : type a b before after.
                  (a, b, before, after) dup_n_gadt_witness ->
                  a * (b * before) ->
                  after =
               fun witness stack ->
                match (witness, stack) with
                | Dup_n_zero, (a, _) -> a
                | Dup_n_succ witness', (_, tl) -> aux witness' tl
              in
              let stack = (accu, stack) in
              let accu = aux witness stack in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          (* Tickets *)
          | ITicket_deprecated (_, _, k) -> (
              let contents = accu and amount, stack = stack in
              match Ticket_amount.of_n amount with
              | Some amount ->
                  let ticketer = Contract.Originated sc.self in
                  let accu = {ticketer; contents; amount} in
                  (step [@ocaml.tailcall]) g gas k ks accu stack
              | None -> tzfail Script_tc_errors.Forbidden_zero_ticket_quantity)
          | ITicket (_, _, k) -> (
              let contents = accu and amount, stack = stack in
              match Ticket_amount.of_n amount with
              | Some amount ->
                  let ticketer = Contract.Originated sc.self in
                  let accu = Some {ticketer; contents; amount} in
                  (step [@ocaml.tailcall]) g gas k ks accu stack
              | None -> (step [@ocaml.tailcall]) g gas k ks None stack)
          | IRead_ticket (_, _, k) ->
              let {ticketer; contents; amount} = accu in
              let stack = (accu, stack) in
              let destination : Destination.t = Contract ticketer in
              let addr = {destination; entrypoint = Entrypoint.default} in
              let accu =
                (addr, (contents, (amount :> Script_int.n Script_int.num)))
              in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | ISplit_ticket (_, k) ->
              let ticket = accu and (amount_a, amount_b), stack = stack in
              let result =
                Option.bind (Ticket_amount.of_n amount_a) @@ fun amount_a ->
                Option.bind (Ticket_amount.of_n amount_b) @@ fun amount_b ->
                let amount = Ticket_amount.add amount_a amount_b in
                if
                  Compare.Int.(
                    Script_int.(
                      compare (amount :> n num) (ticket.amount :> n num))
                    = 0)
                then
                  Some
                    ( {ticket with amount = amount_a},
                      {ticket with amount = amount_b} )
                else None
              in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IJoin_tickets (_, contents_ty, k) ->
              let ticket_a, ticket_b = accu in
              let result =
                if
                  Compare.Int.(
                    Contract.compare ticket_a.ticketer ticket_b.ticketer = 0
                    && Script_comparable.compare_comparable
                         contents_ty
                         ticket_a.contents
                         ticket_b.contents
                       = 0)
                then
                  Some
                    {
                      ticketer = ticket_a.ticketer;
                      contents = ticket_a.contents;
                      amount = Ticket_amount.add ticket_a.amount ticket_b.amount;
                    }
                else None
              in
              (step [@ocaml.tailcall]) g gas k ks result stack
          | IOpen_chest (_, k) ->
              let open Timelock in
              let chest_key = accu in
              let chest, (time_z, stack) = stack in
              (* If the time is not an integer we then consider the proof as
                 incorrect. Indeed the verification asks for an integer for practical reasons.
                 Therefore no proof can be correct.*)
              let accu =
                match Script_int.to_int time_z with
                | None -> None
                | Some time -> (
                    match Script_timelock.open_chest chest chest_key ~time with
                    | Correct bytes -> Some bytes
                    | Bogus_opening -> None)
              in
              (step [@ocaml.tailcall]) g gas k ks accu stack
          | IEmit {tag; ty = event_type; unparsed_ty; k; loc = _} ->
              let event_data = accu in
              let* accu, ctxt, gas =
                emit_event
                  (ctxt, sc)
                  gas
                  ~event_type
                  ~unparsed_ty
                  ~tag
                  ~event_data
              in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack)
end

open Raw

(*

   Entrypoints
   ===========

*)

let step_descr ~log_now logger (ctxt, sc) descr accu stack =
  let open Lwt_result_syntax in
  let gas, outdated_ctxt = local_gas_counter_and_outdated_context ctxt in
  let* accu, stack, ctxt, gas =
    match logger with
    | None -> step (outdated_ctxt, sc) gas descr.kinstr KNil accu stack
    | Some logger ->
        (if log_now then
           let loc = kinstr_location descr.kinstr in
           logger.log_interp descr.kinstr ctxt loc descr.kbef (accu, stack)) ;
        let log =
          ILog
            ( kinstr_location descr.kinstr,
              descr.kbef,
              LogEntry,
              logger,
              descr.kinstr )
        in
        let knil = KLog (KNil, descr.kaft, logger) in
        step (outdated_ctxt, sc) gas log knil accu stack
  in
  return (accu, stack, update_context gas ctxt)

let interp logger g lam arg =
  let open Lwt_result_syntax in
  match lam with
  | LamRec (code, _) ->
      let+ ret, (EmptyCell, EmptyCell), ctxt =
        step_descr ~log_now:true logger g code arg (lam, (EmptyCell, EmptyCell))
      in
      (ret, ctxt)
  | Lam (code, _) ->
      let+ ret, (EmptyCell, EmptyCell), ctxt =
        step_descr ~log_now:true logger g code arg (EmptyCell, EmptyCell)
      in
      (ret, ctxt)

(*

   High-level functions
   ====================

*)
type execution_arg =
  | Typed_arg :
      Script.location * ('a, _) Script_typed_ir.ty * 'a
      -> execution_arg
  | Untyped_arg : Script.expr -> execution_arg

let lift_execution_arg (type a ac) ctxt ~internal (entrypoint_ty : (a, ac) ty)
    (construct : a -> 'b) arg : ('b * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* entrypoint_arg, ctxt =
    match arg with
    | Untyped_arg arg ->
        let arg = Micheline.root arg in
        parse_data
          ctxt
          ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
          ~allow_forged_tickets:internal
          ~allow_forged_lazy_storage_id:internal
          entrypoint_ty
          arg
    | Typed_arg (loc, parsed_arg_ty, parsed_arg) ->
        let*? res, ctxt =
          Gas_monad.run
            ctxt
            (Script_ir_translator.ty_eq
               ~error_details:(Informative loc)
               entrypoint_ty
               parsed_arg_ty)
        in
        let*? Eq = res in
        let parsed_arg : a = parsed_arg in
        return (parsed_arg, ctxt)
  in
  return (construct entrypoint_arg, ctxt)

type execution_result = {
  script : Script_ir_translator.ex_script;
  code_size : int;
  storage : Script.expr;
  lazy_storage_diff : Lazy_storage.diffs option;
  operations : packed_internal_operation list;
  ticket_diffs : Z.t Ticket_token_map.t;
  ticket_receipt : Ticket_receipt.t;
  address_registry_diff : Address_registry.diff list;
  balance_updates : Receipt.balance_updates;
}

let execute_any_arg logger ctxt mode step_constants ~entrypoint ~internal
    unparsed_script cached_script arg =
  let open Lwt_result_syntax in
  let elab_conf =
    Script_ir_translator_config.make
      ~legacy:true
      ~keep_extra_types_for_interpreter_logging:(Option.is_some logger)
      ()
  in
  let* ( Ex_script
           (Script
              {
                code_size;
                implementation;
                arg_type;
                storage = old_storage;
                storage_type;
                entrypoints;
              }),
         ctxt ) =
    match cached_script with
    | None ->
        parse_script
          ctxt
          unparsed_script
          ~elab_conf
          ~allow_forged_tickets_in_storage:true
          ~allow_forged_lazy_storage_id_in_storage:true
    | Some ex_script -> return (ex_script, ctxt)
  in
  let*? r, ctxt =
    Gas_monad.run
      ctxt
      (find_entrypoint
         ~error_details:(Informative ())
         arg_type
         entrypoints
         entrypoint)
  in
  let self_contract = Contract.Originated step_constants.self in
  let*? (Ex_ty_cstr {ty = entrypoint_ty; construct; original_type_expr = _}) =
    record_trace (Bad_contract_parameter self_contract) r
  in
  let* arg, ctxt =
    trace
      (Bad_contract_parameter self_contract)
      (lift_execution_arg ctxt ~internal entrypoint_ty construct arg)
  in
  let*? to_duplicate, ctxt =
    Script_ir_translator.collect_lazy_storage ctxt arg_type arg
  in
  let*? to_update, ctxt =
    Script_ir_translator.collect_lazy_storage ctxt storage_type old_storage
  in
  let* (ops, new_storage), ctxt =
    match implementation with
    | Lambda {code; _} ->
        trace
          (Runtime_contract_error step_constants.self)
          (interp logger (ctxt, step_constants) code (arg, old_storage))
    | Native {kind} ->
        trace
          (Runtime_contract_error step_constants.self)
          (Script_native.execute (ctxt, step_constants) kind arg old_storage)
  in
  let* storage, lazy_storage_diff, ctxt =
    Script_ir_translator.extract_lazy_storage_diff
      ctxt
      mode
      ~temporary:false
      ~to_duplicate
      ~to_update
      storage_type
      new_storage
  in
  let* unparsed_storage, ctxt =
    trace Cannot_serialize_storage (unparse_data ctxt mode storage_type storage)
  in
  let op_to_couple op = (op.piop, op.lazy_storage_diff) in
  let operations, op_diffs =
    ops.elements |> List.map op_to_couple |> List.split
  in
  let lazy_storage_diff_all =
    match
      List.flatten
        (List.map (Option.value ~default:[]) (op_diffs @ [lazy_storage_diff]))
    with
    | [] -> None
    | diff -> Some diff
  in
  let script =
    Ex_script
      (Script
         {
           code_size;
           implementation;
           arg_type;
           storage;
           storage_type;
           entrypoints;
         })
  in
  let*? arg_type_has_tickets, ctxt =
    Ticket_scanner.type_has_tickets ctxt arg_type
  in
  let*? storage_type_has_tickets, ctxt =
    Ticket_scanner.type_has_tickets ctxt storage_type
  in
  (* Collect the ticket diffs *)
  let* ticket_diffs, ticket_receipt, ctxt =
    Ticket_accounting.ticket_diffs
      ctxt
      ~self_contract
      ~arg_type_has_tickets
      ~storage_type_has_tickets
      ~arg
      ~old_storage
      ~new_storage
      ~lazy_storage_diff:(Option.value ~default:[] lazy_storage_diff)
  in
  (* We consume gas after the fact in order to not have to instrument
     [script_size] (for efficiency).
     This is safe, as we already pay gas proportional to storage size
     in [unparse_data]. *)
  let size, cost = Script_ir_translator.script_size script in
  let*? ctxt = Gas.consume ctxt cost in
  let address_registry_diff = Alpha_context.Address_registry.get_diffs ctxt in
  return
    ( {
        script;
        code_size = size;
        storage = unparsed_storage;
        lazy_storage_diff = lazy_storage_diff_all;
        operations;
        ticket_diffs;
        ticket_receipt;
        address_registry_diff;
        balance_updates = [];
      },
      ctxt )

let execute_with_typed_parameter ?logger ctxt ~cached_script mode step_constants
    ~script ~entrypoint ~parameter_ty ~location ~parameter ~internal =
  execute_any_arg
    logger
    ctxt
    mode
    step_constants
    ~entrypoint
    ~internal
    script
    cached_script
    (Typed_arg (location, parameter_ty, parameter))

let execute ?logger ctxt ~cached_script mode step_constants ~script ~entrypoint
    ~parameter ~internal =
  execute_any_arg
    logger
    ctxt
    mode
    step_constants
    ~entrypoint
    ~internal
    script
    cached_script
    (Untyped_arg parameter)

(*

    Internals
    =========

*)

(*

   We export the internals definitions for tool that requires
   a white-box view on the interpreter, typically snoop, the
   gas model inference engine.

*)
module Internals = struct
  let next logger g gas sty ks accu stack =
    let ks =
      match logger with None -> ks | Some logger -> KLog (ks, sty, logger)
    in
    next g gas ks accu stack

  let kstep logger ctxt step_constants sty kinstr accu stack =
    let open Lwt_result_syntax in
    let kinstr =
      match logger with
      | None -> kinstr
      | Some logger ->
          ILog (kinstr_location kinstr, sty, LogEntry, logger, kinstr)
    in
    let gas, outdated_ctxt = local_gas_counter_and_outdated_context ctxt in
    let* accu, stack, ctxt, gas =
      step (outdated_ctxt, step_constants) gas kinstr KNil accu stack
    in
    return (accu, stack, update_context gas ctxt)

  let step (ctxt, step_constants) gas ks accu stack =
    step (ctxt, step_constants) gas ks KNil accu stack

  let step_descr logger ctxt step_constants descr stack =
    step_descr ~log_now:false logger (ctxt, step_constants) descr stack

  module Raw = Raw
end
