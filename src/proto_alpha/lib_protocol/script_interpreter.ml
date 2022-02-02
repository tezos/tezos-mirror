(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  balance : Tez.t;
  chain_id : Chain_id.t;
  now : Script_timestamp.t;
  level : Script_int.n Script_int.num;
}

(* ---- Run-time errors -----------------------------------------------------*)

type error += Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error of Contract.t

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

(*

    Evaluation of continuations
    ===========================

    As explained in [Script_typed_ir], there are several kinds of
    continuations, each having a specific evaluation rules. The
    following group of functions starts with a list of evaluation
    rules for continuations that generate fresh continuations. This
    group ends with the definition of [next], which dispatches
    evaluation rules depending on the continuation at stake.

 *)
let rec kmap_exit :
    type a b c d e f g h m n o. (a, b, c, d, e, f, g, h, m, n, o) kmap_exit_type
    =
 fun mk g gas (body, xs, ys, yk) ks accu stack ->
  let ys = Script_map.update yk (Some accu) ys in
  let ks = mk (KMap_enter_body (body, xs, ys, ks)) in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and kmap_enter : type a b c d i j k. (a, b, c, d, i, j, k) kmap_enter_type =
 fun mk g gas (body, xs, ys) ks accu stack ->
  match xs with
  | [] -> (next [@ocaml.tailcall]) g gas ks ys (accu, stack)
  | (xk, xv) :: xs ->
      let ks = mk (KMap_exit_body (body, xs, ys, xk, ks)) in
      let res = (xk, xv) in
      let stack = (accu, stack) in
      (step [@ocaml.tailcall]) g gas body ks res stack
 [@@inline]

and klist_exit : type a b c d i j. (a, b, c, d, i, j) klist_exit_type =
 fun mk g gas (body, xs, ys, len) ks accu stack ->
  let ks = mk (KList_enter_body (body, xs, accu :: ys, len, ks)) in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and klist_enter : type a b c d e j. (a, b, c, d, e, j) klist_enter_type =
 fun mk g gas (body, xs, ys, len) ks' accu stack ->
  match xs with
  | [] ->
      let ys = {elements = List.rev ys; length = len} in
      (next [@ocaml.tailcall]) g gas ks' ys (accu, stack)
  | x :: xs ->
      let ks = mk (KList_exit_body (body, xs, ys, len, ks')) in
      (step [@ocaml.tailcall]) g gas body ks x (accu, stack)
 [@@inline]

and kloop_in_left : type a b c d e f g. (a, b, c, d, e, f, g) kloop_in_left_type
    =
 fun g gas ks0 ki ks' accu stack ->
  match accu with
  | L v -> (step [@ocaml.tailcall]) g gas ki ks0 v stack
  | R v -> (next [@ocaml.tailcall]) g gas ks' v stack
 [@@inline]

and kloop_in : type a b c r f s. (a, b, c, r, f, s) kloop_in_type =
 fun g gas ks0 ki ks' accu stack ->
  let (accu', stack') = stack in
  if accu then (step [@ocaml.tailcall]) g gas ki ks0 accu' stack'
  else (next [@ocaml.tailcall]) g gas ks' accu' stack'
 [@@inline]

and kiter : type a b s r f. (a, b, s, r, f) kiter_type =
 fun mk g gas (body, xs) ks accu stack ->
  match xs with
  | [] -> (next [@ocaml.tailcall]) g gas ks accu stack
  | x :: xs ->
      let ks = mk (KIter (body, xs, ks)) in
      (step [@ocaml.tailcall]) g gas body ks x (accu, stack)
 [@@inline]

and next :
    type a s r f.
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, r, f) continuation ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun ((ctxt, _) as g) gas ks0 accu stack ->
  match consume_control gas ks0 with
  | None -> fail Gas.Operation_quota_exceeded
  | Some gas -> (
      match ks0 with
      | KLog (ks, logger) ->
          (klog [@ocaml.tailcall]) logger g gas ks0 ks accu stack
      | KNil -> Lwt.return (Ok (accu, stack, ctxt, gas))
      | KCons (k, ks) -> (step [@ocaml.tailcall]) g gas k ks accu stack
      | KLoop_in (ki, ks') ->
          (kloop_in [@ocaml.tailcall]) g gas ks0 ki ks' accu stack
      | KReturn (stack', ks) -> (next [@ocaml.tailcall]) g gas ks accu stack'
      | KMap_head (f, ks) -> (next [@ocaml.tailcall]) g gas ks (f accu) stack
      | KLoop_in_left (ki, ks') ->
          (kloop_in_left [@ocaml.tailcall]) g gas ks0 ki ks' accu stack
      | KUndip (x, ks) -> (next [@ocaml.tailcall]) g gas ks x (accu, stack)
      | KIter (body, xs, ks) ->
          let extra = (body, xs) in
          (kiter [@ocaml.tailcall]) id g gas extra ks accu stack
      | KList_enter_body (body, xs, ys, len, ks) ->
          let extra = (body, xs, ys, len) in
          (klist_enter [@ocaml.tailcall]) id g gas extra ks accu stack
      | KList_exit_body (body, xs, ys, len, ks) ->
          let extra = (body, xs, ys, len) in
          (klist_exit [@ocaml.tailcall]) id g gas extra ks accu stack
      | KMap_enter_body (body, xs, ys, ks) ->
          let extra = (body, xs, ys) in
          (kmap_enter [@ocaml.tailcall]) id g gas extra ks accu stack
      | KMap_exit_body (body, xs, ys, yk, ks) ->
          let extra = (body, xs, ys, yk) in
          (kmap_exit [@ocaml.tailcall]) id g gas extra ks accu stack
      | KView_exit (orig_step_constants, ks) ->
          let g = (fst g, orig_step_constants) in
          (next [@ocaml.tailcall]) g gas ks accu stack)

(*

   Evaluation of instructions
   ==========================

   The following functions define evaluation rules for instructions that
   generate fresh continuations. As such, they expect a constructor
   [log_if_needed] which inserts a [KLog] if the evaluation is logged.

   The [step] function is taking care of the evaluation of the other
   instructions.

*)
and ilist_map : type a b c d e f g h. (a, b, c, d, e, f, g, h) ilist_map_type =
 fun log_if_needed g gas (body, k) ks accu stack ->
  let xs = accu.elements in
  let ys = [] in
  let len = accu.length in
  let ks =
    log_if_needed (KList_enter_body (body, xs, ys, len, KCons (k, ks)))
  in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and ilist_iter : type a b c d e f g. (a, b, c, d, e, f, g) ilist_iter_type =
 fun log_if_needed g gas (body, k) ks accu stack ->
  let xs = accu.elements in
  let ks = log_if_needed (KIter (body, xs, KCons (k, ks))) in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and iset_iter : type a b c d e f g. (a, b, c, d, e, f, g) iset_iter_type =
 fun log_if_needed g gas (body, k) ks accu stack ->
  let set = accu in
  let l = List.rev (Script_set.fold (fun e acc -> e :: acc) set []) in
  let ks = log_if_needed (KIter (body, l, KCons (k, ks))) in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and imap_map : type a b c d e f g h i. (a, b, c, d, e, f, g, h, i) imap_map_type
    =
 fun log_if_needed g gas (body, k) ks accu stack ->
  let map = accu in
  let xs = List.rev (Script_map.fold (fun k v a -> (k, v) :: a) map []) in
  let ys = Script_map.(empty @@ key_ty map) in
  let ks = log_if_needed (KMap_enter_body (body, xs, ys, KCons (k, ks))) in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and imap_iter : type a b c d e f g h. (a, b, c, d, e, f, g, h) imap_iter_type =
 fun log_if_needed g gas (body, k) ks accu stack ->
  let map = accu in
  let l = List.rev (Script_map.fold (fun k v a -> (k, v) :: a) map []) in
  let ks = log_if_needed (KIter (body, l, KCons (k, ks))) in
  let (accu, stack) = stack in
  (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

and imul_teznat : type a b c d e f. (a, b, c, d, e, f) imul_teznat_type =
 fun logger g gas (kinfo, k) ks accu stack ->
  let x = accu in
  let (y, stack) = stack in
  match Script_int.to_int64 y with
  | None -> get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
  | Some y ->
      Tez.(x *? y) >>?= fun res -> (step [@ocaml.tailcall]) g gas k ks res stack

and imul_nattez : type a b c d e f. (a, b, c, d, e, f) imul_nattez_type =
 fun logger g gas (kinfo, k) ks accu stack ->
  let y = accu in
  let (x, stack) = stack in
  match Script_int.to_int64 y with
  | None -> get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
  | Some y ->
      Tez.(x *? y) >>?= fun res -> (step [@ocaml.tailcall]) g gas k ks res stack

and ilsl_nat : type a b c d e f. (a, b, c, d, e, f) ilsl_nat_type =
 fun logger g gas (kinfo, k) ks accu stack ->
  let x = accu and (y, stack) = stack in
  match Script_int.shift_left_n x y with
  | None -> get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
  | Some x -> (step [@ocaml.tailcall]) g gas k ks x stack

and ilsr_nat : type a b c d e f. (a, b, c, d, e, f) ilsr_nat_type =
 fun logger g gas (kinfo, k) ks accu stack ->
  let x = accu and (y, stack) = stack in
  match Script_int.shift_right_n x y with
  | None -> get_log logger >>=? fun log -> fail (Overflow (kinfo.iloc, log))
  | Some r -> (step [@ocaml.tailcall]) g gas k ks r stack

and ifailwith : type a b. (a, b) ifailwith_type =
 fun logger (ctxt, _) gas kloc tv accu ->
  let v = accu in
  let ctxt = update_context gas ctxt in
  trace Cannot_serialize_failure (unparse_data ctxt Optimized tv v)
  >>=? fun (v, _ctxt) ->
  let v = Micheline.strip_locations v in
  get_log logger >>=? fun log -> fail (Reject (kloc, v, log))

and iexec : type a b c d e f g. (a, b, c, d, e, f, g) iexec_type =
 fun logger g gas k ks accu stack ->
  let arg = accu and (code, stack) = stack in
  let (Lam (code, _)) = code in
  let code =
    match logger with
    | None -> code.kinstr
    | Some logger -> log_kinstr logger code.kinstr
  in
  let ks = KReturn (stack, KCons (k, ks)) in
  (step [@ocaml.tailcall]) g gas code ks arg (EmptyCell, EmptyCell)

and step : type a s b t r f. (a, s, b, t, r, f) step_type =
 fun ((ctxt, sc) as g) gas i ks accu stack ->
  match consume_instr gas i accu stack with
  | None -> fail Gas.Operation_quota_exceeded
  | Some gas -> (
      match i with
      | ILog (_, event, logger, k) ->
          (log [@ocaml.tailcall]) (logger, event) g gas k ks accu stack
      | IHalt _ -> (next [@ocaml.tailcall]) g gas ks accu stack
      (* stack ops *)
      | IDrop (_, k) ->
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IDup (_, k) -> (step [@ocaml.tailcall]) g gas k ks accu (accu, stack)
      | ISwap (_, k) ->
          let (top, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks top (accu, stack)
      | IConst (_, v, k) -> (step [@ocaml.tailcall]) g gas k ks v (accu, stack)
      (* options *)
      | ICons_some (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks (Some accu) stack
      | ICons_none (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks None (accu, stack)
      | IIf_none {branch_if_none; branch_if_some; k; _} -> (
          match accu with
          | None ->
              let (accu, stack) = stack in
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
      | IOpt_map {body; k; kinfo = _} -> (
          match accu with
          | None -> (step [@ocaml.tailcall]) g gas k ks None stack
          | Some v ->
              let ks' = KMap_head (Option.some, KCons (k, ks)) in
              (step [@ocaml.tailcall]) g gas body ks' v stack)
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
      | ICons_left (_, k) -> (step [@ocaml.tailcall]) g gas k ks (L accu) stack
      | ICons_right (_, k) -> (step [@ocaml.tailcall]) g gas k ks (R accu) stack
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
          let (tl, stack) = stack in
          let accu = Script_list.cons accu tl in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | INil (_, k) ->
          let stack = (accu, stack) in
          let accu = Script_list.empty in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IIf_cons {branch_if_cons; branch_if_nil; k; _} -> (
          match accu.elements with
          | [] ->
              let (accu, stack) = stack in
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_nil
                (KCons (k, ks))
                accu
                stack
          | hd :: tl ->
              let tl = {elements = tl; length = accu.length - 1} in
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_cons
                (KCons (k, ks))
                hd
                (tl, stack))
      | IList_map (_, body, k) ->
          (ilist_map [@ocaml.tailcall]) id g gas (body, k) ks accu stack
      | IList_size (_, k) ->
          let list = accu in
          let len = Script_int.(abs (of_int list.length)) in
          (step [@ocaml.tailcall]) g gas k ks len stack
      | IList_iter (_, body, k) ->
          (ilist_iter [@ocaml.tailcall]) id g gas (body, k) ks accu stack
      (* sets *)
      | IEmpty_set (_, ty, k) ->
          let res = Script_set.empty ty in
          let stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISet_iter (_, body, k) ->
          (iset_iter [@ocaml.tailcall]) id g gas (body, k) ks accu stack
      | ISet_mem (_, k) ->
          let (set, stack) = stack in
          let res = Script_set.mem accu set in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISet_update (_, k) ->
          let (presence, (set, stack)) = stack in
          let res = Script_set.update accu presence set in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISet_size (_, k) ->
          let res = Script_set.size accu in
          (step [@ocaml.tailcall]) g gas k ks res stack
      (* maps *)
      | IEmpty_map (_, ty, k) ->
          let res = Script_map.empty ty and stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_map (_, body, k) ->
          (imap_map [@ocaml.tailcall]) id g gas (body, k) ks accu stack
      | IMap_iter (_, body, k) ->
          (imap_iter [@ocaml.tailcall]) id g gas (body, k) ks accu stack
      | IMap_mem (_, k) ->
          let (map, stack) = stack in
          let res = Script_map.mem accu map in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_get (_, k) ->
          let (map, stack) = stack in
          let res = Script_map.get accu map in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_update (_, k) ->
          let (v, (map, stack)) = stack in
          let key = accu in
          let res = Script_map.update key v map in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_get_and_update (_, k) ->
          let key = accu in
          let (v, (map, rest)) = stack in
          let map' = Script_map.update key v map in
          let v' = Script_map.get key map in
          (step [@ocaml.tailcall]) g gas k ks v' (map', rest)
      | IMap_size (_, k) ->
          let res = Script_map.size accu in
          (step [@ocaml.tailcall]) g gas k ks res stack
      (* Big map operations *)
      | IEmpty_big_map (_, tk, tv, k) ->
          let ebm = Script_ir_translator.empty_big_map tk tv in
          (step [@ocaml.tailcall]) g gas k ks ebm (accu, stack)
      | IBig_map_mem (_, k) ->
          let (map, stack) = stack in
          let key = accu in
          ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            Script_ir_translator.big_map_mem ctxt key map )
          >>=? fun (res, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
      | IBig_map_get (_, k) ->
          let (map, stack) = stack in
          let key = accu in
          ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            Script_ir_translator.big_map_get ctxt key map )
          >>=? fun (res, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
      | IBig_map_update (_, k) ->
          let key = accu in
          let (maybe_value, (map, stack)) = stack in
          ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            Script_ir_translator.big_map_update ctxt key maybe_value map )
          >>=? fun (big_map, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks big_map stack
      | IBig_map_get_and_update (_, k) ->
          let key = accu in
          let (v, (map, stack)) = stack in
          ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
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
          consume gas (Interp_costs.concat_string total_length) >>?= fun gas ->
          let s = Script_string.concat ss.elements in
          (step [@ocaml.tailcall]) g gas k ks s stack
      | ISlice_string (_, k) ->
          let offset = accu and (length, (s, stack)) = stack in
          let s_length = Z.of_int (Script_string.length s) in
          let offset = Script_int.to_zint offset in
          let length = Script_int.to_zint length in
          if Compare.Z.(offset < s_length && Z.add offset length <= s_length)
          then
            let s = Script_string.sub s (Z.to_int offset) (Z.to_int length) in
            (step [@ocaml.tailcall]) g gas k ks (Some s) stack
          else (step [@ocaml.tailcall]) g gas k ks None stack
      | IString_size (_, k) ->
          let s = accu in
          let result = Script_int.(abs (of_int (Script_string.length s))) in
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
              S.zero
              ss.elements
          in
          consume gas (Interp_costs.concat_string total_length) >>?= fun gas ->
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
          Tez.(x +? y) >>?= fun res ->
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISub_tez (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let res = Tez.sub_opt x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISub_tez_legacy (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          Tez.(x -? y) >>?= fun res ->
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_teznat (kinfo, k) ->
          imul_teznat None g gas (kinfo, k) ks accu stack
      | IMul_nattez (kinfo, k) ->
          imul_nattez None g gas (kinfo, k) ks accu stack
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
      | INeg (_, k) ->
          let x = accu in
          let res = Script_int.neg x in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAdd_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAdd_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add_n x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISub_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.sub x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul_n x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IEdiv_teznat (_, k) ->
          let x = accu and (y, stack) = stack in
          let x = Script_int.of_int64 (Tez.to_mutez x) in
          let result =
            match Script_int.ediv x y with
            | None -> None
            | Some (q, r) -> (
                match (Script_int.to_int64 q, Script_int.to_int64 r) with
                | (Some q, Some r) -> (
                    match (Tez.of_mutez q, Tez.of_mutez r) with
                    | (Some q, Some r) -> Some (q, r)
                    (* Cannot overflow *)
                    | _ -> assert false)
                (* Cannot overflow *)
                | _ -> assert false)
          in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IEdiv_tez (_, k) ->
          let x = accu and (y, stack) = stack in
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
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IEdiv_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.ediv_n x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ILsl_nat (kinfo, k) -> ilsl_nat None g gas (kinfo, k) ks accu stack
      | ILsr_nat (kinfo, k) -> ilsr_nat None g gas (kinfo, k) ks accu stack
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
      (* control *)
      | IIf {branch_if_true; branch_if_false; k; _} ->
          let (res, stack) = stack in
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
      | IDip (_, b, k) ->
          let ign = accu in
          let ks = KUndip (ign, KCons (k, ks)) in
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas b ks accu stack
      | IExec (_, k) -> iexec None g gas k ks accu stack
      | IApply (_, capture_ty, k) ->
          let capture = accu in
          let (lam, stack) = stack in
          apply ctxt gas capture_ty capture lam >>=? fun (lam', ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks lam' stack
      | ILambda (_, lam, k) ->
          (step [@ocaml.tailcall]) g gas k ks lam (accu, stack)
      | IFailwith (_, kloc, tv) -> ifailwith None g gas kloc tv accu
      (* comparison *)
      | ICompare (_, ty, k) ->
          let a = accu in
          let (b, stack) = stack in
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
          ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            Script_ir_translator.pack_data ctxt ty value )
          >>=? fun (bytes, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks bytes stack
      | IUnpack (_, ty, k) ->
          let bytes = accu in
          ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            unpack ctxt ~ty ~bytes )
          >>=? fun (opt, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks opt stack
      | IAddress (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks accu.address stack
      | IContract (kinfo, t, entrypoint, k) -> (
          let addr = accu in
          let entrypoint_opt =
            if Entrypoint.is_default addr.entrypoint then Some entrypoint
            else if Entrypoint.is_default entrypoint then Some addr.entrypoint
            else (* both entrypoints are non-default *) None
          in
          match entrypoint_opt with
          | Some entrypoint ->
              let ctxt = update_context gas ctxt in
              Script_ir_translator.parse_contract_for_script
                ctxt
                kinfo.iloc
                t
                addr.destination
                ~entrypoint
              >>=? fun (ctxt, maybe_contract) ->
              let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
              let accu = maybe_contract in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
          | None -> (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack)
      | ITransfer_tokens (_, k) ->
          let p = accu in
          let (amount, (tcontract, stack)) = stack in
          let tp = tcontract.arg_ty in
          let destination = tcontract.address.destination in
          let entrypoint = tcontract.address.entrypoint in
          transfer (ctxt, sc) gas amount tp p destination entrypoint
          >>=? fun (accu, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
      | IImplicit_account (_, k) ->
          let key = accu in
          let arg_ty = unit_t in
          let address =
            {
              destination = Contract (Contract.implicit_contract key);
              entrypoint = Entrypoint.default;
            }
          in
          let res = {arg_ty; address} in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IView (_, View_signature {name; input_ty; output_ty}, k) -> (
          let input = accu in
          let (addr, stack) = stack in
          let c = addr.destination in
          let ctxt = update_context gas ctxt in
          match c with
          | Contract c -> (
              Contract.get_script ctxt c >>=? fun (ctxt, script_opt) ->
              let return_none ctxt =
                let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
                (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack
              in
              match script_opt with
              | None -> (return_none [@ocaml.tailcall]) ctxt
              | Some script -> (
                  parse_script
                    ~legacy:true
                    ~allow_forged_in_storage:true
                    ctxt
                    script
                  >>=? fun (Ex_script {storage; storage_type; views; _}, ctxt)
                    ->
                  Gas.consume ctxt (Interp_costs.view_get name views)
                  >>?= fun ctxt ->
                  match SMap.find name views with
                  | None -> (return_none [@ocaml.tailcall]) ctxt
                  | Some view -> (
                      let view_result =
                        Script_ir_translator.parse_view_returning
                          ctxt
                          ~legacy:true
                          storage_type
                          view
                      in
                      trace_eval
                        (fun () ->
                          Script_tc_errors.Ill_typed_contract
                            (Micheline.strip_locations view.view_code, []))
                        view_result
                      >>=? fun (Ex_view f, ctxt) ->
                      match f with
                      | Lam
                          ( {
                              kloc;
                              kaft = Item_t (aft_ty, Bot_t);
                              kbef = Item_t (bef_ty, Bot_t);
                              kinstr;
                            },
                            _script_view ) -> (
                          pair_t kloc input_ty storage_type >>?= fun pair_ty ->
                          let open Gas_monad in
                          let io_ty =
                            Script_ir_translator.merge_types
                              ~error_details:Fast
                              ~legacy:true
                              kloc
                              aft_ty
                              output_ty
                            >>$ fun (out_eq, _ty) ->
                            merge_types
                              ~error_details:Fast
                              ~legacy:true
                              kloc
                              bef_ty
                              pair_ty
                            >|$ fun (in_eq, _ty) -> (out_eq, in_eq)
                          in
                          Gas_monad.run ctxt io_ty >>?= fun (eq, ctxt) ->
                          match eq with
                          | Error Inconsistent_types_fast ->
                              (return_none [@ocaml.tailcall]) ctxt
                          | Ok (Eq, Eq) -> (
                              let kkinfo = kinfo_of_kinstr k in
                              match kkinfo.kstack_ty with
                              | Item_t (_, s) ->
                                  let kstack_ty = Item_t (output_ty, s) in
                                  let kkinfo = {kkinfo with kstack_ty} in
                                  let ks = KCons (ICons_some (kkinfo, k), ks) in
                                  Contract.get_balance_carbonated ctxt c
                                  >>=? fun (ctxt, balance) ->
                                  let (gas, ctxt) =
                                    local_gas_counter_and_outdated_context ctxt
                                  in
                                  (step [@ocaml.tailcall])
                                    ( ctxt,
                                      {
                                        source = sc.self;
                                        self = c;
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
                                    (KView_exit (sc, KReturn (stack, ks)))
                                    (input, storage)
                                    (EmptyCell, EmptyCell)))))))
      | ICreate_contract
          {
            storage_type;
            arg_type;
            lambda = Lam (_, code);
            views;
            root_name;
            k;
            _;
          } ->
          (* Removed the instruction's arguments manager, spendable and delegatable *)
          let delegate = accu in
          let (credit, (init, stack)) = stack in
          create_contract
            g
            gas
            storage_type
            arg_type
            code
            views
            root_name
            delegate
            credit
            init
          >>=? fun (res, contract, ctxt, gas) ->
          let stack =
            ( {destination = Contract contract; entrypoint = Entrypoint.default},
              stack )
          in
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
      | ISet_delegate (_, k) ->
          let delegate = accu in
          let operation = Delegation delegate in
          let ctxt = update_context gas ctxt in
          fresh_internal_nonce ctxt >>?= fun (ctxt, nonce) ->
          let piop = Internal_operation {source = sc.self; operation; nonce} in
          let res = {piop; lazy_storage_diff = None} in
          let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks res stack
      | IBalance (_, k) ->
          let ctxt = update_context gas ctxt in
          let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
          let g = (ctxt, sc) in
          (step [@ocaml.tailcall]) g gas k ks sc.balance (accu, stack)
      | ILevel (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks sc.level (accu, stack)
      | INow (_, k) -> (step [@ocaml.tailcall]) g gas k ks sc.now (accu, stack)
      | ICheck_signature (_, k) ->
          let key = accu and (signature, (message, stack)) = stack in
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
          let destination : Destination.t = Contract sc.payer in
          let res = {destination; entrypoint = Entrypoint.default} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | ISender (_, k) ->
          let destination : Destination.t = Contract sc.source in
          let res = {destination; entrypoint = Entrypoint.default} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | ISelf (_, ty, entrypoint, k) ->
          let destination : Destination.t = Contract sc.self in
          let res = {arg_ty = ty; address = {destination; entrypoint}} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | ISelf_address (_, k) ->
          let destination : Destination.t = Contract sc.self in
          let res = {destination; entrypoint = Entrypoint.default} in
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
              | KRest -> (accu, stack)
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
          let sc_chain_id = Script_chain_id.make sc.chain_id in
          let chain_id = Script_chain_id.to_b58check sc_chain_id in
          let anti_replay = address ^ chain_id in
          let ctxt = update_context gas ctxt in
          Sapling.verify_update ctxt state transaction anti_replay
          >>=? fun (ctxt, balance_state_opt) ->
          let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
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
          Vote.get_voting_power ctxt key_hash >>=? fun (ctxt, rolls) ->
          let power = Script_int.(abs (of_int32 rolls)) in
          let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks power stack
      | ITotal_voting_power (_, k) ->
          let ctxt = update_context gas ctxt in
          Vote.get_total_voting_power ctxt >>=? fun (ctxt, rolls) ->
          let power = Script_int.(abs (of_int32 rolls)) in
          let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
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
          let accu = Script_bls.G1.add x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IAdd_bls12_381_g2 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G2.add x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IAdd_bls12_381_fr (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.Fr.add x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_g1 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G1.mul x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_g2 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G2.mul x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_fr (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.Fr.mul x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_fr_z (_, k) ->
          let x = accu and (y, stack) = stack in
          let x = Script_bls.Fr.of_z (Script_int.to_zint x) in
          let res = Script_bls.Fr.mul x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_bls12_381_z_fr (_, k) ->
          let y = accu and (x, stack) = stack in
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
          let rec aux :
              type before after.
              (before, after) comb_gadt_witness -> before -> after =
           fun witness stack ->
            match (witness, stack) with
            | (Comb_one, stack) -> stack
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
            | (Uncomb_one, stack) -> stack
            | (Uncomb_succ witness', ((a, b), tl)) -> (a, aux witness' (b, tl))
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
            | (Comb_get_zero, v) -> v
            | (Comb_get_one, (a, _)) -> a
            | (Comb_get_plus_two witness', (_, b)) -> aux witness' b
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
            | (Comb_set_zero, _) -> value
            | (Comb_set_one, (_hd, tl)) -> (value, tl)
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
            | (Dup_n_zero, (a, _)) -> a
            | (Dup_n_succ witness', (_, tl)) -> aux witness' tl
          in
          let stack = (accu, stack) in
          let accu = aux witness stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      (* Tickets *)
      | ITicket (_, k) ->
          let contents = accu and (amount, stack) = stack in
          let ticketer = sc.self in
          let accu = {ticketer; contents; amount} in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IRead_ticket (_, k) ->
          let {ticketer; contents; amount} = accu in
          let stack = (accu, stack) in
          let destination : Destination.t = Contract ticketer in
          let addr = {destination; entrypoint = Entrypoint.default} in
          let accu = (addr, (contents, amount)) in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | ISplit_ticket (_, k) ->
          let ticket = accu and ((amount_a, amount_b), stack) = stack in
          let result =
            if
              Compare.Int.(
                Script_int.(compare (add_n amount_a amount_b) ticket.amount) = 0)
            then
              Some
                ( {ticket with amount = amount_a},
                  {ticket with amount = amount_b} )
            else None
          in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IJoin_tickets (_, contents_ty, k) ->
          let (ticket_a, ticket_b) = accu in
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
                  amount = Script_int.add_n ticket_a.amount ticket_b.amount;
                }
            else None
          in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IOpen_chest (_, k) ->
          let open Timelock in
          let chest_key = accu in
          let (chest, (time_z, stack)) = stack in
          (* If the time is not an integer we then consider the proof as
             incorrect. Indeed the verification asks for an integer for practical reasons.
             Therefore no proof can be correct.*)
          let accu =
            match Alpha_context.Script_int.to_int time_z with
            | None -> R false
            | Some time -> (
                match Script_timelock.open_chest chest chest_key ~time with
                | Correct bytes -> L bytes
                | Bogus_cipher -> R false
                | Bogus_opening -> R true)
          in
          (step [@ocaml.tailcall]) g gas k ks accu stack)

(*

  Zero-cost logging
  =================

*)

(*

   The following functions insert a logging instruction and modify the
   continuation to continue the logging process in the next execution
   steps.

   There is a special treatment of instructions that generate fresh
   continuations: we pass a constructor as argument to their
   evaluation rules so that they can instrument these fresh
   continuations by themselves.

   This on-the-fly instrumentation of the execution allows zero-cost
   logging since logging instructions are only introduced if an
   initial logging continuation is pushed in the initial continuation
   that starts the evaluation.

*)
and log :
    type a s b t r f. logger * logging_event -> (a, s, b, t, r, f) step_type =
 fun (logger, event) ((ctxt, _) as g) gas k ks accu stack ->
  (match (k, event) with
  | (ILog _, LogEntry) -> ()
  | (_, LogEntry) -> log_entry logger ctxt gas k accu stack
  | (_, LogExit prev_kinfo) -> log_exit logger ctxt gas prev_kinfo k accu stack) ;
  let k = log_next_kinstr logger k in
  let with_log k = match k with KLog _ -> k | _ -> KLog (k, logger) in
  match k with
  | IList_map (_, body, k) ->
      (ilist_map [@ocaml.tailcall]) with_log g gas (body, k) ks accu stack
  | IList_iter (_, body, k) ->
      (ilist_iter [@ocaml.tailcall]) with_log g gas (body, k) ks accu stack
  | ISet_iter (_, body, k) ->
      (iset_iter [@ocaml.tailcall]) with_log g gas (body, k) ks accu stack
  | IMap_map (_, body, k) ->
      (imap_map [@ocaml.tailcall]) with_log g gas (body, k) ks accu stack
  | IMap_iter (_, body, k) ->
      (imap_iter [@ocaml.tailcall]) with_log g gas (body, k) ks accu stack
  | ILoop (_, body, k) ->
      let ks = with_log (KLoop_in (body, KCons (k, ks))) in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | ILoop_left (_, bl, br) ->
      let ks = with_log (KLoop_in_left (bl, KCons (br, ks))) in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | IMul_teznat (kinfo, k) ->
      let extra = (kinfo, k) in
      (imul_teznat [@ocaml.tailcall]) (Some logger) g gas extra ks accu stack
  | IMul_nattez (kinfo, k) ->
      let extra = (kinfo, k) in
      (imul_nattez [@ocaml.tailcall]) (Some logger) g gas extra ks accu stack
  | ILsl_nat (kinfo, k) ->
      let extra = (kinfo, k) in
      (ilsl_nat [@ocaml.tailcall]) (Some logger) g gas extra ks accu stack
  | ILsr_nat (kinfo, k) ->
      let extra = (kinfo, k) in
      (ilsr_nat [@ocaml.tailcall]) (Some logger) g gas extra ks accu stack
  | IFailwith (_, kloc, tv) ->
      (ifailwith [@ocaml.tailcall]) (Some logger) g gas kloc tv accu
  | IExec (_, k) ->
      (iexec [@ocaml.tailcall]) (Some logger) g gas k ks accu stack
  | _ -> (step [@ocaml.tailcall]) g gas k (with_log ks) accu stack
 [@@inline]

and klog :
    type a s r f.
    logger ->
    outdated_context * step_constants ->
    local_gas_counter ->
    (a, s, r, f) continuation ->
    (a, s, r, f) continuation ->
    a ->
    s ->
    (r * f * outdated_context * local_gas_counter) tzresult Lwt.t =
 fun logger g gas ks0 ks accu stack ->
  (match ks with KLog _ -> () | _ -> log_control logger ks) ;
  let enable_log ki = log_kinstr logger ki in
  let mk k = match k with KLog _ -> k | _ -> KLog (k, logger) in
  match ks with
  | KCons (ki, ks') ->
      let log = enable_log ki in
      let ks = mk ks' in
      (step [@ocaml.tailcall]) g gas log ks accu stack
  | KNil -> (next [@ocaml.tailcall]) g gas ks accu stack
  | KLoop_in (ki, ks') ->
      let ks' = mk ks' in
      let ki = enable_log ki in
      (kloop_in [@ocaml.tailcall]) g gas ks0 ki ks' accu stack
  | KReturn (stack', ks') ->
      let ks' = mk ks' in
      let ks = KReturn (stack', ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KMap_head (f, ks) -> (next [@ocaml.tailcall]) g gas ks (f accu) stack
  | KLoop_in_left (ki, ks') ->
      let ks' = mk ks' in
      let ki = enable_log ki in
      (kloop_in_left [@ocaml.tailcall]) g gas ks0 ki ks' accu stack
  | KUndip (x, ks') ->
      let ks' = mk ks' in
      let ks = KUndip (x, ks') in
      (next [@ocaml.tailcall]) g gas ks accu stack
  | KIter (body, xs, ks') ->
      let ks' = mk ks' in
      let body = enable_log body in
      (kiter [@ocaml.tailcall]) mk g gas (body, xs) ks' accu stack
  | KList_enter_body (body, xs, ys, len, ks') ->
      let ks' = mk ks' in
      let extra = (body, xs, ys, len) in
      (klist_enter [@ocaml.tailcall]) mk g gas extra ks' accu stack
  | KList_exit_body (body, xs, ys, len, ks') ->
      let ks' = mk ks' in
      let extra = (body, xs, ys, len) in
      (klist_exit [@ocaml.tailcall]) mk g gas extra ks' accu stack
  | KMap_enter_body (body, xs, ys, ks') ->
      let ks' = mk ks' in
      (kmap_enter [@ocaml.tailcall]) mk g gas (body, xs, ys) ks' accu stack
  | KMap_exit_body (body, xs, ys, yk, ks') ->
      let ks' = mk ks' in
      (kmap_exit [@ocaml.tailcall]) mk g gas (body, xs, ys, yk) ks' accu stack
  | KView_exit (orig_step_constants, ks') ->
      let g = (fst g, orig_step_constants) in
      (next [@ocaml.tailcall]) g gas ks' accu stack
  | KLog (_, _) ->
      (* This case should never happen. *)
      (next [@ocaml.tailcall]) g gas ks accu stack
 [@@inline]

(*

   Entrypoints
   ===========

*)

let step_descr ~log_now logger (ctxt, sc) descr accu stack =
  let (gas, outdated_ctxt) = local_gas_counter_and_outdated_context ctxt in
  (match logger with
  | None -> step (outdated_ctxt, sc) gas descr.kinstr KNil accu stack
  | Some logger ->
      (if log_now then
       let kinfo = kinfo_of_kinstr descr.kinstr in
       logger.log_interp descr.kinstr ctxt kinfo.iloc descr.kbef (accu, stack)) ;
      let log =
        ILog (kinfo_of_kinstr descr.kinstr, LogEntry, logger, descr.kinstr)
      in
      step (outdated_ctxt, sc) gas log KNil accu stack)
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let interp logger g (Lam (code, _)) arg =
  step_descr ~log_now:true logger g code arg (EmptyCell, EmptyCell)
  >|=? fun (ret, (EmptyCell, EmptyCell), ctxt) -> (ret, ctxt)

let kstep logger ctxt step_constants kinstr accu stack =
  let kinstr =
    match logger with
    | None -> kinstr
    | Some logger -> ILog (kinfo_of_kinstr kinstr, LogEntry, logger, kinstr)
  in
  let (gas, outdated_ctxt) = local_gas_counter_and_outdated_context ctxt in
  step (outdated_ctxt, step_constants) gas kinstr KNil accu stack
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let internal_step ctxt step_constants gas kinstr accu stack =
  step (ctxt, step_constants) gas kinstr KNil accu stack

let step logger ctxt step_constants descr stack =
  step_descr ~log_now:false logger (ctxt, step_constants) descr stack

(*

   High-level functions
   ====================

*)
let execute logger ctxt mode step_constants ~entrypoint ~internal
    unparsed_script cached_script arg :
    (Script.expr
    * packed_internal_operation list
    * context
    * Lazy_storage.diffs option
    * ex_script
    * int)
    tzresult
    Lwt.t =
  (match cached_script with
  | None ->
      parse_script
        ctxt
        unparsed_script
        ~legacy:true
        ~allow_forged_in_storage:true
  | Some ex_script -> return (ex_script, ctxt))
  >>=? fun ( Ex_script
               {
                 code_size;
                 code;
                 arg_type;
                 storage;
                 storage_type;
                 root_name;
                 views;
               },
             ctxt ) ->
  Gas_monad.run
    ctxt
    (find_entrypoint ~error_details:Informative arg_type ~root_name entrypoint)
  >>?= fun (r, ctxt) ->
  record_trace (Bad_contract_parameter step_constants.self) r
  >>?= fun (box, _) ->
  trace
    (Bad_contract_parameter step_constants.self)
    (parse_data ctxt ~legacy:false ~allow_forged:internal arg_type (box arg))
  >>=? fun (arg, ctxt) ->
  Script_ir_translator.collect_lazy_storage ctxt arg_type arg
  >>?= fun (to_duplicate, ctxt) ->
  Script_ir_translator.collect_lazy_storage ctxt storage_type storage
  >>?= fun (to_update, ctxt) ->
  trace
    (Runtime_contract_error step_constants.self)
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
    >>=? fun (unparsed_storage, ctxt) ->
      Lwt.return
        ( Gas.consume ctxt (Script.strip_locations_cost unparsed_storage)
        >>? fun ctxt -> ok (Micheline.strip_locations unparsed_storage, ctxt) )
    )
  >>=? fun (unparsed_storage, ctxt) ->
  Lwt.return
    (let op_to_couple op = (op.piop, op.lazy_storage_diff) in
     let (ops, op_diffs) =
       ops.elements |> List.map op_to_couple |> List.split
     in
     let lazy_storage_diff =
       match
         List.flatten
           (List.map
              (Option.value ~default:[])
              (op_diffs @ [lazy_storage_diff]))
       with
       | [] -> None
       | diff -> Some diff
     in
     let script =
       Ex_script
         {code_size; code; arg_type; storage; storage_type; root_name; views}
     in
     (* We consume gas after the fact in order to not have to instrument
        [script_size] (for efficiency).
        This is safe, as we already pay gas proportional to storage size
        in [unparse_data]. *)
     let (size, cost) = Script_ir_translator.script_size script in
     Gas.consume ctxt cost >>? fun ctxt ->
     ok (unparsed_storage, ops, ctxt, lazy_storage_diff, script, size))

type execution_result = {
  ctxt : context;
  storage : Script.expr;
  lazy_storage_diff : Lazy_storage.diffs option;
  operations : packed_internal_operation list;
}

let execute ?logger ctxt ~cached_script mode step_constants ~script ~entrypoint
    ~parameter ~internal =
  execute
    logger
    ctxt
    mode
    step_constants
    ~entrypoint
    ~internal
    script
    cached_script
    (Micheline.root parameter)
  >|=? fun (storage, operations, ctxt, lazy_storage_diff, ex_script, approx_size)
    -> ({ctxt; storage; lazy_storage_diff; operations}, (ex_script, approx_size))

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
  let next logger g gas ks accu stack =
    let ks =
      match logger with None -> ks | Some logger -> KLog (ks, logger)
    in
    next g gas ks accu stack

  let step (ctxt, step_constants) gas ks accu stack =
    internal_step ctxt step_constants gas ks accu stack
end
