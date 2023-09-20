(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Api
open Vectors

type owned = Types.Func.t Ctypes.ptr

let call_with_inputs params f inputs =
  let rec go : type f r. (f, r) Function_type.params -> f -> int -> r =
   fun params f index ->
    match params with
    | Function_type.End_param -> f
    | Trigger_param params -> (go [@tailcall]) params (f ()) index
    | Cons_param (typ, params) ->
        let value =
          Value_vector.get Ctypes.(!@inputs) index |> Value.unpack typ
        in
        (go [@tailcall]) params (f value) (succ index)
  in
  go params f 0

let pack_outputs results r outputs =
  Value_vector.init_uninitialized outputs (Function_type.num_results results) ;
  let rec go : type r. r Function_type.results -> r -> int -> unit =
   fun results value index ->
    match results with
    | Function_type.No_result -> ()
    | Function_type.One_result typ ->
        let value = Value.pack typ value in
        Value_vector.set Ctypes.(!@outputs) index value
    | Function_type.Cons_result (typ, results) ->
        let x, xs = value in
        let value = Value.pack typ x in
        Value_vector.set Ctypes.(!@outputs) index value ;
        (go [@tailcall]) results xs (succ index)
  in
  go results r 0

module Func_callback_maker = (val Types.Func_callback.m)

let () =
  (* The Ctypes library tries to detect leaked function pointers. However, this
     does not work correctly for our use cases because we don't keep the
     underlying function pointers on the OCaml side. Instead we pass them to
     Wasmer where they will be kept. This means we mustn't prematurely free
     function pointers just because they are no longer accessible from the
     OCaml side. *)
  Foreign.report_leaked_funptr := Fun.const ()

let create : type f. Store.t -> f Function_type.t -> f -> owned * (unit -> unit)
    =
 fun store typ f ->
  let func_type = Function_type.to_owned typ in
  let (Function_type.Function (params, results)) = typ in
  let run inputs outputs =
    let result =
      Lwt_preemptive.run_in_main (fun () -> call_with_inputs params f inputs)
    in
    pack_outputs results result outputs
  in
  let try_run inputs outputs =
    try
      let () = run inputs outputs in
      Trap.none
    with exn -> Trap.from_string store (Printexc.to_string exn)
  in
  let try_run = Func_callback_maker.of_fun try_run in
  let free () = Func_callback_maker.free try_run in
  let try_run =
    Ctypes.coerce Func_callback_maker.t Types.Func_callback.t try_run
  in
  let owned = Functions.Func.new_ store func_type try_run in
  (* [wasm_func_new] doesn't consume [func_type], therefore we must manually
     garbage collect it. *)
  Functions.Functype.delete func_type ;
  (owned, free)

let call_raw func inputs =
  let open Lwt.Syntax in
  let outputs = Value_vector.uninitialized (Functions.Func.result_arity func) in
  let+ trap =
    Lwt_preemptive.detach
      (fun (inputs, outputs) ->
        Functions.Func.call func (Ctypes.addr inputs) (Ctypes.addr outputs))
      (inputs, outputs)
  in
  Trap.check trap ;
  outputs

let pack_inputs (type x r) (params : (x, r Lwt.t) Function_type.params) func
    (unpack : Value_vector.t -> r) =
  let open Lwt.Syntax in
  let inputs = Value_vector.uninitialized (Function_type.num_params params) in
  let rec go_params : type f. (f, r Lwt.t) Function_type.params -> int -> f =
   fun params index ->
    match params with
    | Function_type.End_param ->
        let+ outputs = call_raw func inputs in
        unpack outputs
    | Trigger_param params -> fun () -> (go_params [@tailcall]) params index
    | Cons_param (typ, params) ->
        fun x ->
          Value_vector.set inputs index (Value.pack typ x) ;
          (go_params [@tailcall]) params (succ index)
  in
  go_params params 0

exception
  Not_enough_outputs of {expected : Unsigned.size_t; got : Unsigned.size_t}

let () =
  Printexc.register_printer (function
      | Not_enough_outputs {got; expected} ->
          Some
            (Printf.sprintf
               "Function did return less values (%s) than expected (%s)"
               (Unsigned.Size_t.to_string got)
               (Unsigned.Size_t.to_string got))
      | _ -> None)

let unpack_outputs results outputs =
  let got = Value_vector.length outputs in
  let expected = Function_type.num_results results in
  if (* Fewer outputs than expected. *)
     Unsigned.Size_t.compare got expected < 0
  then raise (Not_enough_outputs {got; expected}) ;
  let rec go : type r x. r Function_type.results -> int -> (r -> x) -> x =
   fun params index k ->
    match params with
    | Function_type.No_result -> k ()
    | Function_type.One_result typ ->
        Value_vector.get outputs index |> Value.unpack typ |> k
    | Function_type.Cons_result (typ, results) ->
        let x = Value_vector.get outputs index |> Value.unpack typ in
        (go [@tailcall]) results (succ index) (fun xs -> k (x, xs))
  in
  go results 0 Fun.id

let call func typ =
  let func_type = Functions.Func.type_ func in
  Function_type.check_types typ func_type ;
  (* Once the types have been checked, [func_type] can be deleted. *)
  Functions.Functype.delete func_type ;
  let (Function_type.Function (params, results)) = typ in
  pack_inputs params func (unpack_outputs results)
