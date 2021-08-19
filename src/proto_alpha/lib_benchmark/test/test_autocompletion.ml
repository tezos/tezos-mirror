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

let sampling_parameters =
  let open Michelson_samplers_parameters in
  let size = {Tezos_benchmark.Base_samplers.min = 4; max = 32} in
  {
    int_size = size;
    string_size = size;
    bytes_size = size;
    stack_size = size;
    type_size = size;
    list_size = size;
    set_size = size;
    map_size = size;
  }

module Full = Michelson_samplers_base.Make_full (struct
  let parameters = sampling_parameters

  let algo = `Default

  let size = 16
end)

module Autocomp = Autocomp.Make (Full)

let () = Format.eprintf "===============================@.%!"

let () = Format.eprintf "Testing dummy program generator@.%!"

let run x =
  x rng_state (Tezos_benchmark_type_inference_alpha.Inference.M.empty ())

let invent_term bef aft =
  let (term, _state) = run (Autocomp.invent_term bef aft) in
  Mikhailsky.seq term

let invent_term bef aft =
  Format.eprintf
    "requested type: %a => %a@."
    Type.Stack.pp
    bef
    Type.Stack.pp
    aft ;
  let term = invent_term bef aft in
  let (bef', aft') = Inference.infer term in
  Format.eprintf
    "generated type: %a => %a@."
    Type.Stack.pp
    bef'
    Type.Stack.pp
    aft' ;
  Format.eprintf "%a@." Mikhailsky.pp term

module T = Tezos_benchmark_type_inference_alpha.Type

let bef = T.(item unit (item unit (item unit empty)))

let aft = T.(item int (item unit (item (pair nat nat) empty)))

let () = invent_term bef aft

let () = Format.eprintf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~@.%!"

let () = invent_term bef aft

let () = Format.eprintf "===============================@.%!"

let () = Format.eprintf "Testing completion@.%!"

let complete term =
  Format.eprintf "term: %a@." Mikhailsky.pp term ;
  let ((bef, aft), state) = Inference.infer_with_state term in
  Format.eprintf "Inferred type: %a => %a@." Type.Stack.pp bef Type.Stack.pp aft ;
  let (term, (bef', aft'), _state) =
    Autocomp.complete_code state term rng_state
  in
  Format.eprintf "completed: %a@." Mikhailsky.pp term ;
  Format.eprintf
    "Inferred type after generation: %a => %a@."
    Type.Stack.pp
    bef'
    Type.Stack.pp
    aft' ;
  let node = Micheline.strip_locations @@ Michelson.of_mikhailsky term state in
  Test_helpers.typecheck_by_tezos bef' node

open Mikhailsky
open Instructions

let push_int = Instructions.push int_ty (Data.big_integer (Z.of_int 100))

let add_ii = Instructions.(add Mikhailsky.int_ty Mikhailsky.int_ty)

let () = complete (lambda [if_left right (dip (seq [push_int; hole]))])

let () = Format.eprintf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~@.%!"

let () = complete (seq [push_int; add_ii; lambda [dip (seq [dup; dip hole])]])
