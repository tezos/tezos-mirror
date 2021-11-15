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

open StaTz

let print_m (term : Mikhailsky.node) =
  Mikhailsky.pp Format.str_formatter term ;
  Format.flush_str_formatter ()

module type Sampler_parameters_sig = sig
  val initial : State_space.t

  val energy : State_space.t -> float

  val rules : Rules.rule_set list

  val infer : Mikhailsky.node -> Inference.state

  val verbosity : [`Silent | `Progress | `Trace]
end

module T = Benchmark_utils.Stubs.Time

(* Generic MCMC michelson sampler (can be used for code and data) *)
module Make (P : Sampler_parameters_sig) = struct
  module MH_params : MH.MH_parameters with type t = State_space.t = struct
    let uniform (l : State_space.t list) : State_space.t Stats.fin_prb =
      match l with
      | [] -> assert false
      | _ ->
          let arr = Array.of_list l in
          let emp = Stats.empirical_of_raw_data arr in
          Stats.fin_prb_of_empirical (module State_space) emp

    let trace state =
      match P.verbosity with
      | `Silent | `Progress -> ()
      | `Trace ->
          Format.eprintf "@." ;
          Format.eprintf "%a" State_space.pp state ;
          Format.eprintf "energy:@." ;
          Format.eprintf "%f:@." (P.energy state)

    let unrecoverable_failure err current result =
      Format.eprintf "Error when typechecking term:@." ;
      Format.eprintf "%a@." Inference.pp_inference_error err ;
      Format.eprintf "Original state: @[%a@]@." State_space.pp current ;
      Format.eprintf "Erroneous term: %a@." Mikhailsky.pp result ;
      Stdlib.failwith "in sampler.ml: unrecoverable failure."

    let rec proposal ({State_space.term; _} as current) =
      trace current ;
      let rewriting_options = Rules.rewriting current P.rules in
      let rewritings =
        List.fold_left
          (fun rewritings (path, replacement) ->
            let result = Kernel.Rewriter.subst ~term ~path ~replacement in
            let typing =
              Lazy.from_fun (fun () ->
                  try P.infer result
                  with Inference.Ill_typed_script err ->
                    unrecoverable_failure err current result)
            in
            {State_space.typing; term = result} :: rewritings)
          []
          rewriting_options
      in
      match rewritings with [] -> proposal current | _ -> uniform rewritings

    let log_weight state = -.P.energy state

    include State_space
  end

  module Sampler = MH.Make (MH_params)

  let generator ~burn_in =
    Sampler.mcmc ~verbosity:P.verbosity ~initial:P.initial ~burn_in
end
