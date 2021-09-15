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

(** Generic MCMC michelson sampler (can be used for code and data) *)
module Make_generic (P : Sampler_parameters_sig) = struct
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

  let generator ~burn_in = P.(Sampler.mcmc ~verbosity ~initial ~burn_in)
end

module Code (X : sig
  module Samplers : Michelson_samplers_base.Full_S

  val rng_state : Random.State.t

  val target_size : int

  val verbosity : [`Silent | `Progress | `Trace]
end) =
struct
  module Autocomp = Autocomp.Make (X.Samplers)

  module MCMC = Make_generic (struct
    let initial =
      let term = Mikhailsky.Instructions.hole in
      let typing = Lazy.from_val @@ snd (Inference.infer_with_state term) in
      {State_space.term; typing}

    let energy state =
      let stats = State_space.statistics state in
      let size_deficit =
        abs_float
          (float_of_int X.target_size -. float_of_int stats.State_space.size)
      in
      let holes_proportion = float stats.holes /. float stats.size in
      let holes_deficit =
        (* we want at least 1% of holes, above is ok *)
        if holes_proportion < 0.01 then
          (0.01 -. holes_proportion) *. size_deficit
        else 0.0
      in
      size_deficit +. holes_deficit

    let rules = Rules.Instruction.rules

    let infer term = snd (Inference.infer_with_state term)

    let verbosity = X.verbosity
  end)

  let to_michelson ({typing; term} : State_space.t) =
    let typing = Lazy.force typing in
    let (node, typ, state) = Autocomp.complete_code typing term X.rng_state in
    let node =
      Micheline.strip_locations @@ Mikhailsky_to_michelson.convert node state
    in
    (node, typ)

  let generator ~burn_in =
    let open StaTz in
    Stats.map_gen to_michelson (MCMC.generator ~burn_in)
end

module Data (X : sig
  module Samplers : Michelson_samplers_base.Full_S

  val rng_state : Random.State.t

  val target_size : int

  val verbosity : [`Silent | `Progress | `Trace]
end) =
struct
  module Autocomp = Autocomp.Make (X.Samplers)
  module Rewrite_rules = Rules.Data_rewrite_leaves (X.Samplers)

  module MCMC = Make_generic (struct
    let initial =
      let term = Mikhailsky.Data.hole in
      let typing =
        Lazy.from_val @@ snd (Inference.infer_data_with_state term)
      in
      {State_space.term; typing}

    let energy state =
      let stats = State_space.statistics state in
      let size_deficit =
        abs_float
          (float_of_int X.target_size -. float_of_int stats.State_space.size)
      in
      let holes_proportion =
        float_of_int stats.holes /. float_of_int stats.size
      in
      let holes_deficit =
        (* we want at least 10% of holes, above is ok *)
        if holes_proportion < 0.5 then (0.5 -. holes_proportion) *. size_deficit
        else 0.0
      in
      let depth_deficit =
        abs_float
          ((0.1 *. float_of_int X.target_size) -. float_of_int stats.depth)
      in
      size_deficit +. holes_deficit +. depth_deficit

    let rules = Rewrite_rules.rules X.rng_state

    let infer term = snd (Inference.infer_data_with_state term)

    let verbosity = X.verbosity
  end)

  let to_michelson ({typing; term} : State_space.t) =
    let typing = Lazy.force typing in
    let (node, _) = Autocomp.complete_data typing term X.rng_state in
    let (typ, state) =
      try Inference.infer_data_with_state node
      with _ ->
        Format.eprintf "Bug found!@." ;
        Format.eprintf "Ill-typed autocompletion. Resulting term:@." ;
        Format.eprintf "%a@." Mikhailsky.pp node ;
        Stdlib.failwith "in generators.ml: unrecoverable failure"
    in
    let node =
      Micheline.strip_locations @@ Mikhailsky_to_michelson.convert node state
    in
    (node, typ)

  let generator ~burn_in =
    let open StaTz in
    Stats.map_gen to_michelson (MCMC.generator ~burn_in)
end
