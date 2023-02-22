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

(** MCMC-based Michelson data and code samplers. *)

open Protocol
open Stats

type michelson_code = {
  term : Script_repr.expr;
  bef : Script_repr.expr list;
  aft : Script_repr.expr list;
}

type michelson_data = {term : Script_repr.expr; typ : Script_repr.expr}

type michelson_sample = Code of michelson_code | Data of michelson_data

let michelson_sample_list_encoding =
  let open Data_encoding in
  let e = Script_repr.expr_encoding in
  list
  @@ union
       [
         case
           ~title:"Code"
           (Tag 0)
           (tup3 e (list e) (list e))
           (function
             | Code {term; bef; aft} -> Some (term, bef, aft) | _ -> None)
           (fun (term, bef, aft) -> Code {term; bef; aft});
         case
           ~title:"Data"
           (Tag 1)
           (tup2 e e)
           (function Data {term; typ} -> Some (term, typ) | _ -> None)
           (fun (term, typ) -> Data {term; typ});
       ]

let save ~filename ~terms =
  let str =
    match
      Data_encoding.Binary.to_string michelson_sample_list_encoding terms
    with
    | Error err ->
        Format.eprintf
          "Michelson_mcmc_samplers.save: encoding failed (%a); exiting@."
          Data_encoding.Binary.pp_write_error
          err ;
        exit 1
    | Ok res -> res
  in
  try Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file filename str
  with exn ->
    Format.eprintf
      "Michelson_mcmc_samplers.save: create_file %s failed (%s); exiting@."
      filename
      (Printexc.to_string exn) ;
    exit 1

let load ~filename =
  let open TzPervasives in
  let string =
    try Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.read_file filename
    with exn ->
      Format.eprintf
        "Michelson_mcmc_samplers.load: read_file %s failed (%s); exiting@."
        filename
        (Printexc.to_string exn) ;
      exit 1
  in
  let bytes = Bytes.of_string string in
  match Data_encoding.Binary.of_bytes michelson_sample_list_encoding bytes with
  | Ok result -> result
  | Error err ->
      Format.eprintf
        "Michelson_mcmc_samplers.load: decoding %s failed (%a); exiting@."
        filename
        Data_encoding.Binary.pp_read_error
        err ;
      exit 1

(* Helpers *)

let base_type_to_michelson_type (typ : Type.Base.t) =
  let typ = Mikhailsky.map_var (fun _ -> Mikhailsky.unit_ty) typ in
  Mikhailsky.to_michelson typ

module type Sampler_parameters_sig = sig
  val initial : State_space.t

  val energy : State_space.t -> float

  val rules : Rules.rule_set list

  val infer : Mikhailsky.node -> Inference.state

  val verbosity : [`Silent | `Progress | `Trace]
end

(* The Markov chain in state [state] *)
type mc_state = {
  state : State_space.t;
  jump : State_space.t Fin.Float.prb Lazy.t;
}

module State_hashtbl = Hashtbl.Make (State_space)

(** Generic MCMC michelson sampler (can be used for code and data) *)
module Make_generic (P : Sampler_parameters_sig) = struct
  let uniform (l : State_space.t list) : State_space.t Fin.Float.prb =
    match l with
    | [] ->
        (* This can only happen is the MCMC was driven to a coffin state,
           which means that it's not reversible (this is a bug) *)
        assert false
    | _ ->
        let arr = Array.of_list l in
        let emp = Emp.of_raw_data arr in
        Fin.Float.counts_of_empirical (module State_hashtbl) emp
        |> Fin.Float.normalize

  let unrecoverable_failure err current result =
    Format.eprintf "Error when typechecking term:@." ;
    Format.eprintf "%a@." Inference.pp_inference_error err ;
    Format.eprintf "Original state: @[%a@]@." State_space.pp current ;
    Format.eprintf "Erroneous term: %a@." Mikhailsky.pp result ;
    Stdlib.failwith "in sampler.ml: unrecoverable failure."

  let of_state : State_space.t -> mc_state =
   fun state ->
    {
      state;
      jump =
        Lazy.from_fun (fun () ->
            let current = state in
            let rewriting_options = Rules.rewriting current P.rules in
            let term = current.term in
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
            uniform rewritings);
    }

  module MH_params : Mh.MH_parameters with type t = mc_state = struct
    type t = mc_state

    let pp fmtr {state; jump = _} = State_space.pp fmtr state

    let trace state =
      match P.verbosity with
      | `Silent | `Progress -> ()
      | `Trace ->
          Format.eprintf "@." ;
          Format.eprintf "%a" State_space.pp state ;
          Format.eprintf "energy:@." ;
          Format.eprintf "%f:@." (P.energy state)

    let proposal_log_density s1 s2 =
      let jump = Lazy.force s1.jump in
      let p = try Fin.Float.eval_prb jump s2.state with Not_found -> 0.0 in
      Log_space.of_float p

    let proposal mcmc_state rng_state =
      trace mcmc_state.state ;
      let dist = Lazy.force mcmc_state.jump in
      let next = Fin.Float.sample (Fin.Float.as_measure dist) rng_state in
      of_state next

    let log_weight state = Log_space.unsafe_cast (-.P.energy state.state)
  end

  module Sampler = Mh.Make (MH_params)

  let generator ~burn_in =
    P.(Sampler.mcmc ~verbosity ~initial:(of_state initial) ~burn_in)
end

module Make_code_sampler
    (Michelson_base : Michelson_samplers_base.S)
    (Crypto_samplers : Crypto_samplers.Finite_key_pool_S) (X : sig
      val rng_state : Random.State.t

      val target_size : int

      val verbosity : [`Silent | `Progress | `Trace]
    end) =
struct
  module Autocomp = Autocomp.Make (Michelson_base) (Crypto_samplers)

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

  let to_michelson {state = ({typing; term} : State_space.t); jump = _} =
    let typing = Lazy.force typing in
    let node, (bef, aft), state =
      Autocomp.complete_code typing term X.rng_state
    in
    let node =
      Micheline.strip_locations @@ Mikhailsky_to_michelson.convert node state
    in
    {
      term = node;
      bef = Type_helpers.stack_type_to_michelson_type_list bef;
      aft = Type_helpers.stack_type_to_michelson_type_list aft;
    }

  let generator ~burn_in =
    Gen.map (MCMC.generator ~burn_in) @@ fun after_burn_in ->
    Gen.map after_burn_in to_michelson
end

module Make_data_sampler
    (Michelson_base : Michelson_samplers_base.S)
    (Crypto_samplers : Crypto_samplers.Finite_key_pool_S) (X : sig
      val rng_state : Random.State.t

      val target_size : int

      val verbosity : [`Silent | `Progress | `Trace]
    end) =
struct
  module Autocomp = Autocomp.Make (Michelson_base) (Crypto_samplers)
  module Rewrite_rules =
    Rules.Data_rewrite_leaves (Michelson_base) (Crypto_samplers)

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

  let to_michelson {state = ({typing; term} : State_space.t); jump = _} =
    let typing = Lazy.force typing in
    let node, _ = Autocomp.complete_data typing term X.rng_state in
    let typ, state =
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
    {term = node; typ = base_type_to_michelson_type typ}

  let generator ~burn_in =
    Gen.map (MCMC.generator ~burn_in) @@ fun after_burn_in ->
    Gen.map after_burn_in to_michelson
end
