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

(** {2 Wrappers around some Michelson generators and related helpers} *)

(** [generator_config] specifies some parameters to the
    {!Tezos_benchmark_alpha.Michelson_mcmc_samplers} Michelson code and data generators. *)
type generator_config = {
  target_size : Base_samplers.range;
      (** The target size of the terms, in number of nodes, is sampled uniformly
          in [target_size]. *)
  burn_in_multiplier : int;
      (** The generators are based on a Markov chain, which must be
          "heated-up" until it reaches its stationary state. A prefix of samples
          are therefore thrown away: this is called the {e burn-in} phase.
          The number of thrown away terms is proportional to [burn_in_multiplier]
          and [target_size]. *)
}

(** Default configuration for the generators. *)
val default_generator_config : generator_config

val generator_config_encoding : generator_config Data_encoding.t

(** Samplers *)

(** [make_data_sampler] constructs a Michelson data sampler based on the
    infrastructure available in {!Tezos_benchmark_alpha.Michelson_mcmc_samplers}. *)
val make_data_sampler :
  Random.State.t -> generator_config -> Michelson_mcmc_samplers.michelson_data

(** [make_code_sampler] constructs a Michelson code sampler based on the
    infrastructure available in {!Tezos_benchmark_alpha.Michelson_mcmc_samplers}. *)
val make_code_sampler :
  Random.State.t -> generator_config -> Michelson_mcmc_samplers.michelson_code

(** [Samplers] is an instance of the direct-style (non-MCMC based) samplers
    implemented in {!Tezos_benchmark_alpha.Michelson_samplers}. *)
module Samplers : Michelson_samplers.S
