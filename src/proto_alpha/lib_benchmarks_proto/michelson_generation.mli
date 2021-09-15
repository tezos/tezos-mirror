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

open Protocol

(** [generator_config] specifies some parameters to the
    {!Tezos_benchmark_alpha.Generators} Michelson code and data generators. *)
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

(** [michelson_data] is the type of Michelson data, as produced by the
    samplers. *)
type michelson_data =
  | Code of {
      term : Script_repr.expr;
          (** [term] is a typeable Michelson program in Micheline form. *)
      bef : Script_repr.expr list;
          (** [bef] is the type of the initial stack of [term]. *)
    }
  | Data of {
      term : Script_repr.expr;
          (** [term] is a typeable Michelson data in Micheline form. *)
      typ : Script_repr.expr;  (** [typ] is the type of [term]. *)
    }

val michelson_data_list_encoding : michelson_data list Data_encoding.t

(** Saving/loading michelson data to disk *)

(** [save ~filename ~terms] saves the list [terms] to the file [filename]. *)
val save : filename:string -> terms:michelson_data list -> unit

(** [load filename] loads the Michelson data in [filename]. *)
val load : filename:string -> michelson_data list

(** Type conversion helpers *)

(** [michelson_type_list_to_ex_stack_ty] converts a list of types in
    Micheline form to a stack type in IR form. *)
val michelson_type_list_to_ex_stack_ty :
  Alpha_context.Script.expr list ->
  Alpha_context.t ->
  Script_ir_translator.ex_stack_ty * Alpha_context.t

(** [michelson_type_to_ex_ty ty ctxt] parses the type [ty].

    @raise Failure if an error arises during parsing. *)
val michelson_type_to_ex_ty :
  Alpha_context.Script.expr ->
  Alpha_context.t ->
  Script_ir_translator.ex_ty * Alpha_context.t

(** Samplers *)

(** [make_data_sampler] constructs a Michelson data sampler based on the
    infrastructure available in {!Tezos_benchmark_alpha.Generators}. *)
val make_data_sampler : Random.State.t -> generator_config -> michelson_data

(** [make_code_sampler] constructs a Michelson code sampler based on the
    infrastructure available in {!Tezos_benchmark_alpha.Generators}. *)
val make_code_sampler : Random.State.t -> generator_config -> michelson_data

(** [Samplers] is an instance of the direct-style (non-MCMC based) samplers
    implemented in {!Tezos_benchmark_alpha.Michelson_samplers}. *)
module Samplers : Michelson_samplers.S
