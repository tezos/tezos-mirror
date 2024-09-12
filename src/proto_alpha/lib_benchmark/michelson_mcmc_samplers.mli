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

(** MCMC samplers can either produce data or code. Note that the samplers
    natively produce data and code in Micheline (ie untyped) form. *)

type michelson_code = {
  term : Script_repr.expr;
      (** [term] is a typeable Michelson program in Micheline form. *)
  bef : Script_repr.expr list;
      (** [bef] is an input stack type for which [term] is a well-typed script. *)
  aft : Script_repr.expr list;
      (** [aft] is the stack type corresponding to the execution of [term]
        on a stack of type [bef]. *)
}

type michelson_data = {
  term : Script_repr.expr;
      (** [term] is a typeable Michelson data in Micheline form. *)
  typ : Script_repr.expr;  (** [typ] is the type of [term]. *)
}

(** A [michelson_sample] is either a code sample or a data sample. *)
type michelson_sample = Code of michelson_code | Data of michelson_data

(** Encoding used for saving or loading data. *)
val michelson_sample_list_encoding : michelson_sample list Data_encoding.t

(** Saving a list of samples to a file.
    Exits with code 1 if an error arises during encoding or file manipulation. *)
val save : filename:string -> terms:michelson_sample list -> unit

(** Loading a list of samples from a file.
    Exits with code 1 if an error arises during decoding or file manipulation. *)
val load : filename:string -> michelson_sample list

(** Appends a list of samples to a file.
    Creates the file if non-existent.
    Exits with code 1 if an error arises during encoding or file manipulation. *)
val append : filename:string -> terms:michelson_sample list -> unit

(** [Make_code_sampler] produces a sampler for well-typed Michelson code.
    The parameters of the functor are:
    - a module [Michelson_base] implementing samplers for basic values
    - a module [Crypto_samplers] implementing samplers for pk/pkh/sk triplets
    - a module [X] containing some parameters to the Markov chain sampler:
      - [rng_state] is the mutable state that will be used during sampling
      - [target_size] specifies the size, in terms of Micheline nodes, of the
        terms that the sampler should try to produce
      - [verbosity] specifies how much information should be written on stdout
        during the sampling process.

    The outcome is a [michelson_code] [generator]. The [burn_in] parameter
    specifies how much samples should be thrown away before starting to
    produce sample (this is used to let the underlying Markov chain reach
    its stationary distribution - the value should be commensurate with
    the [target_size].
 *)
module Make_code_sampler : functor
  (Michelson_base : Michelson_samplers_base.S)
  (Crypto_samplers : Crypto_samplers.Finite_key_pool_S)
  (X : sig
     val rng_state : Random.State.t

     val target_size : int

     val verbosity : [`Progress | `Silent | `Trace]
   end)
  -> sig
  (** [generator ~burn_in rng_state] performs a burn-in phase consisting of sampling [burn_in] times,
      throwing the results away and returns a michelson term sampler. The goal of burn-in is
      to drive the underlying Markov chain to its stationary distribution, ie to sample
      terms around the specified [X.target_size]. *)
  val generator : burn_in:int -> Random.State.t -> michelson_code Stats.Gen.t
end

(** See documentation for [Make_code_sampler] *)
module Make_data_sampler : functor
  (Michelson_base : Michelson_samplers_base.S)
  (Crypto_samplers : Crypto_samplers.Finite_key_pool_S)
  (X : sig
     val rng_state : Random.State.t

     val target_size : int

     val verbosity : [`Progress | `Silent | `Trace]
   end)
  -> sig
  (** [generator ~burn_in rng_state] performs a burn-in phase consisting of sampling [burn_in] times,
      throwing the results away and returns a michelson term sampler. The goal of burn-in is
      to drive the underlying Markov chain to its stationary distribution, ie to sample
      terms around the specified [X.target_size]. *)
  val generator : burn_in:int -> Random.State.t -> michelson_data Stats.Gen.t
end
