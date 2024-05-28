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

type generator_config = {
  target_size : Base_samplers.range;
  burn_in_multiplier : int;
}

let default_generator_config =
  {target_size = {Base_samplers.min = 100; max = 1000}; burn_in_multiplier = 5}

let generator_config_encoding =
  let open Data_encoding in
  conv
    (fun {target_size; burn_in_multiplier} -> (target_size, burn_in_multiplier))
    (fun (target_size, burn_in_multiplier) -> {target_size; burn_in_multiplier})
    (obj2
       (req "target_size" Base_samplers.range_encoding)
       (req "burn_in_multiplier" int31))

(* ----------------------------------------------------------------------- *)

(* ----------------------------------------------------------------------- *)

module Crypto_samplers = Crypto_samplers.Make_finite_key_pool (struct
  let size = 16

  let algo = `Default
end)

module Samplers =
  Michelson_samplers.Make
    (struct
      let parameters =
        {
          Michelson_samplers.base_parameters =
            {
              int_size = {min = 8; max = 32};
              string_size = {min = 8; max = 128};
              bytes_size = {min = 8; max = 128};
            };
          list_size = {min = 0; max = 1000};
          set_size = {min = 0; max = 1000};
          map_size = {min = 0; max = 1000};
        }
    end)
    (Crypto_samplers)

module Michelson_base_samplers = Samplers.Michelson_base

(* ----------------------------------------------------------------------- *)

let make_data_sampler rng_state config =
  let target_size =
    Base_samplers.sample_in_interval rng_state ~range:config.target_size
  in
  let module Data =
    Michelson_mcmc_samplers.Make_data_sampler
      (Michelson_base_samplers)
      (Crypto_samplers)
      (struct
        let rng_state = rng_state

        let target_size = target_size

        let verbosity = `Silent
      end)
  in
  let burn_in = target_size * config.burn_in_multiplier in
  let generator = Data.generator ~burn_in rng_state in
  generator rng_state

let make_code_sampler rng_state config =
  let target_size =
    Base_samplers.sample_in_interval rng_state ~range:config.target_size
  in
  let module Code =
    Michelson_mcmc_samplers.Make_code_sampler
      (Michelson_base_samplers)
      (Crypto_samplers)
      (struct
        let rng_state = rng_state

        let target_size = target_size

        let verbosity = `Silent
      end)
  in
  let burn_in = target_size * config.burn_in_multiplier in
  let generator = Code.generator ~burn_in rng_state in
  generator rng_state
