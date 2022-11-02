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

(* Input parameter parsing *)

let verbose =
  if Array.length Sys.argv < 2 then (
    Format.eprintf "Executable expects random seed on input\n%!" ;
    exit 1)
  else
    (Random.init (int_of_string Sys.argv.(1)) ;
     List.exists (( = ) "-v"))
      (Array.to_list Sys.argv)

(* ------------------------------------------------------------------------- *)
(* MCMC instantiation *)

let state = Random.State.make [|42; 987897; 54120|]

module Crypto_samplers = Crypto_samplers.Make_finite_key_pool (struct
  let algo = `Default

  let size = 16
end)

module Michelson_base_samplers = Michelson_samplers_base.Make (struct
  let parameters =
    let size = {Base_samplers.min = 4; max = 32} in
    {
      Michelson_samplers_base.int_size = size;
      string_size = size;
      bytes_size = size;
    }
end)

module Data =
  Michelson_mcmc_samplers.Make_data_sampler
    (Michelson_base_samplers)
    (Crypto_samplers)
    (struct
      let rng_state = state

      let target_size = 500

      let verbosity = if verbose then `Trace else `Silent
    end)

let start = Unix.gettimeofday ()

let generator = Data.generator ~burn_in:(200 * 7) state

let stop = Unix.gettimeofday ()

let () = Format.printf "Burn in time: %f seconds@." (stop -. start)

let _ =
  for _i = 0 to 1000 do
    let Michelson_mcmc_samplers.{term = michelson; typ} = generator state in
    if verbose then (
      Format.eprintf "result:@." ;
      Format.eprintf "type: %a@." Test_helpers.print_script_expr typ ;
      Format.eprintf "%a@." Test_helpers.print_script_expr michelson)
  done
