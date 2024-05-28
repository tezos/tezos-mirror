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

open Michelson_generation

let group =
  {
    Tezos_clic.name = "Michelson generation";
    title = "Command for generating random Michelson code and data";
  }

module Michelson_concat_cmd = struct
  let handler () file1 file2 file3 () =
    let open Lwt_result_syntax in
    let trace1 = Michelson_mcmc_samplers.load ~filename:file1 in
    let trace2 = Michelson_mcmc_samplers.load ~filename:file2 in
    let terms = trace1 @ trace2 in
    let l1 = List.length trace1 in
    let l2 = List.length trace2 in
    Format.eprintf
      "Loaded %d terms from %s, %d terms from %s, total %d@."
      l1
      file1
      l2
      file2
      (l1 + l2) ;
    Michelson_mcmc_samplers.save ~filename:file3 ~terms ;
    return_unit

  let params =
    Tezos_clic.(
      prefixes [Protocol.name; "michelson"; "concat"; "files"]
      @@ string ~name:"FILENAME" ~desc:"First file"
      @@ prefixes ["and"]
      @@ string ~name:"FILENAME" ~desc:"Second file"
      @@ prefixes ["into"]
      @@ string ~name:"FILENAME" ~desc:"Target file"
      @@ stop)

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Michelson generation"
      Tezos_clic.no_options
      params
      handler
end

let () = Registration.add_command Michelson_concat_cmd.command

module Michelson_gen_cmd = struct
  let lift_opt f opt_arg state =
    match opt_arg with None -> state | Some arg -> f arg state

  let handler (min_size, max_size, burn_in, seed) terms_count terms_kind
      filename () =
    let open Lwt_result_syntax in
    let default = Michelson_generation.default_generator_config in
    let min = Option.value ~default:default.target_size.min min_size in
    let max = Option.value ~default:default.target_size.max max_size in
    let burn_in_multiplier =
      Option.value ~default:default.burn_in_multiplier burn_in
    in
    let rng_state =
      match seed with
      | None ->
          Format.eprintf "Self-initialization of PRNG@." ;
          let state = Random.State.make_self_init () in
          Format.(eprintf "PRNG state hash: %d@." (Hashtbl.hash state)) ;
          state
      | Some seed ->
          Format.eprintf "PRNG initialized with seed %d@." seed ;
          Random.State.make [|seed|]
    in
    let cfg =
      {Michelson_generation.target_size = {min; max}; burn_in_multiplier}
    in
    let terms_count =
      match int_of_string terms_count with
      | exception Failure _ ->
          Format.eprintf "TERMS-COUNT must be an integer, exiting@." ;
          exit 1
      | terms_count ->
          if terms_count <= 0 then (
            Format.eprintf "TERMS-COUNT must be strictly positive, exiting@." ;
            exit 1)
          else terms_count
    in
    let progress =
      Benchmark_helpers.make_progress_printer
        Format.err_formatter
        terms_count
        "Generating term"
    in
    let terms =
      match terms_kind with
      | "data" ->
          Stdlib.List.init terms_count (fun _i ->
              progress () ;
              Michelson_mcmc_samplers.Data
                (Michelson_generation.make_data_sampler rng_state cfg))
      | "code" ->
          Stdlib.List.init terms_count (fun _i ->
              progress () ;
              Michelson_mcmc_samplers.Code
                (Michelson_generation.make_code_sampler rng_state cfg))
      | _ ->
          Format.eprintf "Term kind must be either \"data\" or \"code\"@." ;
          exit 1
    in
    Michelson_mcmc_samplers.save ~filename ~terms ;
    return_unit

  let min_size_arg =
    let open Lwt_result_syntax in
    let min_size =
      Tezos_clic.parameter (fun (_ : unit) parsed ->
          try return (int_of_string parsed)
          with _ ->
            Format.eprintf "Error while parsing --min-size argument.@." ;
            exit 1)
    in
    Tezos_clic.arg
      ~doc:"Lower bound for target size of terms"
      ~long:"min-size"
      ~placeholder:"int"
      min_size

  let max_size_arg =
    let open Lwt_result_syntax in
    let max_size =
      Tezos_clic.parameter (fun (_ : unit) parsed ->
          try return (int_of_string parsed)
          with _ ->
            Format.eprintf "Error while parsing --max-size argument.@." ;
            exit 1)
    in
    Tezos_clic.arg
      ~doc:"Lower bound for target size of terms"
      ~long:"max-size"
      ~placeholder:"int"
      max_size

  let burn_in_arg =
    let open Lwt_result_syntax in
    let target_size =
      Tezos_clic.parameter (fun (_ : unit) parsed ->
          try return (int_of_string parsed)
          with _ ->
            Format.eprintf "Error while parsing --burn-in argument.@." ;
            exit 1)
    in
    Tezos_clic.arg
      ~doc:"Burn-in multiplier"
      ~long:"burn-in"
      ~placeholder:"int"
      target_size

  let seed_arg =
    let open Lwt_result_syntax in
    let seed =
      Tezos_clic.parameter (fun (_ : unit) parsed ->
          try return (int_of_string parsed)
          with _ ->
            Format.eprintf "Error while parsing --seed argument.@." ;
            exit 1)
    in
    Tezos_clic.arg ~doc:"RNG seed" ~long:"seed" ~placeholder:"int" seed

  let options = Tezos_clic.args4 min_size_arg max_size_arg burn_in_arg seed_arg

  let params =
    Tezos_clic.(
      prefixes [Protocol.name; "michelson"; "generate"]
      @@ string ~name:"TERMS-COUNT" ~desc:"Number of terms to generate"
      @@ prefixes ["terms"; "of"; "kind"]
      @@ string ~name:"{data|code}" ~desc:"Kind of term to generate"
      @@ prefixes ["in"]
      @@ string ~name:"FILENAME" ~desc:"File where to save Michelson terms"
      @@ stop)

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Michelson generation"
      options
      params
      handler
end

let () = Registration.add_command Michelson_gen_cmd.command
