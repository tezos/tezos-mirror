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

module Sapling_gen_cmd = struct
  let lift_opt f opt_arg state =
    match opt_arg with None -> state | Some arg -> f arg state

  (* ----------------------------------------------------------------------- *)
  (* Handling options for the "generate sapling transactions" command *)

  (* Generic max-%s argument *)
  let max name =
    Tezos_clic.arg
      ~doc:(Printf.sprintf "Maximum number of %s" name)
      ~long:(Printf.sprintf "max-%s" name)
      ~placeholder:"integer"
      (Tezos_clic.parameter (fun (_ : unit) parsed ->
           match int_of_string parsed with
           | exception Failure _ ->
               Format.eprintf
                 "Ill-formatted --max-%s option (expected integer), exiting@."
                 name ;
               exit 1
           | res when res < 0 ->
               Format.eprintf
                 "--max-%s should be a  positive integer, exiting@."
                 name ;
               exit 1
           | res -> return res))

  (* Integer argument --seed *)
  let seed_arg =
    let seed =
      Tezos_clic.parameter (fun (_ : unit) parsed ->
          try return (int_of_string parsed)
          with _ ->
            Format.eprintf "Error while parsing --seed argument.@." ;
            exit 1)
    in
    Tezos_clic.arg ~doc:"RNG seed" ~long:"seed" ~placeholder:"int" seed

  let positive_param =
    Tezos_clic.parameter (fun _ s ->
        match int_of_string_opt s with
        | Some i when i > 0 -> return i
        | _ -> failwith "Parameter should be a positive integer literal")

  open Sapling_generation

  let set_max_inputs max_inputs options = {options with max_inputs}

  let set_max_outputs max_outputs options = {options with max_outputs}

  let set_max_nullifiers max_nullifiers options = {options with max_nullifiers}

  let set_max_additional_commitments max_additional_commitments options =
    {options with max_additional_commitments}

  let set_seed seed (options : sapling_gen_options) =
    {options with seed = Some seed}

  let sapling_handler
      (max_inputs, max_outputs, max_nullifiers, max_additional_commitments, seed)
      tx_count save_to () =
    let sapling_gen_options =
      default_sapling_gen_options
      |> lift_opt set_max_inputs max_inputs
      |> lift_opt set_max_outputs max_outputs
      |> lift_opt set_max_nullifiers max_nullifiers
      |> lift_opt set_max_additional_commitments max_additional_commitments
      |> lift_opt set_seed seed
    in
    generate save_to tx_count sapling_gen_options ;
    return_unit

  let options =
    Tezos_clic.args5
      (max "inputs")
      (max "outputs")
      (max "nullifiers")
      (max "additional-commitments")
      seed_arg

  let params =
    Tezos_clic.(
      prefixes [Protocol.name; "sapling"; "generate"]
      @@ param
           ~name:"SAPLING-TX-COUNT"
           ~desc:"Number of sapling transactions to generate"
           positive_param
      @@ prefixes ["transactions"; "in"]
      @@ string
           ~name:"SAPLING-TX-FILE"
           ~desc:"File containing sapling transactions"
      @@ stop)

  let group =
    {
      Tezos_clic.name = "Sapling tx generation";
      title = "Command for generating random sapling transactions";
    }

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Sapling transaction generation"
      options
      params
      sapling_handler
end

let () = Registration.add_command Sapling_gen_cmd.command
