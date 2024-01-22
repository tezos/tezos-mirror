(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Octogram

type format = Yaml | Json

let format_of_path path =
  match Filename.extension path with
  | ".yml" -> Yaml
  | ".json" -> Json
  | ext ->
      Test.fail
        "invalid extension %s (%s): expected \"yml\" or \"json\""
        ext
        path

let value_from_string ~on_error encoding str format =
  try
    match format with
    | Yaml -> Yaml.(of_string_exn str |> Data_encoding.Json.destruct encoding)
    | Json -> Helpers.of_json_string encoding str
  with
  | Json_encoding.Cannot_destruct (path, exn) ->
      Log.error
        "%a: %a"
        (fun fmt -> Json_query.print_path_as_json_path fmt)
        path
        (fun fmt -> Data_encoding.Json.print_error fmt)
        exn ;
      Test.fail "%s" on_error
  | exn ->
      Log.error "%s" (Printexc.to_string exn) ;
      Test.fail "%s" on_error

let value_from_file ~on_error encoding path =
  value_from_string ~on_error encoding (read_file path) (format_of_path path)

let configuration_from_file =
  value_from_file ~on_error:"Cannot parse configuration" Recipe.encoding

let additional_agents_from_file =
  value_from_file
    ~on_error:"Cannot parse agents"
    (Data_encoding.list Recipe.agent_encoding)

let global_variables_from_file =
  value_from_file
    ~on_error:"Cannot parse global variables"
    Global_variables.encoding

let run () =
  let additional_agents =
    match Cli.get_string_opt "agents" with
    | Some agents_path -> additional_agents_from_file agents_path
    | None -> []
  in
  let additional_variables =
    match Cli.get_string_opt "variables" with
    | Some path -> global_variables_from_file path
    | None -> Global_variables.empty
  in
  let configuration_path = Cli.get_string ~default:"octogram.yml" "recipe" in
  let configuration = configuration_from_file configuration_path in

  let configuration =
    {
      configuration with
      agents = configuration.agents @ additional_agents;
      vars = Global_variables.merge configuration.vars additional_variables;
    }
  in

  Orchestrator.run_recipe
    ~keep_alive:(Cli.get_bool ~default:true "keep_alive")
    configuration

let register () =
  Test.register
    ~__FILE__
    ~title:"Octogram:orchestrator"
    ~tags:["orchestrator"]
    run
