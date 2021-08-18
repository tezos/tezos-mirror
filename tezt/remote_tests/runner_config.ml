(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* This module is in charge of loading the configuration for the runner
   used into the [basic] and the [double_baking] remote test. *)

(* Try to parse the config file referenced in the TEZT_RUNNER_CONFIG
   environment variable. If the variable doesn't exist, the default
   value is [./runner_config.json]. If the file is not at the
   specified  location, it raised a [Sys_error]. *)
let runner_json =
  let path =
    match Sys.getenv_opt "TEZT_RUNNER_CONFIG" with
    | None -> "runner_config.json"
    | Some location -> location
  in
  JSON.parse_file path

(* Build a runner from the [runner_json] config file. If the address
   is not specified it raises a [Test.fail]. *)
let runner =
  let open JSON in
  let ssh_alias = runner_json |-> "ssh_alias" |> as_string_opt in
  let ssh_user = runner_json |-> "ssh_user" |> as_string_opt in
  let ssh_port = runner_json |-> "ssh_port" |> as_int_opt in
  let ssh_id = runner_json |-> "ssh_id" |> as_string_opt in
  let address = runner_json |-> "ip_address" |> as_string in
  Runner.create ?ssh_alias ?ssh_user ?ssh_port ?ssh_id ~address ()

(* Get the node executable path from the [runner_json] config file. If no path
   is specified in the configuration it raises [Test.fail]. *)
let node_path =
  let open JSON in
  runner_json |-> "node_path" |> as_string
