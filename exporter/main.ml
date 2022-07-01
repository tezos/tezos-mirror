(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt_result_syntax

let group = {Clic.name = "teztale-exporter"; Clic.title = "Exporter RPC server"}

let path_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p) then failwith "File does not exist: '%s'" p
      else return p)

let db_path_param =
  Clic.param
    ~name:"db_path"
    ~desc:"path to the Sqlite3 database file where the data is stored"
    path_parameter

let addr_arg =
  Clic.default_arg
    ~long:"rpc_addr"
    ~placeholder:"IP addr|host"
    ~doc:"the address at which the server listens (default: 127.0.0.1)"
    ~default:"127.0.0.1"
    (Client_config.string_parameter ())

type error += Invalid_port of string

let () =
  register_error_kind
    `Permanent
    ~id:"exporter.invalidPortArg"
    ~title:"Bad Port Argument"
    ~description:"Port argument could not be parsed"
    ~pp:(fun ppf s -> Format.fprintf ppf "Value %s is not a valid TCP port." s)
    Data_encoding.(obj1 (req "value" string))
    (function Invalid_port s -> Some s | _ -> None)
    (fun s -> Invalid_port s)

let port_arg =
  Clic.default_arg
    ~long:"rpc_port"
    ~placeholder:"number"
    ~doc:"the port at which the server listens (default: 9799)"
    ~default:"9799"
    (Clic.parameter (fun _ x ->
         match int_of_string_opt x with
         | Some i -> return i
         | None -> tzfail (Invalid_port x)))

let run_command =
  let open Clic in
  command
    ~desc:"run the exporter RPC server"
    (args2 addr_arg port_arg)
    (prefixes ["run"; "on"] @@ db_path_param @@ stop)
    (fun addr_port db_path cctxt -> Server.run ~db_path addr_port cctxt)

let commands () = [run_command]

let select_commands _ _ =
  let open Lwt_result_syntax in
  return (commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
