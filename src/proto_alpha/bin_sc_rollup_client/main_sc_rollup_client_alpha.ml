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

let executable_name = Filename.basename Sys.executable_name

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let main () =
  Configuration.parse (argv ()) >>=? fun (configuration, argv) ->
  Clic.dispatch (Commands.all ()) configuration argv

let handle_error = function
  | Ok () -> Stdlib.exit 0
  | Error [Clic.Version] ->
      let version = Tezos_version.Bin_version.version_string in
      Format.printf "%s\n" version ;
      Stdlib.exit 0
  | Error [Clic.Help command] ->
      Clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options:(Configuration.global_options ())
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options:(Configuration.global_options ())
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let () = Lwt_main.run (Lwt.catch main fail_with_exn) |> handle_error
