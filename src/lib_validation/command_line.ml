(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let parse_args () =
  let socket_dir = ref None in
  let readonly_flag = ref false in
  let args =
    Arg.
      [
        ( "--socket-dir",
          String
            (fun s ->
              if not (Sys.file_exists s && Sys.is_directory s) then
                raise
                  (Arg.Bad
                     (Format.sprintf "File '%s' is not a valid directory" s))
              else socket_dir := Some s),
          {|<dir>
      When provided, the validator will communicate through a socket located
      at '<dir>/tezos-validation-socket-<pid>' where <pid> is the
      tezos-validator's process identifier. By default, the validator will
      communicate through its standard input and output.|}
        );
        ( "--readonly",
          Unit (fun () -> readonly_flag := true),
          {|<flag>
      When provided, the validator will start in readonly mode. It will not be
      able to update the context.|}
        );
        ( "--version",
          Unit
            (fun () ->
              Format.printf
                "%s\n"
                Tezos_version_value.Bin_version.octez_version_string ;
              Stdlib.exit 0),
          " Display version information" );
      ]
  in
  let usage_msg =
    Format.sprintf
      "tezos-validator [--version] [--socket-dir <dir>] [--readonly]"
  in
  Arg.parse
    args
    (fun s -> raise (Arg.Bad (Format.sprintf "Unexpected argument: %s" s)))
    usage_msg ;
  (!socket_dir, !readonly_flag)

let run () =
  let socket_dir, readonly = parse_args () in
  let main_promise = External_validator.main ?socket_dir ~readonly () in
  Stdlib.exit
    (let open Lwt_syntax in
    Lwt.Exception_filter.(set handle_all_except_runtime) ;
    Lwt_main.run
      (let* r = Lwt_exit.wrap_and_exit main_promise in
       match r with
       | Ok () -> Lwt_exit.exit_and_wait 0
       | Error err ->
           Format.eprintf "%a\n%!" pp_print_trace err ;
           Lwt_exit.exit_and_wait 1))
