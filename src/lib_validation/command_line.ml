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

let parse_args name =
  let socket_dir = ref None in
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
      The validator will communicate through a socket located
      at '<dir>/tezos-validation-socket-<pid>' where <pid> is the
      tezos-validator's process identifier.|}
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
  let usage_msg = Format.sprintf "%s [--version] [--socket-dir <dir>]" name in
  Arg.parse
    args
    (fun s -> raise (Arg.Bad (Format.sprintf "Unexpected argument: %s" s)))
    usage_msg ;
  match !socket_dir with
  | Some s -> s
  | None ->
      raise (Arg.Bad (Format.sprintf "%s: please provide --socket-dir" name))

let run name main =
  let socket_dir = parse_args name in
  let main_promise = main ~socket_dir in
  Stdlib.exit
    (let open Lwt_syntax in
     Lwt.Exception_filter.(set handle_all_except_runtime) ;
     Tezos_base_unix.Event_loop.main_run (fun () ->
         let* r = Lwt_exit.wrap_and_exit main_promise in
         match r with
         | Ok () -> Lwt_exit.exit_and_wait 0
         | Error err ->
             Format.eprintf "%a\n%!" pp_print_trace err ;
             Lwt_exit.exit_and_wait 1))

module Validator = struct
  let run () = run "octez-validator" External_validator.main
end

module Hypervisor = struct
  let run () =
    run "octez-validator-hypervisor" External_validator.Hypervisor.main
end
