(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let () = Lwt.Exception_filter.(set handle_all_except_runtime)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4025
   Remove backwards compatible Tezos symlinks. *)
let () =
  (* warn_if_argv0_name_not_octez *)
  let executable_name = Filename.basename Sys.argv.(0) in
  let prefix = "tezos-" in
  if TzString.has_prefix executable_name ~prefix then
    let expected_name =
      let len_prefix = String.length prefix in
      "octez-"
      ^ String.sub
          executable_name
          len_prefix
          (String.length executable_name - len_prefix)
    in
    Format.eprintf
      "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
       The executable with name @{<kwd>%s@} has been renamed to @{<kwd>%s@}. \
       The name @{<kwd>%s@} is now@,\
       deprecated, and it will be removed in a future release. Please update@,\
       your scripts to use the new name.@]@\n\
       @."
      executable_name
      expected_name
      executable_name
  else ()

let srcdir = Sys.argv.(1)

let version = Sys.argv.(2)

let srcdir =
  if Filename.basename srcdir = "TEZOS_PROTOCOL" then Filename.dirname srcdir
  else srcdir

let hash, sources =
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  match Lwt_main.run (Tezos_base_unix.Protocol_files.read_dir srcdir) with
  | Ok (None, proto) -> (Protocol.hash proto, proto)
  | Ok (Some hash, proto) -> (hash, proto)
  | Error err ->
      Format.kasprintf
        Stdlib.failwith
        "Failed to read TEZOS_PROTOCOL: %a"
        pp_print_trace
        err

let () =
  Format.printf
    {|
module Source = struct
  let hash =
    Some (Tezos_crypto.Hashed.Protocol_hash.of_b58check_exn %S)
  let sources = Tezos_base.Protocol.%a
end
@.|}
    (Protocol_hash.to_b58check hash)
    Protocol.pp_ocaml
    sources

let () =
  Format.printf
    {|
module Registered =
  Tezos_protocol_updater.Registered_protocol.Register_embedded_%s
    (Tezos_protocol_%s.Environment)
    (Tezos_protocol_%s.Protocol.Main)
    (Source)
@.|}
    (Protocol.module_name_of_env_version sources.expected_env)
    version
    version
