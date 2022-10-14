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

let register_signers () =
  Tezos_client_base.Client_keys.register_aggregate_signer
    (module Tezos_signer_backends.Unencrypted.Aggregate)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4025
   Remove backwards compatible Tezos symlinks. *)
let warn_if_argv0_name_not_octez () =
  let executable_name =
    (* example: tezos-tx-rollup-client-015-PtKathma *)
    Filename.basename Sys.argv.(0)
  in
  let old_head = "tezos-" in
  let new_head = "octez-" in
  match TzString.has_prefix executable_name ~prefix:old_head with
  | false -> ()
  | true ->
      let expected_name =
        let len_prefix = String.length old_head in
        let headless_name =
          (* example: tx-rollup-client-015-PtKathma *)
          String.sub
            executable_name
            len_prefix
            (String.length executable_name - len_prefix)
        in
        let name_without_version name =
          match headless_name |> String.starts_with ~prefix:name with
          | false -> None
          | true ->
              let len_name = String.length name in
              let version_proto =
                (* example: -015-PtKathma *)
                String.sub
                  headless_name
                  len_name
                  (String.length headless_name - len_name)
              in
              let num_hyphens =
                (* example: 2 *)
                version_proto
                |> String.fold_left
                     (fun acc c -> match c with '-' -> acc + 1 | _ -> acc)
                     0
              in
              let proto =
                (* example: -PtKathma *)
                if num_hyphens = 2 then
                  String.sub version_proto 4 (String.length version_proto - 4)
                else version_proto
              in
              Some (name ^ proto)
        in
        (* example: tx-rollup-client-PtKathma *)
        let versionless_name =
          ["accuser"; "baker"; "tx-rollup-client"; "tx-rollup-node"]
          |> List.map name_without_version
          |> List.find Option.is_some |> Option.join
        in
        (* example: octez-tx-rollup-client-PtKathma *)
        new_head
        ^
        match versionless_name with
        | None -> headless_name
        | Some versionless_name -> versionless_name
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

let main () =
  warn_if_argv0_name_not_octez () ;
  Configuration.parse (argv ()) >>=? fun (configuration, argv) ->
  register_signers () ;
  let cctxt = Configuration.make_unix_client_context configuration in
  Clic.dispatch (Commands.all ()) cctxt argv

let handle_error = function
  | Ok () -> Stdlib.exit 0
  | Error [Clic.Version] ->
      let version = Tezos_version.Bin_version.version_string in
      Format.printf "%s@." version ;
      Stdlib.exit 0
  | Error [Clic.Help command] ->
      Clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options:(Configuration.global_options ())
        (Stdlib.Option.to_list command) ;
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
