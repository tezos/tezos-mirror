(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let populate_wallet yes_wallet_dir alias_pkh_pk_list =
  let pkh_filename = "public_key_hashs" in
  let pk_filename = "public_keys" in
  let sk_filename = "secret_keys" in
  if not (Sys.file_exists yes_wallet_dir) then Unix.mkdir yes_wallet_dir 0o750 ;
  Unix.chdir yes_wallet_dir ;
  if
    Sys.file_exists pkh_filename
    || Sys.file_exists pk_filename
    || Sys.file_exists sk_filename
  then (
    Format.eprintf
      "Warning: cannot write wallet, at least one of the following files \
       already exists: %s/{%s,%s,%s} @."
      yes_wallet_dir
      pkh_filename
      pk_filename
      sk_filename ;
    false)
  else (
    json_to_file (pkh_list_json alias_pkh_pk_list) pkh_filename ;
    json_to_file (pk_list_json alias_pkh_pk_list) pk_filename ;
    json_to_file (sk_list_json alias_pkh_pk_list) sk_filename ;
    true)

let active_bakers_only_opt_name = "--active-bakers-only"

let usage () =
  Format.printf "> create minimal in <yes_wallet_dir>@." ;
  Format.printf
    "    creates a yes-wallet with the foundation baker keys in \
     <yes_wallet_dir>@." ;
  Format.printf
    "> create from context <base_dir> in <yes_wallet_dir> [%s]@."
    active_bakers_only_opt_name ;
  Format.printf
    "    creates a yes-wallet with all delegates in the head block of the \
     context in <base_dir> and store it in <yes_wallet_dir>@." ;
  Format.printf
    "    if %s is used the deactivated bakers are filtered out@."
    active_bakers_only_opt_name

let () =
  let argv = Array.to_list Sys.argv in
  let (options, argv) =
    List.partition
      (fun arg -> String.length arg > 0 && String.get arg 0 = '-')
      argv
  in
  let active_bakers_only =
    List.exists (fun opt -> opt = active_bakers_only_opt_name) options
  in
  let unknonw_options =
    List.filter (fun opt -> opt <> active_bakers_only_opt_name) options
  in
  if unknonw_options <> [] then
    Format.eprintf
      "Warning: unknown options %a@."
      (Format.pp_print_list Format.pp_print_string)
      unknonw_options ;
  match argv with
  | [] -> assert false
  | [_] ->
      usage () ;
      exit 0
  | [_; "create"; "minimal"; "in"; yes_wallet_dir] ->
      if active_bakers_only then
        Format.eprintf
          "Warning: option %s is ignored for create minimal@."
          active_bakers_only_opt_name ;
      if populate_wallet yes_wallet_dir Yes_wallet_lib.alias_pkh_pk_list then
        Format.printf "Created minimal wallet in %s@." yes_wallet_dir
  | [_; "create"; "from"; "context"; base_dir; "in"; yes_wallet_dir] ->
      let alias_pkh_pk_list =
        Yes_wallet_lib.load_mainnet_bakers_public_keys
          base_dir
          active_bakers_only
      in
      if populate_wallet yes_wallet_dir alias_pkh_pk_list then
        Format.printf "Created wallet in %s@." yes_wallet_dir
  | _ ->
      Format.eprintf "Invalid command. Usage:@." ;
      usage () ;
      exit 1
