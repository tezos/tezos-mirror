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

let populate_wallet yes_wallet_dir alias_pkh_pk_list =
  Unix.mkdir yes_wallet_dir 0o750 ;
  Unix.chdir yes_wallet_dir ;
  json_to_file (pkh_list_json alias_pkh_pk_list) "public_key_hashs" ;
  json_to_file (pk_list_json alias_pkh_pk_list) "public_keys" ;
  json_to_file (sk_list_json alias_pkh_pk_list) "secret_keys"

let usage () =
  Format.printf "> create minimal in <yes_wallet_dir>@." ;
  Format.printf "    creates a yes-wallet with the foundation baker keys in <yes_wallet_dir>@." ;
  Format.printf "> create from context <base_dir> in <yes_wallet_dir>@." ;
  Format.printf "    creates a yes-wallet with all delegates in the head block of the context in <base_dir> and store it in <yes_wallet_dir>@."

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | [_] ->
    usage () ;
    exit 0
  | [_; "create"; "minimal"; "in"; yes_wallet_dir] ->
    populate_wallet yes_wallet_dir Yes_wallet_lib.alias_pkh_pk_list ;
    Format.printf "Created minimal wallet in %s@." yes_wallet_dir
  | [_; "create"; "from"; "context"; base_dir; "in"; yes_wallet_dir] ->
    let alias_pkh_pk_list =
      Yes_wallet_lib.load_mainnet_bakers_public_keys base_dir in
    populate_wallet yes_wallet_dir alias_pkh_pk_list ;
    Format.printf "Created wallet in %s@." yes_wallet_dir
  | _ ->
    Format.eprintf "Invalid command. Usage:@." ;
    usage () ;
    exit 1
