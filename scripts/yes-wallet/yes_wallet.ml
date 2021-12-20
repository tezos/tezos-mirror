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

type error = Overwrite_forbiden of string | File_not_found of string

let try_copy ~replace source target =
  if source = target then Ok ()
  else
    let tmp_target = Filename.temp_file target ".tmp" in
    if Sys.file_exists source then
      if (not (Sys.file_exists target)) || replace then (
        let chin = open_in source
        and chout = open_out (if replace then tmp_target else target) in
        (try
           while true do
             let input = input_line chin in
             output_string chout input
           done
         with End_of_file -> ()) ;
        close_in chin ;
        close_out chout ;
        Ok (Sys.rename tmp_target target))
      else Error (Overwrite_forbiden target)
    else Error (File_not_found source)

let convert_wallet ~replace wallet_dir target_dir =
  let pkh_filename = "public_key_hashs" in
  let pk_filename = "public_keys" in
  let sk_filename = "secret_keys" in
  if not (Sys.file_exists wallet_dir) then
    failwith "wallet does not exists, cannot convert empty wallet."
  else
    let pkh_target = Filename.concat target_dir pkh_filename in
    let pk_target = Filename.concat target_dir pk_filename in
    let sk_target = Filename.concat target_dir sk_filename in
    let pkh_source = Filename.concat wallet_dir pkh_filename in
    let pk_source = Filename.concat wallet_dir pk_filename in
    if
      (Sys.file_exists pkh_target || Sys.file_exists pk_target
     || Sys.file_exists sk_target)
      && not replace
    then
      Format.eprintf
        "Warning: cannot write yes-wallet, at least one of the following files \
         already exists: %s/{%s,%s,%s} (you can use --force  option to \
         overwrite files)@."
        target_dir
        pkh_filename
        pk_filename
        sk_filename
    else if Sys.file_exists pk_source then (
      let sk_list = sk_list_of_pk_file pk_source in
      json_to_file sk_list sk_target ;
      (match try_copy ~replace pk_source pk_target with
      | Ok () -> ()
      | Error (Overwrite_forbiden _) ->
          assert false (* [replace] is [true] in this branch *)
      | Error (File_not_found _) ->
          assert false (* [pk_source] file exists int this branch *)) ;
      match try_copy ~replace pkh_source pkh_target with
      | Ok () -> ()
      | Error (Overwrite_forbiden _) ->
          assert false (* replace is true in this branch *)
      | Error (File_not_found _) -> ())
    else
      Format.eprintf
        "Warning: cannot produce yes-wallet, the file %s does not exist@."
        pk_source

let populate_wallet ~replace yes_wallet_dir alias_pkh_pk_list =
  let pkh_filename = "public_key_hashs" in
  let pk_filename = "public_keys" in
  let sk_filename = "secret_keys" in
  if not (Sys.file_exists yes_wallet_dir) then Unix.mkdir yes_wallet_dir 0o750 ;
  Unix.chdir yes_wallet_dir ;
  if
    (Sys.file_exists pkh_filename
    || Sys.file_exists pk_filename
    || Sys.file_exists sk_filename)
    && not replace
  then (
    Format.eprintf
      "Warning: cannot write wallet, at least one of the following files \
       already exists: %s/{%s,%s,%s} (you can use --force  option to overwrite \
       files)@."
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

let force_opt_name = "--force"

let staking_share_opt_name = "--staking-share"

let force = ref false

let confirm_rewrite wallet =
  Format.printf
    "/!\\ Warning /!\\: You are about to rewrite all secret keys from wallet \
     %s /!\\\n\n\
     All SECRET KEYS of the wallet will be LOST.\n\n\
     Are you sure you want to do that ? press Y or y to proceed, any other key \
     to cancel.@."
    wallet ;
  String.uppercase_ascii (read_line ()) = "Y"

let usage () =
  Format.printf
    "@[<v>@[<v 4>> convert wallet <source-wallet-dir> in <yes-wallet-dir>@,\
     creates a yes-wallet in <yes-wallet-dir> with the public keys from \
     <source-wallet-dir>@]@,\
     @[<v>@[<v 4>> convert wallet <wallet-dir> inplace@,\
     same as above but overwrite the file in the directory <wallet-dir>@]@,\
     @[<v>@[<v 4>> create minimal in <yes_wallet_dir>@,\
     creates a yes-wallet with the foundation baker keys in <yes_wallet_dir>@]@,\
     @[<v 4>> create from context <base_dir> in <yes_wallet_dir> [%s] [%s \
     <NUM>]@,\
     creates a yes-wallet with all delegates in the head block of the context \
     in <base_dir> and store it in <yes_wallet_dir>@,\
     if %s is used the deactivated bakers are filtered out@,\
     if %s <NUM> is used, the first largest bakers that have an accumulated \
     stake of at least <NUM> percent of the total stake are kept@]@]@,\
     @[<v>if %s is used existing files will be overwritten@]@."
    active_bakers_only_opt_name
    staking_share_opt_name
    active_bakers_only_opt_name
    staking_share_opt_name
    force_opt_name

let () =
  let argv = Array.to_list Sys.argv in
  let staking_share_opt =
    let rec aux argv =
      match argv with
      | [] -> None
      | "--staking-share" :: percentage :: _ ->
          let percentage = Int64.of_string percentage in
          assert (0L < percentage && percentage < 100L) ;
          Some percentage
      | _ :: argv' -> aux argv'
    in
    aux argv
  in
  let (options, argv) =
    List.partition
      (fun arg ->
        (String.length arg > 0 && String.get arg 0 = '-')
        || Str.string_match (Str.regexp "[0-9]+") arg 0)
      argv
  in
  let active_bakers_only =
    List.exists (fun opt -> opt = active_bakers_only_opt_name) options
  in
  force := List.exists (fun opt -> opt = force_opt_name) options ;
  let unknown_options =
    let rec filter args =
      match args with
      | [] -> []
      | opt :: t when opt = active_bakers_only_opt_name -> filter t
      | opt :: t when opt = force_opt_name -> filter t
      | opt :: num :: t
        when opt = staking_share_opt_name
             && Str.string_match (Str.regexp "[0-9]+") num 0 ->
          filter t
      | h :: t -> h :: filter t
    in
    filter options
  in
  if unknown_options <> [] then
    Format.eprintf
      "Warning: unknown options %a@."
      (Format.pp_print_list Format.pp_print_string)
      unknown_options ;
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
      if
        populate_wallet
          ~replace:!force
          yes_wallet_dir
          Yes_wallet_lib.alias_pkh_pk_list
      then Format.printf "Created minimal wallet in %s@." yes_wallet_dir
  | [_; "create"; "from"; "context"; base_dir; "in"; yes_wallet_dir] ->
      let alias_pkh_pk_list =
        Yes_wallet_lib.load_mainnet_bakers_public_keys
          base_dir
          active_bakers_only
          staking_share_opt
      in
      Format.printf
        "@[<h>Number of keys to export:@;<3 0>%d@]@."
        (List.length alias_pkh_pk_list) ;
      if populate_wallet ~replace:!force yes_wallet_dir alias_pkh_pk_list then
        Format.printf "@[<h>Exported path:@;<14 0>%s@]@." yes_wallet_dir
  | [_; "convert"; "wallet"; base_dir; "in"; target_dir] ->
      convert_wallet ~replace:!force base_dir target_dir ;
      Format.printf
        "Wallet %s converted to a yes-wallet in %s@."
        base_dir
        target_dir
  | [_; "convert"; "wallet"; base_dir; "inplace"] ->
      if !force || confirm_rewrite base_dir then (
        convert_wallet ~replace:true base_dir base_dir ;
        Format.printf "Converted wallet in %s@." base_dir)
      else
        Format.printf
          "I refuse to rewrite files in %s without confirmation or --force \
           flag@."
          base_dir
  | _ ->
      Format.eprintf "Invalid command. Usage:@." ;
      usage () ;
      exit 1
