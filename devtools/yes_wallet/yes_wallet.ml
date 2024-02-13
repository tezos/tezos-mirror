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

(* We need to exit Lwt + tzResult context from Yes_wallet. *)
let run_load_bakers_public_keys ?staking_share_opt ?network_opt ?level base_dir
    ~active_bakers_only alias_pkh_pk_list =
  let open Yes_wallet_lib in
  let open Tezos_error_monad in
  match
    Lwt_main.run
      (load_bakers_public_keys
         ?staking_share_opt
         ?network_opt
         ?level
         base_dir
         ~active_bakers_only
         alias_pkh_pk_list)
  with
  | Ok alias_pkh_pk_list -> alias_pkh_pk_list
  | Error trace ->
      Format.eprintf "error:@.%a@." Error_monad.pp_print_trace trace ;
      exit 1

let run_build_yes_wallet ?staking_share_opt ?network_opt base_dir
    ~active_bakers_only ~aliases =
  let open Yes_wallet_lib in
  let open Tezos_error_monad in
  match
    Lwt_main.run
      (build_yes_wallet
         ?staking_share_opt
         ?network_opt
         base_dir
         ~active_bakers_only
         ~aliases)
  with
  | Ok alias_pkh_pk_list -> alias_pkh_pk_list
  | Error trace ->
      Format.eprintf "error:@.%a@." Error_monad.pp_print_trace trace ;
      exit 1

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
  let pkh_filename = Filename.concat yes_wallet_dir "public_key_hashs" in
  let pk_filename = Filename.concat yes_wallet_dir "public_keys" in
  let sk_filename = Filename.concat yes_wallet_dir "secret_keys" in
  if not (Sys.file_exists yes_wallet_dir) then Unix.mkdir yes_wallet_dir 0o750 ;
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
    Yes_wallet_lib.write_yes_wallet yes_wallet_dir alias_pkh_pk_list ;
    true)

let alias_file_opt_name = "--aliases"

let alias_file_extension = ".json"

let active_bakers_only_opt_name = "--active-bakers-only"

let force_opt_name = "--force"

let staking_share_opt_name = "--staking-share"

let network_opt_name = "--network"

let level_opt_name = "--level"

let supported_network =
  List.map fst Octez_node_config.Config_file.builtin_blockchain_networks

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
     @[<v 4>> create from context <base_dir> in <yes_wallet_dir> [%s] [%s \
     <NUM>] [%s <%a>]@,\
     creates a yes-wallet with all delegates in the head block of the context \
     in <base_dir> and store it in <yes_wallet_dir>@,\
     if %s is used the deactivated bakers are filtered out@,\
     if %s <NUM> is used, the first largest bakers that have an accumulated \
     stake of at least <NUM> percent of the total stake are kept@,\
     if %s <%a> is used the store is opened using the right genesis parameter \
     (default is mainnet) @]@]@,\
     @[<v 4>> dump staking balances from <base_dir> in <csv_file>]@,\
     saves the staking balances of all delegates in the target csv file@,\
     @[<v>if %s <FILE> is used, it will input aliases from an .json file.See \
     README.md for the spec of this file and how to generate it.@],@[<v>if %s \
     is used existing files will be overwritten@]@."
    active_bakers_only_opt_name
    staking_share_opt_name
    network_opt_name
    Format.(
      pp_print_list
        ~pp_sep:(fun ppf () -> pp_print_string ppf "|")
        pp_print_string)
    supported_network
    active_bakers_only_opt_name
    staking_share_opt_name
    network_opt_name
    Format.(
      pp_print_list
        ~pp_sep:(fun ppf () -> pp_print_string ppf "|")
        pp_print_string)
    supported_network
    alias_file_opt_name
    force_opt_name

let () =
  let argv = Array.to_list Sys.argv in
  let staking_share_opt =
    let rec aux argv =
      match argv with
      | [] -> None
      | str :: percentage :: _ when str = staking_share_opt_name ->
          let percentage = Int64.of_string percentage in
          assert (0L < percentage && percentage <= 100L) ;
          Some percentage
      | _ :: argv' -> aux argv'
    in
    aux argv
  in
  let network_opt =
    let rec aux argv =
      match argv with
      | [] -> None
      | str :: net :: _ when str = network_opt_name -> Some net
      | _ :: argv' -> aux argv'
    in
    aux argv
  in
  let level_opt =
    let rec aux argv =
      match argv with
      | [] -> None
      | str :: level :: _ when str = level_opt_name ->
          let level = Int32.of_string level in
          Some level
      | _ :: argv' -> aux argv'
    in
    aux argv
  in

  (* Take an alias file as input. *)
  let alias_file_opt =
    let rec aux argv =
      match argv with
      | [] -> None
      | str :: file :: _ when str = alias_file_opt_name ->
          if Sys.file_exists file then Some file
          else (
            Format.eprintf
              "Warning: %a doesn't point to an existing alias file.\n"
              Format.pp_print_string
              file ;
            None)
      | _ :: argv' -> aux argv'
    in
    aux argv
  in
  let options, argv =
    List.partition
      (fun arg ->
        (String.length arg > 0 && String.get arg 0 = '-')
        || Str.string_match (Str.regexp "[0-9]+") arg 0
        (* FIME this is an uggly hack, but hey -lets' force alias files
           to have a .json extension.*)
        || String.ends_with ~suffix:alias_file_extension arg
        || List.mem (String.lowercase_ascii arg) supported_network)
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
      | opt :: num :: t
        when opt = level_opt_name
             && Str.string_match (Str.regexp "[0-9]+") num 0 ->
          filter t
      | opt :: file :: t
        when opt = alias_file_opt_name
             && String.ends_with ~suffix:alias_file_extension file ->
          filter t
      | opt :: net :: t
        when opt = network_opt_name
             && List.mem (String.lowercase_ascii net) supported_network ->
          filter t
      | h :: t -> h :: filter t
    in
    filter options
  in
  (* load alias file *)
  let aliases =
    match alias_file_opt with
    | None -> []
    | Some file -> Yes_wallet_lib.load_alias_file file
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
  | [_; "create"; "from"; "context"; base_dir; "in"; yes_wallet_dir] ->
      if not (Sys.file_exists base_dir) then (
        Format.eprintf "Invalid --data-dir provided: %s.\n" base_dir ;
        exit 1)
      else
        let yes_alias_list =
          run_build_yes_wallet
            ~staking_share_opt
            ?network_opt
            base_dir
            ~active_bakers_only
            ~aliases
        in
        Format.printf
          "@[<h>Number of keys to export:@;<3 0>%d@]@."
          (List.length yes_alias_list) ;
        if populate_wallet ~replace:!force yes_wallet_dir yes_alias_list then
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
  | [_; "dump"; "staking"; "balances"; "from"; base_dir; "in"; csv_file] ->
      let alias_pkh_pk_list =
        run_load_bakers_public_keys
          ~staking_share_opt
          ?network_opt
          ?level:level_opt
          base_dir
          ~active_bakers_only
          aliases
      in
      let flags =
        if !force then [Open_wronly; Open_creat; Open_trunc; Open_text]
        else [Open_wronly; Open_creat; Open_excl; Open_text]
      in
      Out_channel.with_open_gen flags 0o666 csv_file (fun oc ->
          let fmtr = Format.formatter_of_out_channel oc in
          List.iter
            (fun (_alias, pkh, _pk, stake) ->
              Format.fprintf fmtr "%s, %Ld\n" pkh stake)
            alias_pkh_pk_list)
  | _ ->
      Format.eprintf "Invalid command. Usage:@." ;
      usage () ;
      exit 1
