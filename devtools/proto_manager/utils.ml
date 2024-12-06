(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Unix = UnixLabels

module Infix = struct
  let ( // ) = Filename.concat
end

let str_of_fmt fmt = Format.kasprintf Re.str fmt

open Infix

module Process = struct
  let check_status ~error status cmd =
    match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED status ->
        Log.error "%s failed with %d" cmd status ;
        error ()
    | _ ->
        Log.error "%s stopped or killed by a signal" cmd ;
        error ()

  let input_all_check_status ~error in_chan cmd =
    Fun.protect
      ~finally:(fun () ->
        let status = Unix.close_process_in in_chan in
        check_status ~error status cmd)
      (fun () -> In_channel.input_all in_chan)

  let exec ~error ~dir ~bin ~args =
    let in_chan =
      Unix.open_process_args_in (dir // bin) (Array.of_list (bin :: args))
    in
    input_all_check_status ~error in_chan bin

  let shell ~error shell =
    let in_chan = Unix.open_process_in shell in
    input_all_check_status ~error in_chan shell

  let stdout_by_line output =
    if output = "" then [] else String.split_on_char '\n' (String.trim output)
end

module File = struct
  let find ~error ~dir ~opts =
    let output = Process.shell ~error (Format.asprintf "find %s %s" dir opts) in
    Process.stdout_by_line output

  module Content = struct
    let input_all file =
      let in_channel = open_in file in
      Fun.protect
        ~finally:(fun () -> In_channel.close in_channel)
        (fun () -> In_channel.input_all in_channel)

    let search ~regex file =
      let content = input_all file in
      Re.all regex content

    let write out_channel content =
      Fun.protect
        ~finally:(fun () -> Out_channel.close out_channel)
        (fun () ->
          Format.fprintf
            (Format.formatter_of_out_channel out_channel)
            "%s@?"
            content)

    let append ~content file =
      let out_channel = open_out_gen [Open_text; Open_append] 0o700 file in
      write out_channel content

    let overwrite ~content file =
      let out_channel = open_out file in
      write out_channel content

    let replace_string ~regex ~by file =
      let content = input_all file in
      let count = Re.all regex content in
      let count = List.length count in
      let content = Re.replace_string regex ~by content in
      overwrite ~content file ;
      count

    let replace_f ~regex ~f file =
      let content = input_all file in
      let count = ref 0 in
      let f gp =
        count := !count + 1 ;
        f gp
      in
      let content = Re.replace regex ~f content in
      overwrite ~content file ;
      !count

    let replace_assoc ~error ~regex ~assoc =
      let f gp =
        let replacements =
          List.filter_map
            (fun (name, i) ->
              match Re.Group.get_opt gp i with
              | None -> None
              | Some _ -> (
                  match List.assoc_opt name assoc with
                  | None ->
                      Log.error "Unknown group %s" name ;
                      error () ;
                      Stdlib.exit 1
                  | replacement -> replacement))
            (Re.group_names regex)
        in
        match replacements with
        | [r] -> r
        | [] ->
            Log.error "No replacement found" ;
            error () ;
            Stdlib.exit 1
        | _ ->
            Log.error "Multiple replacements found" ;
            error () ;
            Stdlib.exit 1
      in
      replace_f ~regex ~f

    type warn = Msg of string | Loc of string

    let check_modif_count ~warn f file =
      if f file = 0 then
        match warn with
        | Msg msg -> Log.warning "%s" msg
        | Loc loc -> Log.warning "%s: no modification done in file %s" loc file

    let replace_string_all ~regex ~by files =
      List.map (replace_string ~regex ~by) files |> List.fold_left ( + ) 0

    let replace_f_all ~regex ~f files =
      List.map (replace_f ~regex ~f) files |> List.fold_left ( + ) 0

    let replace_assoc_all ~error ~regex ~assoc files =
      List.map (replace_assoc ~error ~regex ~assoc) files
      |> List.fold_left ( + ) 0

    let check_modif_count_all ~warn f files =
      if f files = 0 then
        match warn with
        | Msg msg -> Log.warning "%s" msg
        | Loc loc ->
            Log.warning
              "%s: no modification done in files %a"
              loc
              Format.(
                pp_print_list
                  ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                  pp_print_string)
              files

    let ocamlformat ~error files =
      let _ =
        Process.shell
          ~error
          (Format.asprintf
             "ocamlformat -i %a"
             Format.(
               pp_print_list
                 ~pp_sep:(fun fmt () -> pp_print_string fmt " ")
                 pp_print_string)
             files)
      in
      ()
  end
end
