(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open React

let prompt = LTerm_text.(eval [B_bold true; S "> "; B_bold false])

class read_line cmds ~term ~history =
  let commands = List.map (fun (Commands.Command c) -> Commands.name c) cmds in
  object (self)
    inherit LTerm_read_line.read_line ~history ()

    inherit [Zed_string.t] LTerm_read_line.term term

    method! completion =
      let prefix = Zed_rope.to_string self#input_prev |> Zed_string.to_utf8 in

      let pos, completions =
        match
          String.split ' ' prefix |> List.filter (fun s -> String.trim s <> "")
        with
        | [] ->
            ( 0,
              List.map
                (fun c -> (Zed_string.of_utf8 c, Zed_string.of_utf8 " "))
                commands )
        | [pre_cmd] ->
            ( 0,
              List.filter_map
                (fun c ->
                  if String.starts_with ~prefix:pre_cmd c then
                    Some (Zed_string.of_utf8 c, Zed_string.of_utf8 " ")
                  else None)
                commands )
        | _ -> (0, [])
      in

      self#set_completion pos completions

    initializer self#set_prompt (S.const prompt)
  end

let loop cmds tree term =
  let printer = Printer.lterm_printer term in
  let rec loop history =
    let open Lwt_result_syntax in
    let*! exec_result =
      let*! line = (new read_line ~term ~history cmds)#run in
      protect @@ fun () ->
      let input = Zed_string.to_utf8 line in
      let*! () =
        Commands.read_eval ~args:(String.split ' ' input) cmds printer tree
      in
      return line
    in
    match exec_result with
    | Ok line -> loop (line :: history)
    | Error _ -> loop history
  in
  loop []
