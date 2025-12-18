(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type COMMAND = sig
  type args

  type state

  val name : string

  val parse : args Cli_parser.t

  val run : Printer.t -> state -> args -> unit tzresult Lwt.t
end

type ('a, 'state) t =
  (module COMMAND with type args = 'a and type state = 'state)

type 'state command = Command : ('a, 'state) t -> 'state command

let command (type args state) ~name ~parse ~run : (args, state) t =
  let cmd : (module COMMAND with type args = args and type state = state) =
    (module struct
      type nonrec args = args

      type nonrec state = state

      let name = name

      let parse = parse

      let run = run
    end)
  in
  cmd

let name (type a state)
    (module C : COMMAND with type args = a and type state = state) =
  C.name

let run (type a state) p t
    (module C : COMMAND with type args = a and type state = state) =
  C.run p t

module Cli = struct
  let path =
    let open Cli_parser in
    pos_arg "PATH"
    |> validate (fun p ->
           if String.starts_with ~prefix:"/" p then Ok ()
           else validation_error "%s is not a valid path" p)

  let pp =
    let open Cli_parser in
    let* p = default_long ~default:"hex" "pp" in
    match Pp.of_string p with
    | Some p -> return p
    | _ ->
        fail
          "Unexpected %s, expected one of %a"
          p
          Format.(
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt ", ")
              (fun fmt str -> fprintf fmt "'%s'" str))
          (Pp.supported_pp ())
end

let ls ~subkeys ~inspect =
  command ~name:"ls" ~parse:Cli.path ~run:(fun p tree path ->
      let open Lwt_syntax in
      let* results = subkeys tree path in
      let* () =
        List.iter_s
          (fun l ->
            let* inspect_result =
              inspect tree (Format.sprintf "%s/%s" path l)
            in
            Printer.ln
              p
              "%s%s"
              l
              (if Option.is_none inspect_result then "/" else ""))
          (List.filter (( <> ) "") results)
      in
      return_ok_unit)

let cat ~inspect =
  command
    ~name:"cat"
    ~parse:
      (let open Cli_parser in
       let+ pp = Cli.pp and+ path = Cli.path in
       (path, pp))
    ~run:(fun p tree (path, pp) ->
      let open Lwt_result_syntax in
      let*! inspect_res = inspect tree path in
      match inspect_res with
      | Some v ->
          let*! () = Printer.ln p "%a" (Pp.pp pp) v in
          return_unit
      | None -> return_unit)

let tree ~subkeys =
  command
    ~name:"tree"
    ~parse:
      (let open Cli_parser in
       let+ depth = default_int_positive_long ~default:2 "depth"
       and+ path = Cli.path in
       (path, depth))
    ~run:(fun p tree (path, depth) ->
      let open Lwt_syntax in
      let* () = Printer.ln p "%s" (if path = "" then "." else path) in
      let rec aux prefix current_path current_depth =
        if current_depth <= 0 then return_unit
        else
          let* keys = subkeys tree current_path in
          let rec loop = function
            | [] -> return_unit
            | "" :: ks -> loop ks
            | [k] -> print_entry ~is_last:true k
            | k :: ks ->
                let* () = print_entry ~is_last:false k in
                loop ks
          and print_entry ~is_last k =
            let branch = if is_last then "└── " else "├── " in
            let* () = Printer.ln p "%s%s%s" prefix branch k in
            let next_prefix = prefix ^ if is_last then "    " else "│   " in
            let next_path =
              if current_path = "" then k
              else String.concat "/" [current_path; k]
            in
            aux next_prefix next_path (current_depth - 1)
          in
          loop keys
      in
      let* () = aux "" path depth in
      return_ok_unit)

type eval_read = Eval_read : 'a * ('a -> unit tzresult Lwt.t) -> eval_read

let read_eval (type state) ~args (cmds : state command list) p (tree : state) =
  let open Cli_parser in
  let open Lwt_syntax in
  let parsing_result =
    select
      (List.map
         (fun (Command
                 (module C : COMMAND with type args = _ and type state = state))
            ->
           ( C.name,
             let open Cli_parser in
             let+ a = C.parse in
             Eval_read (a, C.run p tree) ))
         cmds)
    @@ args
  in
  match parsing_result with
  | Ok (Eval_read (x, run), _) -> (
      let* run_result = run x in
      match run_result with
      | Ok () -> return_unit
      | Error err ->
          Printer.errorln p "Something went wrong@.%a" pp_print_trace err)
  | Error errs ->
      let* () = Printer.errorln p "Could not parse input command" in
      List.iter_s (fun err -> Printer.errorln p "- %s" err) errs
