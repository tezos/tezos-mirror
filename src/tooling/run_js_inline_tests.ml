module Sexp = Sexplib0.Sexp

let () = Sys.catch_break true

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic ;
  content

let write_file file ~content =
  let oc = open_out_bin file in
  output_string oc content ;
  close_out oc

let rec find_dune_files root acc =
  let dune = Filename.concat root "dune" in
  let acc =
    try if Sys.file_exists dune then dune :: acc else acc with _ -> acc
  in
  let files = Array.to_list (Sys.readdir root) in
  List.fold_left
    (fun acc file ->
      match file.[0] with
      | '.' | '_' -> acc
      | _ -> (
          let fullname = Filename.concat root file in
          try
            if Sys.is_directory fullname then find_dune_files fullname acc
            else acc
          with _ -> acc))
    acc
    files

let parse_file file = Parsexp.Many.parse_string_exn (read_file file)

let has_inline_tests fields =
  List.exists
    (function Sexp.List (Sexp.Atom "inline_tests" :: _) -> true | _ -> false)
    fields

let has_jsoo fields =
  List.exists
    (function Sexp.List (Sexp.Atom "js_of_ocaml" :: _) -> true | _ -> false)
    fields

let add_inline_tests_mode_js fields =
  List.map
    (function
      | Sexp.List (Sexp.Atom "inline_tests" :: p) ->
          Sexp.List
            (Sexp.Atom "inline_tests"
             ::
             Sexp.List [Sexp.Atom "modes"; Sexp.Atom "js"]
             ::
             List.filter
               (function
                 | Sexp.List (Sexp.Atom "modes" :: _) -> false | _ -> true)
               p)
      | x -> x)
    fields

let patch_inline_tests_for_js (l : Sexp.t list) : Sexp.t list option =
  let found = ref false in
  let l =
    List.map
      (function
        | Sexp.List (Sexp.Atom "library" :: fields) as x ->
            if has_inline_tests fields && has_jsoo fields then (
              found := true ;
              Sexp.List (Sexp.Atom "library" :: add_inline_tests_mode_js fields))
            else x
        | x -> x)
      l
  in
  if !found then Some l else None

let restore l =
  Printf.printf "\n" ;
  List.iter
    (fun (f, _, content) ->
      Printf.printf "Restoring dune file: %s\n%!" f ;
      write_file f ~content)
    l

let usage () = Printf.printf "%s [PATH1 [PATH2]]\n" Sys.executable_name

let () =
  let args =
    match Array.to_list Sys.argv with [] | [_] -> ["./"] | _ :: l -> l
  in
  List.iter
    (function
      | "-help" | "--help" | "" ->
          usage () ;
          exit 0
      | x when x.[0] = '-' ->
          usage () ;
          exit 1
      | _ -> ())
    args ;
  Printf.printf "Collect dune files\n" ;
  let files = List.fold_left (fun acc x -> find_dune_files x acc) [] args in
  let files = List.sort_uniq compare files in
  let files =
    List.filter_map
      (fun filename ->
        try
          let dune = parse_file filename in
          match patch_inline_tests_for_js dune with
          | None -> None
          | Some dune -> Some (filename, dune)
        with _ ->
          Printf.printf "Failed to parse %s, ignoring\n" filename ;
          None)
      files
  in
  (match files with
  | [] ->
      Printf.printf "nothing to run, exiting !\n" ;
      exit 0
  | _ -> ()) ;
  let files_with_backup =
    List.map
      (fun (filename, dune_patched) ->
        (filename, dune_patched, read_file filename))
      files
  in
  at_exit (fun () -> restore files_with_backup) ;
  List.iter
    (fun (filename, dune_patched, backup) ->
      let content =
        String.concat "\n" (List.map Sexp.to_string_hum dune_patched)
      in
      let commented_backup =
        String.split_on_char '\n' backup
        |> List.map (fun x -> ";; " ^ x)
        |> String.concat "\n"
      in
      let content =
        Printf.sprintf
          ";; TODO: REVERT CHANGES TO THIS FILE BEFORE COMMITING\n\
           ;; This dune file was temporarily modified by %s\n\
           ;; and will be restored once tests are done running.\n\n\
           %s\n\n\
           %s\n"
          Sys.executable_name
          content
          commented_backup
      in
      Printf.printf "Patching %s\n%!" filename ;
      write_file filename ~content)
    files_with_backup ;
  let cmd =
    "dune build "
    ^ String.concat
        " "
        (List.map
           (fun (f, _) -> Printf.sprintf "@@%s/runtest" (Filename.dirname f))
           files)
  in
  Printf.printf "About to run: %s\n\n%!" cmd ;
  flush_all () ;
  match Unix.system cmd with
  | WEXITED x -> exit x
  | WSIGNALED _ -> exit 1
  | WSTOPPED _ -> exit 1
