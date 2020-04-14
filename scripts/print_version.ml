#!ocaml

(* Ensure that this script can be run from the root directory. *)
#directory "src/lib_version"

(* Ensure that this script can be run from the scripts directory. *)
#directory "../src/lib_version"

#use "version.ml"

let () =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
    | "--major" ->
        print_endline (string_of_int current.major)
    | "--minor" ->
        print_endline (string_of_int current.minor)
    | "--additional-info" ->
        print_endline (string_of_additional_info current.additional_info)
    | "--full" | "" ->
        print_endline current_string
    | _ ->
        prerr_endline ("invalid argument: " ^ arg);
        exit 1
