#!ocaml

(* Ensure that this script can be run from the root directory. *)
#directory "src/lib_version"

(* Ensure that this script can be run from the scripts directory. *)
#directory "../src/lib_version"

#use "version.ml"


let v ?(patch=0) ?(extra="") major minor = (major, minor, patch, extra)

let parse_version s =
  try
    try
      Scanf.sscanf s "v%d.%d.%d-%s" (fun major minor patch extra ->
          v ~patch ~extra major minor)
    with End_of_file | Scanf.Scan_failure _ -> (
      try
        Scanf.sscanf s "v%d.%d-%s" (fun major minor extra -> v ~extra major minor)
      with End_of_file | Scanf.Scan_failure _ -> (
        try
          Scanf.sscanf s "v%d.%d.%d" (fun major minor patch ->
              v ~patch major minor)
        with End_of_file | Scanf.Scan_failure _ ->
          Scanf.sscanf s "v%d.%d" (fun major minor -> v major minor)))
  with End_of_file | Scanf.Scan_failure _ -> v ~extra:"dev" 0 0

let parse_extra s =
  try Scanf.sscanf s "rc%d" (fun d -> ("rc", d))
  with
  | End_of_file -> ("",0)
  | Scanf.Scan_failure _ -> ("dev",0)

let parse s =
    let (major, minor, patch, extra) = parse_version s in
    (major, minor, patch, parse_extra extra)

let f current = function
    | "--major" ->
        print_endline (string_of_int current.major)
    | "--minor" ->
        print_endline (string_of_int current.minor)
    | "--additional-info" ->
        print_endline (string_of_additional_info current.additional_info)
    | "--full" | "" ->
        print_endline (to_string current)
    | arg ->
        prerr_endline ("invalid argument: " ^ arg);
        exit 1

let () =
    if Array.length Sys.argv = 3 then
      let tag = make_current (parse Sys.argv.(1)) in
      let arg = Sys.argv.(2) in
      f tag arg
    else if Array.length Sys.argv = 2 then
      let tag = make_current (parse Sys.argv.(1)) in
      f tag ""
    else
      (prerr_endline ("invalid arguments");
      exit 1)
