open Version

let () =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "--major" -> print_endline (string_of_int current.Version.major)
  | "--minor" -> print_endline (string_of_int current.minor)
  | "--additional-info" ->
      print_endline (string_of_additional_info current.additional_info)
  | "--full" | "" -> print_endline current_string
  | _ ->
      prerr_endline ("invalid argument: " ^ arg) ;
      exit 1
