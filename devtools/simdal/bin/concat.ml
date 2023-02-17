let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | [_] -> ()
  | _ :: target :: files ->
      List.iter
        (fun file ->
          match Dat.append ~into:target file with
          | None ->
              Format.eprintf "concat: could not append %s into %s@." file target ;
              exit 1
          | Some () -> ())
        files
