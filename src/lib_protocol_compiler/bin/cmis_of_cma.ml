let files = Sys.argv |> Array.to_list |> List.tl

let parse_cma file =
  let units =
    let c = open_in_bin file in
    let buffer =
      really_input_string c (String.length Config.cma_magic_number)
    in
    if buffer = Config.cma_magic_number then (
      let toc_pos = input_binary_int c in
      seek_in c toc_pos ;
      let cma = (input_value c : Cmo_format.library) in
      close_in c ;
      List.map
        Octez_protocol_compiler_compat.Compiler_libs.compunit_name
        cma.lib_units)
    else if buffer = Config.cmxa_magic_number then (
      let cmxa = (input_value c : Cmx_format.library_infos) in
      close_in c ;
      List.map
        (fun ((cmx : Cmx_format.unit_infos), _) -> cmx.ui_name)
        cmxa.lib_units)
    else (
      close_in c ;
      failwith "unsupported file format, cma file expected.")
  in
  let path = Filename.dirname file in
  let find_unit name =
    List.find_map
      (fun fname ->
        let fname = Filename.concat path fname in
        if Sys.file_exists fname then Some fname else None)
      [
        String.capitalize_ascii name ^ ".cmi";
        String.uncapitalize_ascii name ^ ".cmi";
      ]
  in
  List.iter
    (fun unit_name ->
      match find_unit unit_name with
      | None ->
          failwith
            (Printf.sprintf
               "cannot find %S in %S"
               (String.uncapitalize_ascii unit_name ^ ".cmi")
               path)
      | Some p -> Printf.printf "%s\n" p)
    units

let () = List.iter parse_cma files
