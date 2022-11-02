(* This file is used to list hacl-star(-raw) compilation units that declare "raw" bindings.
   The heuristic used here is:
   - The module has a name ending with "_bindings" and
   - containts a submodule "Bindings"
*)

let files = Sys.argv |> Array.to_list |> List.tl

(* gets the list of cmi corresponding to a cma/cmxa *)
let parse_cma file : string list =
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
        (fun (cmo : Cmo_format.compilation_unit) -> cmo.cu_name)
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
  List.map
    (fun unit_name ->
      match find_unit unit_name with
      | None ->
          failwith
            (Printf.sprintf
               "cannot find %S in %S"
               (String.uncapitalize_ascii unit_name ^ ".cmi")
               path)
      | Some p -> p)
    units

let get_cmis () = List.concat_map parse_cma files

(* Returns whether a compilation unit contains a given submodule *)
let has_submodule (cmi : Cmi_format.cmi_infos) m =
  List.exists
    (function
      | Types.Sig_module (ident, _, _, _, _) -> Ident.name ident = m
      | _ -> false)
    cmi.cmi_sign

(* OCaml code generation sent to stdout *)
let _ =
  Printf.printf
    {|
(* This file was automatically generated, do not edit. *)
(* Edit file src/lib_hacl/gen/gen0.ml instead. *)

module type FOREIGN = Ctypes.FOREIGN

module type FOREIGN' = FOREIGN with type 'a result = unit

module type S = functor (F : FOREIGN') -> sig end

let all : (module S) list = [
|} ;
  let () =
    get_cmis ()
    |> List.filter_map (fun cmi ->
           if String.ends_with ~suffix:"_bindings.cmi" cmi then
             let name = Filename.chop_suffix (Filename.basename cmi) ".cmi" in
             let cmi = Cmi_format.read_cmi cmi in
             if has_submodule cmi "Bindings" then Some name else None
           else None)
    |> List.iter (fun name -> Printf.printf "  (module %s.Bindings);\n" name)
  in
  Printf.printf "]\n"
