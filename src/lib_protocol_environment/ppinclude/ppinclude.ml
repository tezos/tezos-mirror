let collect_exentions_with_loc s =
  let open Parsetree in
  let l = ref [] in
  let add (loc : Location.t) (e : extension) = l := (loc, e) :: !l in
  let it =
    let super = Ast_iterator.default_iterator in
    let typ it (x : core_type) =
      (match x.ptyp_desc with Ptyp_extension e -> add x.ptyp_loc e | _ -> ()) ;
      super.typ it x
    in
    let pat it (x : pattern) =
      (match x.ppat_desc with Ppat_extension e -> add x.ppat_loc e | _ -> ()) ;
      super.pat it x
    in
    let expr it (x : expression) =
      (match x.pexp_desc with Pexp_extension e -> add x.pexp_loc e | _ -> ()) ;
      super.expr it x
    in
    let class_type it (x : class_type) =
      (match x.pcty_desc with Pcty_extension e -> add x.pcty_loc e | _ -> ()) ;
      super.class_type it x
    in
    let class_type_field it (x : class_type_field) =
      (match x.pctf_desc with Pctf_extension e -> add x.pctf_loc e | _ -> ()) ;
      super.class_type_field it x
    in
    let class_expr it (x : class_expr) =
      (match x.pcl_desc with Pcl_extension e -> add x.pcl_loc e | _ -> ()) ;
      super.class_expr it x
    in
    let class_field it (x : class_field) =
      (match x.pcf_desc with Pcf_extension e -> add x.pcf_loc e | _ -> ()) ;
      super.class_field it x
    in
    let module_type it (x : module_type) =
      (match x.pmty_desc with Pmty_extension e -> add x.pmty_loc e | _ -> ()) ;
      super.module_type it x
    in
    let module_expr it (x : module_expr) =
      (match x.pmod_desc with Pmod_extension e -> add x.pmod_loc e | _ -> ()) ;
      super.module_expr it x
    in
    let signature_item it (x : signature_item) =
      (match x.psig_desc with
      | Psig_extension (e, _) -> add x.psig_loc e
      | _ -> ()) ;
      super.signature_item it x
    in
    let structure_item it (x : structure_item) =
      (match x.pstr_desc with
      | Pstr_extension (e, _) -> add x.pstr_loc e
      | _ -> ()) ;
      super.structure_item it x
    in
    {
      Ast_iterator.default_iterator with
      typ;
      pat;
      expr;
      class_type;
      class_type_field;
      class_expr;
      class_field;
      module_type;
      module_expr;
      signature_item;
      structure_item;
    }
  in
  it.structure it s ;
  !l

let () =
  let input = Sys.argv.(1) in
  let extensions =
    let ic = open_in_bin input in
    let lex = Lexing.from_channel ic in
    let struct_ = Parse.implementation lex in
    let res = collect_exentions_with_loc struct_ in
    close_in ic ;
    res
  in
  let parse_payload ~loc ~name payload =
    match (payload : Parsetree.payload) with
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( {
                    pexp_desc =
                      Pexp_constant
                        {pconst_desc = Pconst_string (file, _, _); _};
                    _;
                  },
                  _ );
            _;
          };
        ] ->
        file
    | _ -> Location.raise_errorf ~loc "invalid payload for %s extension" name
  in
  let extensions =
    List.filter_map
      (fun (loc, (name, payload)) ->
        let name = name.Location.txt in
        match name with
        | "include" -> Some (loc, `Include (parse_payload ~name ~loc payload))
        | "sig" -> Some (loc, `Sig (parse_payload ~name ~loc payload))
        | _ -> None)
      extensions
  in
  let read_file_content n =
    let ic = open_in_bin n in
    let ic_len = in_channel_length ic in
    let s = really_input_string ic ic_len in
    close_in ic ;
    s
  in
  let output_line_directive n f =
    print_string (Printf.sprintf "\n# %d %S\n" n f)
  in
  let output_range s a b = print_string (String.sub s a (b - a)) in
  let output_file_content f =
    match f with
    | `Sig f ->
        print_string "sig" ;
        output_line_directive 1 f ;
        print_string (read_file_content f) ;
        print_string "end"
    | `Include f ->
        output_line_directive 1 f ;
        print_string (read_file_content f)
  in
  let rec loop s pos = function
    | [] -> output_range s pos (String.length s)
    | ((loc : Location.t), file) :: rest ->
        output_range s pos loc.loc_start.pos_cnum ;
        output_file_content file ;
        output_line_directive loc.loc_end.pos_lnum input ;
        loop s loc.loc_end.pos_cnum rest
  in
  let s = read_file_content input in
  print_string "(* This file was automatically generated, do not edit.*)\n" ;
  print_string (Printf.sprintf "(* Edit file %s instead. *)" input) ;
  output_line_directive 1 input ;
  loop s 0 (List.sort compare extensions)
