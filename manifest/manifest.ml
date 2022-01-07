(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module String_map = Map.Make (String)
module String_set = Set.Make (String)

let ( // ) = Filename.concat

(*****************************************************************************)
(*                                  DUNE                                     *)
(*****************************************************************************)

module Ne_list = struct
  (* Non-empty lists. *)
  type 'a t = 'a * 'a list

  let to_list (head, tail) = head :: tail
end

module Dune = struct
  type kind = Library | Executable | Test

  type mode = Byte | Native | JS

  let string_of_mode = function
    | Byte -> "byte"
    | Native -> "native"
    | JS -> "js"

  type s_expr =
    | E
    | S of string
    | G of s_expr
    | H of s_expr
    | V of s_expr
    | []
    | ( :: ) of s_expr * s_expr

  type language = C

  type foreign_stubs = {
    language : language;
    flags : string list;
    names : string list;
  }

  (* Test whether an s-expression is empty. *)
  let rec is_empty = function
    | E -> true
    | S _ -> false
    | G s | H s | V s -> is_empty s
    | [] -> true
    | head :: tail -> is_empty head && is_empty tail

  (* Pretty-print an atom of an s-expression. *)
  let pp_atom fmt atom =
    let rec need_quotes i =
      if i >= String.length atom then false
      else
        (* https://dune.readthedocs.io/en/stable/lexical-conventions.html#atoms
           (\012 is formfeed) *)
        match atom.[i] with
        | ' ' | '\t' | '\n' | '\012' | '(' | ')' | '"' | ';' -> true
        | _ -> need_quotes (i + 1)
    in
    if atom = "" || need_quotes 0 then
      (* https://dune.readthedocs.io/en/stable/lexical-conventions.html#strings
         It appears that %S should work in most cases, except that:
         - the dune documentation does not explicitely say that it supports escaping
           double quotes;
         - the dune documentation says to escape %{ otherwise it is understood as a variable.
         I tested and escaping double quotes actually works (with dune 2.9.0 at least).
         For variables, we actually want to be able to use variables in our atoms,
         so not escaping them is actually useful. In conclusion, %S looks fine. *)
      Format.fprintf fmt "%S" atom
    else Format.pp_print_string fmt atom

  (* Pretty-print an s-expression. *)
  let rec pp_s_expr fmt s_expr =
    (* If [need_space] is [true], a separator is printed before the first atom, if any.
       This is only used inside lists (i.e. the :: constructor) . *)
    let rec pp_s_expr_items need_space fmt = function
      | E -> ()
      | S atom -> pp_atom fmt atom
      | G _ | H _ | V _ ->
          (* See below: [invalid_arg] prevents this from happening. *)
          assert false
      | [] -> ()
      | E :: tail -> pp_s_expr_items need_space fmt tail
      | G s_expr :: tail ->
          if is_empty s_expr then pp_s_expr_items need_space fmt tail
          else (
            if need_space then Format.pp_print_space fmt () ;
            Format.fprintf fmt "@[<hov>%a@]" (pp_s_expr_items false) s_expr ;
            pp_s_expr_items true fmt tail)
      | H s_expr :: tail ->
          if is_empty s_expr then pp_s_expr_items need_space fmt tail
          else (
            if need_space then Format.pp_print_space fmt () ;
            Format.fprintf fmt "@[<h>%a@]" (pp_s_expr_items false) s_expr ;
            pp_s_expr_items true fmt tail)
      | V s_expr :: tail ->
          if is_empty s_expr then pp_s_expr_items need_space fmt tail
          else (
            if need_space then Format.pp_print_space fmt () ;
            Format.fprintf fmt "@[<v>%a@]" (pp_s_expr_items false) s_expr ;
            pp_s_expr_items true fmt tail)
      | head :: tail ->
          if need_space then Format.pp_print_space fmt () ;
          pp_s_expr fmt head ;
          pp_s_expr_items true fmt tail
    in
    match s_expr with
    | E -> ()
    | S atom -> pp_atom fmt atom
    | G _ | H _ | V _ ->
        invalid_arg
          "Dune.pp_sexpr: grouped s-expressions must be inside s-expressions"
    | [] | _ :: _ ->
        Format.fprintf fmt "(@[<hv>%a@])" (pp_s_expr_items false) s_expr

  let pp fmt dune =
    let rec pp is_first_item fmt = function
      | E -> ()
      | S _ -> invalid_arg "Dune.pp: argument must be a list, not an atom"
      | G _ | H _ | V _ ->
          invalid_arg
            "Dune.pp: grouped s-expressions must be inside s-expressions"
      | [] -> ()
      | E :: tail -> pp is_first_item fmt tail
      | head :: tail ->
          if not is_first_item then Format.fprintf fmt "@.@." ;
          Format.fprintf fmt "%a%a" pp_s_expr head (pp false) tail
    in
    pp true fmt dune

  let of_list list =
    List.fold_left (fun acc item -> item :: acc) [] (List.rev list)

  (* Convert a list of atoms (i.e. strings) to an [s_expr]. *)
  let of_atom_list atoms =
    List.fold_left (fun acc item -> S item :: acc) [] (List.rev atoms)

  (* Helper to define s-expressions where parts are optional.

     Basically the same as [Option.bind] except that [None] becomes [E]. *)
  let opt o f = match o with None -> E | Some x -> f x

  let executable_or_library kind ?(public_names = Stdlib.List.[]) ?package
      ?instrumentation ?(libraries = []) ?flags ?(inline_tests = false)
      ?(preprocess = Stdlib.List.[]) ?(preprocessor_deps = Stdlib.List.[])
      ?(virtual_modules = Stdlib.List.[]) ?implements ?(wrapped = true) ?modules
      ?modes ?foreign_stubs ?c_library_flags ?(private_modules = Stdlib.List.[])
      ?(deps = Stdlib.List.[]) ?js_of_ocaml (names : string list) =
    [
      V
        [
          S
            (match (kind, names) with
            | (Library, [_]) -> "library"
            | (Library, _) -> "libraries"
            | (Executable, [_]) -> "executable"
            | (Executable, _) -> "executables"
            | (Test, [_]) -> "test"
            | (Test, _) -> "tests");
          (match names with
          | [name] -> [S "name"; S name]
          | _ -> S "names" :: of_atom_list names);
          (match public_names with
          | [] -> E
          | [name] -> [S "public_name"; S name]
          | _ :: _ -> S "public_names" :: of_atom_list public_names);
          opt package (fun x -> [S "package"; S x]);
          opt implements (fun x -> [S "implements"; S x]);
          opt instrumentation (fun x -> [S "instrumentation"; x]);
          ( opt modes @@ fun x ->
            S "modes"
            :: of_list (List.map (function mode -> S (string_of_mode mode)) x)
          );
          (match libraries with
          | [] -> E
          | _ -> [V (S "libraries" :: libraries)]);
          (if inline_tests then [S "inline_tests"; [S "flags"; S "-verbose"]]
          else E);
          (match preprocess with
          | [] -> E
          | _ :: _ -> S "preprocess" :: of_list preprocess);
          (match preprocessor_deps with
          | [] -> E
          | _ :: _ -> S "preprocessor_deps" :: of_list preprocessor_deps);
          (match js_of_ocaml with
          | None -> E
          | Some flags -> S "js_of_ocaml" :: flags);
          opt flags (fun x -> [S "flags"; x]);
          (if not wrapped then [S "wrapped"; S "false"] else E);
          (match virtual_modules with
          | [] -> E
          | _ -> S "virtual_modules" :: of_atom_list virtual_modules);
          opt modules (fun x -> S "modules" :: x);
          (match private_modules with
          | [] -> E
          | _ -> S "private_modules" :: of_atom_list private_modules);
          ( opt foreign_stubs @@ fun x ->
            [
              S "foreign_stubs";
              [S "language"; (match x.language with C -> S "c")];
              (match x.flags with
              | [] -> E
              | _ -> [S "flags"; of_atom_list x.flags]);
              S "names" :: of_atom_list x.names;
            ] );
          (opt c_library_flags @@ fun x -> [S "c_library_flags"; of_atom_list x]);
          (match deps with [] -> E | _ -> S "deps" :: of_list deps);
        ];
    ]

  let alias ?(deps = Stdlib.List.[]) name =
    [
      S "alias";
      [S "name"; S name];
      (match deps with [] -> E | _ -> S "deps" :: of_atom_list deps);
    ]

  let alias_rule ?(deps = Stdlib.List.[]) ?(alias_deps = Stdlib.List.[])
      ?deps_dune ?action ?locks ?package name =
    let deps =
      match (deps, alias_deps, deps_dune) with
      | (_ :: _, _, Some _) | (_, _ :: _, Some _) ->
          invalid_arg
            "Dune.alias_rule: cannot specify both ~deps_dune and ~deps or \
             ~alias_deps"
      | ([], [], Some deps) -> deps
      | (_, _, None) ->
          List.map (fun x -> S x) deps
          @ List.map (fun x -> [S "alias"; S x]) alias_deps
          |> of_list
    in
    [
      S "rule";
      [S "alias"; S name];
      (opt package @@ fun x -> [S "package"; S x]);
      (match deps with [] -> E | _ -> S "deps" :: deps);
      (opt locks @@ fun locks -> [S "locks"; S locks]);
      [
        S "action";
        (match action with None -> [S "progn"] | Some action -> action);
      ];
    ]

  let run command args = [S "run"; S command; G (of_atom_list args)]

  let run_exe exe_name args = run ("%{exe:" ^ exe_name ^ ".exe}") args

  let runtest_js ~package ~dir ~(node_wrapper_flags : string list) name =
    let rec go_to_src acc dir =
      match dir with
      | "" | "." ->
          (* we are at the root *)
          Filename.concat acc "src"
      | "src" ->
          (* we are in %root%/src *)
          acc
      | _ -> go_to_src (Filename.concat ".." acc) (Filename.dirname dir)
    in
    let runner =
      match node_wrapper_flags with
      | [] -> S "node"
      | _ ->
          let node = go_to_src "tooling/node_wrapper.exe" dir in
          [S ("%{dep:" ^ node ^ "}"); G (of_atom_list node_wrapper_flags)]
    in
    alias_rule
      "runtest_js"
      ~package
      ~action:[S "run"; G runner; S ("%{dep:./" ^ name ^ ".bc.js}")]

  let setenv name value followup = [G [S "setenv"; S name; S value]; followup]

  let chdir_workspace_root followup =
    [G [S "chdir"; S "%{workspace_root}"]; followup]

  let backend name = [S "backend"; S name]

  let ocamllex name = [S "ocamllex"; S name]

  let pps ?(args = Stdlib.List.[]) name = S "pps" :: S name :: of_atom_list args

  let include_ name = [S "include"; S name]

  let file name = [S "file"; S name]
end

(*****************************************************************************)
(*                                  OPAM                                     *)
(*****************************************************************************)

module Opam = struct
  type version = string

  type version_constraint =
    | Same_as_current_package
    | Exactly of version
    | At_least of version
    | Less_than of version
    | At_most of version
    | Not of version

  type version_constraints = version_constraint list

  type dependency = {
    package : string;
    version : version_constraints;
    with_test : bool;
    optional : bool;
  }

  type command_item = A of string | S of string

  type build_instruction = {command : command_item list; with_test : bool}

  type url = {url : string; sha256 : string; sha512 : string}

  type t = {
    maintainer : string;
    authors : string list;
    homepage : string;
    bug_reports : string;
    dev_repo : string;
    license : string;
    depends : dependency list;
    conflicts : dependency list;
    build : build_instruction list;
    synopsis : string;
    url : url option;
  }

  let pp fmt
      {
        maintainer;
        authors;
        homepage;
        bug_reports;
        dev_repo;
        license;
        depends;
        conflicts;
        build;
        synopsis;
        url;
      } =
    let (depopts, depends) = List.partition (fun dep -> dep.optional) depends in
    let pp_line x =
      Format.kfprintf (fun fmt -> Format.pp_print_newline fmt ()) fmt x
    in
    let pp_string fmt string =
      (* https://opam.ocaml.org/doc/Manual.html#General-syntax
         <string> ::= ( (") { <char> }* (") ) | ( (""") { <char> }* (""") )
         (and there is no definition for <char> so let's assume it is "any byte").
         In other words: if there is no double quote, we can just put the whole
         string in double quotes. Otherwise, we need to use three double quotes.
         If the string itself contains three consecutive double quotes, it seems
         that we are doomed. *)
      let len = String.length string in
      let rec check_quotes default i =
        if i >= len then default
        else if string.[i] <> '"' then
          (* Continue looking for quotes. *)
          check_quotes default (i + 1)
        else if i + 2 >= len then
          (* Found a quote, and there is no space for two more quotes. *)
          `use_triple_quotes
        else if string.[i + 1] = '"' then
          (* Found two consecutive quotes. *)
          if string.[i + 2] = '"' then `doomed
          else
            (* Not three consecutive quotes: continue looking for quotes.
               If we don't find anything, the default is now to use three double quotes. *)
            check_quotes `use_triple_quotes (i + 3)
        else
          (* Not three consecutive quotes: continue looking for quotes. *)
          check_quotes `use_triple_quotes (i + 2)
      in
      match check_quotes `use_regular_quotes 0 with
      | `use_regular_quotes -> Format.fprintf fmt "\"%s\"" string
      | `use_triple_quotes -> Format.fprintf fmt "\"\"\"%s\"\"\"" string
      | `doomed ->
          invalid_arg
            "Cannot use strings with three consecutive double-quotes in opam \
             strings."
    in
    let pp_version = pp_string in
    let pp_list ?(v = false) ?(prefix = "") pp_item fmt = function
      | [] -> Format.fprintf fmt "%s[]" prefix
      | list ->
          let pp_sep_out =
            if v then Format.pp_force_newline else Format.pp_print_cut
          in
          let pp_sep_in =
            if v then Format.pp_force_newline else Format.pp_print_space
          in
          Format.fprintf
            fmt
            "@[@[<hv 2>%s[%a%a@]%a]@]"
            prefix
            pp_sep_out
            ()
            (Format.pp_print_list ~pp_sep:pp_sep_in pp_item)
            list
            pp_sep_out
            ()
    in
    let pp_version_constraint fmt = function
      | Same_as_current_package -> Format.fprintf fmt "= version"
      | Exactly version -> Format.fprintf fmt "= %a" pp_version version
      | At_least version -> Format.fprintf fmt ">= %a" pp_version version
      | Less_than version -> Format.fprintf fmt "< %a" pp_version version
      | At_most version -> Format.fprintf fmt "<= %a" pp_version version
      | Not version -> Format.fprintf fmt "!= %a" pp_version version
    in
    let pp_version_constraints fmt list =
      Format.pp_print_list pp_version_constraint fmt list ~pp_sep:(fun fmt () ->
          Format.pp_print_string fmt " & ")
    in
    let pp_dependency fmt {package; version; with_test; _} =
      match (version, with_test) with
      | ([], false) -> pp_string fmt package
      | ([], true) -> Format.fprintf fmt "@[%a {with-test}@]" pp_string package
      | (_ :: _, false) ->
          Format.fprintf
            fmt
            "@[%a { %a }@]"
            pp_string
            package
            pp_version_constraints
            version
      | (_ :: _, true) ->
          Format.fprintf
            fmt
            "@[%a { with-test & %a }@]"
            pp_string
            package
            pp_version_constraints
            version
    in
    let pp_command_item fmt = function
      | A atom -> Format.pp_print_string fmt atom
      | S string -> pp_string fmt string
    in
    let pp_build_instruction fmt {command; with_test} =
      Format.fprintf
        fmt
        "%a%s"
        (pp_list pp_command_item)
        command
        (if with_test then " {with-test}" else "")
    in
    let pp_url {url; sha256; sha512} =
      pp_line "url {" ;
      pp_line "  src: \"%s\"" url ;
      pp_line "  checksum: [" ;
      pp_line "    \"sha256=%s\"" sha256 ;
      pp_line "    \"sha512=%s\"" sha512 ;
      pp_line "  ]" ;
      pp_line "}"
    in
    pp_line "opam-version: \"2.0\"" ;
    pp_line "maintainer: %a" pp_string maintainer ;
    pp_line "%a" (pp_list ~prefix:"authors: " pp_string) authors ;
    pp_line "homepage: %a" pp_string homepage ;
    pp_line "bug-reports: %a" pp_string bug_reports ;
    pp_line "dev-repo: %a" pp_string dev_repo ;
    pp_line "license: %a" pp_string license ;
    pp_line "%a" (pp_list ~v:true ~prefix:"depends: " pp_dependency) depends ;
    if depopts <> [] then
      pp_line "%a" (pp_list ~v:true ~prefix:"depopts: " pp_dependency) depopts ;
    if conflicts <> [] then
      pp_line
        "%a"
        (pp_list ~v:true ~prefix:"conflicts: " pp_dependency)
        conflicts ;
    pp_line "%a" (pp_list ~prefix:"build: " pp_build_instruction) build ;
    pp_line "synopsis: %a" pp_string synopsis ;
    Option.iter pp_url url
end

(*****************************************************************************)
(*                                 TARGETS                                   *)
(*****************************************************************************)

module Target = struct
  let invalid_argf x = Printf.ksprintf invalid_arg x

  type external_ = {
    name : string;
    opam : string option;
    version : Opam.version_constraints;
    js_compatible : bool;
  }

  type vendored = {name : string; js_compatible : bool}

  type opam_only = {name : string; version : Opam.version_constraints}

  type modules =
    | All
    | Modules of string list
    | All_modules_except of string list

  (* [internal_name] is the name for [name] stanzas in [dune] while [public_name] is the
     name for [public_name] stanzas in [dune] and the name in [.opam] files. *)
  type full_name = {internal_name : string; public_name : string}

  type kind =
    | Public_library of full_name
    | Private_library of string
    | Public_executable of full_name Ne_list.t
    | Private_executable of string Ne_list.t
    | Test of string Ne_list.t
    | Test_executable of string Ne_list.t

  type preprocessor_dep = File of string

  type internal = {
    bisect_ppx : bool;
    c_library_flags : string list option;
    conflicts : target list;
    dep_files : string list;
    deps : target list;
    dune : Dune.s_expr;
    foreign_stubs : Dune.foreign_stubs option;
    implements : target option;
    inline_tests : bool;
    js_compatible : bool;
    js_of_ocaml : Dune.s_expr option;
    kind : kind;
    linkall : bool;
    modes : Dune.mode list option;
    modules : modules;
    nopervasives : bool;
    ocaml : Opam.version_constraints option;
    opam : string option;
    opaque : bool;
    opens : string list;
    path : string;
    preprocess : preprocessor list;
    preprocessor_deps : preprocessor_dep list;
    private_modules : string list;
    opam_only_deps : target list;
    release : bool;
    static : bool;
    static_cclibs : string list;
    synopsis : string option;
    virtual_modules : string list;
    wrapped : bool;
    node_wrapper_flags : string list;
  }

  and preprocessor = PPS of target | PPS_args of target * string list

  and select = {
    package : target;
    source_if_present : string;
    source_if_absent : string;
    target : string;
  }

  and target =
    | Internal of internal
    | Vendored of vendored
    | External of external_
    | Opam_only of opam_only
    | Optional of target
    | Select of select

  let convert_to_identifier = String.map @@ function '-' | '.' -> '_' | c -> c

  (* List of all targets, in reverse order of registration. *)
  let registered = ref []

  (* List of targets sorted by path,
     so that we can create dune files with multiple targets. *)
  let by_path = ref String_map.empty

  (* List of targets sorted by opam package name,
     so that we can create opam files with multiple targets. *)
  let by_opam = ref String_map.empty

  let register_internal ({path; opam; _} as internal) =
    let old = String_map.find_opt path !by_path |> Option.value ~default:[] in
    by_path := String_map.add path (internal :: old) !by_path ;
    Option.iter
      (fun opam ->
        let old =
          String_map.find_opt opam !by_opam |> Option.value ~default:[]
        in
        by_opam := String_map.add opam (internal :: old) !by_opam)
      opam ;
    registered := internal :: !registered ;
    Internal internal

  let rec name_for_errors = function
    | Vendored {name; _} | External {name; _} | Opam_only {name; _} -> name
    | Optional target | Select {package = target; _} -> name_for_errors target
    | Internal {kind; _} -> (
        match kind with
        | Public_library {public_name = name; _}
        | Private_library name
        | Public_executable ({public_name = name; _}, _)
        | Private_executable (name, _)
        | Test (name, _)
        | Test_executable (name, _) ->
            name)

  let rec names_for_dune = function
    | Vendored {name; _} | External {name; _} | Opam_only {name; _} -> (name, [])
    | Optional target | Select {package = target; _} -> names_for_dune target
    | Internal {kind; _} -> (
        match kind with
        | Public_library {public_name; _} -> (public_name, [])
        | Private_library internal_name -> (internal_name, [])
        | Public_executable (head, tail) ->
            (head.public_name, List.map (fun x -> x.public_name) tail)
        | Private_executable names | Test names | Test_executable names -> names
        )

  let rec library_name_for_dune = function
    | Vendored {name; _} | External {name; _} | Opam_only {name; _} -> Ok name
    | Optional target | Select {package = target; _} ->
        library_name_for_dune target
    | Internal {kind; _} -> (
        match kind with
        | Public_library {public_name; _} -> Ok public_name
        | Private_library internal_name -> Ok internal_name
        | Public_executable ({public_name = name; _}, _)
        | Private_executable (name, _)
        | Test (name, _)
        | Test_executable (name, _) ->
            Error name)

  let iter_internal_by_path f =
    String_map.iter (fun path internals -> f path (List.rev internals)) !by_path

  let iter_internal_by_opam f =
    String_map.iter (fun opam internals -> f opam (List.rev internals)) !by_opam

  type 'a maker =
    ?all_modules_except:string list ->
    ?bisect_ppx:bool ->
    ?c_library_flags:string list ->
    ?conflicts:target list ->
    ?dep_files:string list ->
    ?deps:target list ->
    ?dune:Dune.s_expr ->
    ?foreign_stubs:Dune.foreign_stubs ->
    ?implements:target ->
    ?inline_tests:bool ->
    ?js_compatible:bool ->
    ?js_of_ocaml:Dune.s_expr ->
    ?linkall:bool ->
    ?modes:Dune.mode list ->
    ?modules:string list ->
    ?node_wrapper_flags:string list ->
    ?nopervasives:bool ->
    ?ocaml:Opam.version_constraints ->
    ?opam:string ->
    ?opaque:bool ->
    ?opens:string list ->
    ?preprocess:preprocessor list ->
    ?preprocessor_deps:preprocessor_dep list ->
    ?private_modules:string list ->
    ?opam_only_deps:target list ->
    ?release:bool ->
    ?static:bool ->
    ?static_cclibs:string list ->
    ?synopsis:string ->
    ?virtual_modules:string list ->
    ?wrapped:bool ->
    path:string ->
    'a ->
    target

  let internal make_kind ?all_modules_except ?bisect_ppx ?c_library_flags
      ?(conflicts = []) ?(dep_files = []) ?(deps = []) ?(dune = Dune.[])
      ?foreign_stubs ?implements ?(inline_tests = false) ?js_compatible
      ?js_of_ocaml ?(linkall = false) ?modes ?modules ?(node_wrapper_flags = [])
      ?(nopervasives = false) ?ocaml ?opam ?(opaque = false) ?(opens = [])
      ?(preprocess = []) ?(preprocessor_deps = []) ?(private_modules = [])
      ?(opam_only_deps = []) ?release ?static ?static_cclibs ?synopsis
      ?(virtual_modules = []) ?(wrapped = true) ~path names =
    let (js_compatible, js_of_ocaml) =
      match (js_compatible, js_of_ocaml) with
      | (Some false, Some _) ->
          invalid_arg
            "Target.internal: cannot specify both `~js_compatible:false` and \
             `~js_of_ocaml`"
      | (Some true, Some jsoo) -> (true, Some jsoo)
      | (Some true, None) -> (true, Some Dune.[])
      | (None, Some jsoo) -> (true, Some jsoo)
      | (Some false, None) | (None, None) -> (false, None)
    in
    let kind = make_kind names in
    let opam =
      match opam with
      | Some "" -> None
      | Some _ as x -> x
      | None -> (
          match kind with
          | Public_library {public_name; _}
          | Public_executable ({public_name; _}, []) ->
              Some (path // public_name)
          | Public_executable ({public_name; _}, _ :: _) ->
              invalid_argf
                "for targets which provide more than one public executables \
                 such as %S, you must specify ~opam (set it to \"\" for no \
                 opam file)"
                public_name
          | Private_library name ->
              invalid_argf
                "for targets which provide private libraries such as %S, you \
                 must specify ~opam (set it to \"\" for no opam file)"
                name
          | Private_executable (name, _) ->
              invalid_argf
                "for targets which provide private executables such as %S, you \
                 must specify ~opam (set it to \"\" for no opam file)"
                name
          | Test (name, _) ->
              invalid_argf
                "for targets which provide tests such as %S, you must specify \
                 ~opam (set it to \"\" for no opam file)"
                name
          | Test_executable (name, _) ->
              invalid_argf
                "for targets which provide test executables such as %S, you \
                 must specify ~opam (set it to \"\" for no opam file)"
                name)
    in
    let release =
      match release with
      | Some release -> release
      | None -> ( match kind with Public_executable _ -> true | _ -> false)
    in
    let static =
      match static with
      | Some static -> static
      | None -> (
          match static_cclibs with
          | Some _ -> true
          | None -> (
              match kind with Public_executable _ -> true | _ -> false))
    in
    let static_cclibs = Option.value static_cclibs ~default:[] in
    let modules =
      match (modules, all_modules_except) with
      | (None, None) -> All
      | (Some modules, None) -> Modules modules
      | (None, Some all_modules_except) -> All_modules_except all_modules_except
      | (Some _, Some _) ->
          invalid_arg
            "Target.internal: cannot specify both ?modules and \
             ?all_modules_except"
    in
    let not_a_test =
      match kind with
      | Public_library _ | Private_library _ | Public_executable _
      | Private_executable _ ->
          true
      | Test _ | Test_executable _ -> false
    in
    let bisect_ppx = Option.value bisect_ppx ~default:not_a_test in
    let runtest_js_rules =
      match (kind, opam, js_compatible) with
      | (Test names, Some package, true)
      | (Test_executable names, Some package, true) ->
          let collect_node_wrapper_flags deps =
            let rec loop (seen, acc) dep =
              match library_name_for_dune dep with
              | Error _ -> (seen, acc)
              | Ok name -> (
                  if String_set.mem name seen then (seen, acc)
                  else
                    let seen = String_set.add name seen in
                    match dep with
                    | Internal {deps; node_wrapper_flags; _} ->
                        let acc = node_wrapper_flags @ acc in
                        loops (seen, acc) deps
                    | _ -> (seen, acc))
            and loops (seen, acc) deps =
              List.fold_left
                (fun (seen, acc) x -> loop (seen, acc) x)
                (seen, acc)
                deps
            in
            loops (String_set.empty, []) deps
          in
          let node_wrapper_flags : string list =
            snd (collect_node_wrapper_flags deps)
          in
          List.map
            (fun name ->
              Dune.(
                runtest_js
                  ~package:(Filename.basename package)
                  ~node_wrapper_flags
                  ~dir:path
                  name))
            (Ne_list.to_list names)
      | _ -> []
    in
    let dune =
      List.fold_right (fun x dune -> Dune.(x :: dune)) runtest_js_rules dune
    in
    register_internal
      {
        bisect_ppx;
        c_library_flags;
        conflicts;
        dep_files;
        deps;
        dune;
        foreign_stubs;
        implements;
        inline_tests;
        js_compatible;
        js_of_ocaml;
        kind;
        linkall;
        modes;
        modules;
        nopervasives;
        ocaml;
        opam;
        opaque;
        opens;
        path;
        preprocess;
        preprocessor_deps;
        private_modules;
        opam_only_deps;
        release;
        static;
        static_cclibs;
        synopsis;
        virtual_modules;
        node_wrapper_flags;
        wrapped;
      }

  let public_lib ?internal_name =
    internal @@ fun public_name ->
    let internal_name =
      Option.value internal_name ~default:(convert_to_identifier public_name)
    in
    Public_library {internal_name; public_name}

  let private_lib = internal @@ fun name -> Private_library name

  let public_exe ?internal_name =
    internal @@ fun public_name ->
    let internal_name =
      Option.value internal_name ~default:(convert_to_identifier public_name)
    in
    Public_executable ({internal_name; public_name}, [])

  let public_exes ?internal_names =
    internal @@ fun public_names ->
    let names =
      match internal_names with
      | None ->
          List.map
            (fun public_name ->
              {internal_name = convert_to_identifier public_name; public_name})
            public_names
      | Some internal_names -> (
          try
            List.map2
              (fun internal_name public_name -> {internal_name; public_name})
              internal_names
              public_names
          with Invalid_argument _ ->
            invalid_argf
              "Target.public_exes: you must specify exactly one internal name \
               per public name")
    in
    match names with
    | [] -> invalid_argf "Target.public_exes: at least one name must be given"
    | head :: tail -> Public_executable (head, tail)

  let private_exe =
    internal @@ fun internal_name -> Private_executable (internal_name, [])

  let private_exes =
    internal @@ fun internal_names ->
    match internal_names with
    | [] -> invalid_argf "Target.private_exes: at least one name must be given"
    | head :: tail -> Private_executable (head, tail)

  let test = internal @@ fun test_name -> Test (test_name, [])

  let tests =
    internal @@ fun test_names ->
    match test_names with
    | [] -> invalid_arg "Target.tests: at least one name must be given"
    | head :: tail -> Test (head, tail)

  let test_exe = internal @@ fun test_name -> Test_executable (test_name, [])

  let test_exes =
    internal @@ fun test_names ->
    match test_names with
    | [] -> invalid_arg "Target.test_exes: at least one name must be given"
    | head :: tail -> Test_executable (head, tail)

  let vendored_lib ?(js_compatible = false) name =
    Vendored {name; js_compatible}

  let external_lib ?opam ?(js_compatible = false) name version =
    let opam =
      match opam with None -> Some name | Some "" -> None | Some _ as x -> x
    in
    External {name; opam; version; js_compatible}

  let external_sublib ?(js_compatible = false) parent name =
    match parent with
    | External {opam; version; _} ->
        External {name; opam; version; js_compatible}
    | Opam_only _ ->
        invalid_arg
          "Target.external_sublib: parent must be a non-opam-only external lib"
    | Internal _ | Vendored _ ->
        invalid_arg "Target.external_sublib: parent must be an external lib"
    | Optional _ ->
        invalid_arg
          "Target.external_sublib: Optional should be used in dependency \
           lists, not when registering"
    | Select _ ->
        invalid_arg
          "Target.external_sublib: Select should be used in dependency lists, \
           not when registering"

  let opam_only name version = Opam_only {name; version}

  let optional target = Optional target

  let select ~package ~source_if_present ~source_if_absent ~target =
    Select {package; source_if_present; source_if_absent; target}
end

type release = {version : string; url : Opam.url}

(*****************************************************************************)
(*                                GENERATOR                                  *)
(*****************************************************************************)

let has_prefix ~prefix string =
  let prefix_len = String.length prefix in
  String.length string >= prefix_len && String.sub string 0 prefix_len = prefix

(* Gather the list of generated files so that we can find out whether
   there are other files that we should have generated. *)
let generated_files = ref String_set.empty

let rec create_parent path =
  let parent = Filename.dirname path in
  if String.length parent < String.length path then (
    create_parent parent ;
    if not (Sys.file_exists parent) then Sys.mkdir parent 0o755)

(* Write a file relatively to the root directory of the repository. *)
let write filename f =
  let filename = Filename.parent_dir_name // filename in
  if String_set.mem filename !generated_files then
    failwith
      (filename ^ " is generated twice; did you declare the same library twice?") ;
  generated_files := String_set.add filename !generated_files ;
  create_parent filename ;
  let outch = open_out filename in
  match f (Format.formatter_of_out_channel outch) with
  | exception exn ->
      close_out outch ;
      raise exn
  | x ->
      close_out outch ;
      x

let generate_dune ~dune_file_has_static_profile (internal : Target.internal) =
  let (libraries, empty_files_to_create) =
    let empty_files_to_create = ref [] in
    let get_library (dep : Target.target) =
      let name =
        match Target.library_name_for_dune dep with
        | Ok name -> name
        | Error name ->
            invalid_arg
              ("unsupported: in "
              ^ Target.name_for_errors (Internal internal)
              ^ ": dependency on a target that is not a library (" ^ name ^ ")"
              )
      in
      match dep with
      | Opam_only _ -> Dune.E
      | Internal _ | External _ | Vendored _ -> Dune.S name
      | Optional _ ->
          (* [Optional] dependencies abuse the alternative dependency mechanism of dune.
             The semantic of [(select a from (b -> c) (-> d))] is: if libraries
             [b] are present, [cp c a] and link [b] else [cp d a]. Here, we don't
             care about the cp part as we are not using the file obtained at
             all. So, we give them names only meant to not clash with anything
             and copy always the same (generated itself) empty file "void_for_linking". *)
          let fix_name = String.map @@ function '.' -> '-' | c -> c in
          let void_name = "void_for_linking-" ^ fix_name name in
          let empty_name = void_name ^ ".empty" in
          empty_files_to_create := empty_name :: !empty_files_to_create ;
          Dune.
            [
              H [S "select"; S void_name; S "from"];
              [H [S name; S "->"; S empty_name]];
              [H [S "->"; S empty_name]];
            ]
      | Select {package = _; source_if_present; source_if_absent; target} ->
          Dune.
            [
              G [S "select"; S target; S "from"];
              [G [S name; S "->"; S source_if_present]];
              [G [S "->"; S source_if_absent]];
            ]
    in
    let libraries = List.map get_library internal.deps |> Dune.of_list in
    (libraries, List.rev !empty_files_to_create)
  in
  let flags = List.map (fun m -> Dune.(G [S "-open"; S m])) internal.opens in
  let flags = if internal.linkall then Dune.S "-linkall" :: flags else flags in
  let flags =
    if internal.nopervasives then Dune.S "-nopervasives" :: flags else flags
  in
  let flags = if internal.opaque then Dune.S "-opaque" :: flags else flags in
  let flags =
    if dune_file_has_static_profile && not internal.static then
      (* Disable static compilation for this particular target
         (the static profile is global for the dune file).
         This must be at the end of the flag list. *)
      flags @ [Dune.(G [S "\\"; S "-ccopt"; S "-static"])]
    else flags
  in
  let flags =
    match flags with
    | [] -> None
    | _ :: _ -> Some (Dune.of_list (Dune.S ":standard" :: flags))
  in
  let preprocess =
    let make_pp (preprocessor : Target.preprocessor) =
      let pps target args =
        match Target.names_for_dune target with
        | (name, []) -> Dune.pps ~args name
        | (hd, (_ :: _ as tl)) ->
            invalid_arg
              ("preprocessor target has multiple names, don't know which one \
                to choose: "
              ^ String.concat ", " (hd :: tl))
      in
      match preprocessor with
      | PPS target -> pps target []
      | PPS_args (target, args) -> pps target args
    in
    List.map make_pp internal.preprocess
  in
  let preprocessor_deps =
    let make_pp_dep (Target.File filename) = Dune.file filename in
    List.map make_pp_dep internal.preprocessor_deps
  in
  let modules =
    match internal.modules with
    | All -> None
    | Modules modules -> Some (Dune.of_atom_list modules)
    | All_modules_except modules ->
        Some Dune.[S ":standard" :: S "\\" :: Dune.of_atom_list modules]
  in
  let create_empty_files =
    match empty_files_to_create with
    | [] -> Dune.E
    | _ :: _ ->
        let make_write empty_file =
          (* We use H instead of G because we *really* need those to be on
             a single line until we update link_protocol.sh. *)
          Dune.[H [S "write-file"; S empty_file; S ""]]
        in
        let writes =
          List.map make_write empty_files_to_create |> Dune.of_list
        in
        Dune.[S "rule"; [S "action"; S "progn" :: writes]]
  in
  let package =
    match (internal.kind, internal.opam) with
    | ((Public_executable _ | Test _), Some opam) ->
        Some (Filename.basename opam)
    | _ -> None
  in
  let instrumentation =
    if internal.bisect_ppx then Some (Dune.backend "bisect_ppx") else None
  in
  let ((kind : Dune.kind), internal_names, public_names) =
    let get_internal_name {Target.internal_name; _} = internal_name in
    let get_public_name {Target.public_name; _} = public_name in
    match internal.kind with
    | Public_library name ->
        (Library, [get_internal_name name], [get_public_name name])
    | Private_library name -> (Library, [name], [])
    | Public_executable (head, tail) ->
        ( Executable,
          List.map get_internal_name (head :: tail),
          List.map get_public_name (head :: tail) )
    | Private_executable (head, tail) -> (Executable, head :: tail, [])
    | Test (head, tail) -> (Test, head :: tail, [])
    | Test_executable (head, tail) -> (Executable, head :: tail, [])
  in
  let get_virtual_target_name target =
    match Target.library_name_for_dune target with
    | Ok name -> name
    | Error name ->
        invalid_arg
          ("unsupported: ~implements on a target that is not a library (" ^ name
         ^ ")")
  in
  Dune.(
    executable_or_library
      kind
      internal_names
      ~public_names
      ?package
      ?instrumentation
      ~libraries
      ?flags
      ~inline_tests:internal.inline_tests
      ~preprocess
      ~preprocessor_deps
      ~virtual_modules:internal.virtual_modules
      ?implements:(Option.map get_virtual_target_name internal.implements)
      ~wrapped:internal.wrapped
      ?modules
      ?modes:internal.modes
      ?foreign_stubs:internal.foreign_stubs
      ?c_library_flags:internal.c_library_flags
      ~private_modules:internal.private_modules
      ~deps:(List.map Dune.file internal.dep_files)
      ?js_of_ocaml:internal.js_of_ocaml
    :: create_empty_files :: internal.dune)

let static_profile (cclibs : string list) =
  Dune.
    [
      S "static";
      [
        S "flags";
        [
          S ":standard";
          G [S "-ccopt"; S "-static"];
          (match cclibs with
          | [] -> E
          | _ :: _ ->
              let arg =
                List.map (fun lib -> "-l" ^ lib) cclibs |> String.concat " "
              in
              G [S "-cclib"; S arg]);
        ];
      ];
    ]

(* Remove duplicates from a list.
   Items that are not removed are kept in their original order.
   In case of duplicates, the first occurrence is kept.
   [get_key] returns the comparison key (a string).
   [merge] is used in case a key is present several times. *)
let deduplicate_list ?merge get_key list =
  let add ((list, set) as acc) item =
    let key = get_key item in
    if String_set.mem key set then
      match merge with
      | None -> acc
      | Some merge ->
          (* Go back and merge the previous occurrence. *)
          let merge_if_equal previous_item =
            if String.compare (get_key previous_item) key = 0 then
              merge previous_item item
            else previous_item
          in
          let list = List.map merge_if_equal list in
          (list, set)
    else (item :: list, String_set.add key set)
  in
  List.fold_left add ([], String_set.empty) list |> fst |> List.rev

let generate_dune_files () =
  Target.iter_internal_by_path @@ fun path internals ->
  let has_static =
    List.exists (fun (internal : Target.internal) -> internal.static) internals
  in
  let dunes =
    List.map (generate_dune ~dune_file_has_static_profile:has_static) internals
  in
  write (path // "dune") @@ fun fmt ->
  Format.fprintf
    fmt
    "; This file was automatically generated, do not edit.@.; Edit file \
     manifest/main.ml instead.@.@." ;
  let env =
    if has_static then
      let cclibs =
        List.map
          (fun (internal : Target.internal) -> internal.static_cclibs)
          internals
        |> List.flatten |> deduplicate_list Fun.id
      in
      [static_profile cclibs]
    else []
  in
  (match env with
  | [] -> ()
  | _ :: _ ->
      let env_stanza = Dune.[S "env" :: of_list env] in
      Format.fprintf fmt "%a@." Dune.pp env_stanza) ;
  List.iteri
    (fun i dune ->
      if i <> 0 || has_static then Format.fprintf fmt "@." ;
      Format.fprintf fmt "%a@." Dune.pp dune)
    dunes

(* Convert [target] into an opam dependency so that it can be added as a dependency
   in packages that depend on it.

   [for_package] is the name of the opam package that we are generating
   and in which the dependency will be added.
   If it is the same package as the one in which [target] belongs,
   [None] is returned, since a package cannot depend on itself
   and there is no need to.

   If [fix_version] is [true], require [target]'s version to be
   exactly the same as [for_package]'s version, but only if [target] is internal. *)
let rec as_opam_dependency ~fix_version ~(for_package : string) ~with_test
    (target : Target.target) : Opam.dependency option =
  match target with
  | Internal {opam = None; _} | External {opam = None; _} -> None
  | Internal {opam = Some package; _} ->
      let package = Filename.basename package in
      if package = for_package then None
      else
        let version =
          if fix_version then [Opam.Same_as_current_package] else []
        in
        Some {Opam.package; version; with_test; optional = false}
  | Vendored {name = package; _} ->
      Some {Opam.package; version = []; with_test; optional = false}
  | External {opam = Some opam; version; _} | Opam_only {name = opam; version}
    ->
      Some {Opam.package = opam; version; with_test; optional = false}
  | Optional target | Select {package = target; _} ->
      Option.map
        (fun (dep : Opam.dependency) -> {dep with optional = true})
        (as_opam_dependency ~fix_version ~for_package ~with_test target)

let generate_opam ?release this_package (internals : Target.internal list) :
    Opam.t =
  let for_package = Filename.basename this_package in
  let map l f = List.map f l in
  let depends =
    List.flatten @@ map internals
    @@ fun internal ->
    let with_test =
      match internal.kind with Test _ | Test_executable _ -> true | _ -> false
    in
    let deps =
      Option.to_list internal.implements
      @ internal.deps @ internal.opam_only_deps
    in
    let deps =
      List.filter_map
        (as_opam_dependency
           ~fix_version:(release <> None)
           ~for_package
           ~with_test)
        deps
    in
    let get_preprocess_dep (Target.PPS target | PPS_args (target, _)) =
      as_opam_dependency
        ~fix_version:(release <> None)
        ~for_package
        ~with_test
        target
    in
    List.filter_map get_preprocess_dep internal.preprocess @ deps
  in
  let depends =
    match
      List.filter_map
        (fun (internal : Target.internal) -> internal.ocaml)
        internals
    with
    | [] -> depends
    | versions ->
        {
          Opam.package = "ocaml";
          version = List.flatten versions;
          with_test = false;
          optional = false;
        }
        :: depends
  in
  let depends =
    {
      Opam.package = "dune";
      version = [At_least "2.9"];
      with_test = false;
      optional = false;
    }
    :: depends
  in
  let depends =
    (* Remove duplicate dependencies but when one occurs twice,
       only keep {with-test} if both dependencies had it. *)
    let merge (a : Opam.dependency) (b : Opam.dependency) =
      {a with with_test = a.with_test && b.with_test}
    in
    deduplicate_list ~merge (fun {Opam.package; _} -> package) depends
  in
  let conflicts =
    List.flatten @@ map internals
    @@ fun internal ->
    List.filter_map
      (as_opam_dependency ~fix_version:false ~for_package ~with_test:false)
      internal.conflicts
  in
  let synopsis =
    String.concat " " @@ List.flatten @@ map internals
    @@ fun internal -> Option.to_list internal.synopsis
  in
  let build =
    let build : Opam.build_instruction =
      {
        command = [S "dune"; S "build"; S "-p"; A "name"; S "-j"; A "jobs"];
        with_test = false;
      }
    in
    let runtest : Opam.build_instruction =
      {
        command = [S "dune"; S "runtest"; S "-p"; A "name"; S "-j"; A "jobs"];
        with_test = true;
      }
    in
    match release with
    | None -> [build; runtest]
    | Some _ ->
        [
          {Opam.command = [S "rm"; S "-r"; S "vendors"]; with_test = false};
          build;
          {
            Opam.command =
              [
                S "mv";
                S (Filename.dirname this_package ^ "/%{name}%.install");
                S "./";
              ];
            with_test = false;
          };
          runtest;
        ]
  in
  {
    maintainer = "contact@tezos.com";
    authors = ["Tezos devteam"];
    homepage = "https://www.tezos.com/";
    bug_reports = "https://gitlab.com/tezos/tezos/issues";
    dev_repo = "git+https://gitlab.com/tezos/tezos.git";
    license = "MIT";
    depends;
    conflicts;
    build;
    synopsis;
    url = Option.map (fun {url; _} -> url) release;
  }

let generate_opam_files () =
  (* Note:
     - dune files are not always in the same directory of their corresponding .opam
       (usually the .opam is not in a subdir though);
     - there can be multiple .opam in the same directory, for a single dune file
       (or even multiple dune files when some of those dune are in subdirectories).
     So each [Target.t] comes with an [opam] path, which defaults to being in the
     same directory as the dune file, with the package as filename (suffixed with .opam),
     but one can specify a custom .opam path too. *)
  Target.iter_internal_by_opam @@ fun package internals ->
  let opam = generate_opam package internals in
  write (package ^ ".opam") @@ fun fmt ->
  Format.fprintf
    fmt
    "# This file was automatically generated, do not edit.@.# Edit file \
     manifest/main.ml instead.@.%a"
    Opam.pp
    opam

let generate_opam_files_for_release release =
  Target.iter_internal_by_opam @@ fun package internal_pkgs ->
  let package_name = Filename.basename package in
  let opam_filename =
    "packages" // package_name
    // (package_name ^ "." ^ release.version)
    // "opam"
  in
  let opam = generate_opam ~release package internal_pkgs in
  write opam_filename @@ fun fmt -> Opam.pp fmt opam

let check_for_non_generated_files ?(exclude = fun _ -> false) () =
  let rec find_opam_and_dune_files acc dir =
    let dir_contents = Sys.readdir dir in
    let add_item acc filename =
      let full_filename = dir // filename in
      if try Sys.is_directory full_filename with Sys_error _ -> false then
        find_opam_and_dune_files acc full_filename
      else if filename = "dune" || Filename.extension filename = ".opam" then
        String_set.add full_filename acc
      else acc
    in
    Array.fold_left add_item acc dir_contents
  in
  let all_files = find_opam_and_dune_files String_set.empty "../src" in
  let diff = String_set.diff all_files !generated_files in
  let error = ref false in
  let ignore_or_fail filename =
    if not (exclude filename) then (
      Printf.eprintf "Error: %s: exists but was not generated\n%!" filename ;
      error := true)
  in
  String_set.iter ignore_or_fail diff ;
  if !error then (
    prerr_endline
      "Please modify manifest/main.ml to generate the above file(s)\n\
       or declare them in the 'exclude' function." ;
    exit 1)

let check_js_of_ocaml () =
  let internal_name ({kind; path; _} : Target.internal) =
    match kind with
    | Public_library {public_name; _} -> public_name
    | Private_library internal_name -> internal_name
    | Public_executable ({public_name = name; _}, _) -> name
    | Private_executable (name, _) | Test (name, _) | Test_executable (name, _)
      ->
        Filename.concat path name
  in
  let missing_from_target = ref String_map.empty in
  let missing_with_js_mode = ref String_set.empty in
  let missing_jsoo_for_target ~used_by:internal target =
    let name = internal_name internal in
    let old =
      match String_map.find_opt name !missing_from_target with
      | None -> []
      | Some x -> x
    in
    missing_from_target :=
      String_map.add name (target :: old) !missing_from_target
  in
  let missing_jsoo_with_js_mode name =
    missing_with_js_mode := String_set.add name !missing_with_js_mode
  in
  let rec check_target ~used_by (target : Target.target) =
    match target with
    | External {js_compatible; name; _} ->
        if not js_compatible then missing_jsoo_for_target ~used_by name
    | Vendored {js_compatible; name} ->
        if not js_compatible then missing_jsoo_for_target ~used_by name
    | Internal ({js_compatible; _} as internal) ->
        if not js_compatible then
          missing_jsoo_for_target ~used_by (internal_name internal)
    | Optional internal -> check_target ~used_by internal
    | Select {package; _} -> check_target ~used_by package
    | Opam_only _ -> (* irrelevent to this check *) ()
  in
  let check_internal (internal : Target.internal) =
    if internal.js_compatible then
      List.iter (check_target ~used_by:internal) internal.deps
    else
      match internal.modes with
      | Some modes ->
          if List.mem Dune.JS modes then
            missing_jsoo_with_js_mode (internal_name internal)
      | _ -> ()
  in
  Target.iter_internal_by_path (fun _path internals ->
      List.iter check_internal internals) ;
  let jsoo_ok = ref true in
  if String_set.cardinal !missing_with_js_mode > 0 then (
    jsoo_ok := false ;
    Printf.eprintf
      "The following targets use `(modes js)` and are missing \
       `~js_compatible:true`\n" ;
    String_set.iter
      (fun name -> Printf.eprintf "- %s\n" name)
      !missing_with_js_mode) ;
  if String_map.cardinal !missing_from_target > 0 then (
    jsoo_ok := false ;
    Printf.eprintf
      "The following targets are not `~js_compatible` but their dependant \
       expect them to be\n" ;
    String_map.iter
      (fun k v -> List.iter (fun v -> Printf.eprintf "- %s used by %s\n" v k) v)
      !missing_from_target) ;
  if not !jsoo_ok then exit 1

let usage_msg = "Usage: " ^ Sys.executable_name ^ " [OPTIONS]"

let release =
  let url = ref "" in
  let sha256 = ref "" in
  let sha512 = ref "" in
  let version = ref "" in
  let anon_fun _args = () in
  let spec =
    Arg.align
      [
        ("--url", Arg.Set_string url, "<URL> Set url for release");
        ("--sha256", Arg.Set_string sha256, "<HASH> Set sha256 for release");
        ("--sha512", Arg.Set_string sha512, "<HASH> Set sha512 for release");
        ( "--release",
          Arg.Set_string version,
          "<VERSION> Generate opam files for release instead, for VERSION" );
      ]
  in
  Arg.parse spec anon_fun usage_msg ;
  match (!url, !sha256, !sha512, !version) with
  | ("", "", "", "") -> None
  | ("", _, _, _) | (_, "", _, _) | (_, _, "", _) | (_, _, _, "") ->
      prerr_endline
        "Error: either all of --url, --sha256, --sha512 and --release must be \
         specified, or none of them." ;
      exit 1
  | (url, sha256, sha512, version) ->
      Some {version; url = {url; sha256; sha512}}

let generate ?exclude () =
  Printexc.record_backtrace true ;
  try
    generate_dune_files () ;
    generate_opam_files () ;
    check_for_non_generated_files ?exclude () ;
    check_js_of_ocaml () ;
    Option.iter generate_opam_files_for_release release
  with exn ->
    Printexc.print_backtrace stderr ;
    prerr_endline ("Error: " ^ Printexc.to_string exn) ;
    exit 1

include Target
