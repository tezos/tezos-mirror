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

let list_iter l f = List.iter f l

let ( // ) = Filename.concat

let has_error = ref false

let info fmt = Format.eprintf fmt

let error fmt =
  Format.ksprintf
    (fun s ->
      has_error := true ;
      Format.eprintf "Error: %s" s)
    fmt

let pp_do_not_edit ~comment_start fmt () =
  Format.fprintf
    fmt
    "%s This file was automatically generated, do not edit.@.%s Edit file \
     manifest/main.ml instead.@."
    comment_start
    comment_start

let sanitize_path x = Filename.(dirname x // Filename.basename x)

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

(*****************************************************************************)
(*                                  DUNE                                     *)
(*****************************************************************************)

module Ne_list = struct
  (* Non-empty lists. *)
  type 'a t = 'a * 'a list

  let to_list (head, tail) = head :: tail
end

module Dune = struct
  type kind = Library | Executable

  type mode = Byte | Native | JS

  let string_of_mode = function
    | Byte -> "byte"
    | Native -> "native"
    | JS -> "js"

  type ppx_kind = Ppx_rewriter | Ppx_deriver

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
    flags : s_expr;
    names : string list;
  }

  let dand s1 s2 = S "and" :: s1 :: s2

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
      | S atom ->
          if need_space then Format.pp_print_space fmt () ;
          pp_atom fmt atom
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
      ?(instrumentation = Stdlib.List.[]) ?(libraries = []) ?flags
      ?library_flags ?link_flags ?(inline_tests = false)
      ?(inline_tests_deps = Stdlib.List.[]) ?(optional = false) ?ppx_kind
      ?(ppx_runtime_libraries = []) ?(preprocess = Stdlib.List.[])
      ?(preprocessor_deps = Stdlib.List.[]) ?(virtual_modules = Stdlib.List.[])
      ?default_implementation ?implements ?modules
      ?modules_without_implementation ?modes
      ?(foreign_archives = Stdlib.List.[]) ?foreign_stubs ?c_library_flags
      ?(ctypes = E) ?(private_modules = Stdlib.List.[]) ?js_of_ocaml
      (names : string list) =
    [
      V
        [
          S
            (match (kind, names) with
            | Library, [_] -> "library"
            | Library, _ -> "libraries"
            | Executable, [_] -> "executable"
            | Executable, _ -> "executables");
          (match names with
          | [name] -> [S "name"; S name]
          | _ -> S "names" :: of_atom_list names);
          (match public_names with
          | [] -> E
          | [name] -> [S "public_name"; S name]
          | _ :: _ -> S "public_names" :: of_atom_list public_names);
          opt package (fun x -> [S "package"; S x]);
          opt implements (fun x -> [S "implements"; S x]);
          (match instrumentation with
          | [] -> E
          | _ ->
              G
                (of_list
                @@ List.map (fun x -> [S "instrumentation"; x]) instrumentation
                ));
          ( opt modes @@ fun x ->
            S "modes"
            :: of_list (List.map (function mode -> S (string_of_mode mode)) x)
          );
          (if optional then [S "optional"] else E);
          (match libraries with
          | [] -> E
          | _ -> [V (S "libraries" :: libraries)]);
          (if inline_tests then
           let modes : mode list =
             match (modes, js_of_ocaml) with
             | None, None ->
                 (* Make the default dune behavior explicit *)
                 [Native]
             | None, Some _ -> [Native; JS]
             | Some modes, _ ->
                 (* always preserve mode if specified *)
                 modes
           in
           [
             S "inline_tests";
             [S "flags"; S "-verbose"];
             S "modes"
             :: of_list (List.map (fun mode -> S (string_of_mode mode)) modes);
             (match inline_tests_deps with
             | [] -> E
             | deps -> S "deps" :: of_list deps);
           ]
          else E);
          (match ppx_kind with
          | None -> E
          | Some Ppx_rewriter -> [S "kind"; S "ppx_rewriter"]
          | Some Ppx_deriver -> [S "kind"; S "ppx_deriver"]);
          (match ppx_runtime_libraries with
          | [] -> E
          | _ -> [V (S "ppx_runtime_libraries" :: ppx_runtime_libraries)]);
          (match preprocess with
          | [] -> E
          | _ :: _ -> S "preprocess" :: of_list preprocess);
          (match preprocessor_deps with
          | [] -> E
          | _ :: _ -> S "preprocessor_deps" :: of_list preprocessor_deps);
          (match js_of_ocaml with
          | None -> E
          | Some flags -> S "js_of_ocaml" :: flags);
          opt library_flags (fun x -> [S "library_flags"; x]);
          opt link_flags (fun l -> [V (of_list (List.cons (S "link_flags") l))]);
          opt flags (fun l -> [V (of_list (List.cons (S "flags") l))]);
          (match virtual_modules with
          | [] -> E
          | _ -> S "virtual_modules" :: of_atom_list virtual_modules);
          opt default_implementation (fun x ->
              [S "default_implementation"; S x]);
          opt modules (fun x -> S "modules" :: x);
          opt modules_without_implementation (fun x ->
              S "modules_without_implementation" :: x);
          (match private_modules with
          | [] -> E
          | _ -> S "private_modules" :: of_atom_list private_modules);
          (match foreign_archives with
          | [] -> E
          | _ -> S "foreign_archives" :: of_atom_list foreign_archives);
          ( opt foreign_stubs @@ fun x ->
            [
              S "foreign_stubs";
              [S "language"; (match x.language with C -> S "c")];
              (match x.flags with [] -> E | _ -> [S "flags"; x.flags]);
              S "names" :: of_atom_list x.names;
            ] );
          (opt c_library_flags @@ fun x -> [S "c_library_flags"; of_atom_list x]);
          ctypes;
        ];
    ]

  let alias ?(deps = Stdlib.List.[]) name =
    [
      S "alias";
      [S "name"; S name];
      (match deps with [] -> E | _ -> S "deps" :: of_atom_list deps);
    ]

  let alias_rule ?(deps = Stdlib.List.[]) ?(alias_deps = Stdlib.List.[])
      ?deps_dune ?action ?locks ?package ?enabled_if name =
    let deps =
      match (deps, alias_deps, deps_dune) with
      | _ :: _, _, Some _ | _, _ :: _, Some _ ->
          invalid_arg
            "Dune.alias_rule: cannot specify both ~deps_dune and ~deps or \
             ~alias_deps"
      | [], [], Some deps -> deps
      | _, _, None ->
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
      (opt enabled_if @@ fun enabled_if -> [S "enabled_if"; enabled_if]);
      [
        S "action";
        (match action with None -> [S "progn"] | Some action -> action);
      ];
    ]

  let run command args = [S "run"; S command; G (of_atom_list args)]

  let run_exe exe_name args = run ("%{exe:" ^ exe_name ^ ".exe}") args

  let file name = [S "file"; S name]

  let glob_files expr = [S "glob_files"; S expr]

  let glob_files_rec expr = [S "glob_files_rec"; S expr]

  let runtest ?(alias = "runtest") ?action ?package ?locks ?enabled_if
      ~dep_files ~dep_globs ~dep_globs_rec name =
    let deps_dune =
      let files = List.map (fun s -> S s) dep_files in
      let globs = List.map glob_files dep_globs in
      let globs_rec = List.map glob_files_rec dep_globs_rec in
      match files @ globs @ globs_rec with
      | [] -> None
      | deps -> Some (of_list deps)
    in
    let action =
      Option.value ~default:[S "run"; S ("%{dep:./" ^ name ^ ".exe}")] action
    in
    alias_rule alias ?package ?deps_dune ?locks ?enabled_if ~action

  let runtest_js ?(alias = "runtest_js") ?package ?locks ?enabled_if ~dep_files
      ~dep_globs ~dep_globs_rec name =
    let action = [S "run"; S "node"; S ("%{dep:./" ^ name ^ ".bc.js}")] in
    runtest
      ~alias
      ~action
      ?package
      ?locks
      ?enabled_if
      ~dep_files
      ~dep_globs
      ~dep_globs_rec
      name

  let setenv name value followup = [G [S "setenv"; S name; S value]; followup]

  let progn l = [S "progn"; G (of_list l)]

  let chdir_workspace_root followup =
    [G [S "chdir"; S "%{workspace_root}"]; followup]

  let backend ?(args = Stdlib.List.[]) name =
    S "backend" :: S name :: of_atom_list args

  let ocamllex name = [S "ocamllex"; S name]

  let menhir name = [S "menhir"; [S "modules"; S name]]

  let ocamlyacc name = [S "ocamlyacc"; S name]

  let pps ?(args = Stdlib.List.[]) names =
    let atoms = match args with [] -> names | _ :: _ -> names @ args in
    S "pps" :: of_atom_list atoms

  let staged_pps names =
    let s_exprs = Stdlib.List.map (fun n -> S n) names in
    S "staged_pps" :: of_list s_exprs

  let include_ name = [S "include"; S name]

  let target_or_targets_rule ~promote ?deps ?enabled_if s ~action =
    [
      S "rule";
      s;
      (if promote then [S "mode"; S "promote"] else E);
      (match deps with None -> E | Some deps -> [S "deps"; G (of_list deps)]);
      [S "action"; action];
      (opt enabled_if @@ fun enabled_if -> [S "enabled_if"; enabled_if]);
    ]

  let targets_rule ?(promote = false) ?deps ?enabled_if targets ~action =
    target_or_targets_rule
      ~promote
      ?deps
      ?enabled_if
      [S "targets"; G (of_atom_list targets)]
      ~action

  let target_rule ?(promote = false) ?deps ?enabled_if target ~action =
    target_or_targets_rule
      ~promote
      ?deps
      ?enabled_if
      [S "target"; S target]
      ~action

  let protobuf_rule filename_without_extension =
    let proto_filename = filename_without_extension ^ ".proto" in
    let compiled_filename = filename_without_extension ^ ".ml" in
    target_rule
      compiled_filename
      ~deps:[[S ":proto"; S proto_filename]]
      ~action:
        [
          S "run";
          H [S "protoc"; S "-I"; S "."];
          S "--ocaml_out=annot=[@@deriving show { with_path = false }]:.";
          S "%{proto}";
        ]

  let install ?package files ~section =
    [
      S "install";
      (match package with
      | None -> E
      | Some package -> [S "package"; S package]);
      [S "section"; S section];
      [S "files"; G (of_list files)];
    ]

  let as_ target alias = [S target; S "as"; S alias]
end

(*****************************************************************************)
(*                                VERSIONS                                   *)
(*****************************************************************************)

module Version = struct
  type t = string

  type atom = V of t | Version

  (* Note: opam does not actually support [False], which makes sense since
     why would one want to have a dependency which cannot be installed.
     We support [False] in order to be able to negate any version constraint. *)
  type constraints =
    | True
    | False
    | Exactly of atom
    | Different_from of atom
    | At_least of atom
    | More_than of atom
    | At_most of atom
    | Less_than of atom
    | Not of constraints
    | And of constraints * constraints
    | Or of constraints * constraints

  let exactly x = Exactly (V x)

  let different_from x = Different_from (V x)

  let at_least x = At_least (V x)

  let more_than x = More_than (V x)

  let at_most x = At_most (V x)

  let less_than x = Less_than (V x)

  let not_ = function
    | True -> False
    | False -> True
    | Exactly x -> Different_from x
    | Different_from x -> Exactly x
    | At_least x -> Less_than x
    | More_than x -> At_most x
    | At_most x -> More_than x
    | Less_than x -> At_least x
    | Not x -> x
    | (And _ | Or _) as x ->
        (* We could distribute but it could lead to an exponential explosion. *)
        Not x

  let ( && ) a b =
    match (a, b) with
    | True, x | x, True -> x
    | False, _ | _, False -> False
    | _ -> And (a, b)

  let and_list = List.fold_left ( && ) True

  let rec to_and_list c =
    match c with
    | True | False | Exactly _ | Different_from _ | At_least _ | More_than _
    | At_most _ | Less_than _ | Not _ ->
        [c]
    | And (c1, c2) -> to_and_list c1 @ to_and_list c2
    | Or (_, _) ->
        invalid_arg "Version.to_and_list: passed cosntraint is not an and list"

  let ( || ) a b =
    match (a, b) with
    | True, _ | _, True -> True
    | False, x | x, False -> x
    | _ -> Or (a, b)

  let or_list = List.fold_left ( || ) False

  let rec version_to_string = function
    | True -> "true"
    | False -> "false"
    | Exactly (V x) -> Format.sprintf "= %s" x
    | Exactly Version -> "= version"
    | Different_from (V x) -> Format.sprintf "!= %s" x
    | Different_from Version -> "!= version"
    | At_least (V x) -> Format.sprintf ">= %s" x
    | At_least Version -> ">= version"
    | More_than (V x) -> Format.sprintf "> %s" x
    | More_than Version -> "> version"
    | At_most (V x) -> Format.sprintf "<= %s" x
    | At_most Version -> "<= version"
    | Less_than (V x) -> Format.sprintf "< %s" x
    | Less_than Version -> "< version"
    | Not constraints ->
        Format.sprintf "not %s" @@ version_to_string constraints
    | And (c1, c2) ->
        Format.sprintf "%s && %s" (version_to_string c1) (version_to_string c2)
    | Or (c1, c2) ->
        Format.sprintf "%s || %s" (version_to_string c1) (version_to_string c2)
end

module Npm = struct
  type version_or_path = Version of Version.constraints | Path of string

  type t = {package : string; version_or_path : version_or_path}

  let make package version_or_path = {package; version_or_path}

  let node_preload t =
    match String.index_opt t.package '/' with
    | None -> t.package
    | Some i -> String.sub t.package (i + 1) (String.length t.package - i - 1)
end

(*****************************************************************************)
(*                                  OPAM                                     *)
(*****************************************************************************)

type available =
  | Always
  | Never
  | No_32
  | No_x86
  | No_ppc
  | No_arm
  | No_s390x
  | N_ary_and of available list

let string_of_available = function
  | Always -> "Always"
  | Never -> "Never"
  | No_32 -> "No_32"
  | No_x86 -> "No_x86"
  | No_ppc -> "No_ppc"
  | No_arm -> "No_arm"
  | No_s390x -> "No_s390x"
  | N_ary_and _ -> "N_ary_and"

type with_test = Always | Never | Only_on_64_arch

let show_with_test = function
  | Always -> "Always"
  | Never -> "Never"
  | Only_on_64_arch -> "Only_on_64_arch"

module Opam = struct
  type dependency = {
    package : string;
    version : Version.constraints;
    with_test : with_test;
    optional : bool;
  }

  type command_item = A of string | S of string

  type build_instruction = {command : command_item list; with_test : with_test}

  type url = {url : string; sha256 : string option; sha512 : string option}

  type t = {
    maintainer : string;
    authors : string list;
    homepage : string;
    doc : string;
    bug_reports : string;
    dev_repo : string;
    licenses : string list;
    depends : dependency list;
    conflicts : dependency list;
    build : build_instruction list;
    available : available;
    synopsis : string;
    url : url option;
    description : string option;
    x_opam_monorepo_opam_provided : string list;
  }

  let pp fmt
      {
        maintainer;
        authors;
        homepage;
        doc;
        bug_reports;
        dev_repo;
        licenses;
        depends;
        conflicts;
        build;
        available;
        synopsis;
        url;
        description;
        x_opam_monorepo_opam_provided;
      } =
    if synopsis = "" then invalid_arg "Manifest.Opam.pp: empty synopsis" ;
    (match synopsis.[0] with
    | 'A' .. 'Z' -> ()
    | _ ->
        invalid_arg
          ("Manifest.Opam.pp: synopsis must start with a capital letter: "
         ^ synopsis)) ;
    if synopsis.[String.length synopsis - 1] = '.' then
      invalid_arg
        ("Manifest.Opam.pp: synopsis cannot end with a period: " ^ synopsis) ;
    let depopts, depends = List.partition (fun dep -> dep.optional) depends in
    (* Tries to dedpublicate dependencies constraints *)
    let deduplicate_constraints_list constraints =
      try
        deduplicate_list
          ~merge:(fun c1 _c2 -> c1)
          Version.version_to_string
          (Version.to_and_list constraints)
        |> Version.and_list
      with _ -> constraints
    in
    let depends =
      List.map
        (fun dep ->
          {
            package = dep.package;
            with_test = dep.with_test;
            optional = dep.optional;
            version = deduplicate_constraints_list dep.version;
          })
        depends
    in
    let depopts, conflicts =
      (* Opam documentation says this about [depopts]:
         "If you require specific versions, add a [conflicts] field with the ones
         that won't work."
         One could assume that this is because version constraints need to existe
         whether the optional dependencies are selected or not?
         In any case the following piece of code converts version constraints
         on optional dependencies into conflicts. *)
      let optional_dep_conflicts =
        let negate_dependency_constraint dependency =
          match dependency.version with
          | True ->
              (* No conflict to introduce. *)
              None
          | version -> Some {dependency with version = Version.not_ version}
        in
        List.filter_map negate_dependency_constraint depopts
      in
      let depopts =
        let remove_constraint dependency = {dependency with version = True} in
        List.map remove_constraint depopts
      in
      let conflicts = conflicts @ optional_dep_conflicts in
      (depopts, conflicts)
    in
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
    let pp_version_atom fmt = function
      | Version.V x -> pp_string fmt x
      | Version -> Format.pp_print_string fmt "version"
    in
    let rec pp_version_constraint ~in_and fmt = function
      | Version.True ->
          invalid_arg "pp_version_constraint cannot be called with True"
      | False -> invalid_arg "pp_version_constraint cannot be called with False"
      | Exactly version -> Format.fprintf fmt "= %a" pp_version_atom version
      | Different_from version ->
          Format.fprintf fmt "!= %a" pp_version_atom version
      | At_least version -> Format.fprintf fmt ">= %a" pp_version_atom version
      | More_than version -> Format.fprintf fmt "> %a" pp_version_atom version
      | At_most version -> Format.fprintf fmt "<= %a" pp_version_atom version
      | Less_than version -> Format.fprintf fmt "< %a" pp_version_atom version
      | Not atom ->
          Format.fprintf fmt "! (%a)" (pp_version_constraint ~in_and:false) atom
      | And (a, b) ->
          Format.fprintf
            fmt
            "%a & %a"
            (pp_version_constraint ~in_and:true)
            a
            (pp_version_constraint ~in_and:true)
            b
      | Or (a, b) ->
          Format.fprintf
            fmt
            "%s%a & %a%s"
            (if in_and then "(" else "")
            (pp_version_constraint ~in_and:false)
            a
            (pp_version_constraint ~in_and:false)
            b
            (if in_and then ")" else "")
    in
    let condition_of_with_test = function
      | Always -> ["with-test"]
      | Never -> []
      | Only_on_64_arch ->
          ["with-test"; "arch != \"arm32\""; "arch != \"x86_32\""]
    in
    let available =
      let rec condition_of_available = function
        | No_32 ->
            ["arch != \"arm32\""; "arch != \"x86_32\""; "arch != \"ppc32\""]
        | No_x86 -> ["arch != \"x86_32\""; "arch != \"x86_64\""]
        | No_arm -> ["arch != \"arm32\""; "arch != \"arm64\""]
        | No_ppc -> ["arch != \"ppc64\""; "arch != \"ppc64\""]
        | No_s390x -> ["arch != \"s390x\""]
        | Always -> []
        | Never -> ["false"]
        | N_ary_and available_list ->
            List.map condition_of_available available_list
            |> List.flatten
            |> deduplicate_list (fun s -> s)
      in
      condition_of_available available
    in
    let pp_condition fmt = function
      | [] -> ()
      | ["with-test"] -> Format.pp_print_string fmt " {with-test}"
      | items ->
          Format.fprintf
            fmt
            " { %a }"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " & ")
               Format.pp_print_string)
            items
    in
    let pp_dependency fmt {package; version; with_test; _} =
      let condition =
        let with_test = condition_of_with_test with_test in
        let version =
          match version with
          | True -> []
          | _ ->
              [
                Format.asprintf
                  "%a"
                  (pp_version_constraint ~in_and:false)
                  version;
              ]
        in
        List.concat [with_test; version]
      in
      Format.fprintf fmt "@[%a%a@]" pp_string package pp_condition condition
    in
    let pp_command_item fmt = function
      | A atom -> Format.pp_print_string fmt atom
      | S string -> pp_string fmt string
    in
    let pp_build_instruction fmt {command; with_test} =
      Format.fprintf
        fmt
        "%a%a"
        (pp_list pp_command_item)
        command
        pp_condition
        (condition_of_with_test with_test)
    in
    let pp_url {url; sha256; sha512} =
      pp_line "url {" ;
      pp_line "  src: \"%s\"" url ;
      if sha256 <> None || sha512 <> None then (
        pp_line "  checksum: [" ;
        Option.iter (fun sha256 -> pp_line "    \"sha256=%s\"" sha256) sha256 ;
        Option.iter (fun sha512 -> pp_line "    \"sha512=%s\"" sha512) sha512 ;
        pp_line "  ]") ;
      pp_line "}"
    in
    pp_line "opam-version: \"2.0\"" ;
    pp_line "maintainer: %a" pp_string maintainer ;
    pp_line "%a" (pp_list ~prefix:"authors: " pp_string) authors ;
    if homepage <> "" then pp_line "homepage: %a" pp_string homepage ;
    if doc <> "" then pp_line "doc: %a" pp_string doc ;
    if bug_reports <> "" then pp_line "bug-reports: %a" pp_string bug_reports ;
    pp_line "dev-repo: %a" pp_string dev_repo ;
    (match licenses with
    | [license] -> pp_line "license: %a" pp_string license
    | _ -> pp_line "license: %a" (pp_list pp_string) licenses) ;
    pp_line "%a" (pp_list ~v:true ~prefix:"depends: " pp_dependency) depends ;
    if depopts <> [] then
      pp_line "%a" (pp_list ~v:true ~prefix:"depopts: " pp_dependency) depopts ;
    if x_opam_monorepo_opam_provided <> [] then
      pp_line
        "%a"
        (pp_list ~v:true ~prefix:"x-opam-monorepo-opam-provided: " pp_string)
        x_opam_monorepo_opam_provided ;
    if conflicts <> [] then
      pp_line
        "%a"
        (pp_list ~v:true ~prefix:"conflicts: " pp_dependency)
        conflicts ;
    pp_line "%a" (pp_list ~prefix:"build: " pp_build_instruction) build ;
    if available <> [] then
      pp_line
        "available: %a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " & ")
           Format.pp_print_string)
        available ;
    pp_line "synopsis: %a" pp_string synopsis ;
    Option.iter pp_url url ;
    Option.iter (pp_line "description: %a" pp_string) description
end

module Opam_dependency_set = Set.Make (struct
  type t = Opam.dependency

  let compare x y =
    let pkg = String.compare x.Opam.package y.Opam.package in
    if pkg = 0 then Stdlib.compare x.version y.version else pkg
end)

module Flags = struct
  type t = {standard : bool; rest : Dune.s_expr list}

  let if_true b name = if b then Some (Dune.S name) else None

  let disabled_warnings_to_string ws =
    let int_ranges l =
      List.sort_uniq compare l
      |> List.fold_left
           (fun acc x ->
             match acc with
             | [] -> [(x, x)]
             | (l, u) :: acc when succ u = x -> (l, x) :: acc
             | _ -> (x, x) :: acc)
           []
      |> List.rev
    in
    let range_to_flag (x, y) =
      if x = y then Printf.sprintf "-%d" x
      else if x + 1 = y then Printf.sprintf "-%d-%d" x y
      else Printf.sprintf "-%d..%d" x y
    in
    if List.exists (fun x -> x <= 0) ws then
      invalid_arg "Warning number must be positive" ;
    List.map range_to_flag (int_ranges ws) |> String.concat ""

  let standard ?disable_warnings ?(nopervasives = false) ?(nostdlib = false)
      ?(opaque = false) () =
    {
      standard = true;
      rest =
        List.filter_map
          (fun x -> x)
          [
            (match disable_warnings with
            | None | Some Stdlib.List.[] -> None
            | Some l ->
                Some Dune.(H [S "-w"; S (disabled_warnings_to_string l)]));
            if_true nostdlib "-nostdlib";
            if_true nopervasives "-nopervasives";
            if_true opaque "-opaque";
          ];
    }

  let include_ f = {standard = false; rest = Dune.[S ":include"; S f]}
end

module Ctypes = struct
  type description = {instance : string; functor_ : string}

  type t = {
    external_library_name : string;
    include_header : string;
    extra_search_dir : string;
    type_description : description;
    function_description : description;
    generated_types : string;
    generated_entry_point : string;
    c_flags : string list;
    c_library_flags : string list;
    deps : string list;
  }

  let to_dune desc =
    let open Dune in
    let deps =
      match desc.deps with [] -> E | deps -> S "deps" :: of_atom_list deps
    in
    [
      S "ctypes";
      [S "external_library_name"; S desc.external_library_name];
      [
        S "build_flags_resolver";
        [
          S "vendored";
          of_atom_list
            ([
               "c_flags";
               ":standard";
               "-Wno-discarded-qualifiers";
               "-I" ^ desc.extra_search_dir;
             ]
            @ desc.c_flags);
          of_atom_list
            (["c_library_flags"; ":standard"]
            @ desc.c_library_flags
            @ ["-l" ^ desc.external_library_name; "-L" ^ desc.extra_search_dir]
            );
        ];
      ];
      [S "headers"; [S "include"; S desc.include_header]];
      [
        S "type_description";
        [S "instance"; S desc.type_description.instance];
        [S "functor"; S desc.type_description.functor_];
      ];
      [
        S "function_description";
        [S "concurrency"; S "unlocked"];
        [S "instance"; S desc.function_description.instance];
        [S "functor"; S desc.function_description.functor_];
      ];
      [S "generated_types"; S desc.generated_types];
      [S "generated_entry_point"; S desc.generated_entry_point];
      deps;
    ]
end

module Env : sig
  (* See manifest.mli for documentation *)

  type t

  type profile = Profile of string | Any

  val empty : t

  val to_s_expr : t -> Dune.s_expr

  val add : profile -> key:string -> Dune.s_expr -> t -> t
end = struct
  type entry = string * Dune.s_expr

  type profile = Profile of string | Any

  type t = (profile * entry) list

  let empty = []

  let add profile ~key payload env =
    (match profile with
    | Profile "_" ->
        invalid_arg "Env.add: [Provide \"_\"] is not allowed. Use [Any]."
    | Profile _ | Any -> ()) ;
    (profile, (key, payload)) :: env

  let s_expr_of_entry (name, payload) = Dune.[S name; payload]

  let to_s_expr (t : t) =
    let any, names =
      List.partition_map
        (function
          | Any, entry -> Left entry | Profile name, entry -> Right (name, entry))
        t
    in
    let names =
      List.fold_left
        (fun names (n, entry) ->
          String_map.update
            n
            (function
              | None -> Some [entry] | Some prev -> Some (entry :: prev))
            names)
        String_map.empty
        names
    in
    let names =
      match any with
      | [] -> names
      | _ -> String_map.add "_" any (String_map.map (fun x -> any @ x) names)
    in
    String_map.iter
      (fun name entries ->
        let entry_names = List.map fst entries in
        if
          List.length (List.sort_uniq compare entry_names)
          <> List.length entry_names
        then invalid_arg ("Env.to_s_expr: duplicated entry in env " ^ name))
      names ;
    if String_map.is_empty names then Dune.E
    else
      let compare_key ((a : string), _) (b, _) =
        match (a, b) with
        | "_", "_" -> 0
        | "_", _ -> 1
        | _, "_" -> -1
        | _ -> compare a b
      in
      let l : Dune.s_expr list =
        List.map
          (fun (name, entries) ->
            Dune.(
              S name
              :: of_list
                   (List.map s_expr_of_entry (List.sort compare_key entries))))
          (List.sort compare_key (String_map.bindings names))
      in
      Dune.(S "env" :: of_list l)
end

(*****************************************************************************)
(*                                 TARGETS                                   *)
(*****************************************************************************)

let default_ocaml_dependency =
  (* ocamlformat is set to produce code compatible with this version; thus, the
     code may contain syntactic constructs which are not available beforehand;
     e.g., let-punning in binding-operator *)
  Version.at_least "4.14"

module Target = struct
  let invalid_argf x = Printf.ksprintf invalid_arg x

  type external_ = {
    name : string;
    main_module : string option;
    opam : string option;
    version : Version.constraints;
    js_compatible : bool;
    npm_deps : Npm.t list;
  }

  type vendored = {
    name : string;
    main_module : string option;
    version : Version.constraints;
    js_compatible : bool;
    npm_deps : Npm.t list;
    released_on_opam : bool;
  }

  type opam_only = {
    name : string;
    version : Version.constraints;
    can_vendor : bool;
  }

  type modules =
    | All
    | Modules of string list
    | All_modules_except of string list

  (* [internal_name] is the name for [name] stanzas in [dune] while [public_name] is the
     name for [public_name] stanzas in [dune] and the name in [.opam] files. *)
  type full_name = {internal_name : string; public_name : string}

  type preprocessor_dep = File of string

  type release_status = Unreleased | Experimental | Released | Auto_opam

  type bisect_ppx = No | Yes | With_sigterm

  let show_release_status = function
    | Unreleased -> "Unreleased"
    | Experimental -> "Experimental"
    | Released -> "Released"
    | Auto_opam -> "Auto_opam"

  type kind =
    | Public_library of full_name
    | Private_library of string
    | Public_executable of full_name Ne_list.t
    | Private_executable of string Ne_list.t
    | Test_executable of {
        names : string Ne_list.t;
        runtest_alias : string option;
        locks : string option;
        enabled_if : Dune.s_expr option;
        lib_deps : t option list;
      }

  and internal = {
    bisect_ppx : bisect_ppx;
    time_measurement_ppx : bool;
    c_library_flags : string list option;
    conflicts : t list;
    deps : t list;
    tests_deps : t option list;
    dune : Dune.s_expr;
    flags : Flags.t option;
    foreign_archives : string list option;
    foreign_stubs : Dune.foreign_stubs option;
    implements : t option;
    inline_tests : bool;
    inline_tests_deps : Dune.s_expr list option;
    js_compatible : bool;
    js_of_ocaml : Dune.s_expr option;
    documentation : Dune.s_expr option;
    kind : kind;
    linkall : bool;
    modes : Dune.mode list option;
    modules : modules;
    modules_without_implementation : string list;
    ocaml : Version.constraints;
    opam : string option;
    opam_bug_reports : string option;
    opam_doc : string option;
    opam_homepage : string option;
    opam_with_test : with_test;
    optional : bool;
    opens : string list;
    path : string;
    ppx_kind : Dune.ppx_kind option;
    ppx_runtime_libraries : t list;
    preprocess : preprocessor list;
    preprocessor_deps : preprocessor_dep list;
    private_modules : string list;
    profile : string option;
    opam_only_deps : t list;
    release_status : release_status;
    static : bool;
    synopsis : string option;
    description : string option;
    available : available;
    virtual_modules : string list;
    default_implementation : string option;
    npm_deps : Npm.t list;
    cram : bool;
    license : string option;
    extra_authors : string list;
    ctypes : Ctypes.t option;
    with_macos_security_framework : bool;
  }

  and preprocessor =
    | PPS of {targets : t list; args : string list}
    | Staged_PPS of t list

  and inline_tests = Inline_tests_backend of t

  and select = {
    package : t;
    source_if_present : string;
    source_if_absent : string;
    target : string;
  }

  and t =
    | Internal of internal
    | Vendored of vendored
    | External of external_
    | Opam_only of opam_only
    | Optional of t
    | Re_export of t  (** Stanza [(re_export <t>)] *)
    | Select of select
    | Open of t * string

  let rec get_internal = function
    | Internal i -> Some i
    | Optional t | Re_export t | Open (t, _) | Select {package = t; _} ->
        get_internal t
    | Vendored _ -> None
    | External _ -> None
    | Opam_only _ -> None

  let pps ?(args = []) = function
    | None -> invalid_arg "Manifest.Target.pps cannot be given no_target"
    | Some target -> PPS {targets = [target]; args}

  let ppses targets =
    let targets =
      List.map
        (function
          | None ->
              invalid_arg "Manifest.Target.ppses cannot be given no_target"
          | Some target -> target)
        targets
    in
    PPS {targets; args = []}

  let staged_pps targets =
    Staged_PPS (Stdlib.List.concat_map Option.to_list targets)

  let inline_tests_backend = function
    | None ->
        invalid_arg
          "Manifest.Target.inline_tests_backend cannot be given no_target"
    | Some target -> Inline_tests_backend target

  let convert_to_identifier = String.map @@ function '-' | '.' -> '_' | c -> c

  (* List of all targets, in reverse order of registration. *)
  let registered = ref []

  (* List of targets sorted by path,
     so that we can create dune files with multiple targets. *)
  let by_path = ref String_map.empty

  (* List of targets sorted by opam package name,
     so that we can create opam files with multiple targets. *)
  let by_opam = ref String_map.empty

  (* Set to [false] by [generate] to prevent further modifying the above references. *)
  let can_register = ref true

  let register_internal ({path; opam; _} as internal) =
    let path = sanitize_path path in
    if not !can_register then
      invalid_arg
        "cannot register new targets after calling Manifest.check or \
         Manifest.generate" ;
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
    Some (Internal internal)

  let kind_name_for_errors kind =
    match kind with
    | Public_library {public_name = name; _}
    | Private_library name
    | Public_executable ({public_name = name; _}, _)
    | Private_executable (name, _)
    | Test_executable {names = name, _; _} ->
        name

  (* Note: this function is redefined below for the version with optional targets. *)
  let rec name_for_errors = function
    | Vendored {name; _} | External {name; _} | Opam_only {name; _} -> name
    | Optional target
    | Re_export target
    | Select {package = target; _}
    | Open (target, _) ->
        name_for_errors target
    | Internal {kind; _} -> kind_name_for_errors kind

  let rec names_for_dune = function
    | Vendored {name; _} | External {name; _} | Opam_only {name; _} -> (name, [])
    | Optional target
    | Re_export target
    | Select {package = target; _}
    | Open (target, _) ->
        names_for_dune target
    | Internal {kind; _} -> (
        match kind with
        | Public_library {public_name; _} -> (public_name, [])
        | Private_library internal_name -> (internal_name, [])
        | Public_executable (head, tail) ->
            (head.public_name, List.map (fun x -> x.public_name) tail)
        | Private_executable names | Test_executable {names; _} -> names)

  let rec library_name_for_dune = function
    | Vendored {name; _} | External {name; _} | Opam_only {name; _} -> Ok name
    | Optional target
    | Re_export target
    | Select {package = target; _}
    | Open (target, _) ->
        library_name_for_dune target
    | Internal {kind; _} -> (
        match kind with
        | Public_library {public_name; _} -> Ok public_name
        | Private_library internal_name -> Ok internal_name
        | Public_executable ({public_name = name; _}, _)
        | Private_executable (name, _)
        | Test_executable {names = name, _; _} ->
            Error name)

  let iter_internal_by_path f =
    String_map.iter (fun path internals -> f path (List.rev internals)) !by_path

  let iter_internal_by_opam f =
    String_map.iter (fun opam internals -> f opam (List.rev internals)) !by_opam

  type 'a maker =
    ?all_modules_except:string list ->
    ?bisect_ppx:bisect_ppx ->
    ?c_library_flags:string list ->
    ?conflicts:t option list ->
    ?deps:t option list ->
    ?dune:Dune.s_expr ->
    ?flags:Flags.t ->
    ?foreign_archives:string list ->
    ?foreign_stubs:Dune.foreign_stubs ->
    ?ctypes:Ctypes.t ->
    ?implements:t option ->
    ?inline_tests:inline_tests ->
    ?inline_tests_deps:Dune.s_expr list ->
    ?js_compatible:bool ->
    ?js_of_ocaml:Dune.s_expr ->
    ?documentation:Dune.s_expr ->
    ?linkall:bool ->
    ?modes:Dune.mode list ->
    ?modules:string list ->
    ?modules_without_implementation:string list ->
    ?npm_deps:Npm.t list ->
    ?ocaml:Version.constraints ->
    ?opam:string ->
    ?opam_bug_reports:string ->
    ?opam_doc:string ->
    ?opam_homepage:string ->
    ?opam_with_test:with_test ->
    ?optional:bool ->
    ?ppx_kind:Dune.ppx_kind ->
    ?ppx_runtime_libraries:t option list ->
    ?preprocess:preprocessor list ->
    ?preprocessor_deps:preprocessor_dep list ->
    ?private_modules:string list ->
    ?profile:string ->
    ?opam_only_deps:t option list ->
    ?release_status:release_status ->
    ?static:bool ->
    ?synopsis:string ->
    ?description:string ->
    ?time_measurement_ppx:bool ->
    ?available:available ->
    ?virtual_modules:string list ->
    (* A note on [default_implementation]. In the .mli,  this argument is
       given type [string] instead of [target]. This is because one can't
       have mutually recursive target definitions, as in:

       let rec virtual_package =
          ... ~virtual_modules:"Foo" ~default_implementation:implem
       and implem = ... ~implements:virtual_package

       A solution would be to declare [default_implementation]
       in [implem]:

       let virtual_package = ... ~virtual_modules:"Foo"
       let implem = ... ~implements_default:virtual_package

       But that would be more complex to implement.
    *)
    ?default_implementation:string ->
    ?cram:bool ->
    ?license:string ->
    ?extra_authors:string list ->
    ?with_macos_security_framework:bool ->
    path:string ->
    'a ->
    t option

  let node_preload deps : string list =
    let collect deps =
      let rec loop (seen, acc) dep =
        match library_name_for_dune dep with
        | Error _ -> (seen, acc)
        | Ok name -> (
            if String_set.mem name seen then (seen, acc)
            else
              match dep with
              | Internal {deps; npm_deps; _} ->
                  let acc = List.map Npm.node_preload npm_deps @ acc in
                  let seen = String_set.add name seen in
                  loops (seen, acc) deps
              | External {npm_deps; _} ->
                  let seen = String_set.add name seen in
                  (seen, List.map Npm.node_preload npm_deps @ acc)
              | Vendored {npm_deps; _} ->
                  let seen = String_set.add name seen in
                  (seen, List.map Npm.node_preload npm_deps @ acc)
              | Opam_only _ -> (seen, acc)
              | Optional target
              | Re_export target
              | Select {package = target; _}
              | Open (target, _) ->
                  loop (seen, acc) target)
      and loops (seen, acc) deps =
        List.fold_left
          (fun (seen, acc) x -> loop (seen, acc) x)
          (seen, acc)
          deps
      in
      loops (String_set.empty, []) deps
    in
    snd (collect deps)

  let internal make_kind ?all_modules_except ?bisect_ppx ?c_library_flags
      ?(conflicts = []) ?(dep_files = []) ?(dep_globs = [])
      ?(dep_globs_rec = []) ?(deps = []) ?(dune = Dune.[]) ?flags
      ?foreign_archives ?foreign_stubs ?ctypes ?implements ?inline_tests
      ?inline_tests_deps ?js_compatible ?js_of_ocaml ?documentation
      ?(linkall = false) ?modes ?modules ?(modules_without_implementation = [])
      ?(npm_deps = []) ?(ocaml = default_ocaml_dependency) ?opam
      ?opam_bug_reports ?opam_doc ?opam_homepage ?(opam_with_test = Always)
      ?(optional = false) ?ppx_kind ?(ppx_runtime_libraries = [])
      ?(preprocess = []) ?(preprocessor_deps = []) ?(private_modules = [])
      ?profile ?(opam_only_deps = []) ?(release_status = Auto_opam) ?static
      ?synopsis ?description ?(time_measurement_ppx = false)
      ?(available : available = Always) ?(virtual_modules = [])
      ?default_implementation ?(cram = false) ?license ?(extra_authors = [])
      ?(with_macos_security_framework = false) ~path names =
    let conflicts = List.filter_map Fun.id conflicts in
    let deps = List.filter_map Fun.id deps in
    let opam_only_deps = List.filter_map Fun.id opam_only_deps in
    let ppx_runtime_libraries = List.filter_map Fun.id ppx_runtime_libraries in
    let implements =
      match implements with
      | None -> None
      | Some None ->
          invalid_arg "Target.internal: cannot pass no_target to ~implements"
      | Some (Some _ as x) -> x
    in
    let opens =
      let rec get_opens acc = function
        | Internal _ | Vendored _ | External _ | Opam_only _ -> acc
        | Optional target | Re_export target | Select {package = target; _} ->
            get_opens acc target
        | Open (target, module_name) -> get_opens (module_name :: acc) target
      in
      List.flatten (List.map (get_opens []) deps)
    in
    let js_compatible, js_of_ocaml =
      match (js_compatible, js_of_ocaml) with
      | Some false, Some _ ->
          invalid_arg
            "Target.internal: cannot specify both `~js_compatible:false` and \
             `~js_of_ocaml`"
      | Some true, Some jsoo -> (true, Some jsoo)
      | Some true, None -> (true, Some Dune.[])
      | None, Some jsoo -> (true, Some jsoo)
      | Some false, None | None, None -> (false, None)
    in
    let kind = make_kind names in
    let preprocess, inline_tests =
      match (inline_tests, inline_tests_deps) with
      | None, None -> (preprocess, false)
      | None, Some _ ->
          invalid_arg
            "Target.internal: cannot specify `inline_tests_deps` without \
             inline_tests"
      | Some (Inline_tests_backend target), (Some _ | None) -> (
          match kind with
          | Public_library _ | Private_library _ ->
              (PPS {targets = [target]; args = []} :: preprocess, true)
          | Public_executable _ | Private_executable _ | Test_executable _ ->
              invalid_arg
                "Target.internal: cannot specify `inline_tests` for \
                 executables and tests")
    in
    let opam =
      match opam with
      | Some "" -> (
          match kind with
          | Test_executable {names = name, _; runtest_alias = Some _; _} ->
              invalid_argf
                "for targets which provide test executables such as %S, you \
                 must specify a non-empty ~opam or have it not run by default \
                 with ~alias:\"\""
                name
          | Public_library {public_name; _} ->
              invalid_argf
                "public_library %s cannot have ~opam set to empty string (\"\")"
                public_name
          | Public_executable ({public_name; _}, _) ->
              invalid_argf
                "for targets which provide public executables such as %S, you \
                 cannot have ~opam set to empty string (\"\")"
                public_name
          | Test_executable {runtest_alias = None; _}
          | Private_library _ | Private_executable _ ->
              None)
      | Some opam as x ->
          if
            String.for_all
              (function
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
                | _ -> false)
              opam
          then (
            (match kind with
            | Public_library {public_name; _} -> (
                (* We allow an opam package on public_library even if
                   it's not necessary.  We just need to make sure
                   [package] agrees with [public_name] *)
                match String.split_on_char '.' public_name with
                | [] -> assert false
                | first :: _ ->
                    if first <> opam then
                      error
                        "Mismatch between public_name %S and opam package %S\n"
                        public_name
                        opam)
            | _ -> ()) ;
            x)
          else
            invalid_argf
              "%s is not a valid opam package name: should be of the form \
               [A-Za-z0-9_-]+"
              opam
      | None -> (
          match kind with
          | Public_library {public_name; _} -> (
              match String.split_on_char '.' public_name with
              | [] -> assert false
              | first :: _ -> Some first)
          | Public_executable ({public_name; _}, []) -> Some public_name
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
          | Test_executable {names = name, _; _} ->
              invalid_argf
                "for targets which provide test executables such as %S, you \
                 must specify ~opam (set it to \"\" for no opam file)"
                name)
    in
    let () =
      (* Sanity checks around virtual packages.
         - If a target [X] [implements] another target [Y], [X] must specify its opam file
         - [Y] must be an internal target
         - [Y] must be virtual, ie specify virtual modules
         - [Y] must specify an opam package
         - If [Y] specifies [X] as default implementation, [X] and [Y] must live in the
           same package.
      *)
      match (implements, opam) with
      | None, _ -> ()
      | Some _, None ->
          error
            "Target %s implements a virtual target, it must specify its opam \
             package"
            (kind_name_for_errors kind)
      | Some target, Some opam -> (
          match get_internal target with
          | None ->
              error
                "The `implements` directive of %s specifies %s which is a \
                 non-internal target"
                (kind_name_for_errors kind)
                (name_for_errors target)
          | Some internal -> (
              (match internal.virtual_modules with
              | [] ->
                  error
                    "A target can only implement a virtual internal target, \
                     but %s claims to implement %s which declares no virtual \
                     modules"
                    (kind_name_for_errors kind)
                    (name_for_errors target)
              | _ -> ()) ;
              match internal.opam with
              | None ->
                  error
                    "While processing %s: virtual target %s must specify its \
                     opam package"
                    (kind_name_for_errors kind)
                    (name_for_errors target)
              | Some internal_opam -> (
                  match (internal.default_implementation, kind) with
                  | None, _ -> ()
                  | Some default_impl, Public_library {public_name; _}
                    when default_impl = public_name ->
                      if not (String.equal opam internal_opam) then
                        let name = name_for_errors target in
                        error
                          "%s specifies %s as default implementation but these \
                           do not live in the same package"
                          name
                          public_name
                      else ()
                  | _ -> ())))
    in
    let () =
      match kind with
      | Public_library {public_name; _} -> (
          match
            List.filter_map
              (function
                | Internal {kind = Private_library name; opam = private_pkg; _}
                  when opam <> private_pkg ->
                    Some name
                | _ -> None)
              deps
          with
          | [] -> ()
          | privates ->
              error
                "The public library %s depend on private libraries not part of \
                 the same package: %s"
                public_name
                (String.concat ", " privates))
      | _ -> ()
    in
    let () =
      (* Sanity checks around [ppx_rewriter] and [ppx_deriver] libraries. *)
      match ppx_kind with
      | Some (Dune.Ppx_rewriter | Ppx_deriver) -> (
          match kind with
          | Public_library _ | Private_library _ -> ()
          | Public_executable _ | Private_executable _ | Test_executable _ ->
              error
                "Argument ~ppx_kind is only allowed for libraries; target %s \
                 is not a library"
                (kind_name_for_errors kind))
      | None -> (
          match ppx_runtime_libraries with
          | [] -> ()
          | _ :: _ ->
              error
                "Argument ~ppx_runtime_libraries is only allowed when \
                 ~ppx_kind is also specified; target %s does not qualify"
                (kind_name_for_errors kind))
    in
    let static =
      match (static, kind) with
      | Some static, _ -> static
      | None, Public_executable _ -> true
      | None, _ -> false
    in
    let modules =
      match (modules, all_modules_except) with
      | None, None -> All
      | Some modules, None -> Modules modules
      | None, Some all_modules_except -> All_modules_except all_modules_except
      | Some _, Some _ ->
          invalid_arg
            "Target.internal: cannot specify both ?modules and \
             ?all_modules_except"
    in
    let not_a_test =
      match kind with
      | Public_library _ | Private_library _ | Public_executable _
      | Private_executable _ ->
          true
      | Test_executable _ -> false
    in
    let bisect_ppx =
      Option.value bisect_ppx ~default:(if not_a_test then Yes else No)
    in
    let tests_deps =
      match kind with Test_executable {lib_deps; _} -> lib_deps | _ -> []
    in
    let runtest_rules =
      let run_js = js_compatible in
      let run_native =
        match modes with
        | None | Some [] -> true
        | Some modes -> List.mem Dune.Native modes
      in
      match (kind, opam, dep_files) with
      | ( Test_executable
            {names; runtest_alias = Some alias; locks; enabled_if; _},
          package,
          _ ) ->
          let runtest_js_rules =
            if run_js then
              List.map
                (fun name ->
                  Dune.runtest_js
                    ~alias:(alias ^ "_js")
                    ~dep_files
                    ~dep_globs
                    ~dep_globs_rec
                    ?locks
                    ?enabled_if
                    ?package
                    name)
                (Ne_list.to_list names)
            else []
          in
          let runtest_rules =
            if run_native then
              List.map
                (fun name ->
                  Dune.runtest
                    ~alias
                    ~dep_files
                    ~dep_globs
                    ~dep_globs_rec
                    ?locks
                    ?enabled_if
                    ?package
                    name)
                (Ne_list.to_list names)
            else []
          in
          runtest_rules @ runtest_js_rules
      | ( Test_executable
            {
              names = name, _;
              runtest_alias = None;
              locks = _;
              enabled_if = _;
              lib_deps = _;
            },
          _,
          _ :: _ ) ->
          invalid_argf
            "for targets which provide test executables such as %S, ~dep_files \
             is only meaningful for the runtest alias. It cannot be used with \
             no alias, i.e. ~alias:\"\"."
            name
      | _, _, _ :: _ -> assert false
      | _ -> []
    in
    let dune =
      List.fold_right (fun x dune -> Dune.(x :: dune)) runtest_rules dune
    in
    if
      match release_status with
      | Unreleased -> false
      | Experimental | Released | Auto_opam -> true
    then
      if
        let prefixes = ["src/"; "tezt/"; "etherlink/"; "irmin/"] in
        not
          (List.exists (fun prefix -> String.starts_with ~prefix path) prefixes)
      then
        invalid_argf
          "A target has the release status %s but is located at %s which is \
           outside of `src/` or `tezt/`. This is not supported. Move the code \
           to `src/` or set the release status to %s."
          (show_release_status release_status)
          path
          (show_release_status Unreleased) ;
    register_internal
      {
        bisect_ppx;
        time_measurement_ppx;
        c_library_flags;
        conflicts;
        deps;
        dune;
        flags;
        foreign_archives;
        foreign_stubs;
        implements;
        inline_tests;
        inline_tests_deps;
        js_compatible;
        js_of_ocaml;
        documentation;
        kind;
        linkall;
        modes;
        modules;
        modules_without_implementation;
        ocaml;
        opam;
        opam_bug_reports;
        opam_doc;
        opam_homepage;
        opam_with_test;
        optional;
        opens;
        path;
        ppx_kind;
        ppx_runtime_libraries;
        preprocess;
        preprocessor_deps;
        private_modules;
        profile;
        opam_only_deps;
        release_status;
        static;
        synopsis;
        description;
        npm_deps;
        available;
        virtual_modules;
        default_implementation;
        cram;
        license;
        extra_authors;
        ctypes;
        with_macos_security_framework;
        tests_deps;
      }

  let public_lib ?internal_name =
    internal ?dep_files:None ?dep_globs:None ?dep_globs_rec:None
    @@ fun public_name ->
    let internal_name =
      Option.value internal_name ~default:(convert_to_identifier public_name)
    in
    Public_library {internal_name; public_name}

  let private_lib =
    internal ?dep_files:None ?dep_globs:None ?dep_globs_rec:None @@ fun name ->
    Private_library name

  let public_exe ?internal_name =
    internal ?dep_files:None ?dep_globs:None ?dep_globs_rec:None
    @@ fun public_name ->
    let internal_name =
      Option.value internal_name ~default:(convert_to_identifier public_name)
    in
    Public_executable ({internal_name; public_name}, [])

  let public_exes ?internal_names =
    internal ?dep_files:None ?dep_globs:None ?dep_globs_rec:None
    @@ fun public_names ->
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
    internal ?dep_files:None ?dep_globs:None ?dep_globs_rec:None
    @@ fun internal_name -> Private_executable (internal_name, [])

  let private_exes =
    internal ?dep_files:None ?dep_globs:None ?dep_globs_rec:None
    @@ fun internal_names ->
    match internal_names with
    | [] -> invalid_argf "Target.private_exes: at least one name must be given"
    | head :: tail -> Private_executable (head, tail)

  let test ?(alias = "runtest") ?dep_files ?dep_globs ?dep_globs_rec ?locks
      ?enabled_if ?(dune_with_test = Always) ?(lib_deps = []) =
    (match (alias, enabled_if, locks) with
    | "", Some _, _ | "", _, Some _ ->
        invalid_arg
          "Target.tests: cannot specify enabled_if or locks without alias"
    | _ -> ()) ;
    (* Ensures that there is an alias in case there is test constraint *)
    let runtest_alias =
      if alias = "" then
        match dune_with_test with Always -> None | _ -> Some "runtest"
      else Some alias
    in
    let enabled_if =
      match dune_with_test with
      | Always -> enabled_if
      | Only_on_64_arch -> (
          let enabled_if_dune_with_test = Dune.(S "%{arch_sixtyfour}") in
          match enabled_if with
          | Some enabled_if ->
              Some Dune.(dand enabled_if_dune_with_test [enabled_if])
          | None -> Some enabled_if_dune_with_test)
      | Never -> Some Dune.(S "false")
    in
    internal ?dep_files ?dep_globs ?dep_globs_rec @@ fun test_name ->
    Test_executable
      {names = (test_name, []); runtest_alias; locks; enabled_if; lib_deps}

  let tests ?(alias = "runtest") ?dep_files ?dep_globs ?dep_globs_rec ?locks
      ?enabled_if ?(lib_deps = []) =
    (match (alias, enabled_if, locks) with
    | "", Some _, _ | "", _, Some _ ->
        invalid_arg
          "Target.tests: cannot specify enabled_if or locks without alias"
    | _ -> ()) ;
    let runtest_alias = if alias = "" then None else Some alias in
    internal ?dep_files ?dep_globs ?dep_globs_rec @@ fun test_names ->
    match test_names with
    | [] -> invalid_arg "Target.tests: at least one name must be given"
    | head :: tail ->
        Test_executable
          {names = (head, tail); runtest_alias; locks; enabled_if; lib_deps}

  let vendored_lib ?(released_on_opam = true) ?main_module
      ?(js_compatible = false) ?(npm_deps = []) name version =
    Some
      (Vendored
         {name; main_module; version; js_compatible; npm_deps; released_on_opam})

  let external_lib ?main_module ?opam ?(js_compatible = false) ?(npm_deps = [])
      name version =
    let opam =
      match opam with None -> Some name | Some "" -> None | Some _ as x -> x
    in
    Some (External {name; main_module; opam; version; js_compatible; npm_deps})

  let rec external_sublib ?main_module ?(js_compatible = false) ?(npm_deps = [])
      parent name =
    match parent with
    | External {opam; version; _} ->
        External {name; main_module; opam; version; js_compatible; npm_deps}
    | Opam_only _ ->
        invalid_arg
          "Target.external_sublib: parent must be a non-opam-only external lib"
    | Internal _ | Vendored _ ->
        invalid_arg "Target.external_sublib: parent must be an external lib"
    | Optional _ ->
        invalid_arg
          "Target.external_sublib: Optional should be used in dependency \
           lists, not when registering"
    | Re_export _ ->
        invalid_arg
          "Target.external_sublib: Re_export should be used in dependency \
           lists, not when registering"
    | Select _ ->
        invalid_arg
          "Target.external_sublib: Select should be used in dependency lists, \
           not when registering"
    | Open (target, module_name) ->
        Open (external_sublib target name, module_name)

  let external_sublib ?main_module ?js_compatible ?npm_deps parent name =
    match parent with
    | None -> invalid_arg "external_sublib cannot be called with no_target"
    | Some parent ->
        Some (external_sublib ?main_module ?js_compatible ?npm_deps parent name)

  let opam_only ?(can_vendor = true) name version =
    Some (Opam_only {name; version; can_vendor})

  let optional = function None -> None | Some target -> Some (Optional target)

  let open_ ?m target =
    let rec main_module_name = function
      | Internal {kind; _} -> (
          match kind with
          | Public_library {internal_name; _} | Private_library internal_name ->
              String.capitalize_ascii internal_name
          | Public_executable _ | Private_executable _ | Test_executable _ ->
              invalid_argf
                "Manifest.open_: cannot be used on executable and test targets \
                 (such as %s)"
                (name_for_errors target))
      | Optional target
      | Re_export target
      | Select {package = target; _}
      | Open (target, _) ->
          main_module_name target
      | Vendored {main_module = Some main_module; _}
      | External {main_module = Some main_module; _} ->
          String.capitalize_ascii main_module
      | Vendored {main_module = None; _} ->
          invalid_argf
            "Manifest.open_: cannot be used on vendored targets with no \
             specified main module (such as %s)"
            (name_for_errors target)
      | External {main_module = None; _} ->
          invalid_argf
            "Manifest.open_: cannot be used on external targets with no \
             specified main module (such as %s)"
            (name_for_errors target)
      | Opam_only _ ->
          invalid_argf
            "Manifest.open_: cannot be used on opam-only targets (such as %s)"
            (name_for_errors target)
    in
    let main_module_name = main_module_name target in
    let module_name =
      match m with
      | None -> main_module_name
      | Some m -> main_module_name ^ "." ^ String.capitalize_ascii m
    in
    Open (target, module_name)

  let open_ ?m = function None -> None | Some target -> Some (open_ ?m target)

  let open_if ?m condition target =
    if condition then open_ ?m target else target

  let re_export = function
    | None -> None
    | Some target -> Some (Re_export target)

  let select ~package ~source_if_present ~source_if_absent ~target =
    match package with
    | None -> None
    | Some package ->
        Some (Select {package; source_if_present; source_if_absent; target})

  let all_internal_deps internal =
    let extract_targets = function
      | PPS {targets; args = _} | Staged_PPS targets -> targets
    in
    List.concat_map extract_targets internal.preprocess
    @ internal.deps @ internal.opam_only_deps @ internal.ppx_runtime_libraries
end

type target = Target.t option

let no_target = None

let if_some = function None -> None | Some x -> x

let if_ condition target = if condition then target else None

type release = {version : string; url : Opam.url}

type tezt_target = {
  opam : string;
  lib_deps : target list;
  exe_deps : target list;
  js_deps : target list;
  dep_globs : string list;
  dep_globs_rec : string list;
  dep_files : string list;
  modules : string list;
  js_compatible : bool option;
  modes : Dune.mode list option;
  synopsis : string option;
  opam_with_test : with_test option;
  dune_with_test : with_test option;
  with_macos_security_framework : bool;
  flags : Flags.t option;
  dune : Dune.s_expr;
  tezt_local_test_lib : target;
  preprocess : Target.preprocessor list;
  preprocessor_deps : Target.preprocessor_dep list;
}

let tezt_targets_by_path : tezt_target String_map.t ref = ref String_map.empty

let tezt ~opam ~path ?js_compatible ?modes ?(lib_deps = []) ?(exe_deps = [])
    ?(js_deps = []) ?(dep_globs = []) ?(dep_globs_rec = []) ?(dep_files = [])
    ?synopsis ?opam_with_test ?dune_with_test
    ?(with_macos_security_framework = false) ?flags ?(dune = Dune.[])
    ?(preprocess = []) ?(preprocessor_deps = []) modules =
  if String_map.mem path !tezt_targets_by_path then
    invalid_arg
      ("cannot call Manifest.tezt twice for the same directory: " ^ path) ;
  let path_with_underscores =
    String.map (function '-' | '/' -> '_' | c -> c) path
  in
  (* [linkall] is used to ensure that the test executable is linked with [module_name] and [tezt]. *)
  let tezt_local_test_lib_name = path_with_underscores ^ "_tezt_lib" in
  let tezt_local_test_lib =
    Target.(
      private_lib
        ~path
        ~opam:""
        ?js_compatible
        ~deps:lib_deps
        ~modules
        ~linkall:true
        ?flags
        ~dune
        tezt_local_test_lib_name)
  in
  let tezt_target =
    {
      opam;
      lib_deps;
      exe_deps;
      js_deps;
      dep_globs;
      dep_files;
      dep_globs_rec;
      modules;
      js_compatible;
      modes;
      synopsis;
      opam_with_test;
      dune_with_test;
      with_macos_security_framework;
      flags;
      dune;
      tezt_local_test_lib;
      preprocess;
      preprocessor_deps;
    }
  in
  tezt_targets_by_path := String_map.add path tezt_target !tezt_targets_by_path ;
  tezt_local_test_lib

let register_tezt_targets ~make_tezt_exe =
  let tezt_test_libs = ref [] in
  let register_path path
      {
        opam;
        exe_deps;
        js_deps;
        dep_globs;
        dep_globs_rec;
        dep_files;
        modes;
        synopsis;
        opam_with_test;
        dune_with_test;
        with_macos_security_framework;
        flags;
        tezt_local_test_lib;
        preprocess;
        preprocessor_deps;
        lib_deps;
        _;
      } =
    tezt_test_libs := tezt_local_test_lib :: !tezt_test_libs ;
    let declare_exe ?js_compatible exe_name modes deps main =
      let (_ : Target.t option) =
        Target.test
          exe_name
          (* Tezts can be deselected from the runtest alias by setting
             the environment variable [RUNTEZTALIAS] to [false]. If
             this environment variable is unset, or set to any value
             other than [false], then they are selected.

             For more info on [%{env:VAR=VAL}] see
             https://dune.readthedocs.io/en/stable/concepts.html#variables *)
          ~enabled_if:Dune.[S "<>"; S "false"; S "%{env:RUNTEZTALIAS=true}"]
          ~path
          ~with_macos_security_framework
          ~opam
          ?synopsis
          ?js_compatible
          ?modes
            (* Instrument with sigterm handler, to ensure that coverage from
               Tezt worker processes are collected. *)
          ~bisect_ppx:With_sigterm
          ~deps:(tezt_local_test_lib :: deps)
          ~lib_deps
          ~dep_globs
          ~dep_globs_rec
          ~dep_files
          ~modules:[exe_name]
          ?opam_with_test
          ?dune_with_test
          ~preprocess
          ~preprocessor_deps
          ?flags
          ~dune:
            Dune.
              [
                targets_rule
                  [exe_name ^ ".ml"]
                  ~action:
                    [
                      S "with-stdout-to";
                      S "%{targets}";
                      [S "echo"; S ("let () = " ^ main ^ ".Test.run ()")];
                    ];
              ]
      in
      ()
    in
    match modes with
    | None -> declare_exe "main" None exe_deps "Tezt"
    | Some modes ->
        (match
           List.filter
             (function Dune.Byte | Native -> true | JS -> false)
             modes
         with
        | [] -> ()
        | modes -> declare_exe "main" (Some modes) exe_deps "Tezt") ;
        if List.mem Dune.JS modes then
          declare_exe
            "main_js"
            (Some [JS])
            js_deps
            "Tezt_js"
            ~js_compatible:true
  in
  String_map.iter register_path !tezt_targets_by_path ;
  make_tezt_exe !tezt_test_libs

let profile_deps : Target.t list String_map.t ref = ref String_map.empty

let add_dep_to_profile profile = function
  | None -> ()
  | Some dep ->
      let old =
        String_map.find_opt profile !profile_deps |> Option.value ~default:[]
      in
      profile_deps := String_map.add profile (dep :: old) !profile_deps

module Sub_lib = struct
  type documentation_entrypoint = Module | Page | Sub_lib

  type sub_lib = {
    name : string;
    synopsis : string option;
    documentation_type : documentation_entrypoint;
  }

  type container = sub_lib list ref

  let make_container () = ref []

  let make_documentation ~package ~public_name ~name ~synopsis = function
    | Some docs when not (docs = Dune.[[S "package"; S package]]) ->
        {name; synopsis; documentation_type = Page}
    | _ ->
        (* In the case that the documentation stanza is only a package declaration,
               we don't want the page to be used *)
        if String.contains public_name '.' then
          {
            name = String.capitalize_ascii name;
            synopsis;
            documentation_type = Sub_lib;
          }
        else
          {
            name = String.capitalize_ascii name;
            synopsis;
            documentation_type = Module;
          }

  let pp_documentation pp = function
    | {name; synopsis = None; documentation_type = Module} ->
        Format.fprintf pp "- {{!module-%s}%s}@." name name
    | {name; synopsis = Some synopsis; documentation_type = Module} ->
        Format.fprintf pp "- {{!module-%s}%s}: %s@." name name synopsis
    | {name; synopsis = None; documentation_type = Page} ->
        Format.fprintf
          pp
          "- {{!page-%s}%s}@."
          name
          (String.capitalize_ascii name)
    | {name; synopsis = Some synopsis; documentation_type = Page} ->
        Format.fprintf
          pp
          "- {{!page-%s}%s}: %s@."
          name
          (String.capitalize_ascii name)
          synopsis
    | {documentation_type = Sub_lib; _} ->
        (* In case it's a sub_lib, we don't link anything *) ()

  (* Prints all the registered libs of a container package. *)
  let pp_documentation_of_container ~header fmt registered_libs =
    Format.fprintf
      fmt
      "%s%a"
      header
      (Format.pp_print_list ~pp_sep:(fun _ () -> ()) pp_documentation)
    @@ List.sort
         (fun {name = name1; _} {name = name2; _} ->
           String.compare
             (String.capitalize_ascii name1)
             (String.capitalize_ascii name2))
         !registered_libs

  type maker = ?internal_name:string -> string Target.maker

  let sub_lib ~package_synopsis ~container ~package : maker =
   fun ?internal_name
       ?all_modules_except
       ?bisect_ppx
       ?c_library_flags
       ?conflicts
       ?deps
       ?dune
       ?flags
       ?foreign_archives
       ?foreign_stubs
       ?ctypes
       ?implements
       ?inline_tests
       ?inline_tests_deps
       ?js_compatible
       ?js_of_ocaml
       ?documentation
       ?linkall
       ?modes
       ?modules
       ?modules_without_implementation
       ?npm_deps
       ?ocaml
       ?opam
       ?opam_bug_reports
       ?opam_doc
       ?opam_homepage
       ?opam_with_test
       ?optional
       ?ppx_kind
       ?ppx_runtime_libraries
       ?preprocess
       ?preprocessor_deps
       ?private_modules
       ?profile
       ?opam_only_deps
       ?release_status
       ?static
       ?synopsis
       ?description
       ?time_measurement_ppx
       ?available
       ?virtual_modules
       ?default_implementation
       ?cram
       ?license
       ?extra_authors
       ?with_macos_security_framework
       ~path
       public_name ->
    if Option.is_some opam then
      invalid_arg "sub-libraries cannot be given custom `opam` parameters." ;
    let name =
      let s = Option.value ~default:public_name internal_name in
      String.map
        (function
          | '-' | '.' -> '_'
          | '/' ->
              invalid_arg ("octez library " ^ s ^ " name cannot contain \"/\"")
          | c -> c)
        s
    in
    let registered =
      make_documentation ~package ~public_name ~name ~synopsis documentation
    in
    if
      List.exists
        (fun registered -> String.equal registered.name name)
        !container
    then
      invalid_arg
        (Format.sprintf
           "%s already contains a library that would have the same internal \
            name, %s, as %s"
           package
           (Option.value ~default:public_name internal_name)
           name)
    else container := registered :: !container ;
    Target.public_lib
      (package ^ "." ^ public_name)
      ~path
      ~internal_name:name
      ~opam:package
      ~synopsis:package_synopsis
      ?all_modules_except
      ?bisect_ppx
      ?c_library_flags
      ?conflicts
      ?deps
      ?dune
      ?flags
      ?foreign_archives
      ?foreign_stubs
      ?ctypes
      ?implements
      ?inline_tests
      ?inline_tests_deps
      ?js_compatible
      ?js_of_ocaml
      ?documentation
      ?linkall
      ?modes
      ?modules
      ?modules_without_implementation
      ?npm_deps
      ?ocaml
      ?opam_bug_reports
      ?opam_doc
      ?opam_homepage
      ?opam_with_test
      ?optional
      ?ppx_kind
      ?ppx_runtime_libraries
      ?preprocess
      ?preprocessor_deps
      ?private_modules
      ?profile
      ?opam_only_deps
      ?release_status
      ?static
      ?description
      ?time_measurement_ppx
      ?available
      ?virtual_modules
      ?default_implementation
      ?cram
      ?license
      ?extra_authors
      ?with_macos_security_framework
end

(*****************************************************************************)
(*                                GENERATOR                                  *)
(*****************************************************************************)

let checks_done = ref false

(* Gather the list of generated files so that we can find out whether
   there are other files that we should have generated. *)
let generated_files = ref String_set.empty

let rec create_parent path =
  let parent = Filename.dirname path in
  if String.length parent < String.length path then (
    create_parent parent ;
    if not (Sys.file_exists parent) then Sys.mkdir parent 0o755)

let write_raw filename f =
  create_parent filename ;
  let outch = open_out filename in
  let fmt = Format.formatter_of_out_channel outch in
  match f fmt with
  | exception exn ->
      Format.pp_print_flush fmt () ;
      close_out outch ;
      raise exn
  | x ->
      Format.pp_print_flush fmt () ;
      close_out outch ;
      x

(* Write a file relatively to the root directory of the repository. *)
let write filename f =
  if !checks_done then
    failwith ("trying to generate " ^ filename ^ " after [check] was run") ;
  if String_set.mem filename !generated_files then
    failwith
      (filename ^ " is generated twice; did you declare the same library twice?") ;
  generated_files := String_set.add filename !generated_files ;
  write_raw filename f

let generate_dune (internal : Target.internal) =
  let libraries, ppx_runtime_libraries, empty_files_to_create =
    let empty_files_to_create = ref [] in
    let rec get_library (dep : Target.t) =
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
      | Re_export _ -> Dune.[G [S "re_export"; S name]]
      | Select {package = _; source_if_present; source_if_absent; target} ->
          Dune.
            [
              G [S "select"; S target; S "from"];
              [G [S name; S "->"; S source_if_present]];
              [G [S "->"; S source_if_absent]];
            ]
      | Open (target, _) -> get_library target
    in
    let libraries = List.map get_library internal.deps |> Dune.of_list in
    let ppx_runtime_libraries =
      List.map get_library internal.ppx_runtime_libraries |> Dune.of_list
    in
    (libraries, ppx_runtime_libraries, List.rev !empty_files_to_create)
  in
  let is_lib =
    match internal.kind with
    | Public_library _ | Private_library _ -> true
    | Public_executable _ | Private_executable _ | Test_executable _ -> false
  in
  let library_flags =
    if internal.linkall && is_lib then Some Dune.[S ":standard"; S "-linkall"]
    else None
  in
  let link_flags =
    let linkall = internal.linkall && not is_lib in
    let static_flags =
      if internal.static then
        [Dune.[S ":include"; S "%{workspace_root}/static-link-flags.sexp"]]
      else []
    in
    let macos_link_flags =
      if internal.with_macos_security_framework then
        [Dune.[S ":include"; S "%{workspace_root}/macos-link-flags.sexp"]]
      else []
    in
    let linkall_flags = if linkall then [Dune.[S "-linkall"]] else [] in
    List.concat [static_flags; macos_link_flags; linkall_flags] |> function
    | [] -> None
    | link_flags -> Some (Dune.[S ":standard"] :: link_flags)
  in
  let open_flags : Dune.s_expr list =
    internal.opens |> List.map (fun m -> Dune.(H [S "-open"; S m]))
  in
  let flags =
    match (internal.flags, open_flags) with
    | None, [] | Some {standard = true; rest = []}, [] -> None
    | flags, _ ->
        let flags =
          match flags with None -> Flags.standard () | Some flags -> flags
        in
        let flags =
          match flags.standard with
          | false -> flags.rest
          | true -> Dune.[S ":standard"] :: flags.rest
        in
        Some (flags @ open_flags)
  in

  let preprocess =
    let get_target_name target : string =
      match Target.names_for_dune target with
      | name, [] -> name
      | hd, (_ :: _ as tl) ->
          invalid_arg
            ("preprocessor target has multiple names, don't know which one to \
              choose: "
            ^ String.concat ", " (hd :: tl))
    in
    let make_preprocessors = function
      | (PPS {targets; args} : Target.preprocessor) ->
          Dune.pps ~args @@ List.map get_target_name targets
      | Staged_PPS targets ->
          Dune.staged_pps @@ List.map get_target_name targets
    in

    List.map make_preprocessors internal.preprocess
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
  let modules_without_implementation =
    match internal.modules_without_implementation with
    | [] -> None
    | _ :: _ as modules -> Some (Dune.of_atom_list modules)
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
    | Public_executable _, None ->
        (* Prevented by [Target.internal]. *)
        assert false
    | Public_executable _, (Some _ as opam) -> opam
    | Private_executable _, None -> None
    | Private_executable _, Some _opam ->
        (* private executable can't have a package stanza, but we still want the manifest to know about the package *)
        None
    | Public_library _, None ->
        (* Prevented by [Target.internal]. *)
        assert false
    | Public_library _, Some _ -> None
    | Private_library _, opam ->
        (* Private library can have an optional package.
           - No package means: global to the entire repo
           - A package means: private for the [opam] package only *)
        opam
    | Test_executable _, Some _ ->
        (* private executable can't have a package stanza, but we still want the manifest to know about the package *)
        None
    | Test_executable {runtest_alias = None; _}, None -> None
    | Test_executable {runtest_alias = Some _; _}, None ->
        (* Prevented by [Target.internal]. *)
        assert false
  in

  let instrumentation =
    let bisect_ppx =
      match internal.bisect_ppx with
      | Yes -> Some (Dune.backend "bisect_ppx")
      | With_sigterm ->
          Some (Dune.backend "bisect_ppx" ~args:["--bisect-sigterm"])
      | No -> None
    in
    let time_measurement_ppx =
      if internal.time_measurement_ppx then
        Some (Dune.backend "tezos-time-measurement")
      else None
    in
    List.filter_map (fun x -> x) [bisect_ppx; time_measurement_ppx]
  in
  let (kind : Dune.kind), internal_names, public_names =
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
    | Test_executable {names = head, tail; _} -> (Executable, head :: tail, [])
  in
  let get_virtual_target_name target =
    match Target.library_name_for_dune target with
    | Ok name -> name
    | Error name ->
        invalid_arg
          ("unsupported: ~implements on a target that is not a library (" ^ name
         ^ ")")
  in
  let documentation =
    match internal.documentation with
    | None -> Dune.E
    | Some docs -> Dune.(S "documentation" :: docs)
  in
  let ctypes = Option.map Ctypes.to_dune internal.ctypes in
  Dune.(
    executable_or_library
      kind
      internal_names
      ~public_names
      ?package
      ~instrumentation
      ~libraries
      ?library_flags
      ?link_flags
      ?flags
      ~inline_tests:internal.inline_tests
      ?inline_tests_deps:internal.inline_tests_deps
      ~optional:internal.optional
      ?ppx_kind:internal.ppx_kind
      ~ppx_runtime_libraries
      ~preprocess
      ~preprocessor_deps
      ~virtual_modules:internal.virtual_modules
      ?default_implementation:internal.default_implementation
      ?implements:(Option.map get_virtual_target_name internal.implements)
      ?modules
      ?modules_without_implementation
      ?modes:internal.modes
      ?foreign_archives:internal.foreign_archives
      ?foreign_stubs:internal.foreign_stubs
      ?c_library_flags:internal.c_library_flags
      ?ctypes
      ~private_modules:internal.private_modules
      ?js_of_ocaml:internal.js_of_ocaml
    :: documentation :: create_empty_files :: internal.dune)

(* [Explicitly_unreleased i]: this opam package was explicitly specified not to be released
   in the definition of its internal target [i].
   [Explicitly_released i]: this opam package was explicitly specified to be released
   in the definition of its internal target [i].
   [Transitively_released p]: this opam package needs to be released because package [p],
   which is released, depends on it. *)
type opam_release_status =
  | Explicitly_unreleased of Target.internal
  | Explicitly_released of Target.internal
  | Transitively_released of string
  | Auto

(* [height] is 1 for leaves, 1 + max dependency height for nodes. *)
type opam_dependency_graph_node = {
  mutable contain_executables : bool;
  mutable dependencies : String_set.t;
  mutable release_status : opam_release_status;
  mutable propagated : bool;
  mutable height : int;
}

(* This function relies on [check_circular_opam_deps] having been run prior. *)
let compute_opam_release_graph () : opam_dependency_graph_node String_map.t =
  (* First step:
     - compute [dependencies];
     - propagate [release_status] constraints inside opam packages.
     This step does not propagate [release_status] constraints to dependencies.
     This is done in a later step because we need all nodes to exist first. *)
  let graph =
    let node_of_internals package_name internals =
      let released_example = ref None in
      let unreleased_example = ref None in
      let executable internal =
        match internal.Target.kind with
        | Public_executable _ -> true
        | Public_library _ | Private_library _ | Private_executable _
        | Test_executable _ ->
            false
      in
      let add_status internal =
        match internal.Target.release_status with
        | Auto_opam -> ()
        | Unreleased | Experimental -> unreleased_example := Some internal
        | Released -> released_example := Some internal
      in
      List.iter add_status internals ;
      let release_status =
        match (!released_example, !unreleased_example) with
        | None, None -> Auto
        | Some internal, None -> Explicitly_released internal
        | None, Some internal -> Explicitly_unreleased internal
        | Some a, Some b ->
            error
              "In %s, %S has release status %s, and in %s, %S has release \
               status %s; those two targets cannot be in the same opam package \
               %S.\n"
              a.path
              (Target.kind_name_for_errors a.kind)
              (Target.show_release_status a.release_status)
              b.path
              (Target.kind_name_for_errors b.kind)
              (Target.show_release_status b.release_status)
              package_name ;
            exit 1
      in
      let dependencies =
        let add_dependency acc (target : Target.t) =
          match Target.get_internal target with
          | None | Some {opam = None; _} -> acc
          | Some {opam = Some pkg; _} ->
              if pkg = package_name then acc else String_set.add pkg acc
        in
        let add_tests_dependency acc (target : Target.t option) =
          match target with
          | Some target -> (
              match Target.get_internal target with
              | None | Some {opam = None; _} -> acc
              | Some {opam = Some pkg; _} ->
                  if pkg = package_name then acc else String_set.add pkg acc)
          | None -> acc
        in
        let add_internal_dependency acc internal =
          let tests_deps =
            List.fold_left add_tests_dependency acc internal.Target.tests_deps
          in
          String_set.union
            (List.fold_left
               add_dependency
               acc
               (Target.all_internal_deps internal))
            tests_deps
        in
        List.fold_left add_internal_dependency String_set.empty internals
      in
      let contain_executables = List.exists executable internals in
      {
        dependencies;
        release_status;
        propagated = false;
        height = 0;
        contain_executables;
      }
    in
    String_map.mapi node_of_internals !Target.by_opam
  in
  let with_node package_name k =
    match String_map.find_opt package_name graph with
    | None ->
        (* We added all opam packages to [graph] in the first step. *)
        assert false
    | Some node -> k node
  in
  (* Propagate [release_status] constraints down the dependency tree:
     if a package is released, its dependencies must be released too. *)
  let rec propagate_from parent_name parent_node =
    if not parent_node.propagated then
      match parent_node.release_status with
      | Explicitly_unreleased _ | Auto -> parent_node.propagated <- true
      | Explicitly_released _ | Transitively_released _ ->
          let propagate_to dependency_name =
            with_node dependency_name @@ fun dependency_node ->
            match dependency_node.release_status with
            | Explicitly_unreleased _ ->
                let rec output_reason = function
                  | Explicitly_unreleased internal ->
                      Printf.eprintf
                        "  because in %s, %S is explicitly not released\n"
                        internal.path
                        (Target.kind_name_for_errors internal.kind)
                  | Explicitly_released internal ->
                      Printf.eprintf
                        "  because in %s, %S is explicitly released\n"
                        internal.path
                        (Target.kind_name_for_errors internal.kind)
                  | Transitively_released reverse_dep ->
                      Printf.eprintf
                        "  because its reverse dependency %S is released\n"
                        reverse_dep ;
                      with_node reverse_dep @@ fun reverse_node ->
                      output_reason reverse_node.release_status
                  | Auto ->
                      (* Package is neither released nor released, why is there
                         a contradiction? *)
                      assert false
                in
                Printf.eprintf "Package %S is released\n" parent_name ;
                output_reason parent_node.release_status ;
                Printf.eprintf "Package %S is not released\n" dependency_name ;
                output_reason dependency_node.release_status ;
                error
                  "Released package %S cannot depend on unreleased package %S.\n"
                  parent_name
                  dependency_name ;
                exit 1
            | Explicitly_released _ | Transitively_released _ ->
                (* Either [dependency_node] has already been propagated,
                   or it hasn't but will be later since we visit all nodes
                   with [String_map.iter]. *)
                ()
            | Auto ->
                dependency_node.release_status <-
                  Transitively_released parent_name ;
                (* In case [dependency_node] has already been propagated,
                   we need to re-propagate it now that it has changed. *)
                dependency_node.propagated <- false ;
                propagate_from dependency_name dependency_node
          in
          String_set.iter propagate_to parent_node.dependencies ;
          parent_node.propagated <- true
  in
  String_map.iter propagate_from graph ;
  (* Compute heights. *)
  let rec compute_height node =
    if node.height <= 0 then
      let max_dep_height : int =
        String_set.fold
          (fun dependency_name height ->
            with_node dependency_name @@ fun dependency_node ->
            compute_height dependency_node ;
            max height dependency_node.height)
          node.dependencies
          0
      in
      node.height <- max_dep_height + 1
  in
  String_map.iter (fun _ -> compute_height) graph ;
  graph

let generate_dune_files () =
  Target.iter_internal_by_path @@ fun path internals ->
  let node_preload =
    List.concat_map
      (fun (internal : Target.internal) ->
        if internal.js_compatible then Target.node_preload internal.deps else [])
      internals
    |> List.sort_uniq compare
  in
  let dunes = List.map generate_dune internals in
  write (path // "dune") @@ fun fmt ->
  Format.fprintf fmt "%a@." (pp_do_not_edit ~comment_start:";") () ;
  let env = Env.empty in
  let env =
    match node_preload with
    | [] -> env
    | node_preload ->
        Env.add
          Any
          ~key:"env-vars"
          [S "NODE_PRELOAD"; S (String.concat "," node_preload)]
          env
  in
  let dunes = Dune.[Env.to_s_expr env] :: dunes in
  List.iteri
    (fun i dune ->
      if i <> 0 then Format.fprintf fmt "@." ;
      Format.fprintf fmt "%a@." Dune.pp dune)
    (List.filter (fun x -> not (Dune.is_empty x)) dunes)

(* Convert [target] into an opam dependency so that it can be added as a dependency
   in packages that depend on it.

   [for_package] is the name of the opam package that we are generating
   and in which the dependency will be added.
   If it is the same package as the one in which [target] belongs,
   [None] is returned, since a package cannot depend on itself
   and there is no need to.

   If [for_release] is [true], require [target]'s version to be
   exactly the same as [for_package]'s version, but only if [target] is internal.
   Also, if [for_release] is [true], use version constraints of vendored libraries.

   If [for_release] is [false] but [for_conflicts] is [true],
   ignore vendored libraries. *)
let rec as_opam_dependency ~for_release ~for_conflicts ~(for_package : string)
    ~with_test ~optional (target : Target.t) : Opam.dependency list =
  match target with
  | External {opam = None; _} -> []
  | Internal {opam = Some package; _} ->
      if package = for_package then []
      else
        let version =
          if for_release then Version.(Exactly Version) else Version.True
        in
        [{Opam.package; version; with_test; optional}]
  | Internal ({opam = None; _} as internal) ->
      (* If a target depends on a global "private" target, we must
         include its dependencies as well. *)
      let deps = Target.all_internal_deps internal in
      List.concat_map
        (as_opam_dependency
           ~for_release
           ~for_conflicts
           ~for_package
           ~with_test
           ~optional)
        deps
  | Vendored {name = package; version; _} ->
      if for_release then [{Opam.package; version; with_test; optional}]
      else if for_conflicts then []
      else [{Opam.package; version = True; with_test; optional}]
  | External {opam = Some opam; version; _}
  | Opam_only {name = opam; version; _} ->
      [{Opam.package = opam; version; with_test; optional}]
  | Optional target | Select {package = target; _} ->
      List.map
        (fun (dep : Opam.dependency) -> {dep with optional = true})
        (as_opam_dependency
           ~for_release
           ~for_conflicts
           ~for_package
           ~with_test
           ~optional
           target)
  | Re_export target | Open (target, _) ->
      as_opam_dependency
        ~for_release
        ~for_conflicts
        ~for_package
        ~with_test
        ~optional
        target

let as_opam_monorepo_opam_provided = function
  | Target.Opam_only {can_vendor = false; name; _} -> Some name
  | _ -> None

let dune_depend =
  (* versions 3.11.1 contains a fix for running inline_tests in parallel *)
  {
    Opam.package = "dune";
    version = Version.(at_least "3.11.1");
    with_test = Never;
    optional = false;
  }

let generate_opam ?release for_package (internals : Target.internal list) :
    Opam.t =
  let for_release = release <> None in
  let map l f = List.map f l in
  let depends, x_opam_monorepo_opam_provided =
    List.split @@ map internals
    @@ fun internal ->
    let with_test =
      match internal.kind with Test_executable _ -> Always | _ -> Never
    in
    let deps = Target.all_internal_deps internal in
    let x_opam_monorepo_opam_provided =
      List.filter_map as_opam_monorepo_opam_provided deps
    in
    let deps =
      List.concat_map
        (as_opam_dependency
           ~for_release
           ~for_conflicts:false
           ~for_package
           ~with_test
           ~optional:internal.optional)
        deps
    in
    (deps, x_opam_monorepo_opam_provided)
  in
  let depends = List.flatten depends in
  let x_opam_monorepo_opam_provided =
    List.flatten x_opam_monorepo_opam_provided
  in
  let depends =
    match
      List.filter_map
        (fun (internal : Target.internal) ->
          if internal.ocaml == default_ocaml_dependency then None
          else Some internal.ocaml)
        internals
    with
    | [] ->
        {
          Opam.package = "ocaml";
          version = default_ocaml_dependency;
          with_test = Never;
          optional = false;
        }
        :: depends
    | versions ->
        {
          Opam.package = "ocaml";
          version = Version.and_list versions;
          with_test = Never;
          optional = false;
        }
        :: depends
  in
  let depends = dune_depend :: depends in
  let depends =
    (* Remove duplicate dependencies but when one occurs twice,
       only keep {with-test} if both dependencies had it. *)
    let merge_with_tests a b =
      match (a, b) with
      | Never, _ | _, Never -> Never
      | Always, Always -> Always
      | Only_on_64_arch, Only_on_64_arch -> Only_on_64_arch
      | Only_on_64_arch, Always | Always, Only_on_64_arch ->
          (* Example: a test A depends on a lib L (this is an Always),
             and another test B that can only be ran on 64-bit also depends on lib L
             (this is an Only_on_64_arch). In that case we return Always since the
             dependency is needed for A even on 32-bit. *)
          Always
    in
    let merge (a : Opam.dependency) (b : Opam.dependency) =
      {a with with_test = merge_with_tests a.with_test b.with_test}
    in
    deduplicate_list ~merge (fun {Opam.package; _} -> package) depends
  in
  let merge_available a b =
    match (a, b) with
    | No_32, No_32 -> No_32
    | No_arm, No_arm -> No_arm
    | No_x86, No_x86 -> No_x86
    | No_ppc, No_ppc -> No_ppc
    | Never, _ | _, Never -> Never
    | Always, Always -> Always
    | N_ary_and available_list_1, N_ary_and available_list_2 ->
        N_ary_and
          (deduplicate_list
             string_of_available
             (available_list_1 @ available_list_2))
    | N_ary_and available_list, a | a, N_ary_and available_list ->
        N_ary_and (deduplicate_list string_of_available (a :: available_list))
    | a1, a2 -> N_ary_and [a1; a2]
  in
  let available =
    List.fold_left
      (fun (available : available) (internal : Target.internal) ->
        merge_available available internal.available)
      Always
      internals
  in
  let conflicts =
    List.of_seq @@ Opam_dependency_set.to_seq
    @@ List.fold_left
         (fun set c -> Opam_dependency_set.add c set)
         Opam_dependency_set.empty
    @@ List.flatten @@ map internals
    @@ fun internal ->
    List.concat_map
      (as_opam_dependency
         ~for_release
         ~for_conflicts:true
         ~for_package
         ~with_test:Never
         ~optional:false)
      internal.conflicts
  in
  let get_consistent_value ~name ?default
      (get : Target.internal -> string option) =
    match
      List.filter_map get internals |> String_set.of_list |> String_set.elements
    with
    | [] -> (
        match default with
        | None ->
            error "No %s declared for package %s\n" name for_package ;
            ""
        | Some value -> value)
    | [value] -> value
    | value :: _ :: _ as list ->
        error
          "Package %s was declared with multiple different values for %s: %s\n"
          for_package
          name
          (String.concat ", " (List.map (Format.sprintf "%S") list)) ;
        value
  in
  let description =
    let descriptions =
      List.filter_map Fun.id @@ map internals
      @@ fun internal -> internal.description
    in
    match descriptions with
    | [] -> None
    | descriptions -> Some (String.concat "\n\n" descriptions)
  in
  let build =
    let build : Opam.build_instruction =
      {
        command = [S "dune"; S "build"; S "-p"; A "name"; S "-j"; A "jobs"];
        with_test = Never;
      }
    in
    let with_test =
      match internals with [] -> Never | head :: _ -> head.opam_with_test
    in
    let runtests =
      let get_alias (internal : Target.internal) =
        match internal.kind with
        | Test_executable {runtest_alias; _} -> runtest_alias
        | Public_library _ | Private_library _ | Public_executable _
        | Private_executable _ ->
            None
      in
      let make_runtest alias : Opam.build_instruction list =
        match (with_test, alias) with
        | Never, _ -> []
        | _, "runtest" ->
            (* Special case: Dune has a special command to run this alias. *)
            [
              {
                command =
                  [S "dune"; S "runtest"; S "-p"; A "name"; S "-j"; A "jobs"];
                with_test;
              };
            ]
        | _ ->
            [
              {
                command =
                  [
                    S "dune";
                    S "build";
                    S ("@" ^ alias);
                    S "-p";
                    A "name";
                    S "-j";
                    A "jobs";
                  ];
                with_test;
              };
            ]
      in
      internals |> List.filter_map get_alias
      (* We have to add [runtest] because some targets define the [runtest]
         alias manually and use [~alias:""]. *)
      |> (fun l -> "runtest" :: l)
      |> String_set.of_list |> String_set.elements
      |> List.concat_map make_runtest
    in
    {
      Opam.command = [S "rm"; S "-r"; S "vendors"; S "contrib"];
      with_test = Never;
    }
    :: build :: runtests
  in
  let licenses =
    match
      List.filter_map (fun internal -> internal.Target.license) internals
      |> List.sort_uniq String.compare
    with
    | [] -> ["MIT"]
    | licenses -> licenses
  in
  (* Used to remove any duplicates in extra_authors. *)
  let rec deduplicate known acc = function
    | [] -> List.rev acc
    | author :: tail ->
        if String_set.mem author known then deduplicate known acc tail
        else deduplicate (String_set.add author known) (author :: acc) tail
  in
  let extra_authors =
    List.concat_map (fun internal -> internal.Target.extra_authors) internals
    |> deduplicate String_set.empty []
  in
  {
    maintainer = "contact@tezos.com";
    authors = "Tezos devteam" :: extra_authors;
    homepage =
      get_consistent_value
        ~name:"opam_homepage"
        (fun x -> x.opam_homepage)
        ~default:"https://www.tezos.com/";
    doc =
      get_consistent_value ~name:"opam_doc" (fun x -> x.opam_doc) ~default:"";
    bug_reports =
      get_consistent_value
        ~name:"opam_bug_reports"
        (fun x -> x.opam_bug_reports)
        ~default:"https://gitlab.com/tezos/tezos/issues";
    dev_repo = "git+https://gitlab.com/tezos/tezos.git";
    licenses;
    depends;
    conflicts;
    build;
    available;
    synopsis = get_consistent_value ~name:"synopsis" (fun x -> x.synopsis);
    url = Option.map (fun {url; _} -> url) release;
    description;
    x_opam_monorepo_opam_provided;
  }

let generate_opam_meta_package opam_release_graph add_to_meta_package : Opam.t =
  let depends1 : Opam.dependency list =
    Fun.flip List.concat_map add_to_meta_package @@ fun target ->
    match target with
    | None -> []
    | Some target ->
        as_opam_dependency
          ~for_release:true
          ~for_conflicts:false
          ~for_package:"octez"
          ~with_test:Never
          ~optional:false
          target
  in
  (* Include all packages that are explicitly released. *)
  let depends2 =
    Fun.flip List.filter_map (String_map.bindings opam_release_graph)
    @@ fun (package, node) ->
    match node.release_status with
    | Explicitly_unreleased _ | Auto | Transitively_released _ ->
        (* No need to put transitively released packages explicitly in the meta-package.
           They are, by definition, transitive dependencies of explicitly released
           opam packages, which we do put in the meta-package. *)
        None
    | Explicitly_released _ ->
        Some
          {
            Opam.package;
            version = Exactly Version;
            with_test = Never;
            optional = false;
          }
  in
  {
    maintainer = "contact@tezos.com";
    authors = ["Tezos devteam"];
    homepage = "https://www.tezos.com/";
    doc = "https://tezos.gitlab.io";
    bug_reports = "https://gitlab.com/tezos/tezos/issues";
    dev_repo = "git+https://gitlab.com/tezos/tezos.git";
    licenses = ["MIT"];
    depends = depends1 @ depends2;
    conflicts = [];
    build = [];
    available = Always;
    synopsis = "Main virtual package for Octez, an implementation of Tezos";
    url = None;
    description = None;
    x_opam_monorepo_opam_provided = [];
  }

let generate_opam_files () =
  (* We store all opam files in the predefined directory <ROOT>/opam.
     That way, `opam pin` can work out of the box. *)
  Target.iter_internal_by_opam @@ fun package internals ->
  let opam = generate_opam package internals in
  write ("opam/" ^ package ^ ".opam") @@ fun fmt ->
  pp_do_not_edit ~comment_start:"#" fmt () ;
  Opam.pp fmt opam

let generate_opam_files_for_release packages_dir opam_release_graph
    add_to_meta_package release =
  let write_opam package opam =
    let opam_filename =
      packages_dir // package // (package ^ "." ^ release.version) // "opam"
    in
    (* We don't use [write] here because we don't want these opam files
       to be considered by the [check_for_non_generated_files] check *)
    write_raw opam_filename @@ fun fmt -> Opam.pp fmt opam
  in
  ( Target.iter_internal_by_opam @@ fun package internal_pkgs ->
    match String_map.find_opt package opam_release_graph with
    | None ->
        (* Graph contains all internal packages. *)
        assert false
    | Some node -> (
        match node.release_status with
        | Explicitly_unreleased _ | Auto -> ()
        | Explicitly_released _ | Transitively_released _ ->
            write_opam package (generate_opam ~release package internal_pkgs))
  ) ;
  write_opam
    "octez"
    (generate_opam_meta_package opam_release_graph add_to_meta_package)

(* Bumping the dune lang version can result in different dune stanza
   semantic and could require changes to the generation logic. *)
let dune_lang_version = "3.7"

let generate_dune_project_files () =
  write "dune-project" @@ fun fmt ->
  Format.fprintf fmt "(lang dune %s)@." dune_lang_version ;
  Format.fprintf fmt "(formatting (enabled_for ocaml))@." ;
  Format.fprintf fmt "(cram enable)@." ;
  Format.fprintf fmt "(using ctypes 0.3)@." ;
  Format.fprintf fmt "(using menhir 2.1)@." ;
  ( Target.iter_internal_by_opam @@ fun package internals ->
    let has_public_target =
      List.exists
        (fun (i : Target.internal) ->
          match i.kind with
          | Public_library _ | Public_executable _ -> true
          | Private_library _ -> false
          | Private_executable _ -> false
          | Test_executable _ -> false)
        internals
    in
    let allow_empty = if not has_public_target then "(allow_empty)" else "" in
    Format.fprintf fmt "(package (name %s)%s)@." package allow_empty ) ;
  pp_do_not_edit ~comment_start:";" fmt ()

let generate_package_json_file () =
  let l = ref [] in
  let add npm = if not (List.mem npm !l) then l := npm :: !l in
  let rec collect (target : Target.t) =
    match target with
    | External {npm_deps; _} | Vendored {npm_deps; _} | Internal {npm_deps; _}
      ->
        List.iter add npm_deps
    | Optional package
    | Re_export package
    | Select {package; _}
    | Open (package, _) ->
        collect package
    | Opam_only _ -> ()
  in
  Target.iter_internal_by_path (fun _path internals ->
      List.iter
        (fun (internal : Target.internal) ->
          List.iter add internal.npm_deps ;
          List.iter collect internal.deps)
        internals) ;
  let pp_version_atom fmt = function
    | Version.V x -> Format.fprintf fmt "%s" x
    | Version ->
        invalid_arg "[Version] cannot be used to constrain Npm packages."
  in
  let rec pp_version_constraint ~in_and fmt = function
    | Version.True ->
        invalid_arg "[True] cannot be used to constrain Npm packages."
    | False -> invalid_arg "[False] cannot be used to constrain Npm packages."
    | Not _ -> invalid_arg "[Not] cannot be used to constrain Npm packages."
    | Exactly version -> Format.fprintf fmt "%a" pp_version_atom version
    | Different_from version ->
        Format.fprintf fmt "!= %a" pp_version_atom version
    | At_least version -> Format.fprintf fmt ">=%a" pp_version_atom version
    | More_than version -> Format.fprintf fmt ">%a" pp_version_atom version
    | At_most version -> Format.fprintf fmt "<=%a" pp_version_atom version
    | Less_than version -> Format.fprintf fmt "<%a" pp_version_atom version
    | And (a, b) ->
        Format.fprintf
          fmt
          "%a %a"
          (pp_version_constraint ~in_and:true)
          a
          (pp_version_constraint ~in_and:true)
          b
    | Or (a, b) ->
        if in_and then
          invalid_arg
            "Npm version constraint don't allow [Or] nested inside [And]" ;
        Format.fprintf
          fmt
          "%a || %a"
          (pp_version_constraint ~in_and:false)
          a
          (pp_version_constraint ~in_and:false)
          b
  in
  let pp_dep fmt (npm : Npm.t) =
    match npm.version_or_path with
    | Version version ->
        Format.fprintf
          fmt
          {|    "%s": "%a"|}
          npm.package
          (pp_version_constraint ~in_and:false)
          version
    | Path path -> Format.fprintf fmt {|    "%s": "file:%s"|} npm.package path
  in
  write "package.json" @@ fun fmt ->
  Format.fprintf
    fmt
    {|{
  "DO NOT EDIT": "This file was automatically generated, edit file manifest/main.ml instead",
  "private": true,
  "type": "commonjs",
  "description": "n/a",
  "license": "n/a",
  "dependencies": {
%a
  }
}
|}
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@.")
       pp_dep)
    (List.sort compare !l)

let generate_executable_list filename release_status_to_list =
  write filename @@ fun fmt ->
  Fun.flip List.iter !Target.registered @@ fun (internal : Target.internal) ->
  if internal.release_status = release_status_to_list then
    match internal.kind with
    | Public_library _ | Private_library _ | Private_executable _
    | Test_executable _ ->
        ()
    | Public_executable ne_list ->
        Fun.flip List.iter (Ne_list.to_list ne_list)
        @@ fun (full_name : Target.full_name) ->
        Format.fprintf fmt "%s@." full_name.public_name

let generate_workspace env dune =
  let pp_dune fmt dune =
    if not (Dune.is_empty dune) then Format.fprintf fmt "@.%a@." Dune.pp dune
  in
  write "dune-workspace" @@ fun fmt ->
  Format.fprintf fmt "(lang dune %s)@." dune_lang_version ;
  pp_dune fmt Dune.[Env.to_s_expr env] ;
  pp_dune fmt dune ;
  Format.fprintf fmt "@." ;
  pp_do_not_edit ~comment_start:";" fmt ()

let find_opam_and_dune_files =
  let root = "." in
  let rec loop acc dir =
    let dir_contents = Sys.readdir (root // dir) in
    let add_item acc filename =
      let full_filename = dir // filename in
      if
        filename = "dune"
        || Filename.extension filename = ".opam"
        || filename = "dune-project"
        || filename = "dune-workspace"
      then String_set.add full_filename acc
      else if filename.[0] = '.' || filename.[0] = '_' then acc
      else if
        try Sys.is_directory (root // dir // filename)
        with Sys_error _ -> false
      then loop acc full_filename
      else acc
    in
    Array.fold_left add_item acc dir_contents
  in
  loop String_set.empty

let check_for_non_generated_files ~remove_extra_files
    ?(exclude = fun _ -> false) () =
  let all_files = find_opam_and_dune_files "" in

  let all_non_excluded_files =
    String_set.filter (fun x -> not (exclude x)) all_files
  in
  let error_generated_and_excluded =
    String_set.filter exclude !generated_files
  in
  String_set.iter
    (error "%s: generated but is excluded\n%!")
    error_generated_and_excluded ;
  let error_not_generated =
    String_set.diff all_non_excluded_files !generated_files
  in
  String_set.iter
    (fun file ->
      if remove_extra_files then (
        info "%s: exists but was not generated, removing it.\n%!" file ;
        Sys.remove file)
      else error "%s: exists but was not generated\n%!" file)
    error_not_generated ;
  if
    not
      ((remove_extra_files || String_set.is_empty error_not_generated)
      && String_set.is_empty error_generated_and_excluded)
  then
    error
      "Please modify manifest/main.ml to either generate the above file(s)\n\
       or declare them in the 'exclude' function (but not both).\n\
       If this file is a leftover from some previous work on the build\n\
       system then simply remove it."

let check_js_of_ocaml () =
  let internal_name ({kind; path; _} : Target.internal) =
    match kind with
    | Public_library {public_name; _} -> public_name
    | Private_library internal_name -> internal_name
    | Public_executable ({public_name = name; _}, _) -> name
    | Private_executable (name, _) | Test_executable {names = name, _; _} ->
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
  let rec check_target ~used_by (target : Target.t) =
    match target with
    | External {js_compatible; name; _} ->
        if not js_compatible then missing_jsoo_for_target ~used_by name
    | Vendored {js_compatible; name; _} ->
        if not js_compatible then missing_jsoo_for_target ~used_by name
    | Internal ({js_compatible; _} as internal) ->
        if not js_compatible then
          missing_jsoo_for_target ~used_by (internal_name internal)
    | Optional package
    | Re_export package
    | Select {package; _}
    | Open (package, _) ->
        check_target ~used_by package
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
    error
      "The following targets use `(modes js)` and are missing \
       `~js_compatible:true`\n" ;
    String_set.iter (fun name -> info "- %s\n" name) !missing_with_js_mode) ;
  if String_map.cardinal !missing_from_target > 0 then (
    jsoo_ok := false ;
    error
      "The following targets are not `~js_compatible` but their dependant \
       expect them to be\n" ;
    String_map.iter
      (fun k v -> List.iter (fun v -> info "- %s used by %s\n" v k) v)
      !missing_from_target)

(* This check returns all circular opam deps, always reporting the
   shortest path.

   For every two targets [A0] and [Ax] in package [A], we search for
   chains of dependencies of the form [A0 -> B0 -> -> Bn -> Ax] where
   [B0 .. Bn] do not belong to Package A. If such paths exist, we
   report one path with the minimum length. *)
let check_circular_opam_deps () =
  let name i = Target.name_for_errors (Internal i) in
  let deps_of (t : Target.internal) =
    List.filter_map Target.get_internal (Target.all_internal_deps t)
  in
  Target.iter_internal_by_opam @@ fun this_package internals ->
  let error_header = ref true in
  let report_circular_dep pkg (paths : Target.internal list) =
    if !error_header then (
      error "Circular opam dependency for %s:\n" this_package ;
      error_header := false) ;
    info "- %s\n" (String.concat " -> " (List.map name (pkg :: paths)))
  in
  list_iter internals @@ fun internal_from_this_package ->
  let to_visit : Target.internal Queue.t = Queue.create () in
  let shortest_path : (Target.kind, Target.internal list) Hashtbl.t =
    Hashtbl.create 17
  in
  (* Push to the queue all direct dependencies to other
     packages. Dependencies within the same package are ignored
     because they will never result in a minimum paths. *)
  let () =
    list_iter (deps_of internal_from_this_package)
    @@ fun (dep : Target.internal) ->
    if dep.opam <> Some this_package then (
      Hashtbl.add shortest_path dep.kind [dep] ;
      Queue.push dep to_visit)
  in
  while not (Queue.is_empty to_visit) do
    let elt = Queue.take to_visit in
    let elt_path =
      Option.value ~default:[] (Hashtbl.find_opt shortest_path elt.kind)
    in
    list_iter (deps_of elt) (fun (dep : Target.internal) ->
        if not (Hashtbl.mem shortest_path dep.kind) then (
          let path = dep :: elt_path in
          Hashtbl.add shortest_path dep.kind path ;
          if
            dep.opam = Some this_package
            && List.exists
                 (fun (i : Target.internal) ->
                   match (i.opam : string option) with
                   | None ->
                       (* Targets that do not have a package are private and act
                          as if they belonged to all packages.
                          If the shortest path from package A to package A only goes
                          through package A or private packages, it thus only goes
                          through package A and is not a circular dependency. *)
                       false
                   | Some p -> p <> this_package)
                 path
          then report_circular_dep internal_from_this_package (List.rev path)
          else Queue.push dep to_visit))
  done

let check_opam_with_test_consistency () =
  Target.iter_internal_by_opam @@ fun this_package internals ->
  match internals with
  | [] -> ()
  | {opam_with_test = expected; _} :: tail -> (
      match
        List.find_map
          (fun ({opam_with_test; _} : Target.internal) ->
            if opam_with_test <> expected then Some opam_with_test else None)
          tail
      with
      | None -> ()
      | Some bad ->
          error
            "Opam package %s contains targets with different values for \
             ~opam_with_test: %s and %s.\n"
            this_package
            (show_with_test expected)
            (show_with_test bad))

let usage_msg = "Usage: " ^ Sys.executable_name ^ " [OPTIONS]"

let packages_dir, release, remove_extra_files, manifezt =
  let packages_dir = ref "packages" in
  let url = ref "" in
  let sha256 = ref "" in
  let sha512 = ref "" in
  let remove_extra_files = ref false in
  let version = ref "" in
  let manifezt = ref false in
  let anonymous_args = ref [] in
  let anon_fun arg = anonymous_args := arg :: !anonymous_args in
  let spec =
    Arg.align
      [
        ( "--packages-dir",
          Arg.Set_string packages_dir,
          "<PATH> Path of the 'packages' directory where to write opam files \
           for release (default: 'packages')" );
        ("--url", Arg.Set_string url, "<URL> Set url for release");
        ("--sha256", Arg.Set_string sha256, "<HASH> Set sha256 for release");
        ("--sha512", Arg.Set_string sha512, "<HASH> Set sha512 for release");
        ( "--release",
          Arg.Set_string version,
          "<VERSION> Generate opam files for release instead, for VERSION" );
        ( "--remove-extra-files",
          Arg.Set remove_extra_files,
          " Remove files that are neither generated nor excluded" );
        ( "--manifezt",
          Arg.Set manifezt,
          " Expect a list of modified files on the command-line. Output a TSL \
           expression to select Tezt tests that are impacted by those changes, \
           then exit without generating any file." );
        ( "--",
          Arg.Rest anon_fun,
          " Assume the remaining arguments are anonymous arguments." );
      ]
  in
  Arg.parse spec anon_fun usage_msg ;
  let release =
    match (!url, !version) with
    | "", "" -> None
    | "", _ | _, "" ->
        prerr_endline
          "Error: either --url and --release must be specified, or none of \
           them." ;
        exit 1
    | url, version ->
        let sha256, sha512 =
          match (!sha256, !sha512, version) with
          | sha256, sha512, "dev" ->
              ( (if sha256 = "" then None else Some sha256),
                if sha512 = "" then None else Some sha512 )
          | "", _, _ | _, "", _ ->
              prerr_endline
                "Error: when making a release other than 'dev', --sha256 and \
                 --sha512 are mandatory." ;
              exit 1
          | sha256, sha512, _ -> (Some sha256, Some sha512)
        in
        Some {version; url = {url; sha256; sha512}}
  in
  let manifezt =
    match (!manifezt, !anonymous_args) with
    | false, [] -> None
    | false, head :: _ ->
        prerr_endline ("Error: don't know what to do with: " ^ head) ;
        exit 1
    | true, files -> Some files
  in
  (!packages_dir, release, !remove_extra_files, manifezt)

let print_opam_job_rules fmt batch_index pipeline_type marge_restriction =
  Format.fprintf
    fmt
    {|@..rules_template__trigger_%s_opam_batch_%d:
  rules:
    # Run on scheduled builds.
    - if: '$CI_PIPELINE_SOURCE == "schedule" && $TZ_SCHEDULE_KIND == "EXTENDED_TESTS"'
      when: delayed
      start_in: %d minutes
    # Run when there is label on the merge request
    - if: '$CI_MERGE_REQUEST_LABELS =~ /(?:^|[,])ci--opam(?:$|[,])/'
      when: delayed
      start_in: %d minutes
    # Run on merge requests when opam changes are detected.
    - if: '%s'
      changes:
        - "**/dune"
        - "**/dune.inc"
        - "**/*.dune.inc"
        - "**/dune-project"
        - "**/dune-workspace"
        - "**/*.opam"
        - .gitlab/ci/jobs/packaging/opam:prepare.yml
        - .gitlab/ci/jobs/packaging/opam_package.yml
        - manifest/manifest.ml
        - manifest/main.ml
        - scripts/opam-prepare-repo.sh
        - scripts/version.sh
      when: delayed
      start_in: %d minutes
    - when: never # default
|}
    pipeline_type
    batch_index
    batch_index
    batch_index
    marge_restriction
    batch_index

let generate_opam_ci opam_release_graph =
  (* We only need to test released packages, since those are the only one
     that will need to pass the public Opam CI. *)
  let contain_executables package =
    match String_map.find_opt package opam_release_graph with
    | None -> false
    | Some node -> node.contain_executables
  in
  let released_packages, unreleased_packages =
    List.partition
      (fun (_, node) ->
        match node.release_status with
        | Explicitly_unreleased _ | Auto -> false
        | Explicitly_released _ | Transitively_released _ -> true)
      (String_map.bindings opam_release_graph)
  in
  (* Due to technical limitations of the CI, we want to avoid starting
     all opam package tests at the same time. Instead, we start them by batch,
     with a delay between each batch. To this end we sort jobs by height
     in the dependency tree, then split the resulting list. The idea is that
     the higher in the dependency tree a job is, the longer it takes to run,
     and the sooner we want it to start. *)
  let released_packages =
    let by_height_and_name (name1, node1) (name2, node2) =
      (* Smaller heights first, to put them in the first batches. *)
      let c = Int.compare node2.height node1.height in
      if c <> 0 then c
      else
        (* For more stability (better diffs) we also sort by name. *)
        String.compare name1 name2
    in
    List.sort by_height_and_name released_packages
  in
  let batch_count = 7 in
  let package_count = List.length released_packages in
  let released_packages =
    (* We want each batch to contain about [package_count / batch_count].
       But we want to round up so that we don't have more than [batch_count] batches. *)
    let batch_size = (package_count + batch_count - 1) / batch_count in
    List.mapi (fun i (pkg, _) -> (1 + (i / batch_size), pkg)) released_packages
  in
  let unreleased_packages =
    List.map (fun (pkg, _) -> (0, pkg)) unreleased_packages
  in
  (* Merge and sort by name for nicer diffs. *)
  let packages =
    let l =
      List.map
        (fun (a, name) ->
          if contain_executables name then (a, name, true) else (a, name, false))
        (released_packages @ unreleased_packages)
    in
    let by_name (_, a, _) (_, b, _) = String.compare a b in
    List.sort by_name l
  in
  (* Now [packages] is a list of [batch_index, package_name]
     where [batch_index] is 0 for packages that we do not need to test. *)
  write ".gitlab/ci/jobs/packaging/opam_package.yml" @@ fun fmt ->
  pp_do_not_edit ~comment_start:"#" fmt () ;
  (* Output one template per batch. *)
  let marge_restriction_exec =
    "$CI_PIPELINE_SOURCE == \"merge_request_event\""
  in
  let marge_restriction_all =
    marge_restriction_exec ^ " && $GITLAB_USER_LOGIN == \"nomadic-margebot\""
  in
  for batch_index = 1 to batch_count do
    print_opam_job_rules fmt batch_index "exec" marge_restriction_exec ;
    print_opam_job_rules fmt batch_index "all" marge_restriction_all
  done ;

  (* Output one job per released package. *)
  let output_job (batch_index, package_name, is_executable) =
    if batch_index > 0 then
      Format.fprintf
        fmt
        {|@.opam:%s:
  extends:
    - .opam_template
    - .rules_template__trigger_%s_opam_batch_%d
  variables:
    package: %s
|}
        package_name
        (if is_executable then "exec" else "all")
        batch_index
        package_name
    else Format.fprintf fmt "@.# Ignoring unreleased package %s.\n" package_name
  in
  List.iter output_job packages

let generate_profiles ~default_profile =
  let deps : Version.constraints String_map.t String_map.t ref =
    (* [!deps |> String_map.find profile |> String_map.find pkg]
       are the version constraints for package [pkg] in profile [profile]. *)
    ref String_map.empty
  in
  let conflicts : Version.constraints String_map.t String_map.t ref =
    ref String_map.empty
  in
  let rec add_target_to deps profile = function
    | Target.Internal _ ->
        (* No need to recurse on dependencies because we'll iterate on all internal
           dependencies anyway. *)
        ()
    | Vendored _ ->
        (* It is the developer's duty to integrate the dependencies of
           vendored libraries, manually, into the generated .opam / lock file. *)
        ()
    | External {opam = Some name; version; _} | Opam_only {name; version; _} ->
        let profile_deps =
          String_map.find_opt profile !deps
          |> Option.value ~default:String_map.empty
        in
        let version =
          match String_map.find_opt name profile_deps with
          | None -> version
          | Some old_version ->
              (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5434
                 This comparison is wrong as it compares the terms as
                 a whole. It results in duplicated
                 versions. Additionally, for the conflicts, the `||`
                 operator must be used. *)
              if old_version <> version then Version.(version && old_version)
              else version
        in
        let profile_deps = String_map.add name version profile_deps in
        deps := String_map.add profile profile_deps !deps
    | External {opam = None; _} ->
        (* This corresponds to libs from the stdlib, like dynlink, compiler-libs etc.
           There is no opam package to add to the lock file. *)
        ()
    | Optional target
    | Re_export target
    | Select {package = target; _}
    | Open (target, _) ->
        add_target_to deps profile target
  in
  String_map.iter
    (fun profile deps_to_add ->
      List.iter (add_target_to deps profile) deps_to_add)
    !profile_deps ;
  ( Target.iter_internal_by_path @@ fun _ internals ->
    list_iter internals @@ fun internal ->
    let profile = internal.profile |> Option.value ~default:default_profile in
    list_iter (Target.all_internal_deps internal) (add_target_to deps profile) ;
    list_iter internal.conflicts (add_target_to conflicts profile) ) ;
  let generate_profile profile (profile_deps, profile_conflicts) =
    let depends =
      String_map.bindings profile_deps
      |> List.map @@ fun (package, version) ->
         {Opam.package; version; with_test = Never; optional = false}
    in
    let depends = dune_depend :: depends in
    let conflicts =
      String_map.bindings profile_conflicts
      |> List.map @@ fun (package, version) ->
         {Opam.package; version; with_test = Never; optional = false}
    in
    let opam : Opam.t =
      {
        maintainer = "contact@tezos.com";
        authors = ["Tezos devteam"];
        homepage = "https://www.tezos.com/";
        doc = "";
        bug_reports = "https://gitlab.com/tezos/tezos/issues";
        dev_repo = "git+https://gitlab.com/tezos/tezos.git";
        licenses = ["MIT"];
        depends;
        conflicts;
        build = [];
        available = Always;
        synopsis =
          Printf.sprintf
            "Virtual package depending on Octez dependencies (profile: %s)"
            profile;
        url = None;
        description =
          Some
            (Printf.sprintf
               "Install this package to install all dependencies needed to \
                build the subset of Octez denoted by profile %s."
               profile);
        x_opam_monorepo_opam_provided = [];
      }
    in
    write ("opam/virtual/" ^ profile ^ ".opam") @@ fun fmt ->
    pp_do_not_edit ~comment_start:"#" fmt () ;
    Opam.pp fmt opam
  in
  let merged =
    let merge _profile m1 m2 =
      match (m1, m2) with
      | None, None -> None
      | None, Some m -> Some (String_map.empty, m)
      | Some m, None -> Some (m, String_map.empty)
      | Some m1, Some m2 -> Some (m1, m2)
    in
    String_map.merge merge !deps !conflicts
  in
  String_map.iter generate_profile merged

(* File generated by the regression test in Tezt_wrapper. *)
let tezt_runtime_dependency_tags_path =
  "tezt/lib_wrapper/expected/tezt_wrapper.ml/runtime-dependency-tags.out"

let read_lines_from_file path =
  let ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let rec loop acc =
    match input_line ch with
    | exception End_of_file -> List.rev acc
    | line -> loop (line :: acc)
  in
  loop []

let read_tezt_runtime_dependencies () =
  Fun.flip
    List.filter_map
    (read_lines_from_file tezt_runtime_dependency_tags_path)
  @@ fun line ->
  if line = "" then None
  else
    (* Lines are of the form "tag: path" *)
    match String.index_opt line ':' with
    | None ->
        failwith
          (Printf.sprintf
             "failed to parse %S: invalid line: %S"
             tezt_runtime_dependency_tags_path
             line)
    | Some colon ->
        let tag = String.sub line 0 colon in
        let path =
          String.sub line (colon + 1) (String.length line - colon - 1)
          |> String.trim
        in
        Some (tag, path)

(* Copied from [tezt/lib_wrapper/tezt_wrapper.ml] and adapted to remove ".." as well. *)
let canonicalize_path path =
  let rec simplify_parents acc = function
    | [] -> List.rev acc
    | ".." :: tail ->
        let acc =
          match acc with
          | [] -> failwith ("cannot remove '..' from path: " ^ path)
          | _ :: acc_tail -> acc_tail
        in
        simplify_parents acc tail
    | head :: tail -> simplify_parents (head :: acc) tail
  in
  String.split_on_char '/' path
  |> List.filter (function "" | "." -> false | _ -> true)
  |> simplify_parents [] |> String.concat "/"

(* Compute and print a Tezt TSL expression representing the set of tests
   to run after [changed_files] changed.
   [changed_files] is supposed to only contain files, not directories. *)
let list_tests_to_run_after_changes ~(tezt_exe : target)
    ~(tezt_exe_deps : target list) (changed_files : string list) =
  (* Verbose mode can be activated for debugging this function
     by setting the MANIFEZT_DEBUG environment variable to "true". *)
  let debug = Sys.getenv_opt "MANIFEZT_DEBUG" = Some "true" in
  if debug then List.iter (Printf.eprintf "changed file: %s\n%!") changed_files ;
  (* [directory_has_directly_changed] tests whether a [dir],
     that is supposed to be a directory, is considered to have been modified
     without considering reverse dependencies.
     I.e. it returns whether [changed_files] contains a file that is directly in [dir]. *)
  let directory_has_directly_changed =
    let changed_dirs =
      String_set.of_list (List.map Filename.dirname changed_files)
    in
    fun (dir : string) -> String_set.mem dir changed_dirs
  in
  (* Same as [directory_has_directly_changed] but also consider changes
     in subdirectories, recursively. *)
  let directory_has_changed_recursively =
    let changed_dirs =
      let rec add acc path =
        let dir = Filename.dirname path in
        (* Stop when removing a prefix no longer reduces the size of the path.
           Termination is thus guaranteed since the size strictly reduces towards 0.
           Note that [Filename.dirname "" = "."] so the size can in fact grow. *)
        if String.length dir >= String.length path then
          (* Avoid infinite loops. *)
          acc
        else if String_set.mem dir acc then
          (* No need to add recursively again. *)
          acc
        else
          let acc = String_set.add dir acc in
          (* Recursively add parents. *)
          add acc dir
      in
      List.fold_left add String_set.empty changed_files
    in
    fun (dir : string) -> String_set.mem dir changed_dirs
  in
  (* [internal_has_changed] tells whether an internal [target]
     is considered to have changed, directly or indirectly.
     A target is considered to have changed:
     - if [changed_files] contains a file in the directory of the target
       (only directly: modifying a file in a subdirectory, such as "example/test/",
       does not cause "example/" to be considered to have changed);
     - if one of its dependencies is considered to have changed. *)
  let rec internal_has_changed =
    (* [id] associates a unique identifier to each target,
       so that we can quickly know whether we already traversed it. *)
    let id (target : Target.internal) =
      target.path ^ ": "
      ^
      match target.kind with
      | Public_library {internal_name; _} -> internal_name
      | Private_library name -> name
      | Public_executable ({internal_name; _}, _) -> internal_name
      | Private_executable (name, _) -> name
      | Test_executable {names = name, _; _} -> name
    in
    (* [cache] stores whether a target (identified by its [id]) was already traversed,
       and if so, whether it is considered to have changed. This allows multiple calls
       of [internal_has_changed] on the same [target] to not have to re-traverse the
       whole dependency subtree. *)
    let cache : bool String_map.t ref = ref String_map.empty in
    fun (target : Target.internal) ->
      let id = id target in
      match String_map.find_opt id !cache with
      | Some result -> result
      | None ->
          let result =
            directory_has_directly_changed target.path
            ||
            let internal_deps =
              List.filter_map
                Target.get_internal
                (Target.all_internal_deps target)
            in
            List.exists internal_has_changed internal_deps
          in
          cache := String_map.add id result !cache ;
          result
  in
  (* Variant that works with non-internal targets too, by assuming they never change. *)
  let target_has_changed (target : Target.t) =
    match Target.get_internal target with
    | Some internal -> internal_has_changed internal
    | None -> false
  in
  (* Variant for optional targets. *)
  let target_option_has_changed (target : target) =
    match target with None -> false | Some target -> target_has_changed target
  in
  (* Variant for preprocessors. *)
  let preprocessor_has_changed (preprocessor : Target.preprocessor) =
    match preprocessor with
    | PPS {targets; args = _} | Staged_PPS targets ->
        List.exists target_has_changed targets
  in
  (* If a dependency of [tezt/tests/main.exe] is modified, all tests should be run.
     But those dependencies include the libraries that define tests
     (those that end in [_tezt_lib]). We assume that those only contain
     test definitions, i.e. if they are modified, only their tests need to be run.
     This leaves only [tezt_exe_deps] as the list of dependencies
     that should trigger all tests. *)
  let tezt_exe_dep_changed =
    List.exists target_option_has_changed tezt_exe_deps
  in
  (* We will need the list of (tag, path) from Tezt [~uses] to deduce tags. *)
  let runtime_dependencies = read_tezt_runtime_dependencies () in
  (* [add_tag_for_path] will be called on each changed path.
     It checks if there is an associated tag. *)
  let tags = ref String_set.empty in
  let add_tag_for_path path =
    if debug then Printf.eprintf "- path has changed: %s\n%!" path ;
    Fun.flip List.iter runtime_dependencies @@ fun (tag, tag_path) ->
    (* We are comparing paths that are canonical:
       - Tezt_wrapper writes canonical paths in the list of runtime dependencies;
       - the code below calls [path_has_changed] on canonical path. *)
    if tag_path = path then tags := String_set.add tag !tags
  in
  (* Iterate over all internal targets to find the ones that changed
     and see if they have an associated tag. *)
  ( Fun.flip List.iter !Target.registered @@ fun (target : Target.internal) ->
    if internal_has_changed target then (
      if debug then Printf.eprintf "target changed: %s\n%!" target.path ;
      match target.kind with
      | Public_library _ | Private_library _ -> ()
      | Public_executable names ->
          Fun.flip List.iter (Ne_list.to_list names)
          @@ fun {internal_name; public_name} ->
          (* Assume the executable may be copied to the project root.
             If [~release_status] is [Released] or [Experimental],
             we know it will be copied by the Makefile; but dev executables
             (as defined in the [Makefile] variable [DEV_EXECUTABLES]) are also copied,
             and [~release_status] doesn't tell which executables are dev executables. *)
          add_tag_for_path public_name ;
          add_tag_for_path ("_build/install/default/bin" // public_name) ;
          add_tag_for_path
            ("_build/default" // target.path // (internal_name ^ ".exe"))
      | Private_executable names | Test_executable {names; _} ->
          Fun.flip List.iter (Ne_list.to_list names) @@ fun internal_name ->
          add_tag_for_path
            ("_build/default" // target.path // (internal_name ^ ".exe"))) ) ;
  (* [file_has_changed] just checks if a [path] is in [changed_files].
     It does not assume that [path] is canonical, so it has to make it canonical. *)
  let file_has_changed path = List.mem (canonicalize_path path) changed_files in
  (* [glob_has_changed] tests if, in a given directory [dir],
     a [glob] matches files in [changed_files].
     For now, this is an overapproximation because we do not want to implement
     the whole glob syntax. Special characters are only special after the last "/",
     so we can ignore what is after "/" and just check if something changed in
     the directory. *)
  let glob_has_changed ~rec_ dir glob =
    let dir = canonicalize_path (dir // Filename.dirname glob) in
    if rec_ then directory_has_changed_recursively dir
    else directory_has_directly_changed dir
  in
  (* Iterate over all Tezt targets to find test files
     that are directly or indirectly changed.
     This does not find test files from tezt/tests,
     because those do not result in a [tezt_target];
     this is the special case of [make_tezt_exe]. *)
  let test_files = ref String_set.empty in
  ( Fun.flip String_map.iter !tezt_targets_by_path
  @@ fun tezt_target_dir
             {
               opam = _;
               lib_deps;
               exe_deps;
               js_deps;
               dep_globs;
               dep_globs_rec;
               dep_files;
               modules;
               js_compatible = _;
               modes = _;
               synopsis = _;
               opam_with_test = _;
               dune_with_test = _;
               with_macos_security_framework = _;
               flags = _;
               dune = _;
               tezt_local_test_lib;
               preprocess;
               preprocessor_deps;
             } ->
    if
      List.exists target_option_has_changed lib_deps
      || List.exists target_option_has_changed exe_deps
      || List.exists target_option_has_changed js_deps
      || target_option_has_changed tezt_local_test_lib
      || List.exists preprocessor_has_changed preprocess
      || List.exists
           file_has_changed
           (List.map (fun file -> tezt_target_dir // file) dep_files)
      || List.exists
           file_has_changed
           (List.map
              (fun (File file : Target.preprocessor_dep) ->
                tezt_target_dir // file)
              preprocessor_deps)
      || List.exists (glob_has_changed ~rec_:false tezt_target_dir) dep_globs
      || List.exists (glob_has_changed ~rec_:true tezt_target_dir) dep_globs_rec
    then (
      let path =
        match tezt_local_test_lib with
        | None ->
            (* Function [tezt] always puts [Some] in [tezt_local_test_lib]. *)
            assert false
        | Some lib -> (
            match Target.get_internal lib with
            | None ->
                (* Function [tezt] only puts internal targets in [tezt_local_test_lib]. *)
                assert false
            | Some internal -> internal.path)
      in
      if debug then Printf.eprintf "tezt target changed: %s\n%!" path ;
      Fun.flip List.iter modules @@ fun module_name ->
      let test_file = path // (module_name ^ ".ml") in
      test_files := String_set.add test_file !test_files) ) ;
  (* If a test in [tezt/tests] changes, it needs to be run.
     The above analysis does not detect this because tezt/tests/main.exe is not a
     [tezt_target], it has the special status of "the executable that gathers all tests".
     Note that if a file in [tezt/tests] changes, it could be a helper for other tests,
     so if one wants a safe overapproximation one needs to run all tests.
     But it would mean that changing any file in [tezt/tests] would cause all tests to run,
     which is unnecessary in most cases. The current implementation is thus a heuristic
     where we assume that most files in [tezt/tests] are not helpers but files that register
     tests. In other words, helpers should be put in [tezt/lib_tezos]. *)
  (match tezt_exe with
  | None -> failwith "make_tezt_exe returned no target"
  | Some target -> (
      match Target.get_internal target with
      | None -> failwith "make_tezt_exe did not return an internal target"
      | Some {path; _} ->
          (* Filter [changed_files] to keep those that are in [path].
             Add them to the list of files for which to run tests. *)
          Fun.flip List.iter changed_files @@ fun file ->
          if Filename.dirname file = path then
            test_files := String_set.add file !test_files)) ;
  (* TODO:
     - supports non-executable ~uses:
       - if a target that is not an executable changed, still try to find a matching tag?
       - if changed_files contains something that matches the path of a tag,
         or is in a directory that matches the path of a tag,
         add this tag to the list *)
  let tsl =
    if tezt_exe_dep_changed then "true"
    else if String_set.is_empty !tags && String_set.is_empty !test_files then
      "false"
    else
      let tags = String_set.elements !tags in
      let files =
        String_set.elements !test_files
        |> List.map @@ fun file ->
           if String.contains file '"' then
             failwith ("not supported: path with \" character: " ^ file) ;
           Printf.sprintf "file = \"%s\"" file
      in
      String.concat " || " (tags @ files)
  in
  print_string tsl ;
  flush stdout

let precheck () =
  check_circular_opam_deps () ;
  check_js_of_ocaml () ;
  check_opam_with_test_consistency () ;
  if !has_error then exit 1

let generate ~make_tezt_exe ~tezt_exe_deps ~default_profile ~add_to_meta_package
    =
  Printexc.record_backtrace true ;
  try
    let tezt_exe = register_tezt_targets ~make_tezt_exe in
    precheck () ;
    (match manifezt with
    | None -> ()
    | Some changes ->
        list_tests_to_run_after_changes ~tezt_exe ~tezt_exe_deps changes ;
        exit 0) ;
    Target.can_register := false ;
    generate_dune_files () ;
    generate_opam_files () ;
    generate_dune_project_files () ;
    generate_package_json_file () ;
    let opam_release_graph = compute_opam_release_graph () in
    generate_opam_ci opam_release_graph ;
    generate_executable_list "script-inputs/released-executables" Released ;
    generate_executable_list
      "script-inputs/experimental-executables"
      Experimental ;
    generate_profiles ~default_profile ;
    Option.iter
      (generate_opam_files_for_release
         packages_dir
         opam_release_graph
         add_to_meta_package)
      release
  with exn ->
    Printexc.print_backtrace stderr ;
    prerr_endline ("Error: " ^ Printexc.to_string exn) ;
    exit 1

let postcheck ?exclude () =
  Printexc.record_backtrace true ;
  try
    if !checks_done then failwith "Cannot run check twice" ;
    checks_done := true ;
    check_for_non_generated_files ~remove_extra_files ?exclude () ;
    if !has_error then exit 1
  with exn ->
    Printexc.print_backtrace stderr ;
    prerr_endline ("Error: " ^ Printexc.to_string exn) ;
    exit 1

include Target

let name_for_errors = function
  | None -> "(no target)"
  | Some target -> name_for_errors target

let file_content filename =
  let ch = open_in filename in
  let buffer = Buffer.create 512 in
  Fun.protect
    ~finally:(fun () -> close_in ch)
    (fun () ->
      let bytes = Bytes.create 512 in
      let rec loop () =
        let len = input ch bytes 0 512 in
        if len > 0 then (
          Buffer.add_subbytes buffer bytes 0 len ;
          loop ())
      in
      loop ()) ;
  Buffer.contents buffer

let () =
  (* Note: checking that [.git] is a directory is a bad idea because when using
     git worktrees, [.git] can be a file. *)
  if Sys.file_exists "dune-project" && Sys.file_exists ".git" then ()
  else (
    Printf.eprintf "The manifest should be run from the root of the repo\n" ;
    exit 1)
