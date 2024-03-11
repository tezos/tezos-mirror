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

(** Generate dune and opam files from common definitions. *)

(** Same as [Filename.concat]. *)
val ( // ) : string -> string -> string

module Dune : sig
  (** Dune AST. *)

  (** Compilation modes for executables.

    - [Byte]: compile to bytecode.
    - [Native]: compile to native code.
    - [JS]: compile to JavaScript. *)
  type mode = Byte | Native | JS

  (** The content of the [(kind ...)] stanza of a [dune] file, when a
      library is intended to be used as a PPX rewriter or a
      [[@@deriving ...]]  plugin. *)
  type ppx_kind = Ppx_rewriter | Ppx_deriver

  (** S-expressions.

      S-expressions are lists of atoms and/or s-expressions.
      So basically we define the type of lists (with [] and ::), except that
      items can be lists themselves. By using [] and :: we get to use the list syntax.

      Constructor [S] is for atoms ([S] stands for [String]).

      Constructor [E] stands for "epsilon" or "empty".
      For instance, [[S "x"; E; S "y"]] is equivalent to [[S "x"; S "y"]].
      A typical use case is to insert expressions conditionally, for instance:
      [[S "x"; (if y then S "y" else E); S "z"]].

      Constructor [G] stands for "group".
      It is used to inline an s-expression (i.e. put all of its atoms inside the
      parent list without parentheses around those atoms).
      Additionally, those atoms are grouped together into a box when pretty-printing,
      so they can be put on a single line instead of each atom being put on its own line.

      Constructor [H] stands for "horizontal group".
      It is exactly the same as [G] except that the box enforces that atoms
      are not separated by newlines.

      Constructor [V] stands for "vertical group".
      It is exactly the same as [G] except that the box enforces that atoms
      are separated by newlines. *)
  type s_expr =
    | E
    | S of string
    | G of s_expr
    | H of s_expr
    | V of s_expr
    | []
    | ( :: ) of s_expr * s_expr

  (** Convert a list of [s_expr] to an [s_expr].

      [of_list [a; b; c]] is [(a b c)].

      Tip: you can remove the outer parentheses using [G], [H] or [V]. *)
  val of_list : s_expr list -> s_expr

  (** Convert a list of [string] to an [s_expr].

      [of_atom_list [a; b; c]] is [of_list [S a; S b; S c]]. *)
  val of_atom_list : string list -> s_expr

  (** Programming languages for foreign stubs.

      Only the C programming language is currently supported. *)
  type language = C

  (** Foreign stubs description.

      - [language] is the foreign language of the stubs.
      - [flags] is a list of flags to pass on compilation, such as [-I] flags.
      - [names] is the names of the stubs.

      This becomes a [(foreign_stubs (language ...) (flags ...) (names ...))] stanza
      in the generated dune file. *)
  type foreign_stubs = {
    language : language;
    flags : s_expr;
    names : string list;
  }

  (** Make an [alias] stanza.

      Example: [alias "abc"] results in [(alias (name abc))],
      and [alias "abc" ~deps:["x"; "y"]] results in [(alias (name abc) (deps x y))].

      Such stanzas are usually used to give a name (such as [abc]) to a set of targets,
      so that one can build all of those targets using [dune build @abc]. *)
  val alias : ?deps:string list -> string -> s_expr

  (** Make a [rule] stanza for an alias, of the form [(rule (alias ...) ...)].

      To specify dependencies, either use [deps] and [alias_deps], or [deps_dune].
      The former two are simpler to use since they expect strings, but sometimes
      you may need to specify complex stanzas, in which case you can use [deps_dune].

      - [deps] is a list of target files to build before this rule.
        It becomes a [deps] stanza.

      - [alias_deps] is a list of target aliases to build before this rule.
        They are added to the [deps] stanza in [alias] stanzas
        (resulting in [(deps (alias ...) ...)]).

      - [deps_dune] can be used to specify the arguments of the [deps] stanza
        directly as an s-expression instead.

      - [action] specifies the command to run when building this rule.
        It defaults to [(progn)], i.e. do nothing.
        Typically, actions can be built using {!run} or {!run_exe}.

      - [locks] specifies a path to lock when running this rule.
        Other rules that require the same locks will not be run in parallel.
        Different paths may denote the same lock (e.g. [./x] and [x]),
        but paths do not denote actual files: files are not actually created.

      - [package] specifies the opam package in which this rule belong.
        This is important in particular for [runtest] rules, so that dune knows
        which tests to run when opam runs the tests for a package.

      - [enabled_if] specifies the Dune [enabled_if] clause of this rule.

      The last [string] argument is the name of the alias.
      For instance, if this name is [abc], you can build the rule with [dune build @abc]. *)
  val alias_rule :
    ?deps:string list ->
    ?alias_deps:string list ->
    ?deps_dune:s_expr ->
    ?action:s_expr ->
    ?locks:string ->
    ?package:string ->
    ?enabled_if:s_expr ->
    string ->
    s_expr

  (** Make a stanza of the form [(run ...)].

      Example: [run "%{gen}" ["%{targets}"]] results in [(run %{gen} %{targets})].

      Such stanzas are typically used in [action] parameters of {!alias_rule}. *)
  val run : string -> string list -> s_expr

  (** Make a stanza of the form [(run %{exe:....exe} ...)].

      Example: [run_exe "main" ["-v"; "x.txt"]]
      results in [(run %{exe:main.exe} -v x.txt)]. *)
  val run_exe : string -> string list -> s_expr

  (** Make a [setenv] stanza.

      Example: [setenv "HOME" "/tmp" (run_exe "test" [])] results in
      [(setenv HOME /tmp (run %{exe:test.exe}))].

      This causes the executed command to be run with [HOME=/tmp] in its environment. *)
  val setenv : string -> string -> s_expr -> s_expr

  (** Make a [progn] user action construction, which allows executing more than
      one command.

      Example:
        [~action:(progn [run_exe "main"; [S "diff?"; S "expected"; S "output"];])]
      results in
        [(action (progn (run %{exe:main.exe}) (diff? expected output)))]
  *)
  val progn : s_expr list -> s_expr

  (** Make a [(chdir %{workspace_root} ...)] stanza.

      Such stanzas are typically used to wrap [run] stanzas (e.g. built with {!run_exe})
      to make them run in the root directory of workspace.

      Example: [chdir_workspace_root (run_exe "test" [])] results in
      [(chdir %{workspace_root} (run %{exe:test.exe}))]. *)
  val chdir_workspace_root : s_expr -> s_expr

  (** Make an [ocamllex] stanza.

      Example: [ocamllex "lexer"] results in [(ocamllex lexer)], which tells dune
      that [lexer.ml] can be obtained from [lexer.mll] using ocamllex. *)
  val ocamllex : string -> s_expr

  (** Make an [ocamlyacc] stanza.

      Example: [ocamlyacc "parser"] results in [(ocamlyacc parser)], which tells dune
      that [parser.ml] and [parser.mli] can be obtained from [parser.mly] using ocamlyacc. *)
  val ocamlyacc : string -> s_expr

  (** Make a [menhir] stanza.

      Example: [menhir "parser"] results in [(menhir (modules parser))], which tells dune
      that [parser.ml] and [parser.mli] can be obtained from [parser.mly] using menhir. *)
  val menhir : string -> s_expr

  (** Make an [include] stanza.

      Example: [include_ "rules.inc"] results in [(include rules.inc)].

      Such stanzas are used at toplevel to include other dune files. *)
  val include_ : string -> s_expr

  (** Makes a rule stanza to generate targets.

      Example: [targets_rule ?deps targets ~action] results in:

      (rule
        (targets <targets>)
        (deps <deps>)
        (action <action>))

      Set the optional argument [~promote] to true to generate
      a [(mode promote)] stanza.
  *)
  val targets_rule :
    ?promote:bool ->
    ?deps:s_expr list ->
    ?enabled_if:s_expr ->
    string list ->
    action:s_expr ->
    s_expr

  (** Same as [targets_rule] but for a single target *)
  val target_rule :
    ?promote:bool ->
    ?deps:s_expr list ->
    ?enabled_if:s_expr ->
    string ->
    action:s_expr ->
    s_expr

  (** Makes a rule for compiling protobuf files (.proto) into OCaml files (.ml). *)
  val protobuf_rule : string -> s_expr

  (** Makes an [install] stanza.

      Example: [install files ~package ~section] creates a stanza of the form:

      [(install
        (package <package>)
        (section <section>)
        (files <files>))] *)
  val install : ?package:string -> s_expr list -> section:string -> s_expr

  (** Makes an [as] expression.

      Example: [as_ "foo" "bar"] results in [(foo as bar)] *)
  val as_ : string -> string -> s_expr
end

module Version : sig
  (** Opam package versions and version constraints. *)

  (** Opam package versions.

      Example: ["1.1.0"] *)
  type t = string

  (** Atoms of Opam package versions.

      [V] is a regular version number, and [Version] is Opam's [version] variable
      which denotes the version of the current package. *)
  type atom = V of t | Version

  (** Opam package version constraints.

      - [True] means no constraint.

      - [False] means the package cannot be installed.
        Opam does not actually support this constructor and trying to
        write an opam package with such a version constraint results in an error.
        This constructor is provided for completeness sake so that
        functions {!not_}, {!and_list} and {!or_list} can be more general.

      - [Exactly v] means that the version number must be [v].
        It becomes [=] in the generated opam file.

      - [Different_from v] means that the version number cannot be [v].
        It becomes [!=] in the generated opam file.

      - [At_least v] means that the version number must be [v] or more.
        It becomes [>=] in the generated opam file.

      - [More_than v] means that the version number must be greater than [v].
        In particular it cannot be [v].
        It becomes [>] in the generated opam file.

      - [At_most v] means that the version number must be [v] or less.
        It becomes [<=] in the generated opam file.

      - [Less_than v] means that the version number must be lower than [v].
        In particular it cannot be [v].
        It becomes [<] in the generated opam file.

      - [Not a] is the negation of constraint [a].
        It becomes [! (...)] in the generated opam file.
        It is advised to use {!not_} instead.

      - [And (a, b)] is the conjunction of [a] and [b].
        It becomes [&] in the generated opam file.
        It is advised to use {!and_} or {and_list} instead.

      - [Or (a, b)] is the disjunction of [a] and [b].
        It becomes [|] in the generated opam file.
        It is advised to use {!or_} or {or_list} instead. *)
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

  (** Same as [Exactly (V ...)]. *)
  val exactly : t -> constraints

  (** Same as [Different_from (V ...)]. *)
  val different_from : t -> constraints

  (** Same as [At_least (V ...)]. *)
  val at_least : t -> constraints

  (** Same as [More_than (V ...)]. *)
  val more_than : t -> constraints

  (** Same as [At_most (V ...)]. *)
  val at_most : t -> constraints

  (** Same as [Less_than (V ...)]. *)
  val less_than : t -> constraints

  (** Smart constructor for [Not].

      Simple terms are converted without using [Not].
      For instance [not_ True] is [False] and [not_ (At_most x)] is [More_than x].
      [not_ (Not x)] is [x]. Only [And] and [Or] result in an actual [Not], i.e.
      [not_ (And ...)] is [Not (And ...)]. *)
  val not_ : constraints -> constraints

  (** Smart constructor for [And].

      Conjunctions between [x] and [True] return [x] instead of [And (x, True)],
      and conjunctions between [x] and [False] return [False]. *)
  val ( && ) : constraints -> constraints -> constraints

  (** Same as [List.fold_left and_ True]. *)
  val and_list : constraints list -> constraints

  (** Smart constructor for [Or].

      Disjunctions between [x] and [True] return [True] instead of [And (x, True)],
      and disjunctions between [x] and [False] return [x]. *)
  val ( || ) : constraints -> constraints -> constraints

  (** Same as [List.fold_left or_ False]. *)
  val or_list : constraints list -> constraints
end

(** Module lists for the [(modules)] stanza in [dune] files.

    - [All] means "all modules of the current directory".
      This is the default.

    - [Modules] means "exactly this list of modules".
      Use this for directories which contain several libraries, executables or tests,
      to specify which modules are used by which targets.

    - [All_modules_except] can be used to express the set difference of
      [All] and [Modules]. Use this if you just want to exclude some files.

    For most cases [All] is strongly recommended.
    If you are tempted to explicitly list modules, consider splitting
    your files in subdirectories instead. One exception is if you need to
    be extra sure on which modules are available. Even then, it is recommended
    to not put extra source files in the same directory. *)
type modules =
  | All
  | Modules of string list
  | All_modules_except of string list

(** Preprocessor dependencies.

    - [File]: becomes a [(preprocessor_deps (file ...))] stanza in the [dune] file. *)
type preprocessor_dep = File of string

(** Target descriptions.

    Targets can be external or internal.
    External targets are dependencies that are not part of the project
    and for which no [dune] and [.opam] file need to be generated.
    They can be defined anyway, using e.g. [external_lib], so that internal
    targets can declare that they need them.

    Internal targets are libraries (public or private), executables (public or private)
    and tests that are defined in your [dune] files and packaged in your [.opam] files.
    These are the main values you want to define; everything else is only a tool to
    define internal targets. From those internal target descriptions, [dune] and [.opam]
    files can be generated.

    Each internal target corresponds to part of a [dune] file, and optionally to
    one [.opam] file. The [dune] file is located in the directory specified by
    the [path] argument that is given to the function used to declare the target.
    The full path of the [.opam] file is specified by the [opam] argument,
    to which extension [.opam] is appended.

    Note that several internal targets may use the same [path],
    in which case all of them will be put in the same [dune] file.
    Similarly, several internal targets may use the same [opam] path,
    in which case all of them will be considered part of this same opam package.
    Alternatively, targets for the same [path] file can have different [opam] paths.
    This means that you can have one [dune] file corresponding to several [.opam] files,
    or one [.opam] file with several [dune] files, or any other combinations. *)
type target

(** A target that is ignored when it appears in lists given to target [maker]s.

    [no_target] can be used in dependency lists, i.e. in [?deps], [?conflicts]
    and [?opam_only_deps]. [no_target] is removed from those lists automatically.
    It can thus be used to conveniently build target lists where some targets
    are computed programatically and may not always be included.

    [optional no_target], [select ~package:no_target ...] and [open_ no_target]
    are equivalent to [no_target].

    See also [if_some] and [if_]. *)
val no_target : target

(** Make a target that is ignored if [None].

    [if_some (Some x)] is equivalent to [x], and
    [if_some None] is equivalent to [no_target]. *)
val if_some : target option -> target

(** Make a target that is ignored depending on a condition.

    [target |> if_ condition] is equivalent to [target] if [condition] is [true],
    and to [no_target] if [condition] is [false]. *)
val if_ : bool -> target -> target

module Npm : sig
  (** Npm package description

     An npm package can be added as a dependency to an OCaml
     library. For example, to get the wasm equivalent of a C library
     when targeting JavaScript. *)

  (** Npm package description *)
  type t

  (** Version of the package if it comes form an NPM registry, or a path to a
      local NPM package or JavaScript file. *)
  type version_or_path = Version of Version.constraints | Path of string

  (** Make a npm package.

    Usage: [Npm.make package_name version]

  - [package_name] is the name of the npm package.
  - [version]: version constraint used by npm when installing dependencies.
  *)
  val make : string -> version_or_path -> t
end

module Flags : sig
  (** OCaml flags

      This module is used to construct flags to be passed to the OCaml compiler (in the [(flags ...)] stanza)
  *)

  (** OCaml flags *)
  type t

  (** Extend standard flags with custom ones.

      - [disable_warnings]: disable additional warnings

      - [nopervasives]: if [true], add [-nopervasives] to the list of flags.

      - [nostdlib]: if [true], add [-nostdlib] to the list of flags.

      - [opaque]: if [true], add [-opaque] to the list of flags.
   *)
  val standard :
    ?disable_warnings:int list ->
    ?nopervasives:bool ->
    ?nostdlib:bool ->
    ?opaque:bool ->
    unit ->
    t

  (** [include_ file] will use the flags defined in the file [file]. *)
  val include_ : string -> t

  (** Helper function to compute the string that correspond to
      disabling warnings given as a list of integers. It deduplicates
      warnings, sorts them, and collapses ranges using the [n..m]
      syntax. *)
  val disabled_warnings_to_string : int list -> string
end

module Ctypes : sig
  type description = {instance : string; functor_ : string}

  (** Dune Ctypes stanza description *)
  type t = {
    external_library_name : string;
        (** Base name of the shared object or library archive that you want to
            link against *)
    include_header : string;  (** Header file to include *)
    extra_search_dir : string;
        (** The C compiler and linker will look in this directory to find header
            files and libraries. *)
    type_description : description;
        (** Module information for the type stub descriptions *)
    function_description : description;
        (** Module information for the function stub descriptions *)
    generated_types : string;
        (** Module in which the generated stub types are placed *)
    generated_entry_point : string;
        (* Output module name of the final generated stub module *)
    c_flags : string list;
    c_library_flags : string list;
    deps : string list;
        (** Other targets that Dune shall build before Ctypes does its thing *)
  }
end

(** Preprocessors. *)
type preprocessor

(** Make a preprocessor.

    [pps target] becomes a [(preprocess (pps target))] stanza in the [dune] file.
    The target's package is also added as a dependency in the [.opam] file.

    - [args]: provides extra arguments, e.g. [~args:["--"; "--lib";
      "Type"]] produces [(preprocess (pps target -- --lib Type))]. *)
val pps : ?args:string list -> target -> preprocessor

(** Apply multiple preprocessors.

    [pps targets] becomes a [(preprocess (pps targets...))] stanza in the [dune]
    file. The targets' packages are also added as dependencies in the [.opam]
    file. *)
val ppses : target list -> preprocessor

(** Make a staged preprocessor.

    [staged_pps targets] becomes a [(preprocess (staged_pps target1 target2 ..))] stanza in the [dune] file.
    The target's package is also added as a dependency in the [.opam] file. *)
val staged_pps : target list -> preprocessor

(** Inline_tests backend.

    Can be used when declaring a library to enable inline_tests with the given backend.
*)
type inline_tests

(** Declare an inline_tests backend. *)
val inline_tests_backend : target -> inline_tests

(** Used to specify the availability of an opam package.

    - [Always]: The package is always available.
      No entry will be added to the opam file.
    - [Never]: The package will be marked as unavailable.
    - [No_32]: The package is marked as unavailable on all 32-bits architectures.
    - [No_x86]: The package is marked as unavailable on x86 architectures.
    - [No_ppc]: The package is marked as unavailable on ppc architectures.
    - [No_arm]: The package is marked as unavailable on arm architectures.
    - [N_ary_and [a1;...;an]]: The package availability is [a1] & ... & [an].

*)
type available =
  | Always
  | Never
  | No_32
  | No_x86
  | No_ppc
  | No_arm
  | No_s390x
  | N_ary_and of available list

(** Whether to add the [dune runtest] command in the [.opam] file.

    - [Never]: do not add it.
    - [Always]: add it with [{with-test}].
    - [Only_on_64_arch]: add it with [{with-test & ARCH}]
      where [ARCH] is a condition that only holds on 64-bit architectures. *)
type with_test = Always | Never | Only_on_64_arch

(** Release status for internal targets.

    The semantics of release statuses is summed up in this table:

    |                       | Unreleased | Experimental | Released | Auto_opam |
    |-----------------------+------------+--------------+----------+-----------|
    | make experimental     | no         | yes          | yes      | no        |
    | docker (master)       | no         | yes          | yes      | no        |
    | static (master)       | no         | yes          | yes      | no        |
    | make                  | no         | no           | yes      | no        |
    | docker (release tag)) | no         | no           | yes      | no        |
    | static (release tag)  | no         | no           | yes      | no        |
    | opam (release tag)    | no         | no           | yes      | if needed |

    [Unreleased] means: never release.

    [Experimental] means: release for [master], but not for release tags.
    Since opam packages are not released for [master], they are never published
    for experimental targets. However, [make experimental] builds public executables
    and those executables are put in Docker images and made available as static
    executables for [master].

    [Released] means: release both for [master] and release tags.
    Opam packages of released targets are published when making release tags.
    [make] builds released public executables and those executables are put in
    Docker images and made available as static executables, both for [master]
    and release tags.

    [Auto_opam] means: only release opam package, and only if needed.
    The opam package is released if:
    - one of its targets is released;
    - or it is a transitive dependency of another released opam package.

    Note that a given opam package cannot be both published and not published.
    This means that it is not possible to have an [Unreleased] or [Experimental]
    target in the same package as a [Released] target. It is also not possible
    for a target to be [Unreleased] or [Experimental] if it is in an opam package
    which is needed as a transitive dependency of a [Released] target. *)
type release_status = Unreleased | Experimental | Released | Auto_opam

(** Configure [bisect_ppx] instrumentation.

    If [No], disable instrumentation. If [Yes], enable
    instrumentation. If [With_sigterm], enable instrumentation and
    install a sigterm handler. See {!maker} for more details. *)
type bisect_ppx = No | Yes | With_sigterm

(** Functions that build internal targets.

    The ['a] argument is instantiated by the relevant type for the name(s)
    of the target.

    - [all_modules_except]: short-hand for [~modules: (All_module_except ...)].

    - [bisect_ppx]: if [Yes] (respectively [With_sigterm]), the target's [dune] file
      is generated with [(instrumentation (backend bisect_ppx))] (respectively
      [(instrumentation (backend bisect_ppx --bisect-sigterm))]) for this target.
      This makes it possible to compute coverage. It is recommended to set this
      to [Yes] for all libraries and executables except those that are only used for tests
      (and thus are never run by users). It should be set to [With_sigterm] for test
      executables that spawn sub-processes which are susceptible to termination
      through sigterm in non-exceptional circumstances (e.g. Tezt entrypoints).

    - [time_measurement_ppx]: if [true], the target's [dune] file is generated
      with [(instrumentation (backend time_measurement_ppx))] for this target.
      This makes it possible to add time measurement tooling for the target.

    - [c_library_flags]: specifies a [(c_library_flags ...)] stanza.
      Those flags are passed to the C compiler when constructing the library archive
      for the foreign stubs.

    - [conflicts]: a list of target; all of their packages will be put in the
      [conflicts] section of the [.opam] file.

    - [deps]: a list of targets to add as dependencies using [(libraries)]
      in the [dune] file.

    - [dune]: added to the [dune] file after this target.
      A typical use is to add [rule] or [install] stanzas.

    - [flags]: specifies a [(flags ...)] stanza.
      Those flags are passed to the OCaml compiler when compiling and linking OCaml units.

    - [foreign_archives]: specifies a [(foreign_archives)] stanza for the [dune] target.

    - [foreign_stubs]: specifies a [(foreign_stubs)] stanza for the [dune] target.

    - [implements]: specifies an [(implements)] stanza for the [dune] target.

    - [inline_tests]: specifies an inline_tests backend. Can only be used when constructing a library.
      If used, will add [(inline_tests)] and the corresponding preprocessor in the dune stanza.

    - [inline_tests_deps]: specifies inline_tests dependencies. Can only be used when constructing
      a library with inline_tests enabled.

    - [js_compatible]: whether the target can be compiled to JavaScript.
      Default value for [js_compatible] is
      [false] if [js_of_ocaml] is [None],
      [true] otherwise.

    - [js_of_ocaml]: specifies a [(js_of_ocaml ...)] stanza for the [dune] target,
      where [...] is the value of the parameter. The toplevel parentheses are removed.
      For instance, [~js_of_ocaml:Dune.[[S "javascript_files"; S "file.js"]]]
      becomes [(js_of_ocaml (javascript_files file.js))].

    - [wrapped]: specifies a [(wrapped ...)] stanza for the [dune] target.

    - [documentation]: specifies a [(documentation ...)] stanza for the [dune]
      target where [...] is the value of the parameter. Use this parameter if
      the library includes an [index.mld] file.

    - [linkall]: if [true], add [-linkall] to the list of flags to be passed
      to the OCaml compiler. In executables and tests, it is added to the [(link_flags ...)]
      stanza, causing all modules of all dependencies to be linked unconditionally,
      even if they are not referenced directly in the code.
      In libraries, it is added to the [(library_flags ...)] stanza,
      causing all modules of the library to be linked unconditionally when the
      library is used as a dependency of an executable or test.

    - [modes]: list of modes this target can be compiled to.

    - [modules]: list of modules to include in this target.

    - [modules_without_implementation]: list of modules without implementation to include in this target.

    - [npm]: npm dependencies used when targeting JavaScript.

    - [ocaml]: constraints for the version of the [ocaml] opam package,
      i.e. on the version of the OCaml compiler.

    - [opam]: path and name of the [.opam] file, without the [.opam] extension.
      If [""], no [.opam] file is generated for this target.
      If unspecified, for public libraries and executables a default value of
      [path/name] is used, where [path] is the path of the [dune] file
      and [name] is the public name of the target.
      For private libraries, private executables and tests, you must specify
      this argument (you can explicitely set it to [""] to generate no [.opam] file).

    - [opam_bug_reports], [opam_doc] and [opam_homepage]: URLs to put in the [.opam] file
      in the [bug-reports], [doc] and [homepage] clauses respectively.
      Clauses are omitted for empty strings.
      You usually do not want to specify those and keep default values,
      but there can be some exceptions for packages that are particularly useful
      on their own outside of Octez.

    - [opam_with_test]: whether to add the [dune runtest] command.
      Note that for a given package all targets must have the same value of [opam_with_test].

    - [opam_version]: the version of the opam package.
      This is only useful for releasing different products that depend on each-other.

    - [path]: path of the directory in which to generate the [dune] file for this target.

    - [ppx_kind]: specifies a [(kind ppx_rewriter)] or [(kind ppx_deriver)]
      stanza for the [dune] target. Must be set when the library is intended
      to be used as a PPX rewriter or a [@@deriving ...] plugin.

    - [ppx_runtime_libraries]: adds a [(ppx_runtime_libraries ...)]
      stanza to the [dune] target, allowing to specify runtime
      dependencies when the library is a [ppx_rewriter] or a [ppx_deriver].

    - [preprocess]: preprocessor directives to add using the [(preprocess ...)] stanza.
      Those preprocessors are also added as dependencies in the [.opam] file.

    - [preprocessor_deps]: preprocessor dependencies.

    - [private_modules]: similar to [modules], but those modules are not part of the
      library interface. They are not part of the toplevel module of the library.

    - [profile]: the name of the profile to which the target belongs.
      For each profile, a file [opam/virtual/<PROFILE_NAME>.opam] is generated.
      This file depends on all external dependencies on which targets that belong
      to this profile depend on, and contains no build instructions.
      It is thus a good basis for [opam lock] to generate a [<PROFILE_NAME>.opam.locked] file.
      Running [opam install <PROFILE_NAME>.opam.locked] will then install all dependencies
      needed to build targets belonging to this profile.

      Targets for which no profile is specified will belong to the default profile
      given to {!generate}. Most of the time you do not need to specify a profile:
      the default profile is fine. But if your target depends on packages that users
      would most likely not want to install, it may be a good idea to separate it
      into its own profile.

    - [opam_only_deps]: dependencies to add to the [.opam] file but not to the [dune] file.
      Typical use cases are runtime dependencies and build dependencies for users
      of the target (but not the target itself).

    - [optional]: if [true], do not build this target if some of its dependencies
      are not available. The dependencies of this target themselves become optional
      dependencies in the [.opam] file (unless they are required by other targets).
      Default is [false].

    - [release_status]: if and when this should be released.
      See the documentation of type {!release_status}.
      Default value is [Auto_opam].

    - [static]: whether to incluce [ %{workspace_root}/static-link-flags.sexp ] to the link
      flags to provide a static compilation profile.
      Default is [true] for public executables and [false] for other targets.

    - [synopsis]: short description for the [.opam] file.

    - [description]: long description for the [.opam] file.

    - [virtual_modules]: similar to [modules], but for modules that should have an
      implementation (an [.ml] file) but that have not. Those modules only come
      with an [.mli]. This turns the target into a virtual target.
      Other targets can declare that they implement those modules with [implements].

    - [default_implementation] specifies a [(default_implementation)] stanza for the
      [dune] target. Note that this argument has type [string] instead of type [target].
      The user should give the name of e.g. the public library that serves as default
      implementation.

    - [license]: specific license to use for that target. If omitted it will
      default to MIT.

    - [extra_authors]: list of authors in addition to the Tezos Dev Team.

    - [path]: the path to the directory of the [dune] file that will define this target.

    - [with_macos_security_framework]: Default value is [false]. When set to
      [true], the [-ccopt "-framework Security"]flag is added at link time for
      macOS system. *)
type 'a maker =
  ?all_modules_except:string list ->
  ?bisect_ppx:bisect_ppx ->
  ?c_library_flags:string list ->
  ?conflicts:target list ->
  ?deps:target list ->
  ?dune:Dune.s_expr ->
  ?flags:Flags.t ->
  ?foreign_archives:string list ->
  ?foreign_stubs:Dune.foreign_stubs ->
  ?ctypes:Ctypes.t ->
  ?implements:target ->
  ?inline_tests:inline_tests ->
  ?inline_tests_deps:Dune.s_expr list ->
  ?js_compatible:bool ->
  ?js_of_ocaml:Dune.s_expr ->
  ?wrapped:bool ->
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
  ?opam_version:Version.t ->
  ?optional:bool ->
  ?ppx_kind:Dune.ppx_kind ->
  ?ppx_runtime_libraries:target list ->
  ?preprocess:preprocessor list ->
  ?preprocessor_deps:preprocessor_dep list ->
  ?private_modules:string list ->
  ?profile:string ->
  ?opam_only_deps:target list ->
  ?release_status:release_status ->
  ?static:bool ->
  ?synopsis:string ->
  ?description:string ->
  ?time_measurement_ppx:bool ->
  ?available:available ->
  ?virtual_modules:string list ->
  ?default_implementation:string ->
  ?cram:bool ->
  ?license:string ->
  ?extra_authors:string list ->
  ?with_macos_security_framework:bool ->
  path:string ->
  'a ->
  target

module Env : sig
  (** [env] stanza *)
  type t

  (** profile selector for the env stanza. A profile name or Any (_) *)
  type profile = Profile of string | Any

  (** The empty env *)
  val empty : t

  (** [add profile ~key payload] adds a [key] entry with its [payload]
      to the given [profile].  Adding an entry to [Any] profile means
      that it will apply regardless of the profile. In practice, it
      means that the entry can end up being duplicated in the resulting
      [s_expr].
    {[
      Env.empty
      |> Env.add Any ~key:"flags" Dune.[S "-flag"]
      |> Env.add (Profile "static") ~key:"link_flags" Dune.[S "-link-flag"]
    ]}

    will generate

    {v
      (env
        (_
          (flags (-flag))
        )
        (static
          (flags (-flag))
          (link_flags (-link_flag))
        )
      )
    v}

    Also note that [Profile "_"] is not allowed. One should use [Any] instead.
 *)
  val add : profile -> key:string -> Dune.s_expr -> t -> t
end

(** Register a Tezt test.

    Usage: [tezt module_names]

    This declares:
    - a library [PACKAGE_tezt_lib] in [path] where [PACKAGE] is the name of the
      opam package denoted by [opam];
    - an executable [main] in [path] that links with [PACKAGE_tezt_lib]
      and runs [Tezt.Test.run].

    [module_names] is the list of modules to link in [PACKAGE_tezt_lib].
    Those should be files in [path] that call [Tezt.Test.register].

    Note that a wrapper in [main.ml] adds a dependency to the [tezt] library
    and [-open]s modules [Tezt] and [Tezt.Base] when compiling [module_names].

    Additionally, the library [PACKAGE_tezt_lib] is also linked in [tezt/tests/main.exe]
    so that this executable can be used to run all tests with auto-balancing
    and other Tezt features.

    The flag [with_macos_security_framework] has a default [false]. When set to
    [true], the [-ccopt "-framework Security"]flag is added at link time for
    macOS system.

    The function returns a target that is the local library created by tezt
    to register all tests. This can be reused locally in the same module
    to share code with other targets. *)
val tezt :
  opam:string ->
  path:string ->
  ?js_compatible:bool ->
  ?modes:Dune.mode list ->
  ?lib_deps:target list ->
  ?exe_deps:target list ->
  ?js_deps:target list ->
  ?dep_globs:string list ->
  ?dep_globs_rec:string list ->
  ?dep_files:string list ->
  ?synopsis:string ->
  ?opam_with_test:with_test ->
  ?dune_with_test:with_test ->
  ?with_macos_security_framework:bool ->
  ?flags:Flags.t ->
  ?dune:Dune.s_expr ->
  ?preprocess:preprocessor list ->
  ?preprocessor_deps:preprocessor_dep list ->
  product:string ->
  string list ->
  target

(** Make an external vendored library, for use in internal target dependencies.

    [main_module] is the name of the main module provided by the library (see [open_]).

    [js_compatible]: whether the library can be compiled to JavaScript.
    Default value for [js_compatible] is false.

    [npm_deps]: npm dependencies used when targeting JavaScript.

    [released_on_opam]: whether the library is available on the upstream opam-repository
    (default true). In case the lib is not available on opam, tezos packages depending
    on it won't be installable on opam.

    Version constraints are only added to released [.opam] files
    (i.e. when running the manifest with [--release]).
    And when appearing in [~conflicts], vendored libraries are only added to released
    [.opam] files. The idea is that vendored libraries are compiled from the local copy,
    so version and conflicts do not matter, except when installing with [opam install],
    in which case local copies are ignored and vendored libraries are installed through
    opam. This is because we [rm -r vendors] in [.opam] files because of a bug of dune
    when multiple layers of vendored libraries are involved. *)
val vendored_lib :
  ?released_on_opam:bool ->
  ?main_module:string ->
  ?js_compatible:bool ->
  ?npm_deps:Npm.t list ->
  string ->
  Version.constraints ->
  target

(** Make an external library, for use in internal target dependencies.

    Usage: [external_lib name version_constraints]

    [name] is used in [dune] files, while [opam] is used in [.opam] files.
    Default value for [opam] is [name].

    [main_module] is the name of the main module provided by the library (see [open_]).

    [js_compatible]: whether the library can be compiled to JavaScript.
    Default value for [js_compatible] is false.

    [npm]: npm dependencies used when targeting JavaScript.
  *)
val external_lib :
  ?main_module:string ->
  ?opam:string ->
  ?js_compatible:bool ->
  ?npm_deps:Npm.t list ->
  string ->
  Version.constraints ->
  target

(** Make an external library that is a sublibrary of an other one.

    Usage: [external_sublib main_lib name]

    If [main_lib]'s [opam] is [main_opam] and its version constaints are
    [version_constraints], this is equivalent to:
    [external_lib ~opam: main_opam name version_constraints].

    [main_module] is the name of the main module provided by the library (see [open_]).
    The main module of [main_lib] is ignored.

    [js_compatible]: whether the library can be compiled to JavaScript.
    Default value for [js_compatible] is false.

    [npm_deps]: npm dependencies used when targeting JavaScript.

    @raise Invalid_arg if [main_lib] was not built with [external_lib]. *)
val external_sublib :
  ?main_module:string ->
  ?js_compatible:bool ->
  ?npm_deps:Npm.t list ->
  target ->
  string ->
  target

(** Make an external library that is to only appear in [.opam] dependencies.

    This avoids using [~opam_only_deps] each time you declare this dependency.

    [can_vendor] specifies whether opam-monorepo should vendor this dependency, defaults
    to [true]. *)
val opam_only : ?can_vendor:bool -> string -> Version.constraints -> target

(** Make an optional dependency with a source file to be selected depending on presence.

    In the [dune] file, this corresponds to a stanza of the form:
    [(select target from (package -> source_if_present) (-> source_if_absent))]
    where [package] is the opam package of the [package] target.

    This tells Dune that if [package] is present, it should be used to compile
    and link, and that [source_if_present] should be used in place of [target],
    while [source_if_absent] should be ignored. On the opposite, if [package] is
    absent, the target can still be compiled, but [package] should not be used
    to compile and link (obviously), and [source_if_absent] should be used in
    place of [target], while [source_if_absent] should be ignored.

    For instance,
    {[
      select
        ~package:"p"
        ~source_if_present:"x.available.ml"
        ~source_if_absent:"x.none.ml"
        "x.ml"
    }]
    corresponds to:
    {[
      (select x.ml from
         (p -> x.available.ml)
         (-> x.none.ml))
    ]}
    and means: if package [p] is installed, compile with [x.ml] equal to [x.available.ml],
    else compile with [x.ml] equal to [x.none.ml]. File [x.none.ml] con for instance
    contain a dummy implementation.

    The target is put in the [depopts] section instead of the [depends] section
    of the [.opam] file. *)
val select :
  package:target ->
  source_if_present:string ->
  source_if_absent:string ->
  target:string ->
  target

(** Make an optional dependency, to be linked only if available.

    [optional] is a simplified version of [select]: [optional p] corresponds to
    [[
      (select void_for_linking-p from
       (p -> void_for_linking-p.empty)
       (-> void_for_linking-p.empty))
    ]]
    i.e. if [p] is available, it is linked, and if not, it is not linked.

    Depending on an [optional] target also adds a Dune rule of the form
    [(rule (action progn (write-file void_for_linking-p.empty "")))].
    [void_for_linking-p] is a dummy file created in both cases of the [(select)]
    from the empty file [void_for_linking-p.empty] which is generated automatically
    thanks to this rule.

    Like [select], the target is put in the [depopts] section of the [.opam] file
    instead of the [depends] section. *)
val optional : target -> target

(** Make a target with a module that should automatically be opened.

    If [m] is specified, open this submodule instead of the main module.

    When such targets appear in [?deps] of a target [maker], they are
    converted into [-open] in the order of declaration in [?deps].
    If you use [open_] on an [open_], the innermost [open_]s is opened first;
    For instance, [tezos_base |> open_ |> open_ ~m: "TzPervasives"]
    is target [tezos_base], but when used in [?deps], this automatically opens
    ["Tezos_base"], followed by ["Tezos_base.TzPervasives"].

    Can only be used on internal libraries and on external or vendored
    libraries for which a [main_module] was specified. *)
val open_ : ?m:string -> target -> target

(** Same as [open_], but only open if a condition holds.

    Example: [tezos_base |> open_if protocol_is_recent_enough] *)
val open_if : ?m:string -> bool -> target -> target

(** Selectively makes a dependency available to the library's users:
    extends to [(re_export <target's name>)] inside a dependency
    stanza. *)
val re_export : target -> target

(** Add a dependency to a profile.

    See the documentation of the [?profile] argument of target makers for
    more information about profiles.

    Use [add_dep_to_profile profile dep] to add [dep] as a dependency to the
    [opam/virtual/profile.opam] file without having to add it as a dependency of an
    actual package. *)
val add_dep_to_profile : string -> target -> unit

(** Common types for sub-libs (makers are in the [Product] functor) *)
module Sub_lib : sig
  type documentation_entrypoint = Module | Page | Sub_lib

  type sub_lib = {
    name : string;
    synopsis : string option;
    documentation_type : documentation_entrypoint;
  }

  (** The type of a container for a set of sub-libraries *)
  type container

  (** A sub-lib [maker] is similar to a generic [maker] except that:

      - Passing a value for the [opam] parameter raises [Invalid_argument],
        as all sub-libraires are necessarily restricted to a single opam
        package.

      - The [internal_name] parameter can be given to set the internal name of the
      library. *)
  type nonrec maker = ?internal_name:string -> string maker

  (** Prints all the registered sub-libraries of a package. *)
  val pp_documentation_of_container :
    header:string -> Format.formatter -> container -> unit
end

(** [Product] is a functor which instantiates [maker]s for [target]s. The
    product name passed as a functor parameter is used for all the made targets. *)
module Product (M : sig
  val name : string
end) : sig
  (** Register and return an internal public library.

    The ['a] argument of [maker] is [string]: it is the public name.
    If [internal_name] is not specified, a default is chosen by converting
    the public name, by replacing characters ['-'] and ['.'] to ['_'].

    Internal names correspond to the [(name ...)] stanza in [dune] files,
    while public names correspond to the [(public_name ...)] stanza
    (and usually to the name of the [.opam] file). *)
  val public_lib : ?internal_name:string -> string maker

  (** Same as {!public_lib} but for a public executable. *)
  val public_exe : ?internal_name:string -> string maker

  (** Same as {!public_exe} but with several names, to define multiple executables at once.

    If given, the list of internal names must be in the same order as the list of
    public names. If not given, the list of internal names is derived from the
    list of names as for [public_lib].

    @raise Invalid_arg if the list of names is empty or if the length of
    [internal_names] differs from the length of the list of public names. *)
  val public_exes : ?internal_names:string list -> string list maker

  (** Register and return an internal private (non-public) library.

    Since it is private, it has no public name: the ['a] argument of [maker]
    is its internal name. *)
  val private_lib : string maker

  (** Register and return an internal private (non-public) executable.

    Since it is private, it has no public name: the ['a] argument of [maker]
    is its internal name. *)
  val private_exe : string maker

  (** Same as {!private_exe} but with several names, to define multiple executables at once. *)
  val private_exes : string list maker

  (** Register and return an internal test.

    - [alias]: if non-empty, an alias is set up for the given test, named [alias].
      Default is ["runtest"]. Note that for JS tests, ["_js"] is appended to this alias.
      Also note that if [alias] is non-empty, the target must belong to an opam package
      (i.e. [~opam] must also be non-empty). If given, the [enabled_if] and/or [locks]
      clauses are added to this alias.

    - [dep_files]: a list of files to add as dependencies using [(deps (file ...))]
      in the [runtest] alias.

    - [dep_globs]: a list of files to add as dependencies using [(deps (glob_files ...))]
      in the [dune] file.

    - [dep_globs_rec]: a list of files to add as dependencies using [(deps (glob_files_rec ...))]
      in the [dune] file.

    - [dune_with_test]: Specifies a condition for the test to be run on the dune file.
      If set to [Only_on_64_arch], [%{arch_sixtyfour}] is added to the [enabled_if] clause.
      If set to [Always], nothing is added to the [enabled_if] clause.
      If set to [Never], [false] is added to the [enabled_if] clause.

    - [enabled_if]: add a custom [enabled_if] clause. If both [dune_with_test] and
      [enabled_if] are set, then logically, the resulting clause is the conjunction
      of the two (i.e. [(and <enabled_if> <dune_with_test>)])

    Since tests are private, they have no public name: the ['a]
    argument of [maker] is the internal name. *)
  val test :
    ?alias:string ->
    ?dep_files:string list ->
    ?dep_globs:string list ->
    ?dep_globs_rec:string list ->
    ?locks:string ->
    ?enabled_if:Dune.s_expr ->
    ?dune_with_test:with_test ->
    ?lib_deps:target list ->
    string maker

  (** Same as {!test} but with several names, to define multiple tests at once. *)
  val tests :
    ?alias:string ->
    ?dep_files:string list ->
    ?dep_globs:string list ->
    ?dep_globs_rec:string list ->
    ?locks:string ->
    ?enabled_if:Dune.s_expr ->
    ?lib_deps:target list ->
    string list maker

  (** This module is used to register multiple libraries (sub-libraries)
    for a single container package. See
    [https://dune.readthedocs.io/en/stable/concepts/package-spec.html#libraries]
    for the corresponding dune feature. *)
  module Sub_lib : sig
    (** Create a container *)
    val make_container : unit -> Sub_lib.container

    (** Define a maker for sub-libraries of a given [container]. *)
    val sub_lib :
      package_synopsis:string ->
      container:Sub_lib.container ->
      package:string ->
      Sub_lib.maker
  end
end

(** Get a name for a given target, to display in errors.

    If a target has multiple names, one is chosen arbitrarily.
    So this should not be used except to display errors. *)
val name_for_errors : target -> string

(** Generate dune and opam files.

    Call this after you declared all your targets with functions such as
    [public_lib], [test], etc.

    [make_tezt_exe] is given the list of libraries that register Tezt tests
    and shall create a test executable that links all of them.

    [tezt_exe_deps] is the list of dependencies linked with the executable
    registered by [make_tezt_exe], not including the libraries that were passed
    to [make_tezt_exe]. It is the list of dependencies that should trigger
    all tests to run when they are modified.

    [default_profile] is the name of the profile to use for targets that
    were declared without [?profile]. See the documentation of the [?profile]
    argument of type ['a maker].

    Some checks are performed before generating:

    - Check that there are no circular dependencies of opam packages.
      If this check is not performed before [generate], generation may cause
      a stack overflow.

    - Check that the transitive closure of dependencies of a [js_compatible] target
      is [js_compatible].

    - Check that all targets of an opam package contain the same value for
      [~opam_with_test]. *)
val generate :
  make_tezt_exe:(target list -> target) ->
  tezt_exe_deps:target list ->
  default_profile:string ->
  add_to_meta_package:target list ->
  unit

(** Run checks that should be performed after [generate].

    - Check that all [dune], [dune-project], [dune-workspace] and [.opam] files
      are either generated or excluded. It is an error if a generated file is excluded.
      You can use [exclude] to specify which files should be excluded.
      [exclude] is given a path relative to the [root] directory
      and shall return [true] for excluded paths.

    In case of errors, errors are printed and the process exits with exit code 1. *)
val postcheck : ?exclude:(string -> bool) -> unit -> unit

(** Generate dune-workspace file.

    [generate_workspace env dune] will generate a dune-workspace file at the root of the repo.
    [env] will translate into the corresponding env stanza.
    [dune] is a free form s-expression that will be included as is at the end of the file.
 *)
val generate_workspace : Env.t -> Dune.s_expr -> unit

(** [write filename f] writes a file relatively to the root directory of the repository.

    The callback [f] is reponsible for feeding the content of the file by using the formmater. *)
val write : string -> (Format.formatter -> unit) -> unit

(** [file_content filename] reads the contents of the file identified by [filename].
    Note that the manifest is assumed to be running in the project root directory,
    so all paths should be relative to it. *)
val file_content : string -> string
