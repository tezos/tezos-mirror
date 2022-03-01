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

(** Return whether [prefix] is a prefix of a string. *)
val has_prefix : prefix:string -> string -> bool

module Dune : sig
  (** Dune AST. *)

  (** Compilation modes for executables.

    - [Byte]: compile to bytecode.
    - [Native]: compile to native code.
    - [JS]: compile to JavaScript. *)
  type mode = Byte | Native | JS

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
    flags : string list;
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
        (resulting in [(deps (alias ...) ...)].

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

      The last [string] argument is the name of the alias.
      For instance, if this name is [abc], you can build the rule with [dune build @abc]. *)
  val alias_rule :
    ?deps:string list ->
    ?alias_deps:string list ->
    ?deps_dune:s_expr ->
    ?action:s_expr ->
    ?locks:string ->
    ?package:string ->
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

  (** Make a [(chdir %{workspace_root} ...)] stanza.

      Such stanzas are typically used to wrap [run] stanzas (e.g. built with {!run_exe})
      to make them run in the root directory of workspace.

      Example: [chdir_workspace_root (run_exe "test" [])] results in
      [(chdir %{workspace_root} (run %{exe:test.exe}))]. *)
  val chdir_workspace_root : s_expr -> s_expr

  (** Make an [ocamllex] stanza.

      Example: [ocamllex "lexer"] results in [(ocamllex parser)], which tells dune
      that [lexer.ml] can be obtained from [lexer.mll] using ocamllex. *)
  val ocamllex : string -> s_expr

  (** Make an [include] stanza.

      Example: [include_ "rules.inc"] results in [(include rules.inc)].

      Such stanzas are used at toplevel to include other dune files. *)
  val include_ : string -> s_expr

  (* Makes a rule stanza to generate targets.

     Example: [targets_rule ?deps targets ~action] results in:

     (rule
       (targets <targets>)
       (deps <deps>)
       (action <action>)) *)
  val targets_rule : ?deps:s_expr list -> string list -> action:s_expr -> s_expr

  (* Makes an [install] stanza.

     Example: [install files ~package ~section] creates a stanza of the form:

     [(install
       (package <package>)
       (section <section>)
       (files <files>))] *)
  val install : ?package:string -> s_expr list -> section:string -> s_expr

  (* Makes an [as] expression.

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

(** Preprocessors. *)
type preprocessor

(** Make a preprocessor.

    [pps ?args target] becomes a [(preprocess (pps target args))] stanza in the [dune] file.
    The target's package is also added as a dependency in the [.opam] file. *)
val pps : ?args:string list -> target -> preprocessor

(** Functions that build internal targets.

    The ['a] argument is instantiated by the relevant type for the name(s)
    of the target.

    - [all_modules_except]: short-hand for [~modules: (All_module_except ...)].

    - [bisect_ppx]: if [true], the target's [dune] file is generated
      with [(instrumentation (backend bisect_ppx))] for this target.
      This makes it possible to compute coverage. It is recommended to set this
      for all libraries and executables except those that are only used for tests
      (and thus are never run by users).

    - [time_measurement_ppx]: if [true], the target's [dune] file is generated
      with [(instrumentation (backend time_measurement_ppx))] for this target.
      This makes it possible to add time measurement tooling for the target.

    - [c_library_flags]: specifies a [(c_library_flags ...)] stanza.
      Those flags are passed to the C compiler when constructing the library archive
      for the foreign stubs.

    - [conflicts]: a list of target; all of their packages will be put in the
      [conflicts] section of the [.opam] file.

    - [dep_files]: a list of files to add as dependencies using [(deps (file ...))]
      in the [dune] file. A typical use is if you generate code: this tells [dune]
      to make those files available to your generator.

    - [deps]: a list of targets to add as dependencies using [(libraries)]
      in the [dune] file.

    - [dune]: added to the [dune] file after this target.
      A typical use is to add [rule] or [install] stanzas.

    - [foreign_stubs]: specifies a [(foreign_stubs)] stanza for the [dune] target.

    - [inline_tests]: if [true], add [(inline_tests)] to the [dune] target.
      This does NOT add [ppx_inline_test] to [preprocess].

    - [js_compatible]: whether the target can be compiled to JavaScript.
      Default value for [js_compatible] is
      [false] if [js_of_ocaml] is [None],
      [true] otherwise.

    - [js_of_ocaml]: specifies a [(js_of_ocaml ...)] stanza for the [dune] target,
      where [...] is the value of the parameter. The toplevel parentheses are removed.
      For instance, [~js_of_ocaml:Dune.[[S "javascript_files"; S "file.js"]]]
      becomes [(js_of_ocaml (javascript_files file.js))].

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

    - [node_wrapper_flags]: flags to add to the [node_wrapper] executable
      command in [runtest_js] aliases. Only used for test and executable test targets.

    - [nopervasives]: if [true], add [-nopervasives] to the list of flags to
      be passed to the OCaml compiler (in the [(flags ...)] stanza).

    - [nostdlib]: if [true], add [-nostdlib] to the list of flags to
      be passed to the OCaml compiler (in the [(flags ...)] stanza).

    - [ocaml]: constraints for the version of the [ocaml] opam package,
      i.e. on the version of the OCaml compiler.

    - [opam]: path and name of the [.opam] file, without the [.opam] extension.
      If [""], no [.opam] file is generated for this target.
      If unspecified, for public libraries and executables a default value of
      [path/name] is used, where [path] is the path of the [dune] file
      and [name] is the public name of the target.
      For private libraries, private executables and tests, you must specify
      this argument (you can explicitely set it to [""] to generate no [.opam] file).

    - [opaque]: if [true], add [-opaque] to the list of flags to be passed
      to the OCaml compiler (in the [(flags ...)] stanza).

    - [opens]: list of module names to open when compiling.
      They are passed as [-open] flags to the OCaml compiler (in the [(flags ...)] stanza).

    - [path]: path of the directory in which to generate the [dune] file for this target.

    - [preprocess]: preprocessor directives to add using the [(preprocess ...)] stanza.
      Those preprocessors are also added as dependencies in the [.opam] file.

    - [preprocessor_deps]: preprocessor dependencies, such as files for [ppx_blob].

    - [private_modules]: similar to [modules], but those modules are not part of the
      library interface. They are not part of the toplevel module of the library.

    - [opam_only_deps]: dependencies to add to the [.opam] file but not to the [dune] file.
      Typical use cases are runtime dependencies and build dependencies for users
      of the target (but not the target itself).

    - [release]: unused for now. The intent is to define whether this should be released.
      Default is [true] for public executables and [false] for other targets.

    - [static]: whether to generate a [(env (static (flags (:standard -ccopt -static ...))))]
      stanza to provide a static compilation profile.
      Default is [true] for public executables and [false] for other targets,
      unless you specify [static_cclibs], in which case default is [true].

    - [static_cclibs]: list of static libraries to link with for targets
      in this [dune] file when building static executables.
      Added using [-cclib] to the stanza that is generated when [static] is [true].

    - [synopsis]: short description for the [.opam] file.

    - [warnings]: the argument passed to the -w flag when building.

    - [wrapped]: if [false], add the [(wrapped false)] stanza in the [dune] file.
      This causes the library to not come with a toplevel module with aliases to
      all other modules. Not recommended (according to the dune documentation).

    - [path]: the path to the directory of the [dune] file that will define this target. *)
type 'a maker =
  ?all_modules_except:string list ->
  ?bisect_ppx:bool ->
  ?c_library_flags:string list ->
  ?conflicts:target list ->
  ?dep_files:string list ->
  ?deps:target list ->
  ?dune:Dune.s_expr ->
  ?foreign_stubs:Dune.foreign_stubs ->
  ?inline_tests:bool ->
  ?js_compatible:bool ->
  ?js_of_ocaml:Dune.s_expr ->
  ?documentation:Dune.s_expr ->
  ?linkall:bool ->
  ?modes:Dune.mode list ->
  ?modules:string list ->
  ?modules_without_implementation:string list ->
  ?node_wrapper_flags:string list ->
  ?nopervasives:bool ->
  ?nostdlib:bool ->
  ?ocaml:Version.constraints ->
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
  ?time_measurement_ppx:bool ->
  ?warnings:string ->
  ?wrapped:bool ->
  ?cram:bool ->
  path:string ->
  'a ->
  target

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

    @raise [Invalid_arg] if the list of names is empty or if the length of
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

    Since tests are private, they have no public name: the ['a] argument of [maker]
    is the internal name. *)
val test : string maker

(** Same as {!test} but with several names, to define multiple tests at once. *)
val tests : string list maker

(** Register and return an internal executable that is only used for tests.

    Same as {!private_exe} but the dependencies are only required to run tests:
    in the [.opam] file, they are marked [with-test] (unless they are also needed
    by non-test code). *)
val test_exe : string maker

(** Same as {!test_exe} but with several names, to define multiple tests at once. *)
val test_exes : string list maker

(** Make an external vendored library, for use in internal target dependencies.

    [main_module] is the name of the main module provided by the library (see [open_]).

    [js_compatible]: whether the library can be compiled to JavaScript.
    Default value for [js_compatible] is false.

    [node_wrapper_flags]: flags to add to the [node_wrapper] executable
    command in [runtest_js] aliases. Only used for test and executable test targets.
 *)
val vendored_lib :
  ?main_module:string ->
  ?js_compatible:bool ->
  ?node_wrapper_flags:string list ->
  string ->
  target

(** Make an external library, for use in internal target dependencies.

    Usage: [external_lib name version_constraints]

    [name] is used in [dune] files, while [opam] is used in [.opam] files.
    Default value for [opam] is [name].

    [main_module] is the name of the main module provided by the library (see [open_]).

    [js_compatible]: whether the library can be compiled to JavaScript.
    Default value for [js_compatible] is false.

    [node_wrapper_flags]: flags to add to the [node_wrapper] executable
    command in [runtest_js] aliases. Only used for test and executable test targets.
  *)
val external_lib :
  ?main_module:string ->
  ?opam:string ->
  ?js_compatible:bool ->
  ?node_wrapper_flags:string list ->
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

    [node_wrapper_flags]: flags to add to the [node_wrapper] executable
    command in [runtest_js] aliases. Only used for test and executable test targets.

    @raise [Invalid_arg] if [main_lib] was not built with [external_lib]. *)
val external_sublib :
  ?main_module:string ->
  ?js_compatible:bool ->
  ?node_wrapper_flags:string list ->
  target ->
  string ->
  target

(** Make an external library that is to only appear in [.opam] dependencies.

    This avoids using [~opam_only_deps] each time you declare this dependency. *)
val opam_only : string -> Version.constraints -> target

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
    automatically prepended to [?opens] in the order of declaration in [?deps].
    If you use [open_] on an [open_], the innermost [open_]s is opened first;
    For instance, [tezos_base |> open_ |> open_ ~m: "TzPervasives"]
    is target [tezos_base], but when used in [?deps], this automatically prepend
    ["Tezos_base"], followed by ["Tezos_base__TzPervasives"], to [?opens].

    Can only be used on internal libraries and on external or vendored
    libraries for which a [main_module] was specified. *)
val open_ : ?m:string -> target -> target

(** Get a name for a given target, to display in errors.

    If a target has multiple names, one is chosen arbitrarily.
    So this should not be used except to display errors. *)
val name_for_errors : target -> string

(** Generate dune and opam files.

    Call this after you declared all your targets with functions such as
    [public_lib], [test], etc.

    If a [dune] or [.opam] file exists but is not generated by this function,
    an error is printed and the process exits with exit code 1.
    You can use [exclude] to specify that some files should not cause
    this error. [exclude] is given a path relative to the [manifest]
    directory (i.e. that usually starts with [../]) and shall return [true]
    if this path should not result in an error. *)
val generate : ?exclude:(string -> bool) -> unit -> unit
