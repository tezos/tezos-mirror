(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Error_monad
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type ('p, 'ctx) parameter = {
  converter : 'ctx -> string -> 'p tzresult Lwt.t;
  autocomplete : ('ctx -> string list tzresult Lwt.t) option;
}

let parameter ?autocomplete converter = {converter; autocomplete}

let compose_parameters {converter = c1; autocomplete = a1'}
    {converter = c2; autocomplete = a2'} =
  let open Lwt_result_syntax in
  {
    converter =
      (fun ctx s ->
        let*! r = c1 ctx s in
        match r with Ok r -> return r | Error _ -> c2 ctx s);
    autocomplete =
      (match a1' with
      | None -> a2'
      | Some a1 -> (
          match a2' with
          | None -> a1'
          | Some a2 ->
              Some
                (fun ctx ->
                  let* r1 = a1 ctx in
                  let* r2 = a2 ctx in
                  return (List.concat [r1; r2]))));
  }

let map_parameter ~f {converter; autocomplete} =
  let open Lwt_result_syntax in
  {
    converter =
      (fun ctx s ->
        let+ v = converter ctx s in
        f v);
    autocomplete;
  }

let map_es_parameter ~f {converter; autocomplete} =
  let open Lwt_result_syntax in
  {
    converter =
      (fun ctx s ->
        let* v = converter ctx s in
        f ctx v);
    autocomplete;
  }

type label = {long : string; short : char option}

type ('a, 'ctx) arg =
  | Arg : {
      doc : string;
      label : label;
      placeholder : string;
      kind : ('p, 'ctx) parameter;
    }
      -> ('p option, 'ctx) arg
  | DefArg : {
      doc : string;
      label : label;
      placeholder : string;
      kind : ('p, 'ctx) parameter;
      default : string;
    }
      -> ('p, 'ctx) arg
  | Switch : {label : label; doc : string} -> (bool, 'ctx) arg
  | Constant : 'a -> ('a, 'ctx) arg
  | Map : {
      spec : ('a, 'ctx) arg;
      converter : 'ctx -> 'a -> 'b tzresult Lwt.t;
    }
      -> ('b, 'ctx) arg
  | Pair : ('a, 'ctx) arg * ('b, 'ctx) arg -> ('a * 'b, 'ctx) arg

(* A simple structure for command interpreters.
   This is more generic than the exported one, see end of file. *)
type ('a, 'ctx) params =
  | Prefix : string * ('a, 'ctx) params -> ('a, 'ctx) params
  | Param :
      string * string * ('p, 'ctx) parameter * ('a, 'ctx) params
      -> ('p -> 'a, 'ctx) params
  | Stop : ('ctx -> unit tzresult Lwt.t, 'ctx) params
  | Seq :
      string * string * ('p, 'ctx) parameter
      -> ('p list -> 'ctx -> unit tzresult Lwt.t, 'ctx) params
  | NonTerminalSeq :
      string
      * string
      * ('p, 'ctx) parameter
      * string list
      * ('a -> 'b, 'ctx) params
      -> ('p list -> 'a -> 'b, 'ctx) params

type ('a, 'ctx) options = ('a, 'ctx) arg

let aggregate spec = spec

(* A command group *)
type group = {name : string; title : string}

(* A command wraps a callback with its type and info *)
type 'arg command =
  | Command : {
      params : ('a, 'iarg) params;
      options : ('b, 'iarg) options;
      handler : 'b -> 'a;
      desc : string;
      group : group option;
      conv : 'arg -> 'iarg;
    }
      -> 'arg command

type error += Bad_argument of int * string

type error += Unterminated_command : string list * 'ctx command list -> error

type error += Command_not_found : string list * 'ctx command list -> error

type error += Unknown_option : string * 'ctx command option -> error

type error += Option_expected_argument : string * 'ctx command option -> error

type error += Bad_option_argument : string * 'ctx command option -> error

type error += Multiple_occurrences : string * 'ctx command option -> error

type error += Extra_arguments : string list * 'ctx command -> error

let trim s =
  (* config-file workaround *)
  TzString.split_no_empty '\n' s |> List.map String.trim |> String.concat "\n"

let print_desc ppf doc =
  let short, long =
    match String.index_opt doc '\n' with
    | None -> (doc, None)
    | Some len ->
        ( String.sub doc 0 len,
          Some (String.sub doc (len + 1) (String.length doc - len - 1)) )
  in
  match long with
  | None -> Format.fprintf ppf "%s" short
  | Some doc ->
      Format.fprintf
        ppf
        "%s@{<full>@\n  @[<hov 0>%a@]@}"
        short
        Format.pp_print_text
        doc

let print_label ppf = function
  | {long; short = None} -> Format.fprintf ppf "--%s" long
  | {long; short = Some short} -> Format.fprintf ppf "-%c --%s" short long

let rec print_options_detailed :
    type ctx a. Format.formatter -> (a, ctx) options -> unit =
 fun ppf -> function
  | Arg {label; placeholder; doc; _} ->
      Format.fprintf
        ppf
        "@{<opt>%a <%s>@}: %a"
        print_label
        label
        placeholder
        print_desc
        doc
  | DefArg {label; placeholder; doc; default; _} ->
      Format.fprintf
        ppf
        "@{<opt>%a <%s>@}: %a"
        print_label
        label
        placeholder
        print_desc
        (doc ^ "\nDefaults to `" ^ default ^ "`.")
  | Switch {label; doc} ->
      Format.fprintf ppf "@{<opt>%a@}: %a" print_label label print_desc doc
  | Constant _ -> ()
  | Pair (speca, specb) ->
      Format.fprintf
        ppf
        "%a@,%a"
        print_options_detailed
        speca
        print_options_detailed
        specb
  | Map {spec; converter = _} -> print_options_detailed ppf spec

let rec has_args : type a ctx. (a, ctx) arg -> bool = function
  | Constant _ -> false
  | Arg _ | DefArg _ | Switch _ -> true
  | Pair (speca, specb) -> has_args speca || has_args specb
  | Map {spec; _} -> has_args spec

let rec print_options_brief :
    type ctx a. Format.formatter -> (a, ctx) arg -> unit =
 fun ppf -> function
  | DefArg {label; placeholder; _} ->
      Format.fprintf ppf "[@{<opt>%a <%s>@}]" print_label label placeholder
  | Arg {label; placeholder; _} ->
      Format.fprintf ppf "[@{<opt>%a <%s>@}]" print_label label placeholder
  | Switch {label; _} -> Format.fprintf ppf "[@{<opt>%a@}]" print_label label
  | Constant _ -> ()
  | Pair (speca, specb) ->
      Format.fprintf
        ppf
        "%a@ %a"
        print_options_brief
        speca
        print_options_brief
        specb
  | Map {spec; converter = _} -> print_options_brief ppf spec

let print_highlight highlight_strings formatter str =
  let rec print_string = function
    | [] -> Format.fprintf formatter "%s" str
    | regex :: tl -> (
        match Re.Str.full_split regex str with
        | [] | [Re.Str.Text _] -> print_string tl
        | list ->
            List.iter
              (function
                | Re.Str.Text text -> Format.fprintf formatter "%s" text
                | Re.Str.Delim delimiter ->
                    Format.fprintf formatter "@{<hilight>%s@}" delimiter)
              list)
  in
  print_string (List.map Re.Str.regexp_string highlight_strings)

let print_commandline ppf (highlights, options, args) =
  let print_suffix =
    Format.(pp_print_list ~pp_sep:pp_print_space (print_highlight highlights))
  in
  let rec print : type a ctx. Format.formatter -> (a, ctx) params -> unit =
   fun ppf -> function
    | Stop -> Format.fprintf ppf "%a" print_options_brief options
    | Seq (n, _, _) when not (has_args options) ->
        Format.fprintf ppf "[@{<arg>%s@}...]" n
    | Seq (n, _, _) ->
        Format.fprintf ppf "[@{<arg>%s@}...] %a" n print_options_brief options
    | NonTerminalSeq (n, _, _, suffix, Stop) when not (has_args options) ->
        Format.fprintf ppf "[@{<arg>%s@}...] @{<kwd>%a@}" n print_suffix suffix
    | NonTerminalSeq (n, _, _, suffix, next) ->
        Format.fprintf
          ppf
          "[@{<arg>%s@}...] @{<kwd>%a@} %a %a"
          n
          print_suffix
          suffix
          print
          next
          print_options_brief
          options
    | Prefix (n, Stop) when not (has_args options) ->
        Format.fprintf ppf "@{<kwd>%a@}" (print_highlight highlights) n
    | Prefix (n, next) ->
        Format.fprintf
          ppf
          "@{<kwd>%a@} %a"
          (print_highlight highlights)
          n
          print
          next
    | Param (n, _, _, Stop) when not (has_args options) ->
        Format.fprintf ppf "@{<arg>%s@}" n
    | Param (n, _, _, next) -> Format.fprintf ppf "@{<arg>%s@} %a" n print next
  in
  Format.fprintf ppf "@{<commandline>%a@}" print args

let rec print_params_detailed :
    type a b ctx. (b, ctx) arg -> Format.formatter -> (a, ctx) params -> unit =
 fun spec ppf -> function
  | Stop -> print_options_detailed ppf spec
  | Seq (n, desc, _) ->
      Format.fprintf ppf "@{<arg>%s@}: %a" n print_desc (trim desc) ;
      if has_args spec then
        Format.fprintf ppf "@,%a" print_options_detailed spec
  | NonTerminalSeq (n, desc, _, _, next) ->
      Format.fprintf ppf "@{<arg>%s@}: %a" n print_desc (trim desc) ;
      if has_args spec then
        Format.fprintf ppf "@,%a" (print_params_detailed spec) next
  | Prefix (_, next) -> print_params_detailed spec ppf next
  | Param (n, desc, _, Stop) ->
      Format.fprintf ppf "@{<arg>%s@}: %a" n print_desc (trim desc) ;
      if has_args spec then
        Format.fprintf ppf "@,%a" print_options_detailed spec
  | Param (n, desc, _, next) ->
      Format.fprintf
        ppf
        "@{<arg>%s@}: %a@,%a"
        n
        print_desc
        (trim desc)
        (print_params_detailed spec)
        next

let contains_params_args : type a ctx. (a, ctx) params -> (_, ctx) arg -> bool =
 fun params args ->
  let rec help : (a, ctx) params -> bool = function
    | Stop -> has_args args
    | Seq (_, _, _) -> true
    | NonTerminalSeq (_, _, _, _, _) -> true
    | Prefix (_, next) -> help next
    | Param (_, _, _, _) -> true
  in
  help params

let print_command :
    type ctx.
    ?prefix:(Format.formatter -> unit -> unit) ->
    ?highlights:string list ->
    Format.formatter ->
    ctx command ->
    unit =
 fun ?(prefix = fun _ () -> ())
     ?(highlights = [])
     ppf
     (Command {params; desc; options; _}) ->
  if contains_params_args params options then
    Format.fprintf
      ppf
      "@{<command>%a%a@{<short>@,@{<commanddoc>%a@,%a@}@}@}"
      prefix
      ()
      print_commandline
      (highlights, options, params)
      print_desc
      desc
      (print_params_detailed options)
      params
  else
    Format.fprintf
      ppf
      "@{<command>%a%a@{<short>@,@{<commanddoc>%a@}@}@}"
      prefix
      ()
      print_commandline
      (highlights, options, params)
      print_desc
      desc

type ex_command = Ex : _ command -> ex_command

let group_commands commands =
  let grouped, ungrouped =
    List.fold_left
      (fun (grouped, ungrouped) (Ex (Command {group; _}) as command) ->
        match group with
        | None -> (grouped, command :: ungrouped)
        | Some group -> (
            match
              List.find_opt (fun ({name; _}, _) -> group.name = name) grouped
            with
            | None -> ((group, ref [command]) :: grouped, ungrouped)
            | Some ({title; _}, r) ->
                if title <> group.title then
                  invalid_arg "Tezos_clic.usage: duplicate group name" ;
                r := command :: !r ;
                (grouped, ungrouped)))
      ([], [])
      commands
  in
  List.map
    (fun (g, c) -> (g, List.rev !c))
    (match ungrouped with
    | [] -> grouped
    | l ->
        grouped @ [({name = "misc"; title = "Miscellaneous commands"}, ref l)])

let print_group print_command ppf ({title; _}, commands) =
  Format.fprintf
    ppf
    "@{<title>%s@}@,@{<list>%a@}"
    title
    (Format.pp_print_list print_command)
    commands

type formatter_state =
  Format.formatter_out_functions * Format.formatter_stag_functions * bool

type format = Plain | Ansi | Html

type verbosity = Terse | Short | Details | Full

let setup_formatter ppf format verbosity =
  let skip = ref false in
  let ((orig_out_functions, _, _) as orig_state) =
    ( Format.pp_get_formatter_out_functions ppf (),
      Format.pp_get_formatter_stag_functions ppf (),
      Format.pp_get_print_tags ppf () )
  in
  (Format.pp_print_flush ppf () ;
   Format.pp_set_formatter_out_functions
     ppf
     {
       out_string =
         (fun s b a ->
           if s = "\000\000\000" then skip := true
           else if s = "\255\255\255" then skip := false
           else if not !skip then orig_out_functions.out_string s b a);
       out_spaces = (fun n -> if not !skip then orig_out_functions.out_spaces n);
       out_newline =
         (fun () -> if not !skip then orig_out_functions.out_newline ());
       out_flush = (fun () -> if not !skip then orig_out_functions.out_flush ());
       out_indent = orig_out_functions.out_indent;
     } ;
   let levels = ref [] in
   let setup_level (level, op) =
     if op level verbosity then Format.fprintf ppf "@<0>%s" "\255\255\255"
     else Format.fprintf ppf "@<0>%s" "\000\000\000"
   in
   let push_level level =
     levels := level :: !levels ;
     setup_level level
   in
   let pop_level () =
     match !levels with
     | _ :: level :: rest ->
         levels := level :: rest ;
         setup_level level
     | [_] | [] -> Stdlib.failwith "Tezos_clic: unclosed verbosity tag"
   in
   push_level (Terse, ( <= )) ;
   let push_level_tag = function
     | Format.String_tag tag ->
         let push op = function
           | "full" -> push_level (Full, op)
           | "details" -> push_level (Details, op)
           | "short" -> push_level (Short, op)
           | "terse" -> push_level (Terse, op)
           | tag ->
               Stdlib.failwith
                 ("Tezos_clic: invalid semantic string tag <" ^ tag ^ ">")
         in
         if String.length tag > 0 && tag.[0] = '=' then
           push ( = ) (String.sub tag 1 (String.length tag - 1))
         else if String.length tag > 0 && tag.[0] = '-' then
           push ( > ) (String.sub tag 1 (String.length tag - 1))
         else push ( <= ) tag
     | _stag -> Stdlib.failwith "Tezos_clic: invalid semantic tag"
   in
   let pop_level_tag = function
     | Format.String_tag "full"
     | Format.String_tag "details"
     | Format.String_tag "short"
     | Format.String_tag "terse"
     | Format.String_tag "-full"
     | Format.String_tag "-details"
     | Format.String_tag "-short"
     | Format.String_tag "-terse"
     | Format.String_tag "=full"
     | Format.String_tag "=details"
     | Format.String_tag "=short"
     | Format.String_tag "=terse" ->
         pop_level ()
     | Format.String_tag tag ->
         Stdlib.failwith
           ("Tezos_clic: invalid semantic string tag <" ^ tag ^ ">")
     | _stag -> Stdlib.failwith "Tezos_clic: invalid semantic tag"
   in
   match format with
   | Ansi ->
       let color_num = function
         | `Auto -> None
         | `Black -> Some 0
         | `Red -> Some 1
         | `Green -> Some 2
         | `Yellow -> Some 3
         | `Blue -> Some 4
         | `Magenta -> Some 5
         | `Cyan -> Some 6
         | `White -> Some 7
       in
       let ansi_format ppf (fg, bg, b, u) =
         Format.fprintf ppf "@<0>%s" "\027[0m" ;
         match
           (match color_num fg with
           | Some n -> [string_of_int (30 + n)]
           | None -> [])
           @ (match color_num bg with
             | Some n -> [string_of_int (40 + n)]
             | None -> [])
           @ (if b then ["1"] else [])
           @ if u then ["4"] else []
         with
         | [] -> ()
         | l -> Format.fprintf ppf "@<0>%s" ("\027[" ^ String.concat ";" l ^ "m")
       in
       let ansi_stack = ref [(`Auto, `Auto, false, false)] in
       let push_ansi_format (fg, bg, b, u) =
         let format =
           match !ansi_stack with
           | (pfg, pbg, pb, pu) :: _ ->
               ( Option.value ~default:pfg fg,
                 Option.value ~default:pbg bg,
                 pb || b,
                 pu || u )
           | [] -> assert false
         in
         ansi_stack := format :: !ansi_stack ;
         Format.fprintf ppf "@<0>%a" ansi_format format
       in
       let pop_ansi_format () =
         Format.fprintf ppf "@<0>%s" "\027[0m" ;
         match !ansi_stack with
         | _ :: format :: rest ->
             ansi_stack := format :: rest ;
             Format.fprintf ppf "@<0>%a" ansi_format format
         | [_] | [] -> Stdlib.failwith "Tezos_clic: unclosed ansi format"
       in
       Format.pp_set_formatter_stag_functions
         ppf
         {
           mark_open_stag = (fun _ -> "");
           mark_close_stag = (fun _ -> "");
           print_open_stag =
             (function
             | Format.String_tag "title" ->
                 push_ansi_format (None, None, true, true)
             | Format.String_tag "commandline" -> Format.fprintf ppf "@[<hov 4>"
             | Format.String_tag "commanddoc" -> Format.fprintf ppf "  @[<v 0>"
             | Format.String_tag "opt" ->
                 push_ansi_format (Some `Green, None, false, false)
             | Format.String_tag "arg" ->
                 push_ansi_format (Some `Yellow, None, false, false) ;
                 Format.fprintf ppf "<"
             | Format.String_tag "kwd" ->
                 push_ansi_format (None, None, false, true)
             | Format.String_tag "error" ->
                 push_ansi_format (Some `Red, None, true, true)
             | Format.String_tag "warning" ->
                 push_ansi_format (Some `Yellow, None, true, true)
             | Format.String_tag "hilight" ->
                 push_ansi_format (Some `White, Some `Yellow, true, true)
             | Format.String_tag "list" -> Format.fprintf ppf "  @[<v 0>"
             | Format.String_tag "command" -> Format.fprintf ppf "@[<v 0>"
             | Format.String_tag "document" -> Format.fprintf ppf "@[<v 0>"
             | other -> push_level_tag other);
           print_close_stag =
             (function
             | Format.String_tag "title" ->
                 Format.fprintf ppf ":" ;
                 pop_ansi_format ()
             | Format.String_tag "commandline" -> Format.fprintf ppf "@]"
             | Format.String_tag "commanddoc" -> Format.fprintf ppf "@]"
             | Format.String_tag "opt" -> pop_ansi_format ()
             | Format.String_tag "arg" ->
                 Format.fprintf ppf ">" ;
                 pop_ansi_format ()
             | Format.String_tag "kwd" -> pop_ansi_format ()
             | Format.String_tag "error" -> pop_ansi_format ()
             | Format.String_tag "warning" -> pop_ansi_format ()
             | Format.String_tag "hilight" -> pop_ansi_format ()
             | Format.String_tag "command" | Format.String_tag "list" ->
                 Format.fprintf ppf "@]"
             | Format.String_tag "document" -> Format.fprintf ppf "@]"
             | other -> pop_level_tag other);
         } ;
       Format.pp_set_print_tags ppf true
   | Plain ->
       Format.pp_set_formatter_stag_functions
         ppf
         {
           mark_open_stag = (fun _ -> "");
           mark_close_stag = (fun _ -> "");
           print_open_stag =
             (function
             | Format.String_tag "title" -> ()
             | Format.String_tag "commandline" -> Format.fprintf ppf "@[<hov 4>"
             | Format.String_tag "commanddoc" -> Format.fprintf ppf "  @[<v 0>"
             | Format.String_tag "opt" -> ()
             | Format.String_tag "arg" -> Format.fprintf ppf "<"
             | Format.String_tag "kwd" -> ()
             | Format.String_tag "hilight" -> ()
             | Format.String_tag "error" -> ()
             | Format.String_tag "warning" -> ()
             | Format.String_tag "list" -> Format.fprintf ppf "  @[<v 0>"
             | Format.String_tag "command" -> Format.fprintf ppf "@[<v 0>"
             | Format.String_tag "document" -> Format.fprintf ppf "@[<v 0>"
             | other -> push_level_tag other);
           print_close_stag =
             (function
             | Format.String_tag "title" -> Format.fprintf ppf ":"
             | Format.String_tag "commandline" -> Format.fprintf ppf "@]"
             | Format.String_tag "commanddoc" -> Format.fprintf ppf "@]"
             | Format.String_tag "opt" -> ()
             | Format.String_tag "arg" -> Format.fprintf ppf ">"
             | Format.String_tag "kwd" -> ()
             | Format.String_tag "error" -> ()
             | Format.String_tag "warning" -> ()
             | Format.String_tag "hilight" -> ()
             | Format.String_tag "command" | Format.String_tag "list" ->
                 Format.fprintf ppf "@]"
             | Format.String_tag "document" -> Format.fprintf ppf "@]"
             | other -> pop_level_tag other);
         } ;
       Format.pp_set_print_tags ppf true
   | Html ->
       Format.pp_set_formatter_stag_functions
         ppf
         {
           mark_open_stag = (fun _ -> "");
           mark_close_stag = (fun _ -> "");
           print_open_stag =
             (function
             | Format.String_tag "title" -> Format.fprintf ppf "\003h3\004"
             | Format.String_tag "commandline" ->
                 Format.fprintf ppf "\003div class='cmdline'\004@[<h>"
             | Format.String_tag "commanddoc" ->
                 Format.fprintf ppf "\003div class='cmddoc'\004"
             | Format.String_tag "opt" ->
                 Format.fprintf ppf "\003span class='opt'\004"
             | Format.String_tag "arg" ->
                 Format.fprintf ppf "\003span class='arg'\004"
             | Format.String_tag "kwd" ->
                 Format.fprintf ppf "\003span class='kwd'\004"
             | Format.String_tag "hilight" -> ()
             | Format.String_tag "error" -> ()
             | Format.String_tag "warning" -> ()
             | Format.String_tag "list" -> Format.fprintf ppf "\003ul\004@\n"
             | Format.String_tag "command" -> Format.fprintf ppf "\003li\004@\n"
             | Format.String_tag "document" ->
                 Format.fprintf
                   ppf
                   "@[<v 0>\003style\004.cmdline { font-family: monospace \
                    }.cmddoc { white-space: pre-wrap ; font-family: monospace; \
                    line-height: 170%%; margin: 0 0 20px 0 }.cmdline { \
                    background: #343131; padding: 2px 8px; border-radius:10px; \
                    color: white; margin: 5px; }.cmdline+.cmddoc { margin: \
                    -5px 5px 0 20px; padding: 5px }.opt,.arg { background: \
                    #343131; font-weight: bold;  padding: 2px 4px; \
                    border-radius:5px; }.kwd { font-weight: bold; } .opt { \
                    color:#CF0; background: #460; } .arg { color: #CEF; \
                    background: #369; }\003/style\004@\n"
             | other -> push_level_tag other);
           print_close_stag =
             (function
             | Format.String_tag "title" -> Format.fprintf ppf "\003/h3\004@\n"
             | Format.String_tag "commandline" ->
                 Format.fprintf ppf "@]\003/div\004@\n"
             | Format.String_tag "commanddoc" ->
                 Format.fprintf ppf "\003/div\004@\n"
             | Format.String_tag "opt"
             | Format.String_tag "arg"
             | Format.String_tag "kwd" ->
                 Format.fprintf ppf "\003/span\004"
             | Format.String_tag "error"
             | Format.String_tag "warning"
             | Format.String_tag "hilight" ->
                 ()
             | Format.String_tag "list" -> Format.fprintf ppf "\003/ul\004@\n"
             | Format.String_tag "command" ->
                 Format.fprintf ppf "\003/li\004@\n"
             | Format.String_tag "document" -> Format.fprintf ppf "@]"
             | other -> pop_level_tag other);
         } ;
       let orig_out_functions = Format.pp_get_formatter_out_functions ppf () in
       Format.pp_set_formatter_out_functions
         ppf
         {
           orig_out_functions with
           out_string =
             (fun s i j ->
               let buf = Buffer.create (j - i) in
               for n = i to j - 1 do
                 match s.[n] with
                 | '\003' -> Buffer.add_char buf '<'
                 | '\004' -> Buffer.add_char buf '>'
                 | '>' -> Buffer.add_string buf "&gt;"
                 | '<' -> Buffer.add_string buf "&lt;"
                 | c -> Buffer.add_char buf c
               done ;
               let s' = Buffer.contents buf in
               orig_out_functions.out_string s' 0 (String.length s'));
         } ;
       Format.pp_set_print_tags ppf true) ;
  orig_state

let restore_formatter ppf (out_functions, tag_functions, tags) =
  Format.pp_print_flush ppf () ;
  Format.pp_set_formatter_out_functions ppf out_functions ;
  Format.pp_set_formatter_stag_functions ppf tag_functions ;
  Format.pp_set_print_tags ppf tags

let usage_internal ppf ~executable_name ~global_options ?(highlights = [])
    commands =
  let by_group = group_commands commands in
  let print_groups =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,@,")
      (print_group (fun ppf (Ex command) ->
           print_command ?prefix:None ~highlights ppf command))
  in
  Format.fprintf
    ppf
    "@{<document>@{<title>Usage@}@,\
     @{<list>@{<command>@{<commandline>%s [@{<opt>global options@}] \
     @{<kwd>command@} [@{<opt>command options@}]@}@}@,\
     @{<command>@{<commandline>%s @{<opt>--help@} (for global options)@}@}@,\
     @{<command>@{<commandline>%s [@{<opt>global options@}] @{<kwd>command@} \
     @{<opt>--help@} (for command options)@}@}@,\
     @{<command>@{<commandline>%s @{<opt>--version@} (for version \
     information)@}@}@}@,\
     @,\
     @{<title>To browse the documentation@}@,\
     @{<list>@{<command>@{<commandline>%s [@{<opt>global options@}] \
     @{<kwd>man@} (for a list of commands)@}@}@,\
     @{<command>@{<commandline>%s [@{<opt>global options@}] @{<kwd>man@} \
     @{<opt>-v 3@} (for the full manual)@}@}@}@,\
     @,\
     @{<title>Global options (must come before the command)@}@,\
     @{<commanddoc>%a@}%a%a@}@."
    executable_name
    executable_name
    executable_name
    executable_name
    executable_name
    executable_name
    print_options_detailed
    global_options
    (fun ppf () -> if by_group <> [] then Format.fprintf ppf "@,@,")
    ()
    print_groups
    by_group

let constant c = Constant c

let arg ~doc ?short ~long ~placeholder kind =
  Arg {doc; label = {long; short}; placeholder; kind}

let default_arg ~doc ?short ~long ~placeholder ~default kind =
  DefArg {doc; placeholder; label = {long; short}; kind; default}

let map_arg ~f:converter spec = Map {spec; converter}

let args1 a = a

let args2 a b = Pair (a, b)

let args3 a b c =
  map_arg
    ~f:(fun _ ((a, b), c) -> Lwt_result_syntax.return (a, b, c))
    (args2 (args2 a b) c)

let args4 a b c d =
  map_arg
    ~f:(fun _ ((a, b), (c, d)) -> Lwt_result_syntax.return (a, b, c, d))
    (args2 (args2 a b) (args2 c d))

let args5 a b c d e =
  map_arg
    ~f:(fun _ ((a, b, c, d), e) -> Lwt_result_syntax.return (a, b, c, d, e))
    (args2 (args4 a b c d) e)

let args6 a b c d e f =
  map_arg
    ~f:(fun _ ((a, b, c, d), (e, f)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f))
    (args2 (args4 a b c d) (args2 e f))

let args7 a b c d e f g =
  map_arg
    ~f:(fun _ ((a, b, c, d), (e, f, g)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g))
    (args2 (args4 a b c d) (args3 e f g))

let args8 a b c d e f g h =
  map_arg
    ~f:(fun _ ((a, b, c, d), (e, f, g, h)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h))
    (args2 (args4 a b c d) (args4 e f g h))

let args9 a b c d e f g h i =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), i) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i))
    (args2 (args8 a b c d e f g h) i)

let args10 a b c d e f g h i j =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j))
    (args2 (args8 a b c d e f g h) (args2 i j))

let args11 a b c d e f g h i j k =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j, k)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j, k))
    (args2 (args8 a b c d e f g h) (args3 i j k))

let args12 a b c d e f g h i j k l =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j, k, l)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j, k, l))
    (args2 (args8 a b c d e f g h) (args4 i j k l))

let args13 a b c d e f g h i j k l m =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j, k, l, m)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j, k, l, m))
    (args2 (args8 a b c d e f g h) (args5 i j k l m))

let args14 a b c d e f g h i j k l m n =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j, k, l, m, n)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j, k, l, m, n))
    (args2 (args8 a b c d e f g h) (args6 i j k l m n))

let args15 a b c d e f g h i j k l m n o =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j, k, l, m, n, o)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
    (args2 (args8 a b c d e f g h) (args7 i j k l m n o))

let args16 a b c d e f g h i j k l m n o p =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h), (i, j, k, l, m, n, o, p)) ->
      Lwt_result_syntax.return (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
    (args2 (args8 a b c d e f g h) (args8 i j k l m n o p))

let args17 a b c d e f g h i j k l m n o p q =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), q) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
    (args2 (args16 a b c d e f g h i j k l m n o p) q)

let args18 a b c d e f g h i j k l m n o p q r =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q, r)) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args2 q r))

let args19 a b c d e f g h i j k l m n o p q r s =
  map_arg
    ~f:(fun _ ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q, r, s)) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args3 q r s))

let args20 a b c d e f g h i j k l m n o p q r s t =
  map_arg
    ~f:
      (fun _ ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q, r, s, t)) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args4 q r s t))

let args21 a b c d e f g h i j k l m n o p q r s t u =
  map_arg
    ~f:
      (fun _ ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q, r, s, t, u)) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args5 q r s t u))

let args22 a b c d e f g h i j k l m n o p q r s t u v =
  map_arg
    ~f:
      (fun _
           ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q, r, s, t, u, v))
           ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args6 q r s t u v))

let args23 a b c d e f g h i j k l m n o p q r s t u v w =
  map_arg
    ~f:
      (fun _
           ( (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p),
             (q, r, s, t, u, v, w) ) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args7 q r s t u v w))

let args24 a b c d e f g h i j k l m n o p q r s t u v w x =
  map_arg
    ~f:
      (fun _
           ( (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p),
             (q, r, s, t, u, v, w, x) ) ->
      Lwt_result_syntax.return
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args8 q r s t u v w x))

let args25 a b c d e f g h i j k l m n o p q r s t u v w x y =
  map_arg
    ~f:
      (fun _
           ( (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p),
             (q, r, s, t, u, v, w, x, y) ) ->
      Lwt_result_syntax.return
        ( a,
          b,
          c,
          d,
          e,
          f,
          g,
          h,
          i,
          j,
          k,
          l,
          m,
          n,
          o,
          p,
          q,
          r,
          s,
          t,
          u,
          v,
          w,
          x,
          y ))
    (args2 (args16 a b c d e f g h i j k l m n o p) (args9 q r s t u v w x y))

let switch ~doc ?short ~long () = Switch {doc; label = {long; short}}

(* Argument parsing *)
let rec parse_arg :
    type a ctx.
    ?command:_ command ->
    (a, ctx) arg ->
    string list StringMap.t ->
    ctx ->
    a tzresult Lwt.t =
 fun ?command spec args_dict ctx ->
  let open Lwt_result_syntax in
  match spec with
  | Arg {label = {long; short = _}; kind = {converter; _}; _} -> (
      match StringMap.find_opt long args_dict with
      | None | Some [] -> return_none
      | Some [s] ->
          let+ x =
            trace_eval (fun () -> Bad_option_argument ("--" ^ long, command))
            @@ converter ctx s
          in
          Some x
      | Some (_ :: _) -> tzfail (Multiple_occurrences ("--" ^ long, command)))
  | DefArg {label = {long; short = _}; kind = {converter; _}; default; _} -> (
      let*! r = converter ctx default in
      match r with
      | Error _ ->
          invalid_arg
            (Format.sprintf
               "Value provided as default for '%s' could not be parsed by \
                converter function."
               long)
      | Ok default -> (
          match StringMap.find_opt long args_dict with
          | None | Some [] -> return default
          | Some [s] ->
              trace (Bad_option_argument (long, command)) (converter ctx s)
          | Some (_ :: _) -> tzfail (Multiple_occurrences (long, command))))
  | Switch {label = {long; short = _}; _} -> (
      match StringMap.find_opt long args_dict with
      | None | Some [] -> return_false
      | Some [_] -> return_true
      | Some (_ :: _) -> tzfail (Multiple_occurrences (long, command)))
  | Constant c -> return c
  | Pair (speca, specb) ->
      let* arga = parse_arg ?command speca args_dict ctx in
      let* argb = parse_arg ?command specb args_dict ctx in
      return (arga, argb)
  | Map {spec; converter} ->
      let* arg = parse_arg ?command spec args_dict ctx in
      converter ctx arg

let empty_args_dict = StringMap.empty

let rec make_arities_dict :
    type a b.
    (a, b) arg -> (int * string) StringMap.t -> (int * string) StringMap.t =
 fun arg acc ->
  let add {long; short} num =
    (match short with
    | None -> acc
    | Some c -> StringMap.add ("-" ^ String.make 1 c) (num, long) acc)
    |> StringMap.add ("-" ^ long) (num, long)
    |> StringMap.add ("--" ^ long) (num, long)
  in
  match arg with
  | Arg {label; _} -> add label 1
  | DefArg {label; _} -> add label 1
  | Switch {label; _} -> add label 0
  | Constant _c -> acc
  | Pair (speca, specb) ->
      let acc = make_arities_dict speca acc in
      let acc = make_arities_dict specb acc in
      acc
  | Map {spec; _} -> make_arities_dict spec acc

type error += Version : error

type error += Help : 'a command option -> error

let check_help_flag ?command =
  let open Lwt_result_syntax in
  function ("-h" | "--help") :: _ -> tzfail (Help command) | _ -> return_unit

let check_version_flag =
  let open Lwt_result_syntax in
  function
  (* No "-v", it is taken by man output verbosity *)
  | "--version" :: _ -> tzfail Version
  | _ -> return_unit

let add_occurrence long value acc =
  match StringMap.find_opt long acc with
  | Some v -> StringMap.add long v acc
  | None -> StringMap.add long [value] acc

let make_args_dict_consume ?command spec args =
  let open Lwt_result_syntax in
  let rec make_args_dict completing arities acc args =
    let* () = check_help_flag ?command args in
    let* () = check_version_flag args in
    match args with
    | [] -> return (acc, [])
    | arg :: tl ->
        if String.length arg > 0 && arg.[0] = '-' then
          match StringMap.find arg arities with
          | Some (arity, long) -> (
              let* () = check_help_flag ?command tl in
              match (arity, tl) with
              | 0, tl' ->
                  make_args_dict
                    completing
                    arities
                    (add_occurrence long "" acc)
                    tl'
              | 1, value :: tl' ->
                  make_args_dict
                    completing
                    arities
                    (add_occurrence long value acc)
                    tl'
              | 1, [] when completing -> return (acc, [])
              | 1, [] -> tzfail (Option_expected_argument (arg, None))
              | _, _ ->
                  Stdlib.failwith
                    "cli_entries: Arguments with arity not equal to 1 or 0 \
                     unsupported")
          | None -> tzfail (Unknown_option (arg, None))
        else return (acc, args)
  in
  make_args_dict
    false
    (make_arities_dict spec StringMap.empty)
    StringMap.empty
    args

let make_args_dict_filter ?command spec args =
  let open Lwt_result_syntax in
  let rec make_args_dict arities (dict, other_args) args =
    let* () = check_help_flag ?command args in
    match args with
    | [] -> return (dict, other_args)
    | arg :: tl -> (
        match StringMap.find arg arities with
        | Some (arity, long) -> (
            let* () = check_help_flag ?command tl in
            match (arity, tl) with
            | 0, tl ->
                make_args_dict
                  arities
                  (add_occurrence long "" dict, other_args)
                  tl
            | 1, value :: tl' ->
                make_args_dict
                  arities
                  (add_occurrence long value dict, other_args)
                  tl'
            | 1, [] -> tzfail (Option_expected_argument (arg, command))
            | _, _ ->
                Stdlib.failwith
                  "cli_entries: Arguments with arity not equal to 1 or 0 \
                   unsupported")
        | None -> make_args_dict arities (dict, arg :: other_args) tl)
  in
  let+ dict, remaining =
    make_args_dict
      (make_arities_dict spec StringMap.empty)
      (StringMap.empty, [])
      args
  in
  (dict, List.rev remaining)

(* Some combinators for writing commands concisely. *)
let param ~name ~desc kind next = Param (name, desc, kind, next)

let seq_of_param param =
  match param Stop with
  | Param (n, desc, parameter, Stop) -> Seq (n, desc, parameter)
  | _ -> invalid_arg "Tezos_clic.seq_of_param"

let non_terminal_seq ~suffix param next =
  match (suffix, param Stop) with
  | [], _ -> invalid_arg "Tezos_clic.non_terminal_seq: empty suffix"
  | _, Param (n, desc, parameter, Stop) ->
      NonTerminalSeq (n, desc, parameter, suffix, next)
  | _ -> invalid_arg "Tezos_clic.non_terminal_seq"

let prefix keyword next = Prefix (keyword, next)

let rec fixed = function [] -> Stop | n :: r -> Prefix (n, fixed r)

let rec prefixes p next =
  match p with [] -> next | n :: r -> Prefix (n, prefixes r next)

let stop = Stop

let no_options = Constant ()

let command ?group ~desc options params handler =
  Command {params; options; handler; desc; group; conv = (fun x -> x)}

(* Param combinators *)
let string ~name ~desc next =
  param
    ~name
    ~desc
    {converter = (fun _ s -> Lwt.return_ok s); autocomplete = None}
    next

let string_contains ~needle ~haystack =
  Option.catch
    ~catch_only:(function Not_found -> true | _ -> false)
    (fun () -> Re.Str.search_forward (Re.Str.regexp_string needle) haystack 0)

let rec search_params_prefix : type a arg. string -> (a, arg) params -> bool =
 fun prefix -> function
  | Prefix (keyword, next) -> (
      match string_contains ~needle:prefix ~haystack:keyword with
      | None -> search_params_prefix prefix next
      | Some _ -> true)
  | Param (_, _, _, next) -> search_params_prefix prefix next
  | Stop -> false
  | Seq _ -> false
  | NonTerminalSeq (_, _, _, suffix, next) ->
      List.exists
        (fun keyword ->
          match string_contains ~needle:prefix ~haystack:keyword with
          | None -> false
          | Some _ -> true)
        suffix
      || search_params_prefix prefix next

let search_command keyword (Command {params; _}) =
  search_params_prefix keyword params

(* Command execution *)
let exec (type ctx)
    (Command {options = options_spec; params = spec; handler; conv; _} as
    command) (ctx : ctx) params args_dict =
  let open Lwt_result_syntax in
  let rec exec :
      type ctx a.
      int -> ctx -> (a, ctx) params -> a -> string list -> unit tzresult Lwt.t =
   fun i ctx spec cb params ->
    match (spec, params) with
    | Stop, _ -> cb ctx
    | Seq (_, _, {converter; _}), seq ->
        let rec do_seq i acc = function
          | [] -> return (List.rev acc)
          | p :: rest ->
              let* v =
                Error_monad.catch_es (fun () -> converter ctx p)
                |> trace (Bad_argument (i, p))
              in
              do_seq (succ i) (v :: acc) rest
        in
        let* parsed = do_seq i [] seq in
        cb parsed ctx
    | NonTerminalSeq (_, _, {converter; _}, suffix, next), seq ->
        let rec do_seq i acc = function
          | [] -> return (List.rev acc, [])
          | p :: rest as params ->
              (* try to match suffix first *)
              let rec match_suffix = function
                | param :: params, suffix :: suffixes when param = suffix ->
                    match_suffix (params, suffixes)
                | params, [] ->
                    (* all of the suffix parts have been matched *)
                    (params, true)
                | _, _ -> (params, false)
              in
              let unmatched_rest, matched = match_suffix (params, suffix) in
              if matched then return (List.rev acc, unmatched_rest)
              else
                (* if suffix is not match, try to continue with the sequence *)
                Error_monad.catch_es (fun () ->
                    let* v = converter ctx p in
                    do_seq (succ i) (v :: acc) rest)
        in
        let* parsed, rest = do_seq i [] seq in
        exec (succ i) ctx next (cb parsed) rest
    | Prefix (n, next), p :: rest when n = p -> exec (succ i) ctx next cb rest
    | Param (_, _, {converter; _}, next), p :: rest ->
        let* v =
          Error_monad.catch_es (fun () -> converter ctx p)
          |> trace (Bad_argument (i, p))
        in
        exec (succ i) ctx next (cb v) rest
    | _ -> Stdlib.failwith "cli_entries internal error: exec no case matched"
  in
  let ctx = conv ctx in
  let* parsed_options = parse_arg ~command options_spec args_dict ctx in
  exec 1 ctx spec (handler parsed_options) params

[@@@ocaml.warning "-30"]

(* Command dispatch tree *)
type 'arg level = {
  stop : 'arg command option;
  prefix : (string * 'arg tree) list;
}

and 'arg param_level = {
  stop : 'arg command option;
  autocomplete : ('arg -> string list tzresult Lwt.t) option;
  tree : 'arg tree;
}

and 'arg non_terminal_seq_level = {
  stop : 'arg command option;
  autocomplete : ('arg -> string list tzresult Lwt.t) option;
  tree : 'arg tree;
  name : string;
  desc : string;
  suffix : string list;
}

and 'ctx tree =
  | TPrefix : 'ctx level -> 'ctx tree
  | TParam : 'ctx param_level -> 'ctx tree
  | TStop : 'ctx command -> 'ctx tree
  | TSeq :
      'ctx command * ('ctx -> string list tzresult Lwt.t) option
      -> 'ctx tree
  | TNonTerminalSeq : 'ctx non_terminal_seq_level -> 'ctx tree
  | TEmpty : 'ctx tree

let insert_in_dispatch_tree : type ctx. ctx tree -> ctx command -> ctx tree =
 fun root (Command {params; conv; _} as command) ->
  let rec insert_tree :
      type a ictx. (ctx -> ictx) -> ctx tree -> (a, ictx) params -> ctx tree =
   fun conv t c ->
    let insert_tree t c = insert_tree conv t c in
    let rec suffix_to_params suffix next =
      match suffix with
      | suffix :: suffixes -> Prefix (suffix, suffix_to_params suffixes next)
      | [] -> next
    in
    let suffix_to_tree suffix next =
      insert_tree TEmpty (suffix_to_params suffix next)
    in
    let conv_autocomplete = Option.map (fun a c -> a (conv c)) in
    match (t, c) with
    | TEmpty, Stop -> TStop command
    | TEmpty, Seq (_, _, {autocomplete; _}) ->
        TSeq (command, conv_autocomplete autocomplete)
    | TEmpty, Param (_, _, {autocomplete; _}, next) ->
        let autocomplete = conv_autocomplete autocomplete in
        TParam {tree = insert_tree TEmpty next; stop = None; autocomplete}
    | TEmpty, NonTerminalSeq (name, desc, {autocomplete; _}, suffix, next) ->
        let autocomplete = conv_autocomplete autocomplete in
        let tree = suffix_to_tree suffix next in
        TNonTerminalSeq {stop = None; tree; autocomplete; suffix; name; desc}
    | TEmpty, Prefix (n, next) ->
        TPrefix {stop = None; prefix = [(n, insert_tree TEmpty next)]}
    | TStop cmd, Param (_, _, {autocomplete; _}, next) ->
        let autocomplete = conv_autocomplete autocomplete in
        TParam {tree = insert_tree TEmpty next; stop = Some cmd; autocomplete}
    | TStop cmd, Prefix (n, next) ->
        TPrefix {stop = Some cmd; prefix = [(n, insert_tree TEmpty next)]}
    | TStop cmd, NonTerminalSeq (name, desc, {autocomplete; _}, suffix, next) ->
        let autocomplete = conv_autocomplete autocomplete in
        let tree = suffix_to_tree suffix next in
        TNonTerminalSeq
          {stop = Some cmd; tree; autocomplete; suffix; name; desc}
    | TParam t, Param (_, _, _, next) ->
        TParam {t with tree = insert_tree t.tree next}
    | TPrefix ({prefix; _} as l), Prefix (n, next) ->
        let rec insert_prefix = function
          | [] -> [(n, insert_tree TEmpty next)]
          | (n', t) :: rest when n = n' -> (n, insert_tree t next) :: rest
          | item :: rest -> item :: insert_prefix rest
        in
        TPrefix {l with prefix = insert_prefix prefix}
    | TPrefix ({stop = None; _} as l), Stop ->
        TPrefix {l with stop = Some command}
    | TParam ({stop = None; _} as l), Stop ->
        TParam {l with stop = Some command}
    | TParam t, Prefix (_n, next) ->
        TParam {t with tree = insert_tree t.tree next}
    | TNonTerminalSeq t, NonTerminalSeq (n, desc, _, suffix, next) ->
        if
          n <> t.name || desc <> t.desc || t.suffix <> suffix
          (* we should match the parameter too but this would require a bit of refactoring*)
        then
          Stdlib.failwith
            "Command cannot have different non_terminal_seq_level at the same \
             position"
        else
          let params = suffix_to_params suffix next in
          TNonTerminalSeq {t with tree = insert_tree t.tree params}
    | _, _ ->
        Stdlib.failwith
          (Format.asprintf
             "Tezos_clic.Command_tree.insert: conflicting commands \"%a\""
             (fun ppf (Command {params; options; _}) ->
               print_commandline ppf ([], options, params))
             command)
  in
  insert_tree conv root params

let make_dispatch_tree commands =
  List.fold_left insert_in_dispatch_tree TEmpty commands

let rec gather_commands ?(acc = []) tree =
  match tree with
  | TEmpty -> acc
  | TSeq (c, _) | TStop c -> c :: acc
  | TPrefix {stop; prefix} ->
      gather_assoc
        ~acc:(match stop with None -> acc | Some c -> c :: acc)
        prefix
  | TParam {tree; stop; _} | TNonTerminalSeq {tree; stop; _} ->
      gather_commands
        tree
        ~acc:(match stop with None -> acc | Some c -> c :: acc)

and gather_assoc ?(acc = []) trees =
  List.fold_left (fun acc (_, tree) -> gather_commands tree ~acc) acc trees

let find_command tree initial_arguments =
  let open Lwt_result_syntax in
  let is_short_option s =
    String.length s = 2
    && s.[0] = '-'
    && match s.[1] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  in
  let is_long_option s = String.length s >= 2 && s.[0] = '-' && s.[1] = '-' in
  let is_option s = is_short_option s || is_long_option s in
  let rec traverse tree arguments acc =
    match (tree, arguments) with
    | ( ( TStop _ | TSeq _
        | TNonTerminalSeq {stop = Some _; _}
        | TPrefix {stop = Some _; _}
        | TParam {stop = Some _; _} ),
        ("-h" | "--help") :: _ ) -> (
        match gather_commands tree with
        | [] -> assert false
        | [command] -> tzfail (Help (Some command))
        | more -> tzfail (Unterminated_command (initial_arguments, more)))
    | TStop c, [] -> return (c, empty_args_dict, initial_arguments)
    | TStop (Command {options; _} as command), remaining -> (
        let* args_dict, unparsed =
          make_args_dict_filter ~command options remaining
        in
        match unparsed with
        | [] -> return (command, args_dict, initial_arguments)
        | hd :: _ ->
            if String.length hd > 0 && hd.[0] = '-' then
              tzfail (Unknown_option (hd, Some command))
            else tzfail (Extra_arguments (unparsed, command)))
    | TSeq ((Command {options; _} as command), _), remaining ->
        if
          List.exists
            (function "-h" | "--help" -> true | _ -> false)
            remaining
        then tzfail (Help (Some command))
        else
          let+ dict, remaining =
            make_args_dict_filter ~command options remaining
          in
          (command, dict, List.rev_append acc remaining)
    | TNonTerminalSeq {stop = None; _}, ([] | ("-h" | "--help") :: _) ->
        tzfail (Unterminated_command (initial_arguments, gather_commands tree))
    | TNonTerminalSeq {stop = Some c; _}, [] ->
        return (c, empty_args_dict, initial_arguments)
    | ( (TNonTerminalSeq {tree; suffix; _} as nts),
        (parameter :: arguments' as remaining) ) ->
        (* try to match suffix first *)
        let rec match_suffix matched_acc = function
          | param :: params, suffix :: suffixes when param = suffix ->
              match_suffix (param :: matched_acc) (params, suffixes)
          | _, [] ->
              (* all of the suffix parts have been matched *)
              true
          | _, _ -> false
        in
        let matched = match_suffix [] (remaining, suffix) in
        if matched then
          (* continue with the nested tree *)
          traverse tree remaining acc
        else
          (* continue traversing with the current node (non-terminal sequence) *)
          traverse nts arguments' (parameter :: acc)
    | TPrefix {stop = Some cmd; _}, [] ->
        return (cmd, empty_args_dict, initial_arguments)
    | TPrefix {stop = None; prefix}, ([] | ("-h" | "--help") :: _) ->
        tzfail (Unterminated_command (initial_arguments, gather_assoc prefix))
    | TPrefix {prefix; _}, hd_arg :: tl -> (
        match List.assoc ~equal:String.equal hd_arg prefix with
        | None -> tzfail (Command_not_found (List.rev acc, gather_assoc prefix))
        | Some tree' -> traverse tree' tl (hd_arg :: acc))
    | TParam {stop = None; _}, ([] | ("-h" | "--help") :: _) ->
        tzfail (Unterminated_command (initial_arguments, gather_commands tree))
    | TParam {stop = Some c; _}, hd :: _ when is_option hd ->
        (* If the argument looks like an option, we choose the "stop"
           command. *)
        traverse (TStop c) arguments acc
    | TParam {stop = Some c; _}, [] ->
        return (c, empty_args_dict, initial_arguments)
    | TParam {tree; _}, parameter :: arguments' ->
        traverse tree arguments' (parameter :: acc)
    | TEmpty, _ -> tzfail (Command_not_found (List.rev acc, []))
  in
  traverse tree initial_arguments []

let get_arg {long; short} =
  ("--" ^ long)
  :: (match short with None -> [] | Some c -> ["-" ^ String.make 1 c])

let rec list_args : type a ctx. (a, ctx) arg -> string list = function
  | Constant _ -> []
  | Arg {label; _} | DefArg {label; _} | Switch {label; _} -> get_arg label
  | Pair (speca, specb) -> list_args speca @ list_args specb
  | Map {spec; _} -> list_args spec

let complete_func autocomplete cctxt =
  let open Lwt_result_syntax in
  match autocomplete with
  | None -> return_nil
  | Some autocomplete -> autocomplete cctxt

let list_command_args (Command {options; _}) = list_args options

let rec remaining_spec : type a ctx. StringSet.t -> (a, ctx) arg -> string list
    =
 fun seen -> function
  | Constant _ -> []
  | Arg {label; _} | DefArg {label; _} | Switch {label; _} ->
      if StringSet.mem label.long seen then [] else get_arg label
  | Pair (speca, specb) -> remaining_spec seen speca @ remaining_spec seen specb
  | Map {spec; _} -> remaining_spec seen spec

let complete_options (type ctx) continuation args args_spec ind (ctx : ctx) =
  let arities = make_arities_dict args_spec StringMap.empty in
  let rec complete_spec :
      type a. string -> (a, ctx) arg -> string list option tzresult Lwt.t =
   fun name ->
    let open Lwt_result_syntax in
    function
    | Constant _ -> return_none
    | DefArg {kind = {autocomplete; _}; label; _}
    | Arg {kind = {autocomplete; _}; label; _}
      when label.long = name ->
        let* p = complete_func autocomplete ctx in
        return_some p
    | Switch {label; _} when label.long = name -> return_some []
    | Arg _ | DefArg _ | Switch _ -> return_none
    | Pair (speca, specb) -> (
        let* resa = complete_spec name speca in
        match resa with
        | Some _ -> return resa
        | None -> complete_spec name specb)
    | Map {spec; _} -> complete_spec name spec
  in
  let rec help args ind seen =
    let open Lwt_result_syntax in
    match args with
    | _ when ind = 0 ->
        let+ cont_args = continuation args 0 in
        cont_args @ remaining_spec seen args_spec
    | [] -> Stdlib.failwith "cli_entries internal autocomplete error"
    | arg :: tl -> (
        match StringMap.find arg arities with
        | Some (arity, long) -> (
            let seen = StringSet.add long seen in
            match (arity, tl) with
            | 0, args when ind = 0 ->
                let+ cont_args = continuation args 0 in
                remaining_spec seen args_spec @ cont_args
            | 0, args -> help args (ind - 1) seen
            | 1, _ when ind = 1 ->
                let* res = complete_spec arg args_spec in
                return (Option.value ~default:[] res)
            | 1, _ :: tl -> help tl (ind - 2) seen
            | _ -> Stdlib.failwith "cli_entries internal error, invalid arity")
        | None -> continuation args ind)
  in
  help args ind StringSet.empty

let complete_next_tree cctxt =
  let open Lwt_result_syntax in
  function
  | TPrefix {stop; prefix} ->
      return
        ((match stop with
         | None -> []
         | Some command -> list_command_args command)
        @ List.map fst prefix)
  | TSeq (command, autocomplete) ->
      let+ completions = complete_func autocomplete cctxt in
      completions @ list_command_args command
  | TNonTerminalSeq {autocomplete; suffix; _} ->
      let+ completions = complete_func autocomplete cctxt in
      completions @ [WithExceptions.Option.get ~loc:__LOC__ @@ List.hd suffix]
  | TParam {autocomplete; _} -> complete_func autocomplete cctxt
  | TStop command -> return (list_command_args command)
  | TEmpty -> return_nil

let rec args_starting_from_suffix original_suffix ind matched_args = function
  | (s :: s_rest as suffix), a :: a_rest ->
      if s = a then
        args_starting_from_suffix
          original_suffix
          ind
          (matched_args @ [a])
          (s_rest, a_rest)
      else if matched_args = [] then
        (* Suffix not found on args head, check the rest of the args *)
        args_starting_from_suffix
          original_suffix
          (ind - 1)
          matched_args
          (suffix, a_rest)
      else
        (* After there is a suffix match, the rest of the suffix has
           to be matched in the following args, unless it's empty. *)
        None
  | unmatched_suffix, args
  (* Partial or full suffix match found *)
    when Compare.List_lengths.(unmatched_suffix < original_suffix) ->
      Some (matched_args @ args, ind)
  | _ -> None

let complete_tree cctxt tree index args =
  let rec help tree args ind =
    let open Lwt_result_syntax in
    if ind = 0 then complete_next_tree cctxt tree
    else
      match (tree, args) with
      | TSeq _, _ -> complete_next_tree cctxt tree
      | (TNonTerminalSeq {tree; suffix; _} as this_tree), _ :: _tl -> (
          match args_starting_from_suffix suffix ind [] (suffix, args) with
          | Some (args, ind) -> help tree args ind
          | _ -> complete_next_tree cctxt this_tree)
      | TPrefix {prefix; _}, hd :: tl -> (
          match List.assoc ~equal:String.equal hd prefix with
          | None -> return_nil
          | Some p -> help p tl (ind - 1))
      | TParam {tree; _}, _ :: tl -> help tree tl (ind - 1)
      | TStop (Command {options; conv; _}), args ->
          complete_options (fun _ _ -> return_nil) args options ind (conv cctxt)
      | (TParam _ | TPrefix _ | TNonTerminalSeq _), [] | TEmpty, _ -> return_nil
  in
  help tree args index

let autocompletion ~script ~cur_arg ~prev_arg ~args ~global_options commands
    cctxt =
  let open Lwt_result_syntax in
  let tree = make_dispatch_tree commands in
  let rec ind n = function
    | [] -> None
    | hd :: tl ->
        if hd = prev_arg then
          Some (Option.value ~default:(n + 1) (ind (n + 1) tl))
        else ind (n + 1) tl
  in
  let+ completions =
    if prev_arg = script then
      let+ command_completions = complete_next_tree cctxt tree in
      list_args global_options @ command_completions
    else
      match ind 0 args with
      | None -> return_nil
      | Some index ->
          complete_options
            (fun args ind -> complete_tree cctxt tree ind args)
            args
            global_options
            index
            cctxt
  in
  List.filter
    (fun completion ->
      Re.Str.(string_match (regexp_string cur_arg) completion 0))
    completions

let parse_global_options global_options ctx args =
  let open Lwt_result_syntax in
  let* dict, remaining = make_args_dict_consume global_options args in
  let* nested = parse_arg global_options dict ctx in
  return (nested, remaining)

let dispatch commands ctx args =
  let open Lwt_result_syntax in
  let tree = make_dispatch_tree commands in
  match args with
  | []
    when match tree with
         | TPrefix {stop; _} -> stop = None
         | TParam {stop; _} -> stop = None
         | TStop _ -> false
         | TSeq (_, _) -> false
         | TNonTerminalSeq {stop; _} -> stop = None
         | TEmpty -> true ->
      tzfail (Help None)
  | [("-h" | "--help")] -> tzfail (Help None)
  | _ ->
      let* command, args_dict, filtered_args = find_command tree args in
      exec command ctx filtered_args args_dict

type error += No_manual_entry of string list

let manual_group = {name = "man"; title = "Access the documentation"}

let add_manual ~executable_name ~global_options format ppf commands =
  let rec with_manual =
    lazy
      (commands
      @ [
          command
            ~group:manual_group
            ~desc:
              "Print documentation of commands.\n\
               Add search keywords to narrow list.\n\
               Will display only the commands by default, unless [-verbosity \
               <2|3>] is passed or the list of matching commands if less than \
               3."
            (args2
               (arg
                  ~doc:
                    "level of details\n\
                     0. Only shows command mnemonics, without documentation.\n\
                     1. Shows command mnemonics with short descriptions.\n\
                     2. Show commands and arguments with short descriptions\n\
                     3. Show everything"
                  ~long:"verbosity"
                  ~short:'v'
                  ~placeholder:"0|1|2|3"
                  (parameter
                     ~autocomplete:(fun _ -> Lwt.return_ok ["0"; "1"; "2"; "3"])
                     (fun _ arg ->
                       let open Lwt_result_syntax in
                       match arg with
                       | "0" -> return Terse
                       | "1" -> return Short
                       | "2" -> return Details
                       | "3" -> return Full
                       | _ -> failwith "Level of details out of range")))
               (default_arg
                  ~doc:"the manual's output format"
                  ~placeholder:"plain|colors|html"
                  ~long:"format"
                  ~default:
                    (match format with
                    | Ansi -> "colors"
                    | Plain -> "plain"
                    | Html -> "html")
                  (parameter
                     ~autocomplete:(fun _ ->
                       Lwt.return_ok ["colors"; "plain"; "html"])
                     (fun _ arg ->
                       let open Lwt_result_syntax in
                       match arg with
                       | "colors" -> return Ansi
                       | "plain" -> return Plain
                       | "html" -> return Html
                       | _ -> failwith "Unknown manual format"))))
            (prefix
               "man"
               (seq_of_param
                  (string
                     ~name:"keyword"
                     ~desc:
                       "keyword to search for\n\
                        If several are given they must all appear in the \
                        command.")))
            (fun (verbosity, format) keywords _ ->
              let commands =
                List.fold_left
                  (fun commands keyword ->
                    List.filter (search_command keyword) commands)
                  (Lazy.force with_manual)
                  keywords
              in
              let verbosity =
                match verbosity with
                | Some verbosity -> verbosity
                | None when Compare.List_length_with.(commands <= 3) -> Full
                | None -> Short
              in
              let open Lwt_result_syntax in
              match commands with
              | [] -> tzfail (No_manual_entry keywords)
              | _ ->
                  let state = setup_formatter ppf format verbosity in
                  let commands = List.map (fun c -> Ex c) commands in
                  usage_internal
                    ppf
                    ~executable_name
                    ~global_options
                    ~highlights:keywords
                    commands ;
                  restore_formatter ppf state ;
                  return_unit);
        ])
  in
  Lazy.force with_manual

let pp_cli_errors ppf ~executable_name ~global_options ~default errs =
  let pp_one = function
    | Bad_argument (i, v) ->
        Format.fprintf ppf "Erroneous command line argument %d (%s)." i v ;
        Some []
    | Option_expected_argument (arg, command) ->
        Format.fprintf
          ppf
          "Command line option @{<opt>%s@} expects an argument."
          arg ;
        Some (Option.fold ~some:(fun command -> [Ex command]) ~none:[] command)
    | Bad_option_argument (arg, command) ->
        Format.fprintf
          ppf
          "Wrong value for command line option @{<opt>%s@}."
          arg ;
        Some (Option.fold ~some:(fun command -> [Ex command]) ~none:[] command)
    | Multiple_occurrences (arg, command) ->
        Format.fprintf
          ppf
          "Command line option @{<opt>%s@} appears multiple times."
          arg ;
        Some (Option.fold ~some:(fun command -> [Ex command]) ~none:[] command)
    | No_manual_entry [keyword] ->
        Format.fprintf ppf "No manual entry that match @{<hilight>%s@}." keyword ;
        Some []
    | No_manual_entry (keyword :: keywords) ->
        Format.fprintf
          ppf
          "No manual entry that match %a and @{<hilight>%s@}."
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             (fun ppf keyword -> Format.fprintf ppf "@{<hilight>%s@}" keyword))
          keywords
          keyword ;
        Some []
    | Unknown_option (option, command) ->
        Format.fprintf ppf "Unexpected command line option @{<opt>%s@}." option ;
        Some (Option.fold ~some:(fun command -> [Ex command]) ~none:[] command)
    | Extra_arguments (extra, command) ->
        Format.(
          fprintf
            ppf
            "Extra command line arguments:@, @[<h>%a@]."
            (pp_print_list ~pp_sep:pp_print_space pp_print_string)
            extra) ;
        Some [Ex command]
    | Unterminated_command (_, commands) ->
        Format.fprintf
          ppf
          "@[<v 2>Unterminated command, here are possible completions.@,%a@]"
          (Format.pp_print_list (fun ppf (Command {params; options; _}) ->
               print_commandline ppf ([], options, params)))
          commands ;
        Some (List.map (fun c -> Ex c) commands)
    | Command_not_found ([], _all_commands) ->
        Format.fprintf
          ppf
          "@[<v 0>Unrecognized command.@,\
           Try using the @{<kwd>man@} command to get more information.@]" ;
        Some []
    | Command_not_found (_, commands) ->
        Format.fprintf
          ppf
          "@[<v 0>Unrecognized command.@,\
           Did you mean one of the following?@,\
          \  @[<v 0>%a@]@]"
          (Format.pp_print_list (fun ppf (Command {params; options; _}) ->
               print_commandline ppf ([], options, params)))
          commands ;
        Some (List.map (fun c -> Ex c) commands)
    | err ->
        default ppf err ;
        None
  in
  let rec pp acc errs =
    let return command =
      match (command, acc) with
      | None, _ -> acc
      | Some command, Some commands -> Some (command @ commands)
      | Some command, None -> Some command
    in
    match errs with
    | [] -> None
    | [last] -> return (pp_one last)
    | err :: errs ->
        let acc = return (pp_one err) in
        Format.fprintf ppf "@," ;
        pp acc errs
  in
  Format.fprintf ppf "@[<v 2>@{<error>@{<title>Error@}@}@," ;
  match pp None errs with
  | None -> Format.fprintf ppf "@]@\n"
  | Some commands ->
      Format.fprintf
        ppf
        "@]@\n@\n@[<v 0>%a@]"
        (fun ppf commands ->
          usage_internal ppf ~executable_name ~global_options commands)
        commands

let usage ppf ~executable_name ~global_options commands =
  usage_internal
    ppf
    ~executable_name
    ~global_options
    (List.map (fun c -> Ex c) commands)

let map_command f (Command c) = Command {c with conv = (fun x -> c.conv (f x))}
