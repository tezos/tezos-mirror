(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type error =
  | Invalid_action of string
  | Invalid_payload of bool * Ppxlib.payload
  | Invalid_aggregate of Key.t
  | Invalid_mark of Key.t
  | Invalid_record of Key.t
  | Invalid_span of Key.t
  | Invalid_stop of Key.t
  | Invalid_type of string * string * (Ppxlib.longident_loc * Ppxlib.expression)
  | Invalid_wrap of Key.t
  | Invalid_list_of_driver_ids of Ppxlib.expression list
  | Improper_field of (Ppxlib.longident_loc * Ppxlib.expression)
  | Improper_list_field of (Ppxlib.longident_loc * Ppxlib.expression)
  | Improper_let_binding of Ppxlib.expression
  | Improper_record of (Ppxlib.longident_loc * Ppxlib.expression) list
  | Malformed_attribute of Ppxlib.expression
  | No_verbosity of Key.t

let pp_field ppf (lident, expr) =
  Format.fprintf
    ppf
    "%a = %a"
    Ppxlib.Pprintast.longident
    lident.Ppxlib.txt
    Ppxlib.Pprintast.expression
    expr

let pp_accepted ppf () =
  Format.fprintf
    ppf
    "Accepted attributes payload are:@,\
     - [@profiler.aggregate_* <record> <string or ident>]@,\
     - [@profiler.mark <record> [<list of strings or ident>]]@,\
     - [@profiler.record_* <record> <string or ident>]@,\
     - [@profiler.span_* <record> <list of strings or ident>]@,\
     - [@profiler.wrap_* <record> <partial function application>]@,\
     - [@profiler.overwrite <any expression>]@,\
     - [@profiler.stop]"

let error loc err =
  let msg, hint =
    match err with
    | Invalid_action action ->
        ( "Invalid action.",
          Format.asprintf
            "@[<v 2>Accepted actions are %a@,Found: %s@."
            Format.(
              pp_print_list
                ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
                (fun ppf constant ->
                  Format.fprintf ppf "%s" (Constants.get_action constant)))
            Constants.constants
            action )
    | Invalid_payload (missing_record, payload) ->
        ( "Invalid or empty attribute payload.",
          Format.asprintf
            "@[<v 2>%a@,%aFound: @[<v 0>%a@]@]@."
            pp_accepted
            ()
            (fun ppf () ->
              if missing_record then
                Format.fprintf
                  ppf
                  "@[<v 2>With <record> containing the following optional \
                   fields:@,\
                   - verbosity@,\
                   - cpu_profiling@,\
                   - profiler_module@,\
                   - metadata@,\
                   - drivers_ids@]@,"
              else ())
            ()
            Ppxlib.Pprintast.payload
            payload )
    | Invalid_aggregate key ->
        ( "Invalid aggregate.",
          Format.asprintf
            "@[<v 2>A [@profiler.aggregate_*] attribute must be a string or an \
             identifier.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
    | Invalid_mark key ->
        ( "Invalid mark.",
          Format.asprintf
            "@[<v 2>A [@profiler.mark] attribute must be a string list.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
    | Invalid_record key ->
        ( "Invalid record.",
          Format.asprintf
            "@[<v 2>A [@profiler.record_*] attribute must be a string or an \
             identifier.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
    | Invalid_span key ->
        ( "Invalid span.",
          Format.asprintf
            "@[<v 2>A [@profiler.span_*] attribute must be a string list.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
    | Invalid_stop key ->
        ( "Invalid stop.",
          Format.asprintf
            "@[<v 2>A [@profiler.stop] should not have an attribute.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
    | Invalid_type (expected, field_label, field_value) ->
        ( "Invalid type.",
          Format.asprintf
            "@[<v 2>The %s field should be of type %s.@,Found: @[<v 0>%a@]@."
            field_label
            expected
            pp_field
            field_value )
    | Invalid_wrap key ->
        ( "Invalid wrap.",
          Format.asprintf
            "@[<v 2>A [@profiler.wrap_*] attribute must be a function \
             application.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
    | Invalid_list_of_driver_ids expr_list ->
        ( "Invalid list of modules.",
          Format.asprintf
            "@[<v 2>It looks like you tried to provide a list of opt-in \
             drivers through the `driver_ids` field but no drivers could be \
             parsed out of it. A list of opt-in drivers should be a list of \
             modules or idents like [Opentelemetry; prometheus]@,\
             Found: { @[<v 2>%a@] }@."
            Format.(
              pp_print_list
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@,")
                Ppxlib.Pprintast.expression)
            expr_list )
    | Improper_record record ->
        ( "Improper record.",
          Format.asprintf
            "@[<v 2>It looks like you tried to provide some additional options \
             through the mandatory record but no option could be parsed out of \
             it. Possible options are:@,\
             - the verbosity@,\
             - cpu_profiling@,\
             - the profiler_module@,\
             - the metadata@,\
             - the opt-in drivers_ids@,\
             Found: { @[<v 2>%a@] }@."
            Format.(
              pp_print_list
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@,")
                pp_field)
            record )
    | Improper_field field ->
        ( "Improper field.",
          Format.asprintf
            "@[<v 2>Expecting a field specifying either:@,\
             - the verbosity@,\
             - cpu_profiling@,\
             - the profiler_module@,\
             - the metadata@,\
             - the opt-in drivers_ids@,\
             Found: @[<v 0>%a@]@."
            pp_field
            field )
    | Improper_list_field field ->
        ( "Improper list field.",
          Format.asprintf
            "@[<v 2>Expecting a list field@,Found: @[<v 0>%a@]@."
            pp_field
            field )
    | Improper_let_binding expr ->
        ( "Improper let binding expression.",
          Format.asprintf
            "@[<v 2>Expecting a let binding expression.@,Found %a@."
            Ppxlib.Pprintast.expression
            expr )
    | Malformed_attribute expr ->
        ( "Malformed attribute.",
          Format.asprintf
            "@[<v 2>%a@,Found %a@.'"
            pp_accepted
            ()
            Ppxlib.Pprintast.expression
            expr )
    | No_verbosity key ->
        ( "Missing mandatory verbosity field",
          Format.asprintf
            "@[<v 2>A [@profiler] call requires a {verbosity} field. Available \
             options are Notice, Info and Debug.@,\
             Found: @[<v 0>%a@]@."
            Key.pp
            key )
  in
  Location.raise_errorf ~loc "profiling_ppx: %s\nHint: %s" msg hint
