(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol

let group =
  {
    Tezos_clic.name = "scripts";
    title = "Commands for managing the library of known scripts";
  }

open Tezos_micheline
open Client_proto_programs
open Client_proto_args

let safe_decode_json (cctxt : Protocol_client_context.full) encoding json =
  match Data_encoding.Json.destruct encoding json with
  | exception Data_encoding.Json.Cannot_destruct (_, exc) ->
      cctxt#error
        "could not decode json (%a)"
        (Data_encoding.Json.print_error ~print_unknown:(fun fmt exc ->
             Format.fprintf fmt "%s" (Printexc.to_string exc)))
        exc
  | exception ((Stack_overflow | Out_of_memory) as exc) -> raise exc
  | exception exc ->
      cctxt#error "could not decode json (%s)" (Printexc.to_string exc)
  | expr -> return expr

let commands () =
  let open Tezos_clic in
  let emacs_mode_switch =
    switch
      ~long:"emacs"
      ?short:None
      ~doc:"output in `michelson-mode.el` compatible format"
      ()
  in
  let zero_loc_switch =
    switch ~short:'z' ~long:"zero-loc" ~doc:"replace location with \"0\"" ()
  in
  let parse_expr expr =
    Lwt.return @@ Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression expr
  in
  let data_parameter = parameter (fun _ data -> parse_expr data) in
  let data_type_arg =
    arg
      ~doc:"the given data will be type-checked against this type"
      ~short:'t'
      ~long:"type"
      ~placeholder:"unit"
      data_parameter
  in
  let bytes_parameter ~name ~desc =
    param ~name ~desc Client_proto_args.bytes_parameter
  in
  let signature_parameter =
    parameter (fun _cctxt s ->
        match Tezos_crypto.Signature.V0.of_b58check_opt s with
        | Some s -> return s
        | None -> failwith "Not given a valid signature")
  in
  let convert_input_format_param =
    param
      ~name:"input_format"
      ~desc:"format of the input for conversion"
      (parameter
         ~autocomplete:(fun _ -> return ["michelson"; "json"; "binary"])
         (fun _ s ->
           match String.lowercase_ascii s with
           | "michelson" -> return `Michelson
           | "json" -> return `JSON
           | "binary" -> return `Binary
           | _ ->
               failwith
                 "invalid input format, expecting one of \"michelson\", \
                  \"json\" or \"binary\"."))
  in
  let convert_output_format_param =
    param
      ~name:"output_format"
      ~desc:"format of the conversion output"
      (parameter
         ~autocomplete:(fun _ ->
           return ["michelson"; "json"; "binary"; "ocaml"])
         (fun _ s ->
           match String.lowercase_ascii s with
           | "michelson" -> return `Michelson
           | "json" -> return `JSON
           | "binary" -> return `Binary
           | "ocaml" -> return `OCaml
           | _ ->
               failwith
                 "invalid output format, expecting one of \"michelson\", \
                  \"json\", \"binary\" or \"ocaml\"."))
  in
  let file_or_literal_param =
    param
      ~name:"source"
      ~desc:"literal or a path to a file"
      (parameter (fun cctxt s ->
           cctxt#read_file s >>= function
           | Ok v -> return v
           | Error _ -> return s))
  in
  [
    command
      ~group
      ~desc:"Lists all scripts in the library."
      no_options
      (fixed ["list"; "known"; "scripts"])
      (fun () (cctxt : Protocol_client_context.full) ->
        Program.load cctxt >>=? fun list ->
        List.iter_s (fun (n, _) -> cctxt#message "%s" n) list >>= fun () ->
        return_unit);
    command
      ~group
      ~desc:"Add a script to the library."
      (args1 (Program.force_switch ()))
      (prefixes ["remember"; "script"]
      @@ Program.fresh_alias_param @@ Program.source_param @@ stop)
      (fun force name hash cctxt ->
        Program.of_fresh cctxt force name >>=? fun name ->
        Program.add ~force cctxt name hash);
    command
      ~group
      ~desc:"Remove a script from the library."
      no_options
      (prefixes ["forget"; "script"] @@ Program.alias_param @@ stop)
      (fun () (name, _) cctxt -> Program.del cctxt name);
    command
      ~group
      ~desc:"Display a script from the library."
      no_options
      (prefixes ["show"; "known"; "script"] @@ Program.alias_param @@ stop)
      (fun () (_, program) (cctxt : Protocol_client_context.full) ->
        Program.to_source program >>=? fun source ->
        cctxt#message "%s\n" source >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Parse a byte sequence (in hexadecimal notation) as a data expression, \
         as per Michelson instruction `UNPACK`."
      no_options
      (prefixes ["unpack"; "michelson"; "data"]
      @@ bytes_parameter ~name:"bytes" ~desc:"the packed data to parse"
      @@ stop)
      (fun () bytes cctxt ->
        (if Bytes.get bytes 0 != '\005' then
           failwith
             "Not a piece of packed Michelson data (must start with `0x05`)"
         else return_unit)
        >>=? fun () ->
        (* Remove first byte *)
        let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
        match
          Data_encoding.Binary.of_bytes_opt
            Alpha_context.Script.expr_encoding
            bytes
        with
        | None -> failwith "Could not decode bytes"
        | Some expr ->
            cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Check the signature of a byte sequence as per Michelson instruction \
         `CHECK_SIGNATURE`."
      (args1 (switch ~doc:"Use only exit codes" ~short:'q' ~long:"quiet" ()))
      (prefixes ["check"; "that"]
      @@ bytes_parameter ~name:"bytes" ~desc:"the signed data"
      @@ prefixes ["was"; "signed"; "by"]
      @@ Client_keys_v0.Public_key.alias_param ~name:"key"
      @@ prefixes ["to"; "produce"]
      @@ param
           ~name:"signature"
           ~desc:"the signature to check"
           signature_parameter
      @@ stop)
      (fun quiet
           bytes
           (_, (key_locator, _))
           signature
           (cctxt : #Protocol_client_context.full)
         ->
        Client_keys_v0.check key_locator signature bytes >>=? function
        | false -> cctxt#error "invalid signature"
        | true ->
            if quiet then return_unit
            else
              cctxt#message "Signature check successful." >>= fun () ->
              return_unit);
    command
      ~group
      ~desc:"Ask the type of an entrypoint of a script."
      (args2 emacs_mode_switch no_print_source_flag)
      (prefixes ["get"; "script"; "entrypoint"; "type"; "of"]
      @@ string ~name:"entrypoint" ~desc:"the entrypoint to describe"
      @@ prefixes ["for"] @@ Program.source_param @@ stop)
      (fun (emacs_mode, no_print_source) entrypoint program cctxt ->
        match program with
        | program, [] ->
            entrypoint_type
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              program
              ~entrypoint
            >>= fun entrypoint_type ->
            print_entrypoint_type
              ~emacs:emacs_mode
              ~show_source:(not no_print_source)
              ~parsed:program
              ~entrypoint
              cctxt
              entrypoint_type
        | res_with_errors when emacs_mode ->
            cctxt#message
              "(@[<v 0>(entrypoint . ())@ (errors . %a)@])"
              Michelson_v1_emacs.report_errors
              res_with_errors
            >>= fun () -> return_unit
        | parsed, errors ->
            cctxt#message
              "%a"
              (fun ppf () ->
                Michelson_v1_error_reporter.report_errors
                  ~details:(not no_print_source)
                  ~parsed
                  ~show_source:(not no_print_source)
                  ppf
                  errors)
              ()
            >>= fun () -> cctxt#error "syntax error in program");
    command
      ~group
      ~desc:"Ask the node to list the entrypoints of a script."
      (args2 emacs_mode_switch no_print_source_flag)
      (prefixes ["get"; "script"; "entrypoints"; "for"]
      @@ Program.source_param @@ stop)
      (fun (emacs_mode, no_print_source) program cctxt ->
        match program with
        | program, [] ->
            list_entrypoints cctxt ~chain:cctxt#chain ~block:cctxt#block program
            >>= fun entrypoints ->
            print_entrypoints_list
              ~emacs:emacs_mode
              ~show_source:(not no_print_source)
              ~parsed:program
              cctxt
              entrypoints
        | res_with_errors when emacs_mode ->
            cctxt#message
              "(@[<v 0>(entrypoints . ())@ (errors . %a)@])"
              Michelson_v1_emacs.report_errors
              res_with_errors
            >>= fun () -> return_unit
        | parsed, errors ->
            cctxt#message
              "%a"
              (fun ppf () ->
                Michelson_v1_error_reporter.report_errors
                  ~details:(not no_print_source)
                  ~parsed
                  ~show_source:(not no_print_source)
                  ppf
                  errors)
              ()
            >>= fun () -> cctxt#error "syntax error in program");
    command
      ~desc:
        "Conversion of Michelson script from Micheline, JSON or binary to \
         Micheline, JSON, binary or OCaml"
      (args1 zero_loc_switch)
      (prefixes ["convert"; "script"]
      @@ file_or_literal_param @@ prefix "from" @@ convert_input_format_param
      @@ prefix "to" @@ convert_output_format_param @@ stop)
      (fun zero_loc
           expr_string
           from_format
           to_format
           (cctxt : Protocol_client_context.full)
         ->
        (match from_format with
        | `Michelson ->
            let program = Michelson_v1_parser.parse_toplevel expr_string in
            Lwt.return @@ Micheline_parser.no_parsing_error program
            >>=? fun program ->
            ( typecheck_program
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                program
            >>= function
              | Error _ as res ->
                  print_typecheck_result
                    ~emacs:false
                    ~show_types:true
                    ~print_source_on_error:true
                    program
                    res
                    cctxt
              | Ok _ -> return_unit )
            >>=? fun () -> return program.expanded
        | `JSON -> (
            match Data_encoding.Json.from_string expr_string with
            | Error err -> cctxt#error "%s" err
            | Ok json ->
                safe_decode_json cctxt Alpha_context.Script.expr_encoding json)
        | `Binary -> (
            bytes_of_prefixed_string expr_string >>=? fun bytes ->
            match
              Data_encoding.Binary.of_bytes_opt
                Alpha_context.Script.expr_encoding
                bytes
            with
            | None -> failwith "Could not decode bytes"
            | Some expr -> return expr))
        >>=? fun (expression : Alpha_context.Script.expr) ->
        let output =
          match to_format with
          | `Michelson ->
              Micheline_printer.printable
                Michelson_v1_primitives.string_of_prim
                expression
              |> Format.asprintf "%a" Micheline_printer.print_expr
          | `JSON ->
              Data_encoding.Json.(
                construct Alpha_context.Script.expr_encoding expression
                |> to_string)
          | `Binary ->
              Format.asprintf
                "0x%s"
                (Data_encoding.Binary.(
                   to_bytes_exn Alpha_context.Script.expr_encoding expression)
                |> Hex.of_bytes |> Hex.show)
          | `OCaml ->
              Michelson_v1_printer.micheline_string_of_expression
                ~zero_loc
                expression
        in
        cctxt#message "%s" output >>= fun () -> return_unit);
    command
      ~desc:
        "Conversion of Micheline expression from Micheline, JSON or binary to \
         Micheline, JSON, binary or OCaml"
      (args2 zero_loc_switch data_type_arg)
      (prefixes ["convert"; "data"]
      @@ file_or_literal_param @@ prefix "from" @@ convert_input_format_param
      @@ prefix "to" @@ convert_output_format_param @@ stop)
      (fun (zero_loc, data_ty)
           data_string
           from_format
           to_format
           (cctxt : Protocol_client_context.full)
         ->
        let micheline_of_expr expr =
          Micheline_printer.printable
            Michelson_v1_primitives.string_of_prim
            expr
          |> Format.asprintf "%a" Micheline_printer.print_expr
        in
        let typecheck_parsed ~data ~ty =
          Client_proto_programs.typecheck_data
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~data
            ~ty
            ()
          >>= function
          | Error errs ->
              failwith
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:false
                   ?parsed:None)
                errs
          | Ok _gas -> return data.expanded
        in
        let typecheck_expr ~expr ~ty =
          let data_string = micheline_of_expr expr in
          parse_expr data_string >>=? fun data -> typecheck_parsed ~data ~ty
        in
        (match from_format with
        | `Michelson -> (
            parse_expr data_string >>=? fun data ->
            match data_ty with
            | Some ty -> typecheck_parsed ~data ~ty
            | None -> return data.expanded)
        | `JSON -> (
            match Data_encoding.Json.from_string data_string with
            | Error err -> cctxt#error "%s" err
            | Ok json -> (
                safe_decode_json cctxt Alpha_context.Script.expr_encoding json
                >>=? fun expr ->
                match data_ty with
                | None -> return expr
                | Some ty -> typecheck_expr ~expr ~ty))
        | `Binary -> (
            bytes_of_prefixed_string data_string >>=? fun bytes ->
            match
              Data_encoding.Binary.of_bytes_opt
                Alpha_context.Script.expr_encoding
                bytes
            with
            | None -> failwith "Could not decode bytes"
            | Some expr -> (
                match data_ty with
                | None -> return expr
                | Some ty -> typecheck_expr ~expr ~ty)))
        >>=? fun (expression : Alpha_context.Script.expr) ->
        let output =
          match to_format with
          | `Michelson -> micheline_of_expr expression
          | `JSON ->
              Data_encoding.Json.(
                construct Alpha_context.Script.expr_encoding expression
                |> to_string)
          | `Binary ->
              Format.asprintf
                "0x%s"
                (Data_encoding.Binary.(
                   to_bytes_exn Alpha_context.Script.expr_encoding expression)
                |> Hex.of_bytes |> Hex.show)
          | `OCaml ->
              Michelson_v1_printer.micheline_string_of_expression
                ~zero_loc
                expression
        in
        cctxt#message "%s" output >>= fun () -> return_unit);
  ]
