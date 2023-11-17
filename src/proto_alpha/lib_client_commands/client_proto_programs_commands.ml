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
open Client_proto_contracts

let commands () =
  let open Tezos_clic in
  let show_types_switch =
    switch
      ~long:"details"
      ~short:'v'
      ~doc:"show the types of each instruction"
      ()
  in
  let emacs_mode_switch =
    switch
      ~long:"emacs"
      ?short:None
      ~doc:"output in `michelson-mode.el` compatible format"
      ()
  in
  let trace_stack_switch =
    switch ~long:"trace-stack" ~doc:"show the stack after each step" ()
  in
  let zero_loc_switch =
    switch ~short:'z' ~long:"zero-loc" ~doc:"replace location with \"0\"" ()
  in
  let legacy_switch =
    switch
      ~long:"legacy"
      ~doc:"typecheck in legacy mode as if the data was taken from the chain"
      ()
  in
  let amount_arg =
    Client_proto_args.tez_arg
      ~parameter:"amount"
      ~doc:"amount of the transfer in \xEA\x9C\xA9"
      ~default:"0.05"
  in
  let source_arg =
    Contract_alias.destination_arg
      ~name:"source"
      ~doc:"name of the source (i.e. SENDER) contract for the transaction"
      ()
  in
  let payer_arg =
    Client_keys.Public_key_hash.source_arg
      ~long:"payer"
      ~doc:"name of the payer (i.e. SOURCE) contract for the transaction"
      ()
  in
  let self_arg =
    Originated_contract_alias.destination_arg
      ~name:"self-address"
      ~doc:"address of the contract (i.e. SELF_ADDRESS) for the transaction"
      ()
  in
  let balance_arg =
    Client_proto_args.tez_opt_arg
      ~parameter:"balance"
      ~doc:"balance of run contract in \xEA\x9C\xA9"
  in
  let now_arg = Client_proto_args.now_arg in
  let level_arg = Client_proto_args.level_arg in
  let resolve_max_gas cctxt block =
    let open Lwt_result_syntax in
    function
    | None ->
        let* {parametric = {hard_gas_limit_per_operation; _}; _} =
          Alpha_services.Constants.all cctxt (cctxt#chain, block)
        in
        return hard_gas_limit_per_operation
    | Some gas -> return gas
  in
  let parse_expr expr =
    Lwt.return @@ Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression expr
  in
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
    parameter (fun (cctxt : #Client_context.full) s ->
        match Signature.of_b58check_opt s with
        | Some s -> Lwt_result_syntax.return s
        | None -> cctxt#error "Not given a valid signature")
  in
  let convert_input_format_param =
    let open Lwt_result_syntax in
    param
      ~name:"input_format"
      ~desc:"format of the input for conversion"
      (parameter
         ~autocomplete:(fun _ -> return ["michelson"; "json"; "binary"])
         (fun (cctxt : #Client_context.full) s ->
           match String.lowercase_ascii s with
           | "michelson" -> return `Michelson
           | "json" -> return `JSON
           | "binary" -> return `Binary
           | _ ->
               cctxt#error
                 "invalid input format, expecting one of \"michelson\", \
                  \"json\" or \"binary\"."))
  in
  let convert_output_format_param =
    let open Lwt_result_syntax in
    param
      ~name:"output_format"
      ~desc:"format of the conversion output"
      (parameter
         ~autocomplete:(fun _ ->
           return ["michelson"; "json"; "binary"; "ocaml"])
         (fun (cctxt : #Client_context.full) s ->
           match String.lowercase_ascii s with
           | "michelson" -> return `Michelson
           | "json" -> return `JSON
           | "binary" -> return `Binary
           | "ocaml" -> return `OCaml
           | _ ->
               cctxt#error
                 "invalid output format, expecting one of \"michelson\", \
                  \"json\", \"binary\" or \"ocaml\"."))
  in
  let file_or_literal_with_origin_param () =
    param
      ~name:"source"
      ~desc:"literal or a path to a file"
      (Client_proto_args.file_or_text_with_origin_parameter
         ~from_text:(fun _cctxt s -> Lwt_result_syntax.return s)
         ())
  in
  let file_or_literal_param () =
    param
      ~name:"source"
      ~desc:"literal or a path to a file"
      (Client_proto_args.file_or_text_parameter
         ~from_text:(fun _cctx s -> Lwt_result_syntax.return s)
         ())
  in
  let stack_param () =
    param
      ~name:"stack"
      ~desc:
        "a Michelson stack in the following format: {Stack_elt <ty_1> <val_1>; \
         ...; Stack_elt <ty_n> <val_n>}, where each <val_i> is a Michelson \
         value of type <ty_i>. The topmost element of the stack is <val_1>."
      micheline_parameter
  in
  let handle_parsing_error label (cctxt : Protocol_client_context.full)
      (emacs_mode, no_print_source) program body =
    let open Lwt_result_syntax in
    match program with
    | program, [] -> body program
    | res_with_errors when emacs_mode ->
        let*! () =
          cctxt#message
            "(@[<v 0>(%s . ())@ (errors . %a)@])"
            label
            Michelson_v1_emacs.report_errors
            res_with_errors
        in
        return_unit
    | parsed, errors ->
        let*! () =
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
        in
        cctxt#error "syntax error in program"
  in
  [
    command
      ~group
      ~desc:"Lists all scripts in the library."
      no_options
      (fixed ["list"; "known"; "scripts"])
      (fun () (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* list = Program.load cctxt in
        let*! () = List.iter_s (fun (n, _) -> cctxt#message "%s" n) list in
        return_unit);
    command
      ~group
      ~desc:"Add a script to the library."
      (args1 (Program.force_switch ()))
      (prefixes ["remember"; "script"]
      @@ Program.fresh_alias_param @@ Program.source_param @@ stop)
      (fun force name hash cctxt ->
        let open Lwt_result_syntax in
        let* name = Program.of_fresh cctxt force name in
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
        let open Lwt_result_syntax in
        let* source = Program.to_source program in
        let*! () = cctxt#message "%s\n" source in
        return_unit);
    command
      ~group
      ~desc:"Ask the node to run a script."
      (args14
         trace_stack_switch
         amount_arg
         balance_arg
         source_arg
         payer_arg
         self_arg
         no_print_source_flag
         run_gas_limit_arg
         entrypoint_arg
         (unparsing_mode_arg ~default:"Readable")
         now_arg
         level_arg
         other_contracts_arg
         extra_big_maps_arg)
      (prefixes ["run"; "script"]
      @@ Program.source_param
      @@ prefixes ["on"; "storage"]
      @@ param ~name:"storage" ~desc:"the storage data" data_parameter
      @@ prefixes ["and"; "input"]
      @@ param ~name:"input" ~desc:"the input data" data_parameter
      @@ stop)
      (fun ( trace_exec,
             amount,
             balance,
             sender,
             payer,
             self,
             no_print_source,
             gas,
             entrypoint,
             unparsing_mode,
             now,
             level,
             other_contracts,
             extra_big_maps )
           program
           storage
           input
           cctxt ->
        let open Lwt_result_syntax in
        let*? program = Micheline_parser.no_parsing_error program in
        let show_source = not no_print_source in
        if trace_exec then
          let*! res =
            trace
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              {
                amount = Some amount;
                balance;
                program;
                storage;
                shared_params =
                  {
                    input;
                    unparsing_mode;
                    now;
                    level;
                    sender;
                    payer;
                    gas;
                    other_contracts;
                    extra_big_maps;
                  };
                entrypoint;
                self;
              }
          in
          print_trace_result cctxt ~show_source ~parsed:program res
        else
          let*! res =
            run
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              {
                amount = Some amount;
                balance;
                program;
                storage;
                shared_params =
                  {
                    input;
                    unparsing_mode;
                    now;
                    level;
                    sender;
                    payer;
                    gas;
                    other_contracts;
                    extra_big_maps;
                  };
                entrypoint;
                self;
              }
          in
          print_run_result cctxt ~show_source ~parsed:program res);
    command
      ~group
      ~desc:
        "Ask the node to run a Michelson instruction or a sequence of \
         Michelson instructions on a stack."
      (args12
         amount_arg
         balance_arg
         source_arg
         payer_arg
         self_arg
         run_gas_limit_arg
         (unparsing_mode_arg ~default:"Readable")
         now_arg
         level_arg
         other_contracts_arg
         extra_big_maps_arg
         legacy_switch)
      (prefixes ["run"; "michelson"; "code"]
      @@ Program.source_param
      @@ prefixes ["on"; "stack"]
      @@ stack_param () @@ stop)
      (fun ( amount,
             balance,
             sender,
             payer,
             self,
             gas,
             unparsing_mode,
             now,
             level,
             other_contracts,
             extra_big_maps,
             legacy )
           program
           stack
           cctxt ->
        let open Lwt_result_syntax in
        let*? program = Micheline_parser.no_parsing_error program in
        let*? stack = Michelson_v1_stack.parse_stack stack in
        let*! res =
          run_instr
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            {
              stack;
              shared_params =
                {
                  input = program;
                  unparsing_mode;
                  now;
                  level;
                  sender;
                  payer;
                  gas;
                  other_contracts;
                  extra_big_maps;
                };
              amount;
              balance;
              self;
              parameter = None;
              legacy;
            }
        in
        print_run_instr_result cctxt ~show_source:false ~parsed:program res);
    command
      ~group
      ~desc:"Ask the node to compute the size of a script."
      (args4
         emacs_mode_switch
         no_print_source_flag
         run_gas_limit_arg
         legacy_switch)
      (prefixes ["compute"; "size"; "for"; "script"]
      @@ Program.source_param
      @@ prefixes ["on"; "storage"]
      @@ param ~name:"storage" ~desc:"the storage data" data_parameter
      @@ stop)
      (fun (emacs_mode, no_print_source, original_gas, legacy)
           program
           storage
           cctxt ->
        let open Lwt_result_syntax in
        let setup = (emacs_mode, no_print_source) in
        let* original_gas = resolve_max_gas cctxt cctxt#block original_gas in
        handle_parsing_error "size" cctxt setup program @@ fun program ->
        let* code_size =
          script_size
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~gas:(Some original_gas)
            ~legacy
            ~program
            ~storage
            ()
        in
        let*! () = cctxt#message "%d" code_size in
        return_unit);
    command
      ~group
      ~desc:"Ask the node to typecheck one or several scripts."
      (args6
         show_types_switch
         emacs_mode_switch
         no_print_source_flag
         run_gas_limit_arg
         legacy_switch
         display_names_flag)
      (prefixes ["typecheck"; "script"]
      @@ seq_of_param
      @@ file_or_literal_with_origin_param ())
      (fun ( show_types,
             emacs_mode,
             no_print_source,
             original_gas,
             legacy,
             display_names )
           expr_strings
           cctxt ->
        let open Lwt_result_syntax in
        let setup = (emacs_mode, no_print_source) in
        match expr_strings with
        | [] ->
            let*! () =
              cctxt#warning "No scripts were specified on the command line"
            in
            return_unit
        | _ :: _ ->
            let* _number_of_literal_scripts =
              List.fold_left_es
                (fun i content_with_origin ->
                  let expr_string =
                    Client_proto_args.content_of_file_or_text
                      content_with_origin
                  in
                  let program =
                    Michelson_v1_parser.parse_toplevel expr_string
                  in
                  let name, i =
                    match content_with_origin with
                    | Client_proto_args.File {path; _} -> (path, i)
                    | Text _ ->
                        let i = i + 1 in
                        ("Literal script " ^ string_of_int i, i)
                  in
                  let* () =
                    handle_parsing_error "types" cctxt setup program
                    @@ fun program ->
                    let* original_gas =
                      resolve_max_gas cctxt cctxt#block original_gas
                    in
                    let*! res =
                      typecheck_program
                        cctxt
                        ~chain:cctxt#chain
                        ~block:cctxt#block
                        ~gas:(Some original_gas)
                        ~legacy
                        ~show_types
                        program
                    in
                    print_typecheck_result
                      ~emacs:emacs_mode
                      ~show_types
                      ~print_source_on_error:(not no_print_source)
                      ~display_names
                      ~name
                      program
                      res
                      cctxt
                  in
                  return i)
                0
                expr_strings
            in
            return_unit);
    command
      ~group
      ~desc:"Ask the node to typecheck a data expression."
      (args3 no_print_source_flag run_gas_limit_arg legacy_switch)
      (prefixes ["typecheck"; "data"]
      @@ param ~name:"data" ~desc:"the data to typecheck" data_parameter
      @@ prefixes ["against"; "type"]
      @@ param ~name:"type" ~desc:"the expected type" data_parameter
      @@ stop)
      (fun (no_print_source, custom_gas, legacy) data ty cctxt ->
        let open Lwt_result_syntax in
        let* original_gas = resolve_max_gas cctxt cctxt#block custom_gas in
        let*! r =
          Client_proto_programs.typecheck_data
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~gas:(Some original_gas)
            ~legacy
            ~data
            ~ty
            ()
        in
        match r with
        | Ok gas ->
            let*! () =
              cctxt#message
                "@[<v 0>Well typed@,Gas remaining: %a@]"
                Alpha_context.Gas.pp
                gas
            in
            return_unit
        | Error errs ->
            let*! () =
              cctxt#warning
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:(not no_print_source)
                   ?parsed:None)
                errs
            in
            cctxt#error "ill-typed data");
    command
      ~group
      ~desc:
        "Ask the node to pack a data expression.\n\
         The returned hash is the same as what Michelson instruction `PACK` \
         would have produced.\n\
         Also displays the result of hashing this packed data with `BLAKE2B`, \
         `SHA256` or `SHA512` instruction."
      (args2 run_gas_limit_arg (Tezos_clic_unix.Scriptable.clic_arg ()))
      (prefixes ["hash"; "data"]
      @@ param ~name:"data" ~desc:"the data to hash" data_parameter
      @@ prefixes ["of"; "type"]
      @@ param ~name:"type" ~desc:"type of the data" data_parameter
      @@ stop)
      (fun (custom_gas, scriptable) data typ cctxt ->
        let open Lwt_result_syntax in
        let* original_gas = resolve_max_gas cctxt cctxt#block custom_gas in
        let*! r =
          Plugin.RPC.Scripts.pack_data
            cctxt
            (cctxt#chain, cctxt#block)
            ~gas:(Some original_gas)
            ~data:data.expanded
            ~ty:typ.expanded
        in
        match r with
        | Ok (bytes, remaining_gas) ->
            let hash = Script_expr_hash.hash_bytes [bytes] in
            let name_value_rows =
              Format.
                [
                  ( "Raw packed data",
                    asprintf "0x%a" Hex.pp (Hex.of_bytes bytes) );
                  ( "Script-expression-ID-Hash",
                    asprintf "%a" Script_expr_hash.pp hash );
                  ( "Raw Script-expression-ID-Hash",
                    asprintf
                      "0x%a"
                      Hex.pp
                      (Hex.of_bytes (Script_expr_hash.to_bytes hash)) );
                  ( "Ledger Blake2b hash",
                    Tezos_crypto.Base58.raw_encode
                      Tezos_crypto.Blake2B.(hash_bytes [bytes] |> to_string) );
                  ( "Raw Sha256 hash",
                    asprintf
                      "0x%a"
                      Hex.pp
                      (Hex.of_bytes (Environment.Raw_hashes.sha256 bytes)) );
                  ( "Raw Sha512 hash",
                    asprintf
                      "0x%a"
                      Hex.pp
                      (Hex.of_bytes (Environment.Raw_hashes.sha512 bytes)) );
                  ( "Gas remaining",
                    asprintf "%a" Alpha_context.Gas.pp remaining_gas );
                ]
            in
            Tezos_clic_unix.Scriptable.output
              scriptable
              ~for_human:(fun () ->
                let*! () =
                  List.iter_s
                    (fun (name, value) -> cctxt#message "%s: %s" name value)
                    name_value_rows
                in
                return_unit)
              ~for_script:(fun () ->
                name_value_rows |> List.map (fun (name, value) -> [name; value]))
        | Error errs ->
            let*! () =
              cctxt#warning
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:false
                   ?parsed:None)
                errs
            in
            cctxt#error "ill-formed data");
    command
      ~group
      ~desc:"Ask the node to hash a Michelson script with `BLAKE2B`."
      (args3
         enforce_indentation_flag
         display_names_flag
         (Tezos_clic_unix.Scriptable.clic_arg ()))
      (prefixes ["hash"; "script"]
      @@ seq_of_param
      @@ file_or_literal_with_origin_param ())
      (fun (check, display_names, scriptable)
           expr_strings
           (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        match expr_strings with
        | [] ->
            let*! () =
              cctxt#warning "No scripts were specified on the command line"
            in
            return_unit
        | _ :: _ ->
            let* hash_name_rows_rev, _number_of_literal_scripts =
              List.fold_left_es
                (fun (l, i) content_with_origin ->
                  let expr_string =
                    Client_proto_args.content_of_file_or_text
                      content_with_origin
                  in
                  let program =
                    Michelson_v1_parser.parse_toplevel ~check expr_string
                  in
                  let*? program = Micheline_parser.no_parsing_error program in
                  let code = program.expanded in
                  let bytes =
                    Data_encoding.Binary.to_bytes_exn
                      Alpha_context.Script.expr_encoding
                      code
                  in
                  let hash =
                    Format.asprintf
                      "%a"
                      Script_expr_hash.pp
                      (Script_expr_hash.hash_bytes [bytes])
                  in
                  let name, i =
                    match content_with_origin with
                    | Client_proto_args.File {path; _} -> (path, i)
                    | Text _ ->
                        let i = i + 1 in
                        ("Literal script " ^ string_of_int i, i)
                  in
                  return ((hash, name) :: l, i))
                ([], 0)
                expr_strings
            in
            let hash_name_rows = List.rev hash_name_rows_rev in
            Tezos_clic_unix.Scriptable.output
              scriptable
              ~for_human:(fun () ->
                let*! () =
                  List.iter_s
                    (fun (hash, name) ->
                      if display_names then cctxt#answer "%s\t%s" hash name
                      else cctxt#answer "%s" hash)
                    hash_name_rows
                in
                return_unit)
              ~for_script:(fun () ->
                List.map
                  (fun (hash, name) ->
                    if display_names then [hash; name] else [hash])
                  hash_name_rows));
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
        let open Lwt_result_syntax in
        let* () =
          if Bytes.get bytes 0 != '\005' then
            cctxt#error
              "Not a piece of packed Michelson data (must start with `0x05`)"
          else return_unit
        in
        (* Remove first byte *)
        let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
        match
          Data_encoding.Binary.of_bytes_opt
            Alpha_context.Script.expr_encoding
            bytes
        with
        | None -> cctxt#error "Could not decode bytes"
        | Some expr ->
            let*! () =
              cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            in
            return_unit);
    command
      ~group
      ~desc:"Ask the node to normalize a script."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["normalize"; "script"] @@ Program.source_param @@ stop)
      (fun unparsing_mode program cctxt ->
        let open Lwt_result_syntax in
        let*? program = Micheline_parser.no_parsing_error program in
        let*! r =
          Plugin.RPC.Scripts.normalize_script
            cctxt
            (cctxt#chain, cctxt#block)
            ~script:program.expanded
            ~unparsing_mode
        in
        match r with
        | Ok program ->
            let*! () =
              cctxt#message
                "%a"
                (fun ppf () : unit ->
                  Michelson_v1_printer.print_expr_unwrapped ppf program)
                ()
            in
            return_unit
        | Error errs ->
            let*! () =
              cctxt#warning
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:false
                   ?parsed:None)
                errs
            in
            cctxt#error "ill-typed script");
    command
      ~group
      ~desc:"Ask the node to normalize a data expression."
      (args4
         (unparsing_mode_arg ~default:"Readable")
         legacy_switch
         other_contracts_arg
         extra_big_maps_arg)
      (prefixes ["normalize"; "data"]
      @@ param
           ~name:"data"
           ~desc:"the data expression to normalize"
           data_parameter
      @@ prefixes ["of"; "type"]
      @@ param ~name:"type" ~desc:"type of the data expression" data_parameter
      @@ stop)
      (fun (unparsing_mode, legacy, other_contracts, extra_big_maps)
           data
           typ
           cctxt ->
        let open Lwt_result_syntax in
        let*! r =
          Plugin.RPC.Scripts.normalize_data
            cctxt
            (cctxt#chain, cctxt#block)
            ~legacy
            ~data:data.expanded
            ~ty:typ.expanded
            ~unparsing_mode
            ~other_contracts
            ~extra_big_maps
        in
        match r with
        | Ok expr ->
            let*! () =
              cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            in
            return_unit
        | Error errs ->
            let*! () =
              cctxt#warning
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:false
                   ?parsed:None)
                errs
            in
            cctxt#error "ill-typed data expression");
    command
      ~group
      ~desc:"Ask the node to normalize a typed Michelson stack."
      (args4
         (unparsing_mode_arg ~default:"Readable")
         legacy_switch
         other_contracts_arg
         extra_big_maps_arg)
      (prefixes ["normalize"; "stack"] @@ stack_param () @@ stop)
      (fun (unparsing_mode, legacy, other_contracts, extra_big_maps) stack cctxt ->
        let open Lwt_result_syntax in
        let*? stack = Michelson_v1_stack.parse_stack stack in
        let*! r =
          Plugin.RPC.Scripts.normalize_stack
            cctxt
            (cctxt#chain, cctxt#block)
            ~legacy
            ~stack
            ~unparsing_mode
            ~other_contracts
            ~extra_big_maps
        in
        match r with
        | Ok expr ->
            let*! () =
              cctxt#message "%a" Michelson_v1_printer.print_typed_stack expr
            in
            return_unit
        | Error errs ->
            let*! () =
              cctxt#warning
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:false
                   ?parsed:None)
                errs
            in
            cctxt#error "ill-typed stack");
    command
      ~group
      ~desc:"Ask the node to normalize a type."
      no_options
      (prefixes ["normalize"; "type"]
      @@ param
           ~name:"typ"
           ~desc:"the Michelson type to normalize"
           data_parameter
      @@ stop)
      (fun () typ cctxt ->
        let open Lwt_result_syntax in
        let*! r =
          Plugin.RPC.Scripts.normalize_type
            cctxt
            (cctxt#chain, cctxt#block)
            ~ty:typ.expanded
        in
        match r with
        | Ok expr ->
            let*! () =
              cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            in
            return_unit
        | Error errs ->
            let*! () =
              cctxt#warning
                "%a"
                (Michelson_v1_error_reporter.report_errors
                   ~details:false
                   ~show_source:false
                   ?parsed:None)
                errs
            in
            cctxt#error "ill-formed type");
    command
      ~group
      ~desc:
        "Sign a raw sequence of bytes and display it using the format expected \
         by Michelson instruction `CHECK_SIGNATURE`."
      no_options
      (prefixes ["sign"; "bytes"]
      @@ bytes_parameter ~name:"data" ~desc:"the raw data to sign"
      @@ prefixes ["for"] @@ Client_keys.Secret_key.source_param @@ stop)
      (fun () bytes sk cctxt ->
        let open Lwt_result_syntax in
        let* signature = Client_keys.sign cctxt sk bytes in
        let*! () = cctxt#message "Signature: %a" Signature.pp signature in
        return_unit);
    command
      ~group
      ~desc:
        "Check the signature of a byte sequence as per Michelson instruction \
         `CHECK_SIGNATURE`."
      (args1 (switch ~doc:"Use only exit codes" ~short:'q' ~long:"quiet" ()))
      (prefixes ["check"; "that"; "bytes"]
      @@ bytes_parameter ~name:"bytes" ~desc:"the signed data"
      @@ prefixes ["were"; "signed"; "by"]
      @@ Client_keys.Public_key.alias_param ~name:"key"
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
           (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* check = Client_keys.check key_locator signature bytes in
        if check then
          if quiet then return_unit
          else
            let*! () = cctxt#message "Signature check successful." in
            return_unit
        else cctxt#error "invalid signature");
    command
      ~group
      ~desc:"Ask the type of an entrypoint of a script."
      (args2 emacs_mode_switch no_print_source_flag)
      (prefixes ["get"; "script"; "entrypoint"; "type"; "of"]
      @@ param
           ~name:"entrypoint"
           ~desc:"the entrypoint to describe"
           entrypoint_parameter
      @@ prefixes ["for"] @@ Program.source_param @@ stop)
      (fun ((emacs_mode, no_print_source) as setup) entrypoint program cctxt ->
        let open Lwt_syntax in
        handle_parsing_error "entrypoint" cctxt setup program @@ fun program ->
        let* entrypoint_type =
          entrypoint_type
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            program
            ~entrypoint
        in
        print_entrypoint_type
          ~emacs:emacs_mode
          ~show_source:(not no_print_source)
          ~parsed:program
          ~entrypoint
          cctxt
          entrypoint_type);
    command
      ~group
      ~desc:"Ask the node to list the entrypoints of a script."
      (args2 emacs_mode_switch no_print_source_flag)
      (prefixes ["get"; "script"; "entrypoints"; "for"]
      @@ Program.source_param @@ stop)
      (fun ((emacs_mode, no_print_source) as setup) program cctxt ->
        let open Lwt_syntax in
        handle_parsing_error "entrypoints" cctxt setup program @@ fun program ->
        let* entrypoints =
          list_entrypoints cctxt ~chain:cctxt#chain ~block:cctxt#block program
        in
        print_entrypoints_list
          ~emacs:emacs_mode
          ~show_source:(not no_print_source)
          ~parsed:program
          cctxt
          entrypoints);
    command
      ~group
      ~desc:
        "Ask the node to list the unreachable paths in a script's parameter \
         type."
      (args2 emacs_mode_switch no_print_source_flag)
      (prefixes ["get"; "script"; "unreachable"; "paths"; "for"]
      @@ Program.source_param @@ stop)
      (fun ((emacs_mode, no_print_source) as setup) program cctxt ->
        let open Lwt_syntax in
        handle_parsing_error "entrypoints" cctxt setup program @@ fun program ->
        let* entrypoints =
          list_unreachables cctxt ~chain:cctxt#chain ~block:cctxt#block program
        in
        print_unreachables
          ~emacs:emacs_mode
          ~show_source:(not no_print_source)
          ~parsed:program
          cctxt
          entrypoints);
    command
      ~group
      ~desc:"Ask the node to expand the Michelson macros in a script."
      no_options
      (prefixes ["expand"; "macros"; "in"] @@ Program.source_param @@ stop)
      (fun () program (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let*? program = Micheline_parser.no_parsing_error program in
        let*! () =
          cctxt#message
            "%a"
            (fun ppf () : unit ->
              Michelson_v1_printer.print_expr_unwrapped ppf program.expanded)
            ()
        in
        return_unit);
    command
      ~desc:
        "Conversion of Michelson script from Micheline, JSON or binary to \
         Micheline, JSON, binary or OCaml"
      (args3 zero_loc_switch legacy_switch enforce_indentation_flag)
      (prefixes ["convert"; "script"]
      @@ file_or_literal_with_origin_param ()
      @@ prefix "from" @@ convert_input_format_param @@ prefix "to"
      @@ convert_output_format_param @@ stop)
      (fun (zero_loc, legacy, check)
           content_with_origin
           from_format
           to_format
           (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let expr_string =
          Client_proto_args.content_of_file_or_text content_with_origin
        in
        let name =
          match content_with_origin with
          | Client_proto_args.File {path; _} -> path
          | Text _ -> "Literal script"
        in
        let* (expression : Alpha_context.Script.expr) =
          match from_format with
          | `Michelson ->
              let program =
                Michelson_v1_parser.parse_toplevel ~check expr_string
              in
              let*? program = Micheline_parser.no_parsing_error program in
              let* () =
                let*! r =
                  typecheck_program
                    cctxt
                    ~chain:cctxt#chain
                    ~block:cctxt#block
                    ~legacy
                    ~show_types:true
                    ~gas:None
                    program
                in
                match r with
                | Error _ as res ->
                    print_typecheck_result
                      ~emacs:false
                      ~show_types:true
                      ~print_source_on_error:true
                      ~display_names:false
                      ~name
                      program
                      res
                      cctxt
                | Ok _ -> return_unit
              in
              return program.expanded
          | `JSON -> (
              match Data_encoding.Json.from_string expr_string with
              | Error err -> cctxt#error "%s" err
              | Ok json ->
                  safe_decode_json
                    ~name:"script"
                    cctxt
                    Alpha_context.Script.expr_encoding
                    json)
          | `Binary -> (
              let* bytes = bytes_of_prefixed_string cctxt expr_string in
              match
                Data_encoding.Binary.of_bytes_opt
                  Alpha_context.Script.expr_encoding
                  bytes
              with
              | None -> cctxt#error "Could not decode bytes"
              | Some expr -> return expr)
        in
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
        let*! () = cctxt#message "%s" output in
        return_unit);
    command
      ~desc:
        "Conversion of Micheline expression from Micheline, JSON or binary to \
         Micheline, JSON, binary or OCaml"
      (args2 zero_loc_switch data_type_arg)
      (prefixes ["convert"; "data"]
      @@ file_or_literal_param () @@ prefix "from" @@ convert_input_format_param
      @@ prefix "to" @@ convert_output_format_param @@ stop)
      (fun (zero_loc, data_ty)
           data_string
           from_format
           to_format
           (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let micheline_of_expr expr =
          Micheline_printer.printable
            Michelson_v1_primitives.string_of_prim
            expr
          |> Format.asprintf "%a" Micheline_printer.print_expr
        in
        let typecheck_parsed ~data ~ty =
          let*! r =
            Client_proto_programs.typecheck_data
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~data
              ~ty
              ~gas:None
              ~legacy:false
              ()
          in
          match r with
          | Error errs ->
              cctxt#error
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
          let* data = parse_expr data_string in
          typecheck_parsed ~data ~ty
        in
        let* (expression : Alpha_context.Script.expr) =
          match from_format with
          | `Michelson -> (
              let* data = parse_expr data_string in
              match data_ty with
              | Some ty -> typecheck_parsed ~data ~ty
              | None -> return data.expanded)
          | `JSON -> (
              match Data_encoding.Json.from_string data_string with
              | Error err -> cctxt#error "%s" err
              | Ok json -> (
                  let* expr =
                    safe_decode_json
                      ~name:"script"
                      cctxt
                      Alpha_context.Script.expr_encoding
                      json
                  in
                  match data_ty with
                  | None -> return expr
                  | Some ty -> typecheck_expr ~expr ~ty))
          | `Binary -> (
              let* bytes = bytes_of_prefixed_string cctxt data_string in
              match
                Data_encoding.Binary.of_bytes_opt
                  Alpha_context.Script.expr_encoding
                  bytes
              with
              | None -> cctxt#error "Could not decode bytes"
              | Some expr -> (
                  match data_ty with
                  | None -> return expr
                  | Some ty -> typecheck_expr ~expr ~ty))
        in
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
        let*! () = cctxt#message "%s" output in
        return_unit);
    command
      ~group
      ~desc:"Ask the node to run a TZIP-4 view."
      (args8
         source_arg
         payer_arg
         run_gas_limit_arg
         (unparsing_mode_arg ~default:"Readable")
         now_arg
         level_arg
         other_contracts_arg
         extra_big_maps_arg)
      (prefixes ["run"; "tzip4"; "view"]
      @@ param
           ~name:"entrypoint"
           ~desc:"the name of the view"
           entrypoint_parameter
      @@ prefixes ["on"; "contract"]
      @@ Originated_contract_alias.destination_param
           ~name:"contract"
           ~desc:"viewed contract"
      @@ prefixes ["with"; "input"]
      @@ param ~name:"input" ~desc:"the input data" data_parameter
      @@ stop)
      (fun ( sender,
             payer,
             gas,
             unparsing_mode,
             now,
             level,
             other_contracts,
             extra_big_maps )
           entrypoint
           contract
           input
           cctxt ->
        let open Lwt_result_syntax in
        let*! res =
          Client_proto_programs.run_view
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            {
              shared_params =
                {
                  input;
                  unparsing_mode;
                  now;
                  level;
                  sender;
                  payer;
                  gas;
                  other_contracts;
                  extra_big_maps;
                };
              contract;
              entrypoint;
            }
        in
        print_view_result cctxt res);
    command
      ~group
      ~desc:"Ask the node to run a Michelson view with Unit as input."
      (args9
         source_arg
         payer_arg
         run_gas_limit_arg
         unlimited_gas_arg
         (unparsing_mode_arg ~default:"Readable")
         now_arg
         level_arg
         other_contracts_arg
         extra_big_maps_arg)
      (prefixes ["run"; "view"]
      @@ param ~name:"view" ~desc:"the name of the view" string_parameter
      @@ prefixes ["on"; "contract"]
      @@ Originated_contract_alias.destination_param
           ~name:"contract"
           ~desc:"the contract containing the view"
      @@ stop)
      (fun ( sender,
             payer,
             gas,
             unlimited_gas,
             unparsing_mode,
             now,
             level,
             other_contracts,
             extra_big_maps )
           view
           contract
           cctxt ->
        let open Lwt_result_syntax in
        let*? input =
          Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression "Unit"
        in
        let*! res =
          Client_proto_programs.run_script_view
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            {
              shared_params =
                {
                  input;
                  unparsing_mode;
                  now;
                  level;
                  sender;
                  payer;
                  gas;
                  other_contracts;
                  extra_big_maps;
                };
              contract;
              view;
              unlimited_gas;
            }
        in
        print_view_result cctxt res);
    command
      ~group
      ~desc:"Ask the node to run a Michelson view."
      (args9
         source_arg
         payer_arg
         run_gas_limit_arg
         unlimited_gas_arg
         (unparsing_mode_arg ~default:"Readable")
         now_arg
         level_arg
         other_contracts_arg
         extra_big_maps_arg)
      (prefixes ["run"; "view"]
      @@ param ~name:"view" ~desc:"the name of the view" string_parameter
      @@ prefixes ["on"; "contract"]
      @@ Originated_contract_alias.destination_param
           ~name:"contract"
           ~desc:"the contract containing the view"
      @@ prefixes ["with"; "input"]
      @@ param
           ~name:"input"
           ~desc:"the argument provided to the view"
           data_parameter
      @@ stop)
      (fun ( sender,
             payer,
             gas,
             unlimited_gas,
             unparsing_mode,
             now,
             level,
             other_contracts,
             extra_big_maps )
           view
           contract
           input
           cctxt ->
        let open Lwt_result_syntax in
        let*! res =
          Client_proto_programs.run_script_view
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            {
              shared_params =
                {
                  input;
                  unparsing_mode;
                  now;
                  level;
                  sender;
                  payer;
                  gas;
                  other_contracts;
                  extra_big_maps;
                };
              contract;
              view;
              unlimited_gas;
            }
        in
        print_view_result cctxt res);
  ]
