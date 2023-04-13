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
    ContractAlias.destination_arg
      ~name:"source"
      ~doc:"name of the source (i.e. SENDER) contract for the transaction"
      ()
  in
  let payer_arg =
    ContractAlias.destination_arg
      ~name:"payer"
      ~doc:"name of the payer (i.e. SOURCE) contract for the transaction"
      ()
  in
  let balance_arg =
    Client_proto_args.tez_arg
      ~parameter:"balance"
      ~doc:"balance of run contract in \xEA\x9C\xA9"
      ~default:"4_000_000"
  in
  let custom_gas_flag =
    arg
      ~long:"gas"
      ~short:'G'
      ~doc:"Initial quantity of gas for typechecking and execution"
      ~placeholder:"gas"
      (parameter (fun _ctx str ->
           try
             let v = Z.of_string str in
             assert (Compare.Z.(v >= Z.zero)) ;
             return (Alpha_context.Gas.Arith.integral v)
           with _ -> failwith "invalid gas limit (must be a positive number)"))
  in
  let resolve_max_gas cctxt block = function
    | None ->
        Alpha_services.Constants.all cctxt (cctxt#chain, block)
        >>=? fun {parametric = {hard_gas_limit_per_operation; _}; _} ->
        return hard_gas_limit_per_operation
    | Some gas -> return gas
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
  let file_or_literal_param () =
    param
      ~name:"source"
      ~desc:"literal or a path to a file"
      (parameter (fun cctxt s ->
           cctxt#read_file s >>= function
           | Ok v -> return (Some s, v)
           | Error _ -> return (None, s)))
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
      ~desc:"Ask the node to run a script."
      (args9
         trace_stack_switch
         amount_arg
         balance_arg
         source_arg
         payer_arg
         no_print_source_flag
         custom_gas_flag
         entrypoint_arg
         (unparsing_mode_arg ~default:"Readable"))
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
             source,
             payer,
             no_print_source,
             gas,
             entrypoint,
             unparsing_mode )
           program
           storage
           input
           cctxt ->
        let source = Option.map snd source in
        let payer = Option.map snd payer in
        Lwt.return @@ Micheline_parser.no_parsing_error program
        >>=? fun program ->
        let show_source = not no_print_source in
        if trace_exec then
          trace
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~amount
            ~balance
            ~program
            ~storage
            ~input
            ~unparsing_mode
            ?source
            ?payer
            ?gas
            ?entrypoint
            ()
          >>= fun res ->
          print_trace_result cctxt ~show_source ~parsed:program res
        else
          run
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~amount
            ~balance
            ~program
            ~storage
            ~input
            ~unparsing_mode
            ?source
            ?payer
            ?gas
            ?entrypoint
            ()
          >>= fun res -> print_run_result cctxt ~show_source ~parsed:program res);
    command
      ~group
      ~desc:"Ask the node to typecheck a script."
      (args5
         show_types_switch
         emacs_mode_switch
         no_print_source_flag
         custom_gas_flag
         legacy_switch)
      (prefixes ["typecheck"; "script"] @@ Program.source_param @@ stop)
      (fun (show_types, emacs_mode, no_print_source, original_gas, legacy)
           program
           cctxt ->
        match program with
        | program, [] ->
            resolve_max_gas cctxt cctxt#block original_gas
            >>=? fun original_gas ->
            typecheck_program
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~gas:original_gas
              ~legacy
              program
            >>= fun res ->
            print_typecheck_result
              ~emacs:emacs_mode
              ~show_types
              ~print_source_on_error:(not no_print_source)
              program
              res
              cctxt
        | res_with_errors when emacs_mode ->
            cctxt#message
              "(@[<v 0>(types . ())@ (errors . %a)@])"
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
      ~desc:"Ask the node to typecheck a data expression."
      (args3 no_print_source_flag custom_gas_flag legacy_switch)
      (prefixes ["typecheck"; "data"]
      @@ param ~name:"data" ~desc:"the data to typecheck" data_parameter
      @@ prefixes ["against"; "type"]
      @@ param ~name:"type" ~desc:"the expected type" data_parameter
      @@ stop)
      (fun (no_print_source, custom_gas, legacy) data ty cctxt ->
        resolve_max_gas cctxt cctxt#block custom_gas >>=? fun original_gas ->
        Client_proto_programs.typecheck_data
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~gas:original_gas
          ~legacy
          ~data
          ~ty
          ()
        >>= function
        | Ok gas ->
            cctxt#message
              "@[<v 0>Well typed@,Gas remaining: %a@]"
              Alpha_context.Gas.pp
              gas
            >>= fun () -> return_unit
        | Error errs ->
            cctxt#warning
              "%a"
              (Michelson_v1_error_reporter.report_errors
                 ~details:false
                 ~show_source:(not no_print_source)
                 ?parsed:None)
              errs
            >>= fun () -> cctxt#error "ill-typed data");
    command
      ~group
      ~desc:
        "Ask the node to pack a data expression.\n\
         The returned hash is the same as what Michelson instruction `PACK` \
         would have produced.\n\
         Also displays the result of hashing this packed data with `BLAKE2B`, \
         `SHA256` or `SHA512` instruction."
      (args1 custom_gas_flag)
      (prefixes ["hash"; "data"]
      @@ param ~name:"data" ~desc:"the data to hash" data_parameter
      @@ prefixes ["of"; "type"]
      @@ param ~name:"type" ~desc:"type of the data" data_parameter
      @@ stop)
      (fun custom_gas data typ cctxt ->
        resolve_max_gas cctxt cctxt#block custom_gas >>=? fun original_gas ->
        Alpha_services.Helpers.Scripts.pack_data
          cctxt
          (cctxt#chain, cctxt#block)
          ~gas:original_gas
          ~data:data.expanded
          ~ty:typ.expanded
        >>= function
        | Ok (bytes, remaining_gas) ->
            let hash = Script_expr_hash.hash_bytes [bytes] in
            cctxt#message
              "Raw packed data: 0x%a@,\
               Script-expression-ID-Hash: %a@,\
               Raw Script-expression-ID-Hash: 0x%a@,\
               Ledger Blake2b hash: %s@,\
               Raw Sha256 hash: 0x%a@,\
               Raw Sha512 hash: 0x%a@,\
               Gas remaining: %a"
              Hex.pp
              (Hex.of_bytes bytes)
              Script_expr_hash.pp
              hash
              Hex.pp
              (Hex.of_bytes (Script_expr_hash.to_bytes hash))
              (Tezos_crypto.Base58.raw_encode
                 Tezos_crypto.Blake2B.(hash_bytes [bytes] |> to_string))
              Hex.pp
              (Hex.of_bytes (Environment.Raw_hashes.sha256 bytes))
              Hex.pp
              (Hex.of_bytes (Environment.Raw_hashes.sha512 bytes))
              Alpha_context.Gas.pp
              remaining_gas
            >>= fun () -> return_unit
        | Error errs ->
            cctxt#warning
              "%a"
              (Michelson_v1_error_reporter.report_errors
                 ~details:false
                 ~show_source:false
                 ?parsed:None)
              errs
            >>= fun () -> cctxt#error "ill-formed data");
    command
      ~group
      ~desc:"Ask the node to hash a Michelson script with `BLAKE2B`."
      (args3
         enforce_indentation_flag
         display_names_flag
         (Tezos_clic_unix.Scriptable.clic_arg ()))
      (prefixes ["hash"; "script"] @@ seq_of_param @@ file_or_literal_param ())
      (fun (check, display_names, scriptable)
           expr_strings
           (cctxt : Protocol_client_context.full) ->
        match expr_strings with
        | [] ->
            cctxt#warning "No scripts were specified on the command line" >|= ok
        | _ :: _ ->
            List.mapi_ep
              (fun i (src, expr_string) ->
                let program =
                  Michelson_v1_parser.parse_toplevel ~check expr_string
                in
                Micheline_parser.no_parsing_error program >>?= fun program ->
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
                let name =
                  Option.value
                    src
                    ~default:("Literal script " ^ string_of_int (i + 1))
                in
                return (hash, name))
              expr_strings
            >>=? fun hash_name_rows ->
            Tezos_clic_unix.Scriptable.output
              scriptable
              ~for_human:(fun () ->
                List.iter_s
                  (fun (hash, name) ->
                    if display_names then cctxt#answer "%s\t%s" hash name
                    else cctxt#answer "%s" hash)
                  hash_name_rows
                >|= ok)
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
      ~desc:"Ask the node to normalize a data expression."
      (args2 (unparsing_mode_arg ~default:"Readable") legacy_switch)
      (prefixes ["normalize"; "data"]
      @@ param
           ~name:"data"
           ~desc:"the data expression to normalize"
           data_parameter
      @@ prefixes ["of"; "type"]
      @@ param ~name:"type" ~desc:"type of the data expression" data_parameter
      @@ stop)
      (fun (unparsing_mode, legacy) data typ cctxt ->
        Plugin.RPC.normalize_data
          cctxt
          (cctxt#chain, cctxt#block)
          ~legacy
          ~data:data.expanded
          ~ty:typ.expanded
          ~unparsing_mode
        >>= function
        | Ok expr ->
            cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            >>= fun () -> return_unit
        | Error errs ->
            cctxt#warning
              "%a"
              (Michelson_v1_error_reporter.report_errors
                 ~details:false
                 ~show_source:false
                 ?parsed:None)
              errs
            >>= fun () -> cctxt#error "ill-typed data expression");
    command
      ~group
      ~desc:"Ask the node to normalize a Michelson script."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["normalize"; "script"] @@ Program.source_param @@ stop)
      (fun unparsing_mode script cctxt ->
        match script with
        | script, [] ->
            Plugin.RPC.normalize_script
              cctxt
              (cctxt#chain, cctxt#block)
              ~script:script.expanded
              ~unparsing_mode
            >>=? fun expr ->
            cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            >>= fun () -> return_unit
        | parsed, errors ->
            cctxt#message
              "%a"
              (fun ppf () ->
                Michelson_v1_error_reporter.report_errors
                  ~details:true
                  ~parsed
                  ~show_source:true
                  ppf
                  errors)
              ()
            >>= fun () -> cctxt#error "syntax error in program");
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
        Plugin.RPC.normalize_type
          cctxt
          (cctxt#chain, cctxt#block)
          ~ty:typ.expanded
        >>= function
        | Ok expr ->
            cctxt#message "%a" Michelson_v1_printer.print_expr_unwrapped expr
            >>= fun () -> return_unit
        | Error errs ->
            cctxt#warning
              "%a"
              (Michelson_v1_error_reporter.report_errors
                 ~details:false
                 ~show_source:false
                 ?parsed:None)
              errs
            >>= fun () -> cctxt#error "ill-formed type");
    command
      ~group
      ~desc:
        "Sign a raw sequence of bytes and display it using the format expected \
         by Michelson instruction `CHECK_SIGNATURE`."
      no_options
      (prefixes ["sign"; "bytes"]
      @@ bytes_parameter ~name:"data" ~desc:"the raw data to sign"
      @@ prefixes ["for"] @@ Client_keys_v0.Secret_key.source_param @@ stop)
      (fun () bytes sk cctxt ->
        Client_keys_v0.sign cctxt sk bytes >>=? fun signature ->
        cctxt#message "Signature: %a" Tezos_crypto.Signature.V0.pp signature
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
           (cctxt : #Protocol_client_context.full) ->
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
      ~group
      ~desc:
        "Ask the node to list the unreachable paths in a script's parameter \
         type."
      (args2 emacs_mode_switch no_print_source_flag)
      (prefixes ["get"; "script"; "unreachable"; "paths"; "for"]
      @@ Program.source_param @@ stop)
      (fun (emacs_mode, no_print_source) program cctxt ->
        match program with
        | program, [] ->
            list_unreachables
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              program
            >>= fun entrypoints ->
            print_unreachables
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
      ~group
      ~desc:"Ask the node to expand the Michelson macros in a script."
      no_options
      (prefixes ["expand"; "macros"; "in"] @@ Program.source_param @@ stop)
      (fun () program (cctxt : Protocol_client_context.full) ->
        Lwt.return @@ Micheline_parser.no_parsing_error program
        >>=? fun program ->
        cctxt#message
          "%a"
          (fun ppf () : unit ->
            Michelson_v1_printer.print_expr_unwrapped ppf program.expanded)
          ()
        >>= fun () -> return_unit);
    command
      ~desc:
        "Conversion of Michelson script from Micheline, JSON or binary to \
         Micheline, JSON, binary or OCaml"
      (args2 zero_loc_switch enforce_indentation_flag)
      (prefixes ["convert"; "script"]
      @@ file_or_literal_param () @@ prefix "from" @@ convert_input_format_param
      @@ prefix "to" @@ convert_output_format_param @@ stop)
      (fun (zero_loc, check)
           (_, expr_string)
           from_format
           to_format
           (cctxt : Protocol_client_context.full) ->
        (match from_format with
        | `Michelson ->
            let program =
              Michelson_v1_parser.parse_toplevel ~check expr_string
            in
            Lwt.return @@ Micheline_parser.no_parsing_error program
            >>=? fun program ->
            (typecheck_program
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
             | Ok _ -> return_unit)
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
      @@ file_or_literal_param () @@ prefix "from" @@ convert_input_format_param
      @@ prefix "to" @@ convert_output_format_param @@ stop)
      (fun (zero_loc, data_ty)
           (_, data_string)
           from_format
           to_format
           (cctxt : Protocol_client_context.full) ->
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
    command
      ~group
      ~desc:"Ask the node to run a TZIP-4 view."
      (args4
         source_arg
         payer_arg
         custom_gas_flag
         (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["run"; "tzip4"; "view"]
      @@ param ~name:"entrypoint" ~desc:"the name of the view" string_parameter
      @@ prefixes ["on"; "contract"]
      @@ ContractAlias.destination_param
           ~name:"contract"
           ~desc:"viewed contract"
      @@ prefixes ["with"; "input"]
      @@ param ~name:"input" ~desc:"the input data" data_parameter
      @@ stop)
      (fun (source, payer, gas, unparsing_mode)
           entrypoint
           (_, contract)
           input
           cctxt ->
        let source = Option.map snd source in
        let payer = Option.map snd payer in
        Client_proto_programs.run_view
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ?gas
          ~contract
          ~entrypoint
          ~input
          ?source
          ?payer
          ~unparsing_mode
          ()
        >>= fun res -> print_view_result cctxt res);
  ]
