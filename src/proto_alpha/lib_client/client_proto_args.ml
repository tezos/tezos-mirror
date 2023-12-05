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

open Protocol_client_context
open Protocol
open Alpha_context

type error += Bad_tez_arg of string * string (* Arg_name * value *)

type error += Bad_max_priority of string

type error += Bad_minimal_fees of string

type error += Bad_max_waiting_time of string

type error += Bad_preserved_levels of string

type error += Forbidden_Negative_int of string

let () =
  register_error_kind
    `Permanent
    ~id:"badTezArg"
    ~title:"Bad Tez Arg"
    ~description:"Invalid \xEA\x9C\xA9 notation in parameter."
    ~pp:(fun ppf (arg_name, literal) ->
      Format.fprintf
        ppf
        "Invalid \xEA\x9C\xA9 notation in parameter %s: '%s'"
        arg_name
        literal)
    Data_encoding.(obj2 (req "parameter" string) (req "literal" string))
    (function
      | Bad_tez_arg (parameter, literal) -> Some (parameter, literal)
      | _ -> None)
    (fun (parameter, literal) -> Bad_tez_arg (parameter, literal)) ;
  register_error_kind
    `Permanent
    ~id:"badMaxPriorityArg"
    ~title:"Bad -max-priority arg"
    ~description:"invalid priority in -max-priority"
    ~pp:(fun ppf literal ->
      Format.fprintf ppf "invalid priority '%s' in -max-priority" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_max_priority parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_max_priority parameter) ;
  register_error_kind
    `Permanent
    ~id:"badMinimalFeesArg"
    ~title:"Bad -minimal-fees arg"
    ~description:"invalid fee threshold in -fee-threshold"
    ~pp:(fun ppf literal ->
      Format.fprintf ppf "invalid minimal fees '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_minimal_fees parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_minimal_fees parameter) ;
  register_error_kind
    `Permanent
    ~id:"badMaxWaitingTimeArg"
    ~title:"Bad -max-waiting-time arg"
    ~description:"invalid duration in -max-waiting-time"
    ~pp:(fun ppf literal ->
      Format.fprintf
        ppf
        "Bad argument value for -max-waiting-time. Expected an integer, but \
         given '%s'"
        literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_max_waiting_time parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_max_waiting_time parameter) ;
  register_error_kind
    `Permanent
    ~id:"badPreservedLevelsArg"
    ~title:"Bad -preserved-levels arg"
    ~description:"invalid number of levels in -preserved-levels"
    ~pp:(fun ppf literal ->
      Format.fprintf
        ppf
        "Bad argument value for -preserved_levels. Expected a positive \
         integer, but given '%s'"
        literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_preserved_levels parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_preserved_levels parameter) ;
  register_error_kind
    `Permanent
    ~id:"ForbiddenNegativeInt"
    ~title:"Forbidden negative int"
    ~description:"invalid number, must a non negative natural "
    Data_encoding.(obj1 (req "invalid_natural" string))
    ~pp:(fun ppf literal ->
      Format.fprintf
        ppf
        "Bad argument value for natural. Expected a non negative integer, but \
         given '%s'"
        literal)
    (function Forbidden_Negative_int str -> Some str | _ -> None)
    (fun str -> Forbidden_Negative_int str)

let string_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ x -> return x)

let int_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) p ->
      try return (int_of_string p) with _ -> cctxt#error "Cannot read int")

let z_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) p ->
      try return (Z.of_string p) with _ -> cctxt#error "Cannot read integer")

let uri_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ x -> return (Uri.of_string x))

let bytes_of_prefixed_string (cctxt : #Client_context.full) s =
  let open Lwt_result_syntax in
  match
    if String.length s < 2 || s.[0] <> '0' || s.[1] <> 'x' then None
    else Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))
  with
  | Some s -> return s
  | None ->
      cctxt#error
        "Invalid bytes, expecting hexadecimal notation (e.g. 0x1234abcd)"

let bytes_parameter = Tezos_clic.parameter bytes_of_prefixed_string

type 'a file_or_text = File of {path : string; content : 'a} | Text of 'a

let content_of_file_or_text = function
  | File {content; _} | Text content -> content

let parse_file ~from_text ~read_file ~path =
  let open Lwt_result_syntax in
  let* content = read_file path in
  from_text content

let file_or_text ~from_text ~read_file =
  let open Lwt_result_syntax in
  Client_aliases.parse_alternatives
    [
      ( "file",
        fun path ->
          let* content = parse_file ~from_text ~read_file ~path in
          return (File {path; content}) );
      ( "text",
        fun text ->
          let* content = from_text text in
          return (Text content) );
    ]

let file_or_text_with_origin_parameter ~from_text () =
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) ->
      file_or_text ~from_text:(from_text cctxt) ~read_file:cctxt#read_file)

let file_or_text_parameter ~from_text () =
  file_or_text_with_origin_parameter ~from_text ()
  |> Tezos_clic.map_parameter ~f:content_of_file_or_text

let json_with_origin_parameter =
  let open Lwt_result_syntax in
  let from_text (cctxt : #Client_context.full) s =
    match Data_encoding.Json.from_string s with
    | Ok json -> return json
    | Error err ->
        cctxt#error "'%s' is not a valid JSON-encoded value: %s" s err
  in
  file_or_text_with_origin_parameter ~from_text ()

let json_parameter =
  Tezos_clic.map_parameter ~f:content_of_file_or_text json_with_origin_parameter

let data_parameter =
  let from_text (_cctxt : #Client_context.full) input =
    Lwt.return @@ Tezos_micheline.Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression input
  in
  file_or_text_parameter ~from_text ()

let safe_decode_json (cctxt : #Client_context.full) ~name
    ?(pp_error = fun _json fmt exn -> Data_encoding.Json.print_error fmt exn)
    encoding json =
  let open Lwt_result_syntax in
  match Data_encoding.Json.destruct encoding json with
  | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
      cctxt#error
        "@[<v 2>could not decode %s JSON:@,%a@]"
        name
        (pp_error json)
        exn
  | exception ((Stack_overflow | Out_of_memory) as exc) -> raise exc
  | exception exc ->
      cctxt#error "could not decode json (%s)" (Printexc.to_string exc)
  | expr -> return expr

let json_encoded_with_origin_parameter ~name ?pp_error encoding =
  let open Lwt_result_syntax in
  Tezos_clic.map_es_parameter
    ~f:(fun (cctxt : #Client_context.full) json_with_origin ->
      match json_with_origin with
      | File {path; content} ->
          let+ content =
            safe_decode_json ~name ?pp_error cctxt encoding content
          in
          File {path; content}
      | Text content ->
          let+ content = safe_decode_json ~name cctxt encoding content in
          Text content)
    json_with_origin_parameter

let json_encoded_parameter ~name ?pp_error encoding =
  Tezos_clic.map_parameter
    ~f:content_of_file_or_text
    (json_encoded_with_origin_parameter ~name ?pp_error encoding)

let json_encoded_param ~name ~desc ?pp_error encoding =
  Tezos_clic.param ~name ~desc (json_encoded_parameter ~name ?pp_error encoding)

let binary_encoded_parameter ~name encoding =
  let open Lwt_result_syntax in
  let from_text (cctxt : #Client_context.full) s =
    match Data_encoding.Binary.of_bytes_opt encoding (Bytes.of_string s) with
    | None -> cctxt#error "Invalid %s parameter" name
    | Some x -> return x
  in
  file_or_text_parameter ~from_text ()

let parse_micheline_parameter source =
  Lwt.return @@ Tezos_micheline.Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.expand_expression source

let micheline_parameter =
  Tezos_clic.parameter (fun (_ : full) source ->
      parse_micheline_parameter source)

let entrypoint_parameter =
  Tezos_clic.parameter (fun _ str ->
      Lwt.return @@ Environment.wrap_tzresult @@ Entrypoint.of_string_lax str)

let init_arg =
  Tezos_clic.default_arg
    ~long:"init"
    ~placeholder:"data"
    ~doc:"initial value of the contract's storage"
    ~default:"Unit"
    string_parameter

let other_contracts_parameter =
  Tezos_clic.parameter (fun _ source ->
      let open Lwt_result_syntax in
      let* parsed = parse_micheline_parameter source in
      let*? l = Michelson_v1_stack.parse_other_contracts parsed in
      return l)

let other_contracts_arg =
  Tezos_clic.arg
    ~doc:
      {|types and addresses of extra contracts, formatted as {Contract "KT1..." <ty1>; Contract "KT1..." <ty2>; ...}|}
    ~long:"other-contracts"
    ~placeholder:"contracts"
    other_contracts_parameter

let extra_big_maps_parameter =
  Tezos_clic.parameter (fun _ source ->
      let open Lwt_result_syntax in
      let* parsed = parse_micheline_parameter source in
      let*? l = Michelson_v1_stack.parse_extra_big_maps parsed in
      return l)

let extra_big_maps_arg =
  Tezos_clic.arg
    ~doc:
      {|identifier and content of extra big maps, formatted as {Big_map <index> <key_type> <value_type> {Elt <key1> <value1>; Elt <key2> <value2>; ...}}|}
    ~long:"extra-big-maps"
    ~placeholder:"big maps"
    extra_big_maps_parameter

let global_constant_param ~name ~desc next =
  Tezos_clic.param ~name ~desc string_parameter next

let arg_arg =
  Tezos_clic.arg
    ~long:"arg"
    ~placeholder:"data"
    ~doc:"argument passed to the contract's script, if needed"
    string_parameter

let default_arg_arg =
  Tezos_clic.arg
    ~long:"default-arg"
    ~placeholder:"data"
    ~doc:"default argument passed to each contract's script, if needed"
    string_parameter

let delegate_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"delegate"
    ~placeholder:"address"
    ~doc:"delegate of the contract\nMust be a known address."
    ()

let source_arg =
  Tezos_clic.arg
    ~long:"source"
    ~placeholder:"address"
    ~doc:"source of the deposits to be paid\nMust be a known address."
    string_parameter

let entrypoint_arg =
  Tezos_clic.arg
    ~long:"entrypoint"
    ~placeholder:"name"
    ~doc:"entrypoint of the smart contract"
    entrypoint_parameter

let default_entrypoint_arg =
  Tezos_clic.arg
    ~long:"default-entrypoint"
    ~placeholder:"name"
    ~doc:"default entrypoint of the smart contracts"
    entrypoint_parameter

let force_switch =
  Tezos_clic.switch
    ~long:"force"
    ~short:'f'
    ~doc:
      "disables the node's injection checks\n\
       Force the injection of branch-invalid operation or force  the injection \
       of block without a fitness greater than the  current head."
    ()

let minimal_timestamp_switch =
  Tezos_clic.switch
    ~long:"minimal-timestamp"
    ~doc:
      "Use the minimal timestamp instead of the current date as timestamp of \
       the baked block."
    ()

let tez_format =
  "Text format: `DDDDDDD.DDDDDD`.\n\
   Tez and mutez and separated by a period sign. Trailing and pending zeroes \
   are allowed."

let tez_parameter param =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ s ->
      match Tez.of_string s with
      | Some tez -> return tez
      | None -> tzfail (Bad_tez_arg (param, s)))

let everything_tez_parameter param =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ s ->
      match s with
      | "everything" -> return Tez.max_mutez
      | _ -> tzfail (Bad_tez_arg (param, s)))

let everything_or_tez_parameter param =
  Tezos_clic.compose_parameters
    (tez_parameter param)
    (everything_tez_parameter param)

let tez_arg ~default ~parameter ~doc =
  Tezos_clic.default_arg
    ~long:parameter
    ~placeholder:"amount"
    ~doc
    ~default
    (tez_parameter ("--" ^ parameter))

let tez_opt_arg ~parameter ~doc =
  Tezos_clic.arg
    ~long:parameter
    ~placeholder:"amount"
    ~doc
    (tez_parameter ("--" ^ parameter))

let tez_param ~name ~desc next =
  Tezos_clic.param
    ~name
    ~desc:(desc ^ " in \xEA\x9C\xA9\n" ^ tez_format)
    (tez_parameter name)
    next

let everything_or_tez_param ~name ~desc next =
  Tezos_clic.param
    ~name
    ~desc:(desc ^ " in \xEA\x9C\xA9 (or everything)\n" ^ tez_format)
    (everything_or_tez_parameter name)
    next

let non_negative_z_parser (cctxt : #Client_context.io) s =
  match Z.of_string s with
  | exception Invalid_argument _ -> cctxt#error "Expected number"
  | v when Compare.Z.(v < Z.zero) ->
      cctxt#error "Invalid number, must be a non negative number."
  | v -> Lwt_result_syntax.return v

let non_negative_z_parameter () = Tezos_clic.parameter non_negative_z_parser

let non_negative_z_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (non_negative_z_parameter ()) next

let counter_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
      match Manager_counter.Internal_for_injection.of_string s with
      | None -> cctxt#error "Invalid counter, must be a non-negative number."
      | Some c -> return c)

let non_negative_parser (cctxt : #Client_context.io) s =
  let open Lwt_result_syntax in
  match int_of_string_opt s with
  | Some i when i >= 0 -> return i
  | _ -> cctxt#error "Parameter should be a non-negative integer literal"

let non_negative_parameter () = Tezos_clic.parameter non_negative_parser

let non_negative_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (non_negative_parameter ()) next

let positive_int_parser (cctxt : #Client_context.io) s =
  let open Lwt_result_syntax in
  match int_of_string_opt s with
  | Some i when i > 0 -> return i
  | _ -> cctxt#error "Parameter should be a positive integer literal"

let positive_int_parameter () = Tezos_clic.parameter positive_int_parser

let positive_int_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (positive_int_parameter ()) next

let fee_arg =
  Tezos_clic.arg
    ~long:"fee"
    ~placeholder:"amount"
    ~doc:"fee in \xEA\x9C\xA9 to pay to the baker"
    (tez_parameter "--fee")

let default_fee_arg =
  Tezos_clic.arg
    ~long:"default-fee"
    ~placeholder:"amount"
    ~doc:"default fee in \xEA\x9C\xA9 to pay to the baker for each transaction"
    (tez_parameter "--default-fee")

let level_kind =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
      match Option.bind (Script_int.of_string s) Script_int.is_nat with
      | Some n -> return n
      | None -> cctxt#error "invalid level (must be a positive number)")

let level_arg =
  Tezos_clic.arg
    ~long:"level"
    ~placeholder:"level"
    ~doc:"Set the level to be returned by the LEVEL instruction"
    level_kind

let raw_level_parser (cctxt : #Client_context.io) s =
  match Int32.of_string_opt s with
  | Some i when i >= 0l ->
      Lwt.return @@ Environment.wrap_tzresult (Raw_level.of_int32 i)
  | _ ->
      cctxt#error
        "'%s' is not a valid level (should be a non-negative int32 value)"
        s

let raw_level_parameter () = Tezos_clic.parameter raw_level_parser

let raw_level_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (raw_level_parameter ()) next

let timestamp_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
      match Script_timestamp.of_string s with
      | Some time -> return time
      | None ->
          cctxt#error
            "invalid timestamp, must be either a RFC 3339 string or a number \
             of seconds since epoch.")

let now_arg =
  Tezos_clic.arg
    ~long:"now"
    ~placeholder:"timestamp"
    ~doc:
      "Set the timestamp to be returned by the NOW instruction. Allowed format \
       are RFC 3339 (YYYY-MM-DDTHH:MM:SSZ) or number of seconds since epoch."
    timestamp_parameter

let gas_limit_kind =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
      try
        let v = Z.of_string s in
        return (Gas.Arith.integral_exn v)
      with _ -> cctxt#error "invalid gas limit (must be a positive number)")

let gas_limit_arg =
  Tezos_clic.arg
    ~long:"gas-limit"
    ~short:'G'
    ~placeholder:"amount"
    ~doc:
      "Set the gas limit of the transaction instead of letting the client \
       decide based on a simulation"
    gas_limit_kind

let default_gas_limit_arg =
  Tezos_clic.arg
    ~long:"default-gas-limit"
    ~short:'G'
    ~placeholder:"amount"
    ~doc:
      "Set the default gas limit for each transaction instead of letting the \
       client decide based on a simulation"
    gas_limit_kind

let run_gas_limit_arg =
  Tezos_clic.arg
    ~long:"gas"
    ~short:'G'
    ~doc:"Initial quantity of gas for typechecking and execution"
    ~placeholder:"gas"
    gas_limit_kind

let unlimited_gas_arg =
  Tezos_clic.switch
    ~long:"unlimited-gas"
    ~doc:"Allows interpretation with virtually unlimited gas"
    ()

let storage_limit_kind =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
      try
        let v = Z.of_string s in
        assert (Compare.Z.(v >= Z.zero)) ;
        return v
      with _ ->
        cctxt#error "invalid storage limit (must be a positive number of bytes)")

let storage_limit_arg =
  Tezos_clic.arg
    ~long:"storage-limit"
    ~short:'S'
    ~placeholder:"amount"
    ~doc:
      "Set the storage limit of the transaction instead of letting the client \
       decide based on a simulation"
    storage_limit_kind

let default_storage_limit_arg =
  Tezos_clic.arg
    ~long:"default-storage-limit"
    ~short:'S'
    ~placeholder:"amount"
    ~doc:
      "Set the default storage limit for each transaction instead of letting \
       the client decide based on a simulation"
    storage_limit_kind

let counter_arg =
  Tezos_clic.arg
    ~long:"counter"
    ~short:'C'
    ~placeholder:"counter"
    ~doc:"Set the counter to be used by the transaction"
    counter_parameter

let max_priority_arg =
  let open Lwt_result_syntax in
  Tezos_clic.arg
    ~long:"max-priority"
    ~placeholder:"slot"
    ~doc:"maximum allowed baking slot"
    (Tezos_clic.parameter (fun _ s ->
         try return (int_of_string s) with _ -> tzfail (Bad_max_priority s)))

let timelock_locked_value_arg =
  Tezos_clic.arg
    ~long:"timelock-locked-valuec"
    ~placeholder:"timelock-locked"
    ~doc:"Timelock RSA group modulus"
    string_parameter

let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let minimal_fees_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"minimal-fees"
    ~placeholder:"amount"
    ~doc:"exclude operations with fees lower than this threshold (in tez)"
    ~default:(Tez.to_string default_minimal_fees)
    (Tezos_clic.parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> tzfail (Bad_minimal_fees s)))

let minimal_nanotez_per_gas_unit_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"minimal-nanotez-per-gas-unit"
    ~placeholder:"amount"
    ~doc:
      "exclude operations with fees per gas lower than this threshold (in \
       nanotez)"
    ~default:(Q.to_string default_minimal_nanotez_per_gas_unit)
    (Tezos_clic.parameter (fun _ s ->
         try return (Q.of_string s) with _ -> tzfail (Bad_minimal_fees s)))

let minimal_nanotez_per_byte_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"minimal-nanotez-per-byte"
    ~placeholder:"amount"
    ~default:(Q.to_string default_minimal_nanotez_per_byte)
    ~doc:
      "exclude operations with fees per byte lower than this threshold (in \
       nanotez)"
    (Tezos_clic.parameter (fun _ s ->
         try return (Q.of_string s) with _ -> tzfail (Bad_minimal_fees s)))

let replace_by_fees_arg =
  Tezos_clic.switch
    ~long:"replace"
    ~doc:
      "Replace an existing pending transaction from the same source, if any, \
       with another one with higher fees. There are no guarantees that the \
       first operation will not be included or that the second one will be. \
       But, only one of the operations at most will end in a block (in \
       precheck mode)."
    ()

let successor_level_arg =
  Tezos_clic.switch
    ~long:"simulate-successor-level"
    ~doc:"Make the simulate on the successor level of the current head."
    ()

let preserved_levels_arg =
  let open Lwt_result_syntax in
  Tezos_clic.default_arg
    ~long:"preserved-levels"
    ~placeholder:"threshold"
    ~doc:"Number of effective levels kept in the accuser's memory"
    ~default:"200"
    (Tezos_clic.parameter (fun _ s ->
         try
           let preserved_cycles = int_of_string s in
           if preserved_cycles < 0 then tzfail (Bad_preserved_levels s)
           else return preserved_cycles
         with _ -> tzfail (Bad_preserved_levels s)))

let no_print_source_flag =
  Tezos_clic.switch
    ~long:"no-print-source"
    ~short:'q'
    ~doc:
      "don't print the source code\n\
       If an error is encountered, the client will print the contract's source \
       code by default.\n\
       This option disables this behaviour."
    ()

let no_confirmation =
  Tezos_clic.switch
    ~long:"no-confirmation"
    ~doc:"don't print wait for the operation to be confirmed."
    ()

let signature_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
      match Signature.of_b58check_opt s with
      | Some s -> return s
      | None -> cctxt#error "Not given a valid signature")

let unparsing_mode_parameter =
  let open Lwt_result_syntax in
  Tezos_clic.parameter
    ~autocomplete:(fun _cctxt ->
      return ["Readable"; "Optimized"; "Optimized_legacy"])
    (fun (cctxt : #Client_context.full) s ->
      match s with
      | "Readable" -> return Script_ir_unparser.Readable
      | "Optimized" -> return Script_ir_unparser.Optimized
      | "Optimized_legacy" -> return Script_ir_unparser.Optimized_legacy
      | _ -> cctxt#error "Unknown unparsing mode %s" s)

let unparsing_mode_arg ~default =
  Tezos_clic.default_arg
    ~long:"unparsing-mode"
    ~placeholder:"mode"
    ~doc:
      "Unparsing mode to use\n\
       One of \"Readable\", \"Optimized\", or \"Optimized_legacy\".\n\
       This option affects the way the values of the following Michelson types \
       are represented:\n\
       - timestamp: the Readable representation is a RFC3339 string, the \
       Optimized and Optimized_legacy representations are the number of \
       seconds since Epoch\n\
       - key, signature, key_hash, address, contract, chain_id: the Readable \
       representation is a Base58Check string, the Optimized and \
       Optimized_legacy representations are byte sequences\n\
       - nested pairs: in Readable mode, the Pair constructor is used even \
       with arity bigger than 2 such as in Pair 0 1 2; in Optimized_legacy \
       mode, the Pair constructor is always use with arity 2 such as in Pair 0 \
       (Pair 1 2); in Optimized mode, a sequence is used if there are at least \
       4 elements and the behavior is the same as in Optimized_legacy mode \
       otherwise.\n"
    ~default
    unparsing_mode_parameter

let enforce_indentation_flag =
  Tezos_clic.switch
    ~long:"enforce-indentation"
    ~doc:
      "Check that the Micheline expression passed to this command is \
       well-indented."
    ()

let display_names_flag =
  Tezos_clic.switch
    ~long:"display-names"
    ~doc:"Print names of scripts passed to this command"
    ()

let fixed_point_parameter =
  let open Lwt_result_syntax in
  let rec remove_trailing_zeroes ~decimals ~right i =
    if i < decimals then Some (String.sub right 0 decimals)
    else if right.[i] <> '0' then None
    else (remove_trailing_zeroes [@ocaml.tailcall]) ~decimals ~right (i - 1)
  in
  let parse ~decimals p =
    let open Option_syntax in
    let* left, right =
      match String.split_on_char '.' p with
      | [left; right] -> Some (left, right)
      | [left] -> Some (left, "")
      | _ -> None
    in
    let* right =
      if String.length right > decimals then
        remove_trailing_zeroes ~decimals ~right (String.length right - 1)
      else Some (right ^ String.make (decimals - String.length right) '0')
    in
    int_of_string_opt (left ^ right)
  in
  let parse ~decimals p =
    if decimals >= 2 && String.length p > 0 && p.[String.length p - 1] = '%'
    then parse ~decimals:(decimals - 2) (String.sub p 0 (String.length p - 1))
    else parse ~decimals p
  in
  fun ~decimals ->
    if decimals < 0 then
      raise (Invalid_argument "fixed_point_parameter: negative decimals")
    else fun ~name ->
      Tezos_clic.parameter (fun (cctxt : #Client_context.full) p ->
          match parse ~decimals p with
          | Some res -> return res
          | None ->
              cctxt#error
                "Cannot read %s parameter: expecting a fixed point number with \
                 at most %d decimals, or a fixed point number with at most %d \
                 decimals followed by a %% sign."
                name
                decimals
                (decimals - 2))

let limit_of_staking_over_baking_millionth_arg =
  Tezos_clic.arg
    ~long:"limit-of-staking-over-baking"
    ~placeholder:"limit"
    ~doc:
      "Limits the total amount of stake for the source's delegators as a \
       proportion of the source's own stake. Any amount exceeding this limit \
       is considered as delegation in the stake of the delegate. The value \
       should be between 0 and 5 (default 0 if not set). If this parameter is \
       0, as is the default, any staking operation from the source's \
       delegators are forbidden and will fail (unstaking operations are still \
       allowed)."
    ((* TODO #6162: should we check it's between 0 and 5 million? *)
     fixed_point_parameter
       ~decimals:6
       ~name:"limit of staking over baking")

let edge_of_baking_over_staking_billionth_arg =
  Tezos_clic.arg
    ~long:"edge-of-baking-over-staking"
    ~placeholder:"edge"
    ~doc:
      "Sets the portion of the rewards issued to the delegate that should be \
       transfered to its liquid balance. The rest is issued to its stakers \
       (itself included), proportionally to their stake. Value should be \
       between 0 and 1. If not set, default value is 1: all rewards given to \
       the source are issued to their liquid balance."
    ((* TODO #6162: check it's between 0 and 1 billion *)
     fixed_point_parameter
       ~decimals:9
       ~name:"edge of baking over staking")

module Sc_rollup_params = struct
  let rollup_kind_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) name ->
        match Sc_rollup.Kind.of_string name with
        | None ->
            cctxt#error
              "Parameter '%s' is not a valid rollup name (must be one of %s)"
              name
              (String.concat ", " Sc_rollup.Kind.(List.map to_string all))
        | Some k -> return k)

  let boot_sector_parameter =
    let open Lwt_result_syntax in
    let from_text (cctxt : #Client_context.full) s =
      return (fun (Sc_rollup.PVM.Packed (module R)) ->
          R.parse_boot_sector s |> function
          | None -> cctxt#error "Invalid kernel"
          | Some boot_sector -> return boot_sector)
    in
    file_or_text_parameter ~from_text ()

  let messages_parameter =
    let open Lwt_result_syntax in
    let from_json (cctxt : #Client_context.full) text =
      try
        match Ezjsonm.from_string text with
        | `A messages -> return (`Json (`A (`String "raw" :: messages)))
        | _ -> cctxt#error "Expecting a list of string"
      with Ezjsonm.Parse_error _ ->
        cctxt#error "Given text is not valid JSON: '%s'" text
    in
    let from_json_hex (cctxt : #Client_context.full) text =
      try
        match Ezjsonm.from_string text with
        | `A messages -> return (`Json (`A (`String "hex" :: messages)))
        | _ -> cctxt#error "Expecting a list of hex-encoded string"
      with Ezjsonm.Parse_error _ ->
        cctxt#error "Given text is not valid JSON: '%s'" text
    in
    let from_bin_file (cctxt : #Client_context.full) path =
      let* bin = cctxt#read_file path in
      return (`Bin bin)
    in
    let from_json_file (cctxt : #Client_context.full) path =
      let* json_string = cctxt#read_file path in
      from_json cctxt json_string
    in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) p ->
        Client_aliases.parse_alternatives
          [
            ("text", from_json cctxt);
            ("hex", from_json_hex cctxt);
            ("file", from_json_file cctxt);
            ("bin", from_bin_file cctxt);
          ]
          p)

  let commitment_hash_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) commitment_hash ->
        match Sc_rollup.Commitment.Hash.of_b58check_opt commitment_hash with
        | None ->
            cctxt#error
              "Parameter '%s' is not a valid B58-encoded rollup commitment hash"
              commitment_hash
        | Some hash -> return hash)

  let unchecked_payload_parameter =
    let open Lwt_result_syntax in
    file_or_text_parameter ~from_text:(fun _cctxt -> return) ()

  let compressed_state_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) state_hash ->
        match Sc_rollup.State_hash.of_b58check_opt state_hash with
        | None ->
            cctxt#error
              "Parameter '%s' is not a valid B58-encoded compressed state"
              state_hash
        | Some hash -> return hash)

  let number_of_ticks_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) nb_of_ticks ->
        match Int64.of_string_opt nb_of_ticks with
        | Some nb_of_ticks -> (
            match Sc_rollup.Number_of_ticks.of_value nb_of_ticks with
            | None ->
                cctxt#error
                  "Parameter '%Ld' is out of bounds, it should be between %Ld \
                   and %Ld"
                  nb_of_ticks
                  Sc_rollup.Number_of_ticks.min_value
                  Sc_rollup.Number_of_ticks.max_value
            | Some nb_of_ticks -> return nb_of_ticks)
        | None ->
            cctxt#error "'%s' is not valid, should be a int64 value" nb_of_ticks)

  let whitelist =
    json_encoded_parameter
      ~name:"Whitelist for private rollups"
      Sc_rollup.Whitelist.encoding
end

let whitelist_arg =
  Tezos_clic.arg
    ~long:"whitelist"
    ~short:'W'
    ~placeholder:"whitelist"
    ~doc:
      "Whitelist for private rollups. Members of the whitelist are stakers \
       that are allowed to publish commitments."
    Sc_rollup_params.whitelist

module Zk_rollup_params = struct
  let address_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) s ->
        match Zk_rollup.Address.of_b58check_opt s with
        | Some c -> return c
        | None -> cctxt#error "Parameter '%s' is an invalid Epoxy address" s)

  let plonk_public_parameters_parameter =
    binary_encoded_parameter
      ~name:"PlonK public"
      Plonk.Main_protocol.verifier_public_parameters_encoding

  let update_parameter =
    binary_encoded_parameter ~name:"Epoxy Update" Zk_rollup.Update.encoding

  let operations_parameter =
    binary_encoded_parameter
      ~name:"Epoxy Operations"
      Data_encoding.(
        list
        @@ tup2 Zk_rollup.Operation.encoding (option Zk_rollup.Ticket.encoding))

  let state_parameter =
    binary_encoded_parameter ~name:"Epoxy State" Zk_rollup.State.encoding

  let circuits_info_parameter =
    binary_encoded_parameter
      ~name:"Epoxy Circuits_info map"
      Zk_rollup.Account.circuits_info_encoding
end

module Dal = struct
  let commitment_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) commitment_hash ->
        match Dal_slot_repr.Commitment.of_b58check_opt commitment_hash with
        | None ->
            cctxt#error
              "Parameter '%s' is not a valid B58-encoded DAL commitment"
              commitment_hash
        | Some commitment -> return commitment)

  let commitment_proof_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter
      (fun (cctxt : #Client_context.full) commitment_proof_hex ->
        match Hex.to_string (`Hex commitment_proof_hex) with
        | None ->
            cctxt#error
              "Commitment proof parameter '%s' is not a valid hexadecimal \
               string"
              commitment_proof_hex
        | Some commitment_proof_bin -> (
            match
              Data_encoding.Binary.of_string_opt
                Dal_slot_repr.Commitment_proof.encoding
                commitment_proof_bin
            with
            | None ->
                cctxt#error
                  "Commitment proof parameter '%s' is not a valid DAL \
                   commitment proof"
                  commitment_proof_hex
            | Some commitment_proof -> return commitment_proof))
end

let fee_parameter_args =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  let force_low_fee_arg =
    switch
      ~long:"force-low-fee"
      ~doc:"Don't check that the fee is lower than the estimated default value"
      ()
  in
  let fee_cap_arg =
    default_arg
      ~long:"fee-cap"
      ~placeholder:"amount"
      ~default:"1.0"
      ~doc:"Set the fee cap"
      (parameter (fun (cctxt : #Client_context.full) s ->
           match Tez.of_string s with
           | Some t -> return t
           | None -> cctxt#error "Bad fee cap"))
  in
  let burn_cap_arg =
    default_arg
      ~long:"burn-cap"
      ~placeholder:"amount"
      ~default:"0"
      ~doc:"Set the burn cap"
      (parameter (fun (cctxt : #Client_context.full) s ->
           match Tez.of_string s with
           | Some t -> return t
           | None -> cctxt#error "Bad burn cap"))
  in
  Tezos_clic.map_arg
    ~f:
      (fun _cctxt
           ( minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap ) ->
      return
        {
          Injection.minimal_fees;
          minimal_nanotez_per_byte;
          minimal_nanotez_per_gas_unit;
          force_low_fee;
          fee_cap;
          burn_cap;
        })
    (Tezos_clic.aggregate
       (Tezos_clic.args6
          minimal_fees_arg
          minimal_nanotez_per_byte_arg
          minimal_nanotez_per_gas_unit_arg
          force_low_fee_arg
          fee_cap_arg
          burn_cap_arg))
