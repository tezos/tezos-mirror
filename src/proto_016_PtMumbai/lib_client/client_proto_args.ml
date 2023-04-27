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

type error += Bad_endorsement_delay of string

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
    ~id:"badEndorsementDelayArg"
    ~title:"Bad -endorsement-delay arg"
    ~description:"invalid duration in -endorsement-delay"
    ~pp:(fun ppf literal ->
      Format.fprintf
        ppf
        "Bad argument value for -endorsement-delay. Expected an integer, but \
         given '%s'"
        literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_endorsement_delay parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_endorsement_delay parameter) ;
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

let string_parameter = Tezos_clic.parameter (fun _ x -> return x)

let int_parameter =
  Tezos_clic.parameter (fun _ p ->
      try return (int_of_string p) with _ -> failwith "Cannot read int")

let uri_parameter = Tezos_clic.parameter (fun _ x -> return (Uri.of_string x))

let bytes_of_prefixed_string s =
  match
    if String.length s < 2 || s.[0] <> '0' || s.[1] <> 'x' then None
    else Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))
  with
  | Some s -> return s
  | None ->
      failwith "Invalid bytes, expecting hexadecimal notation (e.g. 0x1234abcd)"

let bytes_parameter =
  Tezos_clic.parameter (fun _ s -> bytes_of_prefixed_string s)

let parse_file ~from_text ~read_file ~path =
  let open Lwt_result_syntax in
  let* content = read_file path in
  from_text content

let file_or_text ~from_text ~read_file =
  Client_aliases.parse_alternatives
    [
      ("file", fun path -> parse_file ~from_text ~read_file ~path);
      ("text", from_text);
    ]

let file_or_text_parameter ~from_text () =
  Tezos_clic.parameter (fun (cctxt : #Client_context.full) ->
      file_or_text ~from_text ~read_file:cctxt#read_file)

let json_parameter =
  let from_text s =
    match Data_encoding.Json.from_string s with
    | Ok json -> return json
    | Error err -> failwith "'%s' is not a valid JSON-encoded value: %s" s err
  in
  file_or_text_parameter ~from_text ()

let data_parameter =
  let open Lwt_syntax in
  let from_text input =
    return @@ Tezos_micheline.Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression input
  in
  file_or_text_parameter ~from_text ()

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

let no_endorse_switch =
  Tezos_clic.switch
    ~long:"no-endorse"
    ~doc:"Do not let the client automatically endorse a block that it baked."
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
  Tezos_clic.parameter (fun _ s ->
      match Tez.of_string s with
      | Some tez -> return tez
      | None -> fail (Bad_tez_arg (param, s)))

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

let non_negative_z_parameter =
  Tezos_clic.parameter (fun _ s ->
      try
        let v = Z.of_string s in
        error_when Compare.Z.(v < Z.zero) (Forbidden_Negative_int s)
        >>?= fun () -> return v
      with _ -> failwith "Invalid number, must be a non negative number.")

let non_negative_z_param ~name ~desc next =
  Tezos_clic.param ~name ~desc non_negative_z_parameter next

let counter_parameter =
  Tezos_clic.parameter (fun _ s ->
      match Manager_counter.Internal_for_injection.of_string s with
      | None -> failwith "Invalid counter, must be a non-negative number."
      | Some c -> return c)

let non_negative_parameter =
  Tezos_clic.parameter (fun _ s ->
      match int_of_string_opt s with
      | Some i when i >= 0 -> return i
      | _ -> failwith "Parameter should be a non-negative integer literal")

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
  Tezos_clic.parameter (fun _ s ->
      match Option.bind (Script_int.of_string s) Script_int.is_nat with
      | Some n -> return n
      | None -> failwith "invalid level (must be a positive number)")

let level_arg =
  Tezos_clic.arg
    ~long:"level"
    ~placeholder:"level"
    ~doc:"Set the level to be returned by the LEVEL instruction"
    level_kind

let raw_level_parameter =
  Tezos_clic.parameter (fun _ s ->
      match Int32.of_string_opt s with
      | Some i when i >= 0l ->
          Lwt.return @@ Environment.wrap_tzresult (Raw_level.of_int32 i)
      | _ ->
          failwith
            "'%s' is not a valid level (should be a non-negative int32 value)"
            s)

let timestamp_parameter =
  Tezos_clic.parameter (fun _ s ->
      match Script_timestamp.of_string s with
      | Some time -> return time
      | None ->
          failwith
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
  Tezos_clic.parameter (fun _ s ->
      try
        let v = Z.of_string s in
        return (Gas.Arith.integral_exn v)
      with _ -> failwith "invalid gas limit (must be a positive number)")

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
  Tezos_clic.parameter (fun _ s ->
      try
        let v = Z.of_string s in
        assert (Compare.Z.(v >= Z.zero)) ;
        return v
      with _ ->
        failwith "invalid storage limit (must be a positive number of bytes)")

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
  Tezos_clic.arg
    ~long:"max-priority"
    ~placeholder:"slot"
    ~doc:"maximum allowed baking slot"
    (Tezos_clic.parameter (fun _ s ->
         try return (int_of_string s) with _ -> fail (Bad_max_priority s)))

let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let minimal_fees_arg =
  Tezos_clic.default_arg
    ~long:"minimal-fees"
    ~placeholder:"amount"
    ~doc:"exclude operations with fees lower than this threshold (in tez)"
    ~default:(Tez.to_string default_minimal_fees)
    (Tezos_clic.parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_gas_unit_arg =
  Tezos_clic.default_arg
    ~long:"minimal-nanotez-per-gas-unit"
    ~placeholder:"amount"
    ~doc:
      "exclude operations with fees per gas lower than this threshold (in \
       nanotez)"
    ~default:(Q.to_string default_minimal_nanotez_per_gas_unit)
    (Tezos_clic.parameter (fun _ s ->
         try return (Q.of_string s) with _ -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_byte_arg =
  Tezos_clic.default_arg
    ~long:"minimal-nanotez-per-byte"
    ~placeholder:"amount"
    ~default:(Q.to_string default_minimal_nanotez_per_byte)
    ~doc:
      "exclude operations with fees per byte lower than this threshold (in \
       nanotez)"
    (Tezos_clic.parameter (fun _ s ->
         try return (Q.of_string s) with _ -> fail (Bad_minimal_fees s)))

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
  Tezos_clic.default_arg
    ~long:"preserved-levels"
    ~placeholder:"threshold"
    ~doc:"Number of effective levels kept in the accuser's memory"
    ~default:"200"
    (Tezos_clic.parameter (fun _ s ->
         try
           let preserved_cycles = int_of_string s in
           if preserved_cycles < 0 then fail (Bad_preserved_levels s)
           else return preserved_cycles
         with _ -> fail (Bad_preserved_levels s)))

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
  Tezos_clic.parameter (fun _cctxt s ->
      match Tezos_crypto.Signature.of_b58check_opt s with
      | Some s -> return s
      | None -> failwith "Not given a valid signature")

let unparsing_mode_parameter =
  Tezos_clic.parameter
    ~autocomplete:(fun _cctxt ->
      return ["Readable"; "Optimized"; "Optimized_legacy"])
    (fun _cctxt s ->
      match s with
      | "Readable" -> return Script_ir_unparser.Readable
      | "Optimized" -> return Script_ir_unparser.Optimized
      | "Optimized_legacy" -> return Script_ir_unparser.Optimized_legacy
      | _ -> failwith "Unknown unparsing mode %s" s)

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

module Daemon = struct
  let baking_switch =
    Tezos_clic.switch ~long:"baking" ~short:'B' ~doc:"run the baking daemon" ()

  let endorsement_switch =
    Tezos_clic.switch
      ~long:"endorsement"
      ~short:'E'
      ~doc:"run the endorsement daemon"
      ()

  let denunciation_switch =
    Tezos_clic.switch
      ~long:"denunciation"
      ~short:'D'
      ~doc:"run the denunciation daemon"
      ()
end

module Tx_rollup = struct
  let tx_rollup_address_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Tx_rollup.of_b58check_opt s with
        | Some c -> return c
        | None ->
            failwith
              "Parameter '%s' is an invalid transaction rollup address encoded \
               in a base58 string."
              s)

  let tx_rollup_address_param ?(name = "transaction rollup address") ~usage next
      =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Transaction rollup address encoded in a base58 \
            string.@]@]"
           usage)
      tx_rollup_address_parameter
      next

  let level_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Int32.of_string_opt s with
        | Some i when i >= 0l ->
            Lwt.return @@ Environment.wrap_tzresult (Tx_rollup_level.of_int32 i)
        | _ ->
            failwith
              "'%s' is not a valid transaction rollup level (should be a non \
               negative int32 value)"
              s)

  let level_param ?(name = "tx rollup level") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Transaction rollup level encoded in a non negative \
            int32.@]@]"
           usage)
      level_parameter
      next

  let context_hash_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Context_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith
              "%s is not a valid notation for a context hash encoded in a \
               base58 string"
              s)

  let context_hash_param ?(name = "context hash") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Context hash encoded in a base58 string.@]@]"
           usage)
      context_hash_parameter
      next

  let message_result_path_parameter =
    Tezos_clic.map_parameter
      ~f:(fun json ->
        try
          Data_encoding.Json.destruct
            Tx_rollup_commitment.Merkle.path_encoding
            json
        with Data_encoding.Json.Cannot_destruct (_path, exn) ->
          Stdlib.failwith
            (Format.asprintf
               "Invalid JSON for a message result path: %a"
               (fun ppf -> Data_encoding.Json.print_error ppf)
               exn))
      json_parameter

  let message_result_path_param ?(name = "message result path") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Merkle path (JSON encoded) for a message result hash \
            in a commitment.\n\
            The JSON should be a list of base58-encoded message result \
            hashes.@]@]"
           usage)
      message_result_path_parameter
      next

  let tickets_dispatch_info_parameter =
    Tezos_clic.map_parameter
      ~f:(fun json ->
        try Data_encoding.Json.destruct Tx_rollup_reveal.encoding json
        with Data_encoding.Json.Cannot_destruct (_path, exn) ->
          Stdlib.failwith
            (Format.asprintf
               "Invalid JSON for tickets dispatch info: %a"
               (fun ppf -> Data_encoding.Json.print_error ppf)
               exn))
      json_parameter

  let tickets_dispatch_info_param ?(name = "tickets information") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Tickets related information are encoded in a JSON with \
            the following format: {\"contents\": <tickets content>,\"ty\": \
            <tickets type>, \"ticketer\": <ticketer contract address>, \
            \"amount\": <withdrawn amount>, \"\"claimer\": <new owner's public \
            key hash>}@]@]"
           usage)
      tickets_dispatch_info_parameter
      next

  let message_result_hash_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Tx_rollup_message_result_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith "%s is not a valid notation for a withdraw list hash" s)

  let message_result_hash_param ?(name = "message result hash") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Message result hash encoded in a base58 string.@]@]"
           usage)
      message_result_hash_parameter
      next

  let withdraw_list_hash_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Tx_rollup_withdraw_list_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith "%s is not a valid notation for a withdraw list hash" s)

  let withdraw_list_hash_param ?(name = "withdraw list hash") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Withdraw list hash encoded in a base58 string.@]@]"
           usage)
      withdraw_list_hash_parameter
      next

  let commitment_hash_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Tx_rollup_commitment_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None -> failwith "%s is not a valid notation for a commitment hash" s)

  let commitment_hash_param ?(name = "commitment hash") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Commitment hash encoded in a base58 string.@]@]"
           usage)
      commitment_hash_parameter
      next

  let commitment_hash_arg ?(long = "commitment-hash")
      ?(placeholder = "commitment hash") ~usage () =
    Tezos_clic.arg
      ~long
      ~doc:
        (Format.sprintf
           "@[@[%s@]@.@[Commitment hash encoded in a base58 string.@]@]"
           usage)
      ~placeholder
      commitment_hash_parameter

  let message_parameter =
    Tezos_clic.map_parameter
      ~f:(fun json ->
        try Data_encoding.Json.destruct Tx_rollup_message.encoding json
        with Data_encoding.Json.Cannot_destruct (_path, exn) ->
          Stdlib.failwith
            (Format.asprintf
               "Invalid json for a message: %a"
               (fun ppf -> Data_encoding.Json.print_error ppf)
               exn))
      json_parameter

  let message_param ?(name = "message") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Message are encoded in a JSON with one of the \
            following format: {\"batch\": bytes} or {\"deposit\": {\"sender\": \
            <depositer public key hash>; \"destination\": <layer 2 destination \
            (address or index)>;\"ticket_hash\": <hash of the tickets> \
            ;\"amount\": <deposited amount> }}.@]@]"
           usage)
      message_parameter
      next

  let message_path_parameter =
    Tezos_clic.map_parameter
      ~f:(fun json ->
        try
          Data_encoding.Json.destruct Tx_rollup_inbox.Merkle.path_encoding json
        with Data_encoding.Json.Cannot_destruct (_path, exn) ->
          Stdlib.failwith
            (Format.asprintf
               "Invalid json for a message path: %a"
               (fun ppf -> Data_encoding.Json.print_error ppf)
               exn))
      json_parameter

  let message_path_param ?(name = "message path") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[\n\
            Merkle path (JSON encoded) for a message in an inbox. The JSON \
            should be a list of base58-encoded message hashes.@]@]"
           usage)
      message_path_parameter
      next

  let proof_parameter =
    Tezos_clic.map_parameter
      ~f:(fun json ->
        try Data_encoding.Json.destruct Tx_rollup_l2_proof.encoding json
        with Data_encoding.Json.Cannot_destruct (_path, exn) ->
          Stdlib.failwith
            (Format.asprintf
               "Invalid json for a tx_rollup proof: %a"
               (fun ppf -> Data_encoding.Json.print_error ppf)
               exn))
      json_parameter

  let proof_param ?(name = "rejection proof") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Rejection proof are stream encoded in a JSON. See \
            documentation of transaction rollup for more information.@]@]"
           usage)
      proof_parameter
      next

  let inbox_root_hash_parameter =
    Tezos_clic.parameter (fun _ s ->
        match Tx_rollup_inbox.Merkle.root_of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith
              "%s is not a valid B58-encoded notation for an inbox merkle root"
              s)

  let inbox_root_hash_param ?(name = "inbox root hash") ~usage next =
    Tezos_clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Root's hash of a merkelized inbox list, encoded in a \
            base58 string.@]@]"
           usage)
      inbox_root_hash_parameter
      next
end

module Sc_rollup_params = struct
  let sc_rollup_address_parameter =
    Tezos_clic.parameter
      ~autocomplete:Client_proto_rollups.SoruAlias.autocomplete
      (fun cctxt ->
        Client_aliases.parse_alternatives
          [
            ( "alias",
              fun alias -> Client_proto_rollups.SoruAlias.find cctxt alias );
            ("text", fun text -> Client_proto_rollups.SoruAlias.of_source text);
          ])

  let sc_rollup_address_param ?(name = "smart rollup address")
      ?(desc = "the address of the targeted smart rollup") next =
    let desc =
      String.concat
        "\n"
        [
          desc;
          "Can be an alias or a literal (autodetected in order).\n\
           Use 'alias:name' or 'text:literal' to force.";
        ]
    in
    Tezos_clic.param ~name ~desc sc_rollup_address_parameter next

  let rollup_kind_parameter =
    Tezos_clic.parameter (fun _ name ->
        match Sc_rollup.Kind.of_string name with
        | None ->
            failwith
              "Parameter '%s' is not a valid rollup name (must be one of %s)"
              name
              (String.concat ", " Sc_rollup.Kind.(List.map to_string all))
        | Some k -> return k)

  let boot_sector_parameter =
    let from_text s =
      return (fun (Sc_rollup.PVM.Packed (module R)) ->
          R.parse_boot_sector s |> function
          | None -> failwith "Invalid kernel"
          | Some boot_sector -> return boot_sector)
    in
    file_or_text_parameter ~from_text ()

  let messages_parameter =
    let open Lwt_result_syntax in
    let from_json text =
      try
        match Ezjsonm.from_string text with
        | `A messages -> return (`Json (`A (`String "raw" :: messages)))
        | _ -> failwith "Expecting a list of string"
      with Ezjsonm.Parse_error _ ->
        failwith "Given text is not valid JSON: '%s'" text
    in
    let from_json_hex text =
      try
        match Ezjsonm.from_string text with
        | `A messages -> return (`Json (`A (`String "hex" :: messages)))
        | _ -> failwith "Expecting a list of hex-encoded string"
      with Ezjsonm.Parse_error _ ->
        failwith "Given text is not valid JSON: '%s'" text
    in
    let from_bin_file (cctxt : #Client_context.full) path =
      let* bin = cctxt#read_file path in
      return (`Bin bin)
    in
    let from_json_file (cctxt : #Client_context.full) path =
      let* json_string = cctxt#read_file path in
      from_json json_string
    in
    Tezos_clic.parameter (fun (cctxt : #Client_context.full) p ->
        Client_aliases.parse_alternatives
          [
            ("text", from_json);
            ("hex", from_json_hex);
            ("file", from_json_file cctxt);
            ("bin", from_bin_file cctxt);
          ]
          p)

  let commitment_hash_parameter =
    Tezos_clic.parameter (fun _ commitment_hash ->
        match Sc_rollup.Commitment.Hash.of_b58check_opt commitment_hash with
        | None ->
            failwith
              "Parameter '%s' is not a valid B58-encoded rollup commitment hash"
              commitment_hash
        | Some hash -> return hash)

  let unchecked_payload_parameter = file_or_text_parameter ~from_text:return ()

  let compressed_state_parameter =
    Tezos_clic.parameter (fun _ state_hash ->
        match Sc_rollup.State_hash.of_b58check_opt state_hash with
        | None ->
            failwith
              "Parameter '%s' is not a valid B58-encoded compressed state"
              state_hash
        | Some hash -> return hash)

  let number_of_ticks_parameter =
    Tezos_clic.parameter (fun _ nb_of_ticks ->
        match Int64.of_string_opt nb_of_ticks with
        | Some nb_of_ticks -> (
            match Sc_rollup.Number_of_ticks.of_value nb_of_ticks with
            | None ->
                failwith
                  "Parameter '%Ld' is out of bounds, it should be between %Ld \
                   and %Ld"
                  nb_of_ticks
                  Sc_rollup.Number_of_ticks.min_value
                  Sc_rollup.Number_of_ticks.max_value
            | Some nb_of_ticks -> return nb_of_ticks)
        | None ->
            failwith "'%s' is not valid, should be a int64 value" nb_of_ticks)
end

module Zk_rollup_params = struct
  let address_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun _cctxt s ->
        match Zk_rollup.Address.of_b58check_opt s with
        | Some c -> return c
        | None -> failwith "Parameter '%s' is an invalid Epoxy address" s)

  let plonk_public_parameters_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter @@ fun cctx s ->
    file_or_text
      ~read_file:cctx#read_file
      ~from_text:(fun s ->
        match
          Data_encoding.(
            Binary.of_bytes_opt
              Plonk.Main_protocol.verifier_public_parameters_encoding)
            (Bytes.of_string s)
        with
        | None -> failwith "Invalid PlonK public parameter"
        | Some x -> return x)
      s

  let update_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter @@ fun cctx s ->
    file_or_text
      ~read_file:cctx#read_file
      ~from_text:(fun s ->
        match
          Data_encoding.Binary.of_bytes_opt
            Zk_rollup.Update.encoding
            (Bytes.of_string s)
        with
        | None -> failwith "Invalid Epoxy Update parameter"
        | Some u -> return u)
      s

  let operations_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter @@ fun cctx s ->
    file_or_text
      ~read_file:cctx#read_file
      ~from_text:(fun s ->
        match
          Data_encoding.(
            Binary.of_bytes_opt
              (list
              @@ tup2
                   Zk_rollup.Operation.encoding
                   (option Zk_rollup.Ticket.encoding)))
            (Bytes.of_string s)
        with
        | None -> failwith "Invalid Epoxy Operations parameter"
        | Some ops -> return ops)
      s

  let state_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter @@ fun cctx s ->
    file_or_text
      ~read_file:cctx#read_file
      ~from_text:(fun s ->
        match
          Data_encoding.Binary.of_bytes_opt
            Zk_rollup.State.encoding
            (Bytes.of_string s)
        with
        | None -> failwith "Invalid Epoxy State parameter"
        | Some s -> return s)
      s

  let circuits_info_parameter =
    let open Lwt_result_syntax in
    Tezos_clic.parameter @@ fun cctx s ->
    file_or_text
      ~read_file:cctx#read_file
      ~from_text:(fun s ->
        match
          Data_encoding.Binary.of_bytes_opt
            Zk_rollup.Account.circuits_info_encoding
            (Bytes.of_string s)
        with
        | None -> failwith "Invalid Epoxy Circuits_info map parameter"
        | Some c -> return c)
      s
end

let fee_parameter_args =
  let open Tezos_clic in
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
      (parameter (fun _ s ->
           match Tez.of_string s with
           | Some t -> return t
           | None -> failwith "Bad fee cap"))
  in
  let burn_cap_arg =
    default_arg
      ~long:"burn-cap"
      ~placeholder:"amount"
      ~default:"0"
      ~doc:"Set the burn cap"
      (parameter (fun _ s ->
           match Tez.of_string s with
           | Some t -> return t
           | None -> failwith "Bad burn cap"))
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
