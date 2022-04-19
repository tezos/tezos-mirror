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
open Clic

type error += Bad_tez_arg of string * string (* Arg_name * value *)

type error += Bad_max_priority of string

type error += Bad_minimal_fees of string

type error += Bad_max_waiting_time of string

type error += Bad_endorsement_delay of string

type error += Bad_preserved_levels of string

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
    (fun parameter -> Bad_preserved_levels parameter)

let tez_sym = "\xEA\x9C\xA9"

let string_parameter = parameter (fun _ x -> return x)

let int_parameter =
  parameter (fun _ p ->
      try return (int_of_string p) with _ -> failwith "Cannot read int")

let uri_parameter = parameter (fun _ x -> return (Uri.of_string x))

let bytes_of_prefixed_string s =
  match
    if String.length s < 2 || s.[0] <> '0' || s.[1] <> 'x' then None
    else Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))
  with
  | Some s -> return s
  | None ->
      failwith "Invalid bytes, expecting hexadecimal notation (e.g. 0x1234abcd)"

let bytes_parameter = parameter (fun _ s -> bytes_of_prefixed_string s)

let data_parameter =
  let open Lwt_syntax in
  let parse input =
    return @@ Tezos_micheline.Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression input
  in
  parameter (fun (cctxt : #Client_context.full) ->
      Client_aliases.parse_alternatives
        [
          ( "file",
            fun filename ->
              let open Lwt_result_syntax in
              let* input = catch_es (fun () -> cctxt#read_file filename) in
              parse input );
          ("text", parse);
        ])

let entrypoint_parameter =
  parameter (fun _ str ->
      Lwt.return @@ Environment.wrap_tzresult @@ Entrypoint.of_string_lax str)

let init_arg =
  default_arg
    ~long:"init"
    ~placeholder:"data"
    ~doc:"initial value of the contract's storage"
    ~default:"Unit"
    string_parameter

let global_constant_param ~name ~desc next =
  Clic.param ~name ~desc string_parameter next

let arg_arg =
  arg
    ~long:"arg"
    ~placeholder:"data"
    ~doc:"argument passed to the contract's script, if needed"
    string_parameter

let default_arg_arg =
  arg
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
  arg
    ~long:"source"
    ~placeholder:"address"
    ~doc:"source of the deposits to be paid\nMust be a known address."
    string_parameter

let entrypoint_arg =
  arg
    ~long:"entrypoint"
    ~placeholder:"name"
    ~doc:"entrypoint of the smart contract"
    entrypoint_parameter

let default_entrypoint_arg =
  arg
    ~long:"default-entrypoint"
    ~placeholder:"name"
    ~doc:"default entrypoint of the smart contracts"
    entrypoint_parameter

let force_switch =
  switch
    ~long:"force"
    ~short:'f'
    ~doc:
      "disables the node's injection checks\n\
       Force the injection of branch-invalid operation or force  the injection \
       of block without a fitness greater than the  current head."
    ()

let no_endorse_switch =
  switch
    ~long:"no-endorse"
    ~doc:"Do not let the client automatically endorse a block that it baked."
    ()

let minimal_timestamp_switch =
  switch
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
  parameter (fun _ s ->
      match Tez.of_string s with
      | Some tez -> return tez
      | None -> fail (Bad_tez_arg (param, s)))

let tez_arg ~default ~parameter ~doc =
  default_arg
    ~long:parameter
    ~placeholder:"amount"
    ~doc
    ~default
    (tez_parameter ("--" ^ parameter))

let tez_opt_arg ~parameter ~doc =
  arg
    ~long:parameter
    ~placeholder:"amount"
    ~doc
    (tez_parameter ("--" ^ parameter))

let tez_param ~name ~desc next =
  Clic.param
    ~name
    ~desc:(desc ^ " in \xEA\x9C\xA9\n" ^ tez_format)
    (tez_parameter name)
    next

let non_negative_z_parameter =
  parameter (fun _ s ->
      try
        let v = Z.of_string s in
        assert (Compare.Z.(v >= Z.zero)) ;
        return v
      with _ -> failwith "Invalid number, must be a non negative number.")

let non_negative_z_param ~name ~desc next =
  Clic.param ~name ~desc non_negative_z_parameter next

let fee_arg =
  arg
    ~long:"fee"
    ~placeholder:"amount"
    ~doc:"fee in \xEA\x9C\xA9 to pay to the baker"
    (tez_parameter "--fee")

let default_fee_arg =
  arg
    ~long:"default-fee"
    ~placeholder:"amount"
    ~doc:"default fee in \xEA\x9C\xA9 to pay to the baker for each transaction"
    (tez_parameter "--default-fee")

let level_kind =
  parameter (fun _ s ->
      match Option.bind (Script_int.of_string s) Script_int.is_nat with
      | Some n -> return n
      | None -> failwith "invalid level (must be a positive number)")

let level_arg =
  arg
    ~long:"level"
    ~placeholder:"level"
    ~doc:"Set the level to be returned by the LEVEL instruction"
    level_kind

let timestamp_parameter =
  parameter (fun _ s ->
      match Script_timestamp.of_string s with
      | Some time -> return time
      | None ->
          failwith
            "invalid timestamp, must be either a RFC 3339 string or a number \
             of seconds since epoch.")

let now_arg =
  arg
    ~long:"now"
    ~placeholder:"timestamp"
    ~doc:
      "Set the timestamp to be returned by the NOW instruction. Allowed format \
       are RFC 3339 (YYYY-MM-DDTHH:MM:SSZ) or number of seconds since epoch."
    timestamp_parameter

let gas_limit_kind =
  parameter (fun _ s ->
      try
        let v = Z.of_string s in
        return (Gas.Arith.integral_exn v)
      with _ -> failwith "invalid gas limit (must be a positive number)")

let gas_limit_arg =
  arg
    ~long:"gas-limit"
    ~short:'G'
    ~placeholder:"amount"
    ~doc:
      "Set the gas limit of the transaction instead of letting the client \
       decide based on a simulation"
    gas_limit_kind

let default_gas_limit_arg =
  arg
    ~long:"default-gas-limit"
    ~short:'G'
    ~placeholder:"amount"
    ~doc:
      "Set the default gas limit for each transaction instead of letting the \
       client decide based on a simulation"
    gas_limit_kind

let run_gas_limit_arg =
  arg
    ~long:"gas"
    ~short:'G'
    ~doc:"Initial quantity of gas for typechecking and execution"
    ~placeholder:"gas"
    gas_limit_kind

let storage_limit_kind =
  parameter (fun _ s ->
      try
        let v = Z.of_string s in
        assert (Compare.Z.(v >= Z.zero)) ;
        return v
      with _ ->
        failwith "invalid storage limit (must be a positive number of bytes)")

let storage_limit_arg =
  arg
    ~long:"storage-limit"
    ~short:'S'
    ~placeholder:"amount"
    ~doc:
      "Set the storage limit of the transaction instead of letting the client \
       decide based on a simulation"
    storage_limit_kind

let default_storage_limit_arg =
  arg
    ~long:"default-storage-limit"
    ~short:'S'
    ~placeholder:"amount"
    ~doc:
      "Set the default storage limit for each transaction instead of letting \
       the client decide based on a simulation"
    storage_limit_kind

let counter_arg =
  arg
    ~long:"counter"
    ~short:'C'
    ~placeholder:"counter"
    ~doc:"Set the counter to be used by the transaction"
    non_negative_z_parameter

let max_priority_arg =
  arg
    ~long:"max-priority"
    ~placeholder:"slot"
    ~doc:"maximum allowed baking slot"
    (parameter (fun _ s ->
         try return (int_of_string s) with _ -> fail (Bad_max_priority s)))

let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let minimal_fees_arg =
  default_arg
    ~long:"minimal-fees"
    ~placeholder:"amount"
    ~doc:"exclude operations with fees lower than this threshold (in tez)"
    ~default:(Tez.to_string default_minimal_fees)
    (parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_gas_unit_arg =
  default_arg
    ~long:"minimal-nanotez-per-gas-unit"
    ~placeholder:"amount"
    ~doc:
      "exclude operations with fees per gas lower than this threshold (in \
       nanotez)"
    ~default:(Q.to_string default_minimal_nanotez_per_gas_unit)
    (parameter (fun _ s ->
         try return (Q.of_string s) with _ -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_byte_arg =
  default_arg
    ~long:"minimal-nanotez-per-byte"
    ~placeholder:"amount"
    ~default:(Q.to_string default_minimal_nanotez_per_byte)
    ~doc:
      "exclude operations with fees per byte lower than this threshold (in \
       nanotez)"
    (parameter (fun _ s ->
         try return (Q.of_string s) with _ -> fail (Bad_minimal_fees s)))

let force_low_fee_arg =
  switch
    ~long:"force-low-fee"
    ~doc:"Don't check that the fee is lower than the estimated default value"
    ()

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

let replace_by_fees_arg =
  switch
    ~long:"replace"
    ~doc:
      "Replace an existing pending transaction from the same source, if any, \
       with another one with higher fees. There are no guarantees that the \
       first operation will not be included or that the second one will be. \
       But, only one of the operations at most will end in a block (in \
       precheck mode)."
    ()

let successor_level_arg =
  switch
    ~long:"simulate-successor-level"
    ~doc:"Make the simulate on the successor level of the current head."
    ()

let no_waiting_for_endorsements_arg =
  switch
    ~long:"no-waiting-for-late-endorsements"
    ~doc:"Disable waiting for late endorsements"
    ()

let await_endorsements_arg =
  switch
    ~long:"await-late-endorsements"
    ~doc:"Await late endorsements when baking a block"
    ()

let endorsement_delay_arg =
  default_arg
    ~long:"endorsement-delay"
    ~placeholder:"seconds"
    ~doc:
      "delay before endorsing blocks\n\
       Delay between notifications of new blocks from the node and production \
       of endorsements for these blocks."
    ~default:"0"
    (parameter (fun _ s ->
         try
           let i = int_of_string s in
           fail_when (i < 0) (Bad_endorsement_delay s) >>=? fun () ->
           return (int_of_string s)
         with _ -> fail (Bad_endorsement_delay s)))

let preserved_levels_arg =
  arg
    ~long:"preserved-levels"
    ~placeholder:"threshold"
    ~doc:"Number of effective levels kept in the accuser's memory"
    (parameter (fun _ s ->
         try
           let preserved_cycles = int_of_string s in
           if preserved_cycles < 0 then fail (Bad_preserved_levels s)
           else return preserved_cycles
         with _ -> fail (Bad_preserved_levels s)))

let no_print_source_flag =
  switch
    ~long:"no-print-source"
    ~short:'q'
    ~doc:
      "don't print the source code\n\
       If an error is encountered, the client will print the contract's source \
       code by default.\n\
       This option disables this behaviour."
    ()

let no_confirmation =
  switch
    ~long:"no-confirmation"
    ~doc:"don't print wait for the operation to be confirmed."
    ()

let signature_parameter =
  parameter (fun _cctxt s ->
      match Signature.of_b58check_opt s with
      | Some s -> return s
      | None -> failwith "Not given a valid signature")

let unparsing_mode_parameter =
  parameter
    ~autocomplete:(fun _cctxt ->
      return ["Readable"; "Optimized"; "Optimized_legacy"])
    (fun _cctxt s ->
      match s with
      | "Readable" -> return Script_ir_translator.Readable
      | "Optimized" -> return Script_ir_translator.Optimized
      | "Optimized_legacy" -> return Script_ir_translator.Optimized_legacy
      | _ -> failwith "Unknown unparsing mode %s" s)

let unparsing_mode_arg ~default =
  default_arg
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
  switch
    ~long:"enforce-indentation"
    ~doc:
      "Check that the Micheline expression passed to this command is \
       well-indented."
    ()

let display_names_flag =
  switch
    ~long:"display-names"
    ~doc:"Print names of scripts passed to this command"
    ()

let json_parameter =
  Clic.parameter (fun _ s ->
      match Data_encoding.Json.from_string s with
      | Ok json -> return json
      | Error err -> failwith "'%s' is not a valid JSON-encoded valie: %s" s err)

module Daemon = struct
  let baking_switch =
    switch ~long:"baking" ~short:'B' ~doc:"run the baking daemon" ()

  let endorsement_switch =
    switch ~long:"endorsement" ~short:'E' ~doc:"run the endorsement daemon" ()

  let denunciation_switch =
    switch ~long:"denunciation" ~short:'D' ~doc:"run the denunciation daemon" ()
end

module Tx_rollup = struct
  let tx_rollup_address_parameter =
    Clic.parameter (fun _ s ->
        match Tx_rollup.of_b58check_opt s with
        | Some c -> return c
        | None ->
            failwith
              "Parameter '%s' is an invalid transaction rollup address encoded \
               in a base58 string."
              s)

  let tx_rollup_address_param ?(name = "transaction rollup address") ~usage next
      =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Transaction rollup address encoded in a base58 \
            string.@]@]"
           usage)
      tx_rollup_address_parameter
      next

  let level_parameter =
    Clic.parameter (fun _ s ->
        match Int32.of_string_opt s with
        | Some i when i >= 0l ->
            Lwt.return @@ Environment.wrap_tzresult (Tx_rollup_level.of_int32 i)
        | _ ->
            failwith
              "'%s' is not a valid transaction rollup level (should be a non \
               negative int32 value)"
              s)

  let level_param ?(name = "tx rollup level") ~usage next =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Transaction rollup level encoded in a non negative \
            int32.@]@]"
           usage)
      level_parameter
      next

  let context_hash_parameter =
    Clic.parameter (fun _ s ->
        match Context_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith
              "%s is not a valid notation for a context hash encoded in a \
               base58 string"
              s)

  let context_hash_param ?(name = "context hash") ~usage next =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Context hash encoded in a base58 string.@]@]"
           usage)
      context_hash_parameter
      next

  let message_result_path_parameter =
    Clic.map_parameter
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
    Clic.param
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
    Clic.map_parameter
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
    Clic.param
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
    Clic.parameter (fun _ s ->
        match Tx_rollup_message_result_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith "%s is not a valid notation for a withdraw list hash" s)

  let message_result_hash_param ?(name = "message result hash") ~usage next =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Message result hash encoded in a base58 string.@]@]"
           usage)
      message_result_hash_parameter
      next

  let withdraw_list_hash_parameter =
    Clic.parameter (fun _ s ->
        match Tx_rollup_withdraw_list_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith "%s is not a valid notation for a withdraw list hash" s)

  let withdraw_list_hash_param ?(name = "withdraw list hash") ~usage next =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Withdraw list hash encoded in a base58 string.@]@]"
           usage)
      withdraw_list_hash_parameter
      next

  let commitment_hash_parameter =
    Clic.parameter (fun _ s ->
        match Tx_rollup_commitment_hash.of_b58check_opt s with
        | Some hash -> return hash
        | None -> failwith "%s is not a valid notation for a commitment hash" s)

  let commitment_hash_param ?(name = "commitment hash") ~usage next =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Commitment hash encoded in a base58 string.@]@]"
           usage)
      commitment_hash_parameter
      next

  let commitment_hash_arg ?(long = "commitment-hash")
      ?(placeholder = "commitment hash") ~usage () =
    Clic.arg
      ~long
      ~doc:
        (Format.sprintf
           "@[@[%s@]@.@[Commitment hash encoded in a base58 string.@]@]"
           usage)
      ~placeholder
      commitment_hash_parameter

  let message_parameter =
    Clic.map_parameter
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
    Clic.param
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
    Clic.map_parameter
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
    Clic.param
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
    Clic.map_parameter
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
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Rejection proof are stream encoded in a JSON. See \
            documentation of transaction rollup for more information.@]@]"
           usage)
      proof_parameter
      next

  let inbox_root_hash_parameter =
    Clic.parameter (fun _ s ->
        match Tx_rollup_inbox.Merkle.root_of_b58check_opt s with
        | Some hash -> return hash
        | None ->
            failwith
              "%s is not a valid B58-encoded notation for an inbox merkle root"
              s)

  let inbox_root_hash_param ?(name = "inbox root hash") ~usage next =
    Clic.param
      ~name
      ~desc:
        (Format.sprintf
           "@[@[%s@]@.@[Root's hash of a merkelized inbox list, encoded in a \
            base58 string.@]@]"
           usage)
      inbox_root_hash_parameter
      next
end
