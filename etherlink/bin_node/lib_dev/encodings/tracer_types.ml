(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* Byte used as prefix in the input stored for the kernel
   to identify the tracer.

   The StructLogger is the default tracer, and doesn't use a prefix.
*)
let tracer_input_prefix_calltracer = '\001'

type inconsistent_traces_input = {
  block : Ethereum_types.quantity;
  nb_txs : int;
  nb_traces : int;
}

type error +=
  | Not_supported
  | Transaction_not_found of Ethereum_types.hash
  | Block_not_found of Ethereum_types.quantity
  | Trace_not_found
  | Tracer_not_activated  (** Tracer only activated after a certain level *)
  | Tracer_not_implemented of string
        (** Not all tracer are available for all rpcs *)
  | Inconsistent_traces of inconsistent_traces_input
  | Trace_decoding_error of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_tracer_not_supported"
    ~title:"Tracer not supported"
    ~description:"The tracer specified in the request in not supported"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The tracer specified in the request in not supported")
    Data_encoding.empty
    (function Not_supported -> Some () | _ -> None)
    (fun () -> Not_supported) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_transaction_not_found"
    ~title:"Transaction not found"
    ~description:"The tracer failed to find a transaction"
    ~pp:(fun ppf tx ->
      Format.fprintf
        ppf
        "The tracer failed to find the transaction %a"
        Ethereum_types.pp_hash
        tx)
    Data_encoding.(obj1 (req "hash" Ethereum_types.hash_encoding))
    (function Transaction_not_found tx -> Some tx | _ -> None)
    (fun tx -> Transaction_not_found tx) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_block_not_found"
    ~title:"Block not found"
    ~description:"The tracer failed to find a block"
    ~pp:(fun ppf block ->
      Format.fprintf
        ppf
        "The tracer failed to find the block %a"
        Ethereum_types.pp_quantity
        block)
    Data_encoding.(obj1 (req "block" Ethereum_types.quantity_encoding))
    (function Block_not_found tx -> Some tx | _ -> None)
    (fun tx -> Block_not_found tx) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_trace_not_found"
    ~title:"Trace not found"
    ~description:
      "The tracer failed to recover the information needed to build the trace"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The tracer failed to recover the information needed to build the trace")
    Data_encoding.empty
    (function Trace_not_found -> Some () | _ -> None)
    (fun () -> Trace_not_found) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_tracer_not_activated"
    ~title:"Tracer not activated"
    ~description:"The tracer specified in the request is not activated"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The tracer specified in the request is not activated")
    Data_encoding.empty
    (function Tracer_not_activated -> Some () | _ -> None)
    (fun () -> Tracer_not_activated) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_inconsistent_traces"
    ~title:"Inconsistent traces"
    ~description:"The tracer failed trace a block"
    ~pp:(fun ppf (blocknumber, nb_of_hashes, nb_of_traces) ->
      Format.fprintf
        ppf
        "The tracer failed to trace block %a: failed to find which transaction \
         (among %d) was linked to which trace (among %d)"
        Ethereum_types.pp_quantity
        blocknumber
        nb_of_hashes
        nb_of_traces)
    Data_encoding.(
      obj3
        (req "blocknumber" Ethereum_types.quantity_encoding)
        (req "nb_of_hashes" Data_encoding.int31)
        (req "nb_of_traces" Data_encoding.int31))
    (function
      | Inconsistent_traces {block; nb_txs; nb_traces} ->
          Some (block, nb_txs, nb_traces)
      | _ -> None)
    (fun (block, nb_txs, nb_traces) ->
      Inconsistent_traces {block; nb_txs; nb_traces}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_tracer_not_implemented"
    ~title:"Tracer not implemented"
    ~description:
      "The tracer specified in the request is not implemented for that \n\
      \    particular request"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "The tracer %s is not available for that request" s)
    Data_encoding.(obj1 (req "tracer" string))
    (function Tracer_not_implemented s -> Some s | _ -> None)
    (fun s -> Tracer_not_implemented s) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_trace_decoding_error"
    ~title:"The tracer encountered an error"
    ~description:"The tracer encountered an error during decoding"
    ~pp:(fun ppf s -> Format.fprintf ppf "The tracer failed to decode: %s" s)
    Data_encoding.(obj1 (req "tracer" string))
    (function Trace_decoding_error s -> Some s | _ -> None)
    (fun s -> Trace_decoding_error s)

type tracer_config = {
  (* StructLogger flags *)
  enable_return_data : bool;
  enable_memory : bool;
  disable_stack : bool;
  disable_storage : bool;
  (* CallTracer flags *)
  with_logs : bool;
  only_top_call : bool;
}

let default_tracer_config =
  {
    enable_return_data = false;
    enable_memory = false;
    disable_stack = false;
    disable_storage = false;
    with_logs = true;
    only_top_call = false;
  }

let tracer_config_encoding =
  let open Data_encoding in
  conv
    (fun {
           enable_return_data;
           enable_memory;
           disable_stack;
           disable_storage;
           with_logs;
           only_top_call;
         }
       ->
      ( enable_return_data,
        enable_memory,
        disable_stack,
        disable_storage,
        with_logs,
        only_top_call,
        false,
        5 ))
    (fun ( enable_return_data,
           enable_memory,
           disable_stack,
           disable_storage,
           with_logs,
           only_top_call,
           _,
           _ )
       ->
      {
        enable_return_data;
        enable_memory;
        disable_stack;
        disable_storage;
        with_logs;
        only_top_call;
      })
    (obj8
       (dft "enableReturnData" bool default_tracer_config.enable_return_data)
       (dft "enableMemory" bool default_tracer_config.enable_memory)
       (dft "disableStack" bool default_tracer_config.disable_stack)
       (dft "disableStorage" bool default_tracer_config.disable_storage)
       (dft "withLog" bool default_tracer_config.with_logs)
       (dft "onlyTopCall" bool default_tracer_config.only_top_call)
       (dft "debug" bool false)
       (dft "limit" int31 5))

type tracer_kind = StructLogger | CallTracer

let tracer_version_activation = function StructLogger -> 14 | CallTracer -> 16

(* TODO: (#7212) make it return an error so that we can return an understadable
   error to the user. *)
let tracer_kind_encoding =
  Data_encoding.string_enum
    [("structLogger", StructLogger); ("callTracer", CallTracer)]

type config = {
  tracer : tracer_kind;
  tracer_config : tracer_config;
  timeout : Time.System.Span.t;
  reexec : int64;
}

(* See debug_traceTransaction specification:
   https://geth.ethereum.org/docs/interacting-with-geth/rpc/ns-debug#debugtracetransaction.

   It ignores durations below milliseconds.
*)
let timeout_parser duration =
  let regex = Re.Perl.re {|(?:(\d+)(h|ms|s|m))|} |> Re.compile in
  let groups = Re.all regex duration in
  let to_seconds kind value =
    match kind with
    (* Hours *)
    | "h" -> value *. 3600.
    (* Minutes *)
    | "m" -> value *. 60.
    (* Seconds *)
    | "s" -> value
    (* Milliseconds *)
    | "ms" -> value /. 1000.
    (* There shouldn't be any other group. *)
    | _ -> 0.
  in
  let extract g =
    match
      ( Re.Group.get_opt g 2,
        Option.bind (Re.Group.get_opt g 1) float_of_string_opt )
    with
    | Some kind, Some value -> Some (to_seconds kind value)
    | _, _ -> None
  in
  List.fold_left
    (fun seconds group ->
      match extract group with None -> seconds | Some s -> s +. seconds)
    0.
    groups

let timeout_encoding =
  let open Data_encoding in
  conv
    (fun timeout -> Format.asprintf "%a" Time.System.Span.pp_hum timeout)
    (fun timeout_str ->
      let seconds = timeout_parser timeout_str in
      (* It's okay to raise an exception, it will simply reject the RPC, not
         crash the node. *)
      Time.System.Span.of_seconds_exn seconds)
    string

(* Default config is derived from the specification of the RPC:
   https://geth.ethereum.org/docs/interacting-with-geth/rpc/ns-debug#debugtracetransaction. *)
let default_config =
  {
    tracer = StructLogger;
    tracer_config = default_tracer_config;
    timeout = Time.System.Span.of_seconds_exn 5.;
    reexec = 128L;
  }

(* Technically, the tracer_config is specific to `structLogger`. As we only
   support this one for now, the encoding assumes they can be used. However if
   we need to add a new tracer, we might want to make it more constrained to
   structLogger, or simply ignore it. *)
let config_encoding =
  let open Data_encoding in
  conv
    (fun {tracer; tracer_config; timeout; reexec} ->
      (((tracer, timeout, reexec), tracer_config), ()))
    (fun (((tracer, timeout, reexec), tracer_config), ()) ->
      {tracer; tracer_config; timeout; reexec})
    (merge_objs
       (merge_objs
          (obj3
             (dft "tracer" tracer_kind_encoding default_config.tracer)
             (dft "timeout" timeout_encoding default_config.timeout)
             (dft "reexec" int64 default_config.reexec))
          tracer_config_encoding)
       unit)

type input = Ethereum_types.hash * config

let config_to_string config =
  Data_encoding.Json.to_string
    (Data_encoding.Json.construct config_encoding config)

type call_input =
  (Ethereum_types.call * Ethereum_types.Block_parameter.extended) * config

type block_input = Ethereum_types.Block_parameter.t * config

let input_encoding =
  Helpers.encoding_with_optional_last_param
    Ethereum_types.hash_encoding
    config_encoding
    default_config

let call_input_encoding =
  Helpers.encoding_with_optional_last_param
    (Data_encoding.tup2
       Ethereum_types.call_encoding
       Ethereum_types.Block_parameter.extended_encoding)
    config_encoding
    default_config

let block_input_encoding =
  Helpers.encoding_with_optional_last_param
    Ethereum_types.Block_parameter.encoding
    config_encoding
    default_config

let struct_logger_input_rlp_encoder ?hash config =
  let open Rlp in
  let hash =
    match hash with
    | Some hash -> Value (Ethereum_types.hash_to_bytes hash |> Bytes.of_string)
    | None -> Value Bytes.empty
  in
  let return_data =
    Ethereum_types.bool_to_rlp_bytes config.tracer_config.enable_return_data
  in
  let memory =
    Ethereum_types.bool_to_rlp_bytes config.tracer_config.enable_memory
  in
  let stack =
    Ethereum_types.bool_to_rlp_bytes config.tracer_config.disable_stack
  in
  let storage =
    Ethereum_types.bool_to_rlp_bytes config.tracer_config.disable_storage
  in
  List [hash; memory; return_data; stack; storage] |> encode |> Bytes.to_string

let call_tracer_input_rlp_encoder ?hash config =
  let open Rlp in
  let hash =
    match hash with
    | Some hash -> Value (Ethereum_types.hash_to_bytes hash |> Bytes.of_string)
    | None -> Value Bytes.empty
  in
  let with_logs =
    Ethereum_types.bool_to_rlp_bytes config.tracer_config.with_logs
  in
  let only_top_call =
    Ethereum_types.bool_to_rlp_bytes config.tracer_config.only_top_call
  in
  List [hash; only_top_call; with_logs]
  |> encode
  |> Bytes.cat (Bytes.make 1 tracer_input_prefix_calltracer)
  |> Bytes.to_string

let input_rlp_encoder ?hash config =
  match config.tracer with
  | StructLogger -> struct_logger_input_rlp_encoder ?hash config
  | CallTracer -> call_tracer_input_rlp_encoder ?hash config

module Opcode = struct
  (* These two pattern matchings are generated
     https://ethereum.org/en/developers/docs/evm/opcodes/, with a combination of
     macros. *)

  type t = Char.t

  let opcode_to_string = function
    | '\x00' -> "STOP"
    | '\x01' -> "ADD"
    | '\x02' -> "MUL"
    | '\x03' -> "SUB"
    | '\x04' -> "DIV"
    | '\x05' -> "SDIV"
    | '\x06' -> "MOD"
    | '\x07' -> "SMOD"
    | '\x08' -> "ADDMOD"
    | '\x09' -> "MULMOD"
    | '\x0A' -> "EXP"
    | '\x0B' -> "SIGNEXTEND"
    | '\x0C' .. '\x0F' -> "invalid"
    | '\x10' -> "LT"
    | '\x11' -> "GT"
    | '\x12' -> "SLT"
    | '\x13' -> "SGT"
    | '\x14' -> "EQ"
    | '\x15' -> "ISZERO"
    | '\x16' -> "AND"
    | '\x17' -> "OR"
    | '\x18' -> "XOR"
    | '\x19' -> "NOT"
    | '\x1A' -> "BYTE"
    | '\x1B' -> "SHL"
    | '\x1C' -> "SHR"
    | '\x1D' -> "SAR"
    | '\x1E' .. '\x1F' -> "invalid"
    | '\x20' -> "KECCAK256"
    | '\x21' .. '\x2F' -> "invalid"
    | '\x30' -> "ADDRESS"
    | '\x31' -> "BALANCE"
    | '\x32' -> "ORIGIN"
    | '\x33' -> "CALLER"
    | '\x34' -> "CALLVALUE"
    | '\x35' -> "CALLDATALOAD"
    | '\x36' -> "CALLDATASIZE"
    | '\x37' -> "CALLDATACOPY"
    | '\x38' -> "CODESIZE"
    | '\x39' -> "CODECOPY"
    | '\x3A' -> "GASPRICE"
    | '\x3B' -> "EXTCODESIZE"
    | '\x3C' -> "EXTCODECOPY"
    | '\x3D' -> "RETURNDATASIZE"
    | '\x3E' -> "RETURNDATACOPY"
    | '\x3F' -> "EXTCODEHASH"
    | '\x40' -> "BLOCKHASH"
    | '\x41' -> "COINBASE"
    | '\x42' -> "TIMESTAMP"
    | '\x43' -> "NUMBER"
    | '\x44' -> "PREVRANDAO"
    | '\x45' -> "GASLIMIT"
    | '\x46' -> "CHAINID"
    | '\x47' -> "SELFBALANCE"
    | '\x48' -> "BASEFEE"
    | '\x49' -> "BLOBHASH"
    | '\x4A' -> "BLOBBASEFEE"
    | '\x4B' .. '\x4F' -> "invalid"
    | '\x50' -> "POP"
    | '\x51' -> "MLOAD"
    | '\x52' -> "MSTORE"
    | '\x53' -> "MSTORE8"
    | '\x54' -> "SLOAD"
    | '\x55' -> "SSTORE"
    | '\x56' -> "JUMP"
    | '\x57' -> "JUMPI"
    | '\x58' -> "PC"
    | '\x59' -> "MSIZE"
    | '\x5A' -> "GAS"
    | '\x5B' -> "JUMPDEST"
    | '\x5C' -> "TLOAD"
    | '\x5D' -> "TSTORE"
    | '\x5E' -> "MCOPY"
    | '\x5F' -> "PUSH0"
    | '\x60' -> "PUSH1"
    | '\x61' -> "PUSH2"
    | '\x62' -> "PUSH3"
    | '\x63' -> "PUSH4"
    | '\x64' -> "PUSH5"
    | '\x65' -> "PUSH6"
    | '\x66' -> "PUSH7"
    | '\x67' -> "PUSH8"
    | '\x68' -> "PUSH9"
    | '\x69' -> "PUSH10"
    | '\x6A' -> "PUSH11"
    | '\x6B' -> "PUSH12"
    | '\x6C' -> "PUSH13"
    | '\x6D' -> "PUSH14"
    | '\x6E' -> "PUSH15"
    | '\x6F' -> "PUSH16"
    | '\x70' -> "PUSH17"
    | '\x71' -> "PUSH18"
    | '\x72' -> "PUSH19"
    | '\x73' -> "PUSH20"
    | '\x74' -> "PUSH21"
    | '\x75' -> "PUSH22"
    | '\x76' -> "PUSH23"
    | '\x77' -> "PUSH24"
    | '\x78' -> "PUSH25"
    | '\x79' -> "PUSH26"
    | '\x7A' -> "PUSH27"
    | '\x7B' -> "PUSH28"
    | '\x7C' -> "PUSH29"
    | '\x7D' -> "PUSH30"
    | '\x7E' -> "PUSH31"
    | '\x7F' -> "PUSH32"
    | '\x80' -> "DUP1"
    | '\x81' -> "DUP2"
    | '\x82' -> "DUP3"
    | '\x83' -> "DUP4"
    | '\x84' -> "DUP5"
    | '\x85' -> "DUP6"
    | '\x86' -> "DUP7"
    | '\x87' -> "DUP8"
    | '\x88' -> "DUP9"
    | '\x89' -> "DUP10"
    | '\x8A' -> "DUP11"
    | '\x8B' -> "DUP12"
    | '\x8C' -> "DUP13"
    | '\x8D' -> "DUP14"
    | '\x8E' -> "DUP15"
    | '\x8F' -> "DUP16"
    | '\x90' -> "SWAP1"
    | '\x91' -> "SWAP2"
    | '\x92' -> "SWAP3"
    | '\x93' -> "SWAP4"
    | '\x94' -> "SWAP5"
    | '\x95' -> "SWAP6"
    | '\x96' -> "SWAP7"
    | '\x97' -> "SWAP8"
    | '\x98' -> "SWAP9"
    | '\x99' -> "SWAP10"
    | '\x9A' -> "SWAP11"
    | '\x9B' -> "SWAP12"
    | '\x9C' -> "SWAP13"
    | '\x9D' -> "SWAP14"
    | '\x9E' -> "SWAP15"
    | '\x9F' -> "SWAP16"
    | '\xA0' -> "LOG0"
    | '\xA1' -> "LOG1"
    | '\xA2' -> "LOG2"
    | '\xA3' -> "LOG3"
    | '\xA4' -> "LOG4"
    | '\xA5' .. '\xEF' -> "invalid"
    | '\xF0' -> "CREATE"
    | '\xF1' -> "CALL"
    | '\xF2' -> "CALLCODE"
    | '\xF3' -> "RETURN"
    | '\xF4' -> "DELEGATECALL"
    | '\xF5' -> "CREATE2"
    | '\xF6' .. '\xF9' -> "invalid"
    | '\xFA' -> "STATICCALL"
    | '\xFB' .. '\xFC' -> "invalid"
    | '\xFD' -> "REVERT"
    | '\xFE' ->
        "INVALID"
        (* This is the "official" INVALID opcode, contrary to
           the others that actually doesn't exist. *)
    | '\xFF' -> "SELFDESTRUCT"

  let string_to_opcode = function
    | "STOP" -> '\x00'
    | "ADD" -> '\x01'
    | "MUL" -> '\x02'
    | "SUB" -> '\x03'
    | "DIV" -> '\x04'
    | "SDIV" -> '\x05'
    | "MOD" -> '\x06'
    | "SMOD" -> '\x07'
    | "ADDMOD" -> '\x08'
    | "MULMOD" -> '\x09'
    | "EXP" -> '\x0A'
    | "SIGNEXTEND" -> '\x0B'
    | "LT" -> '\x10'
    | "GT" -> '\x11'
    | "SLT" -> '\x12'
    | "SGT" -> '\x13'
    | "EQ" -> '\x14'
    | "ISZERO" -> '\x15'
    | "AND" -> '\x16'
    | "OR" -> '\x17'
    | "XOR" -> '\x18'
    | "NOT" -> '\x19'
    | "BYTE" -> '\x1A'
    | "SHL" -> '\x1B'
    | "SHR" -> '\x1C'
    | "SAR" -> '\x1D'
    | "KECCAK256" -> '\x20'
    | "ADDRESS" -> '\x30'
    | "BALANCE" -> '\x31'
    | "ORIGIN" -> '\x32'
    | "CALLER" -> '\x33'
    | "CALLVALUE" -> '\x34'
    | "CALLDATALOAD" -> '\x35'
    | "CALLDATASIZE" -> '\x36'
    | "CALLDATACOPY" -> '\x37'
    | "CODESIZE" -> '\x38'
    | "CODECOPY" -> '\x39'
    | "GASPRICE" -> '\x3A'
    | "EXTCODESIZE" -> '\x3B'
    | "EXTCODECOPY" -> '\x3C'
    | "RETURNDATASIZE" -> '\x3D'
    | "RETURNDATACOPY" -> '\x3E'
    | "EXTCODEHASH" -> '\x3F'
    | "BLOCKHASH" -> '\x40'
    | "COINBASE" -> '\x41'
    | "TIMESTAMP" -> '\x42'
    | "NUMBER" -> '\x43'
    | "PREVRANDAO" -> '\x44'
    | "GASLIMIT" -> '\x45'
    | "CHAINID" -> '\x46'
    | "SELFBALANCE" -> '\x47'
    | "BASEFEE" -> '\x48'
    | "BLOBHASH" -> '\x49'
    | "BLOBBASEFEE" -> '\x4A'
    | "POP" -> '\x50'
    | "MLOAD" -> '\x51'
    | "MSTORE" -> '\x52'
    | "MSTORE8" -> '\x53'
    | "SLOAD" -> '\x54'
    | "SSTORE" -> '\x55'
    | "JUMP" -> '\x56'
    | "JUMPI" -> '\x57'
    | "PC" -> '\x58'
    | "MSIZE" -> '\x59'
    | "GAS" -> '\x5A'
    | "JUMPDEST" -> '\x5B'
    | "TLOAD" -> '\x5C'
    | "TSTORE" -> '\x5D'
    | "MCOPY" -> '\x5E'
    | "PUSH0" -> '\x5F'
    | "PUSH1" -> '\x60'
    | "PUSH2" -> '\x61'
    | "PUSH3" -> '\x62'
    | "PUSH4" -> '\x63'
    | "PUSH5" -> '\x64'
    | "PUSH6" -> '\x65'
    | "PUSH7" -> '\x66'
    | "PUSH8" -> '\x67'
    | "PUSH9" -> '\x68'
    | "PUSH10" -> '\x69'
    | "PUSH11" -> '\x6A'
    | "PUSH12" -> '\x6B'
    | "PUSH13" -> '\x6C'
    | "PUSH14" -> '\x6D'
    | "PUSH15" -> '\x6E'
    | "PUSH16" -> '\x6F'
    | "PUSH17" -> '\x70'
    | "PUSH18" -> '\x71'
    | "PUSH19" -> '\x72'
    | "PUSH20" -> '\x73'
    | "PUSH21" -> '\x74'
    | "PUSH22" -> '\x75'
    | "PUSH23" -> '\x76'
    | "PUSH24" -> '\x77'
    | "PUSH25" -> '\x78'
    | "PUSH26" -> '\x79'
    | "PUSH27" -> '\x7A'
    | "PUSH28" -> '\x7B'
    | "PUSH29" -> '\x7C'
    | "PUSH30" -> '\x7D'
    | "PUSH31" -> '\x7E'
    | "PUSH32" -> '\x7F'
    | "DUP1" -> '\x80'
    | "DUP2" -> '\x81'
    | "DUP3" -> '\x82'
    | "DUP4" -> '\x83'
    | "DUP5" -> '\x84'
    | "DUP6" -> '\x85'
    | "DUP7" -> '\x86'
    | "DUP8" -> '\x87'
    | "DUP9" -> '\x88'
    | "DUP10" -> '\x89'
    | "DUP11" -> '\x8A'
    | "DUP12" -> '\x8B'
    | "DUP13" -> '\x8C'
    | "DUP14" -> '\x8D'
    | "DUP15" -> '\x8E'
    | "DUP16" -> '\x8F'
    | "SWAP1" -> '\x90'
    | "SWAP2" -> '\x91'
    | "SWAP3" -> '\x92'
    | "SWAP4" -> '\x93'
    | "SWAP5" -> '\x94'
    | "SWAP6" -> '\x95'
    | "SWAP7" -> '\x96'
    | "SWAP8" -> '\x97'
    | "SWAP9" -> '\x98'
    | "SWAP10" -> '\x99'
    | "SWAP11" -> '\x9A'
    | "SWAP12" -> '\x9B'
    | "SWAP13" -> '\x9C'
    | "SWAP14" -> '\x9D'
    | "SWAP15" -> '\x9E'
    | "SWAP16" -> '\x9F'
    | "LOG0" -> '\xA0'
    | "LOG1" -> '\xA1'
    | "LOG2" -> '\xA2'
    | "LOG3" -> '\xA3'
    | "LOG4" -> '\xA4'
    | "CREATE" -> '\xF0'
    | "CALL" -> '\xF1'
    | "CALLCODE" -> '\xF2'
    | "RETURN" -> '\xF3'
    | "DELEGATECALL" -> '\xF4'
    | "CREATE2" -> '\xF5'
    | "STATICCALL" -> '\xFA'
    | "REVERT" -> '\xFD'
    | "INVALID" -> '\xFE'
    | "SELFDESTRUCT" -> '\xFF'
    | opcode -> Stdlib.failwith (Format.sprintf "Invalid opcode %s" opcode)

  let encoding =
    Data_encoding.conv opcode_to_string string_to_opcode Data_encoding.string
end

(* Serves only for encoding numeric values in JSON that are up to 2^53. *)
type uint53 = Z.t

let uint53_encoding =
  let open Data_encoding in
  let uint53_to_json i =
    (* See {!Json_data_encoding.int53} *)
    if i < Z.shift_left Z.one 53 then `Float (Z.to_float i)
    else Stdlib.failwith "JSON cannot accept integers more than 2^53"
  in
  let json_to_uint53 = function
    | `Float i -> Z.of_float i
    | _ -> Stdlib.failwith "Invalid representation for uint53"
  in
  let json_encoding = conv uint53_to_json json_to_uint53 json in
  splitted ~json:json_encoding ~binary:z

let uint_as_hex_encoding =
  let open Data_encoding in
  let int_to_json i =
    let hex = Z.format "%02x" i in
    let padded_hex = if String.length hex mod 2 = 1 then "0" ^ hex else hex in
    `String ("0x" ^ padded_hex)
  in
  let json_to_int = function
    | `String s -> Z.of_int @@ int_of_string s
    | _ -> Stdlib.failwith "expected hex encoded int"
  in
  let json_encoding = conv int_to_json json_to_int json in
  splitted ~json:json_encoding ~binary:z

module StructLogger = struct
  type opcode_log = {
    pc : uint53;
    op : Opcode.t;
    gas : uint53;
    gas_cost : uint53;
    memory : Ethereum_types.hex list option;
    mem_size : int32 option;
    stack : Ethereum_types.hex list option;
    return_data : Ethereum_types.hex option;
    storage : (Ethereum_types.hex * Ethereum_types.hex) list option;
    depth : uint53;
    refund : uint53;
    error : string option;
  }

  let decode_opcode op =
    let open Result_syntax in
    if Bytes.length op > 1 then
      tzfail
        (error_of_fmt "Invalid opcode encoding: %a" Hex.pp (Hex.of_bytes op))
    else if Bytes.length op = 0 then return '\x00'
    else return (Bytes.get op 0)

  let opcode_rlp_decoder bytes =
    let open Result_syntax in
    let open Ethereum_types in
    let* rlp = Rlp.decode bytes in
    match rlp with
    | Rlp.List
        [
          pc;
          op;
          gas;
          gas_cost;
          depth;
          error;
          stack;
          return_data;
          raw_memory;
          storage;
        ] ->
        let* pc = From_rlp.decode_z pc in
        let* op = Rlp.decode_value decode_opcode op in
        let* gas = From_rlp.decode_z gas in
        let* gas_cost = From_rlp.decode_z gas_cost in
        let* depth = From_rlp.decode_z depth in
        let* error = Rlp.decode_option From_rlp.decode_string error in
        let* return_data = Rlp.decode_option From_rlp.decode_hex return_data in
        let* stack =
          Rlp.decode_option (Rlp.decode_list From_rlp.decode_hex) stack
        in
        let* raw_memory = Rlp.decode_option Rlp.decode_as_bytes raw_memory in
        let mem_size =
          Option.map (fun m -> Bytes.length m |> Int32.of_int) raw_memory
        in
        let* memory =
          Option.map_e
            (fun memory ->
              let* chunks = TzString.chunk_bytes 32 memory in
              return @@ List.map hex_encode_string chunks)
            raw_memory
        in
        let* storage =
          let parse_storage_index = function
            | Rlp.List [Value _; Value index; Value value] ->
                Some (hex_of_bytes index, hex_of_bytes value)
            | _ -> None
          in
          Rlp.decode_option (Rlp.filter_decode_list parse_storage_index) storage
        in
        return
          {
            pc;
            op;
            gas;
            gas_cost;
            memory;
            mem_size;
            stack;
            return_data;
            storage;
            depth;
            refund = Z.zero;
            error;
          }
    | _ ->
        tzfail (error_of_fmt "Invalid rlp encoding for opcode: %a" Rlp.pp rlp)

  let opcode_encoding =
    let open Data_encoding in
    conv
      (fun {
             pc;
             op;
             gas;
             gas_cost;
             memory;
             mem_size;
             stack;
             return_data;
             storage;
             depth;
             refund;
             error;
           }
         ->
        ( ( pc,
            op,
            gas,
            gas_cost,
            memory,
            mem_size,
            stack,
            return_data,
            storage,
            depth ),
          (refund, error) ))
      (fun ( ( pc,
               op,
               gas,
               gas_cost,
               memory,
               mem_size,
               stack,
               return_data,
               storage,
               depth ),
             (refund, error) )
         ->
        {
          pc;
          op;
          gas;
          gas_cost;
          memory;
          mem_size;
          stack;
          return_data;
          storage;
          depth;
          refund;
          error;
        })
      (merge_objs
         (obj10
            (req "pc" uint53_encoding)
            (req "op" Opcode.encoding)
            (req "gas" uint53_encoding)
            (req "gasCost" uint53_encoding)
            (req "memory" (option (list Ethereum_types.hex_encoding_no0x)))
            (req "memSize" (option int32))
            (req "stack" (option (list Ethereum_types.hex_encoding_no0x)))
            (req "returnData" (option Ethereum_types.hex_encoding_no0x))
            (req
               "storage"
               (option
                  (list
                     (tup2
                        Ethereum_types.hex_encoding_no0x
                        Ethereum_types.hex_encoding_no0x))))
            (req "depth" uint53_encoding))
         (obj2 (req "refund" uint53_encoding) (req "error" (option string))))

  type output = {
    gas : int64;
    failed : bool;
    return_value : Ethereum_types.hash;
    struct_logs : opcode_log list;
  }

  let output_encoding =
    let open Data_encoding in
    conv
      (fun {gas; failed; return_value; struct_logs} ->
        (gas, failed, return_value, struct_logs))
      (fun (gas, failed, return_value, struct_logs) ->
        {gas; failed; return_value; struct_logs})
      (obj4
         (req "gas" int64)
         (req "failed" bool)
         (req "returnValue" Ethereum_types.hash_encoding)
         (req "structLogs" (list opcode_encoding)))

  let output_binary_decoder ~gas ~failed ~return_value ~struct_logs =
    let open Result_syntax in
    let gas =
      Ethereum_types.decode_number_le gas |> fun (Ethereum_types.Qty z) ->
      Z.to_int64 z
    in
    let failed =
      if Bytes.length failed = 0 then false else Bytes.get failed 0 = '\x01'
    in
    let return_value =
      let (`Hex hex_value) = Hex.of_bytes return_value in
      Ethereum_types.hash_of_string hex_value
    in
    let* struct_logs = List.map_e opcode_rlp_decoder struct_logs in
    return {gas; failed; return_value; struct_logs}
end

module CallTracer = struct
  type logs = {
    address : Ethereum_types.address;
    topics : Ethereum_types.hex list;
    data : Ethereum_types.hex;
  }

  type output = {
    calls : output list;
    type_ : string;
    from : Ethereum_types.address;
    to_ : Ethereum_types.address option;
    value : uint53;
    gas : uint53 option;
    gas_used : uint53;
    input : Ethereum_types.hex;
    output : Ethereum_types.hex option;
    error : string option;
    revert_reason : string option;
    logs : logs list option;
  }

  let solidity_revert_selector =
    let (Hex error) = Ethereum_types.hex_of_utf8 "Error(string)" in
    let selector = Helpers.keccak256 (`Hex error) in
    Bytes.sub selector 0 4

  let logs_encoding =
    let open Data_encoding in
    conv
      (fun {address; topics; data} -> (address, topics, data))
      (fun (address, topics, data) -> {address; topics; data})
      (obj3
         (req "address" Ethereum_types.address_encoding)
         (req "topics" (list Ethereum_types.hex_encoding))
         (req "data" Ethereum_types.hex_encoding))

  (* Bytes.sub, but returns an error. *)
  let sub_bytes ?error bytes offset length =
    let open Result_syntax in
    if Bytes.length bytes < offset + length then
      tzfail (Trace_decoding_error (Option.value error ~default:"Bytes.sub"))
    else return (Bytes.sub bytes offset length)

  let try_to_string bytes =
    if Bytes.is_valid_utf_8 bytes then Bytes.unsafe_to_string bytes
    else Hex.(of_bytes bytes |> show)

  (* See this documentation about how to encode dynamic types (string, bytes, ...):
     https://docs.soliditylang.org/en/latest/abi-spec.html#use-of-dynamic-types *)
  let decode_ethereum_string data =
    let open Result_syntax in
    let decode_word_to_int ?error bytes offset length =
      let* word = sub_bytes ?error bytes offset length in
      let (Qty position) = Ethereum_types.decode_number_be word in
      return @@ Z.to_int position
    in
    let* position =
      decode_word_to_int ~error:"decode string size position" data 0 32
    in
    let* size =
      decode_word_to_int ~error:"decode string size" data position 32
    in
    let* str = sub_bytes ~error:"extract string" data (position + 32) size in
    return @@ try_to_string str

  let revert_reason_bytes_decoding output_b =
    let open Result_syntax in
    if Bytes.length output_b < 4 then
      (* data is too short to contain the revert reason, we don't know how to
         recover it. *)
      return (Some "Reverted", None)
    else
      let* selector_output = sub_bytes ~error:"decode selector" output_b 0 4 in
      if solidity_revert_selector = selector_output then
        (* This is a solidity revert, attempt to decode the output as
           a string to put it in revert_reason *)
        match
          decode_ethereum_string
            (Bytes.sub output_b 4 (Bytes.length output_b - 4))
        with
        | Ok revert_reason ->
            (* When it's a solidity revert, the common error is
               execution reverted *)
            return (Some "execution reverted", Some revert_reason)
        | Error _ ->
            (* Selector matched but data is malformed/truncated,
               fall back to raw hex representation *)
            return (Some "execution reverted", Some (try_to_string output_b))
      else
        (* data does not correspond to a solidity revert, we don't know how to
           recover the revert reason. *)
        return (Some (try_to_string output_b), None)

  let revert_reason_decoding output =
    let output_b = Ethereum_types.hex_to_real_bytes output in
    revert_reason_bytes_decoding output_b

  let output_encoding =
    let open Data_encoding in
    mu "callTracerOutput" (fun enc ->
        conv
          (fun {
                 calls;
                 type_;
                 from;
                 to_;
                 value;
                 gas;
                 gas_used;
                 input;
                 output;
                 error;
                 revert_reason;
                 logs;
               }
             ->
            ( (type_, from, to_, value, gas, gas_used),
              (input, output, error, revert_reason, logs, calls) ))
          (fun ( (type_, from, to_, value, gas, gas_used),
                 (input, output, error, revert_reason, logs, calls) )
             ->
            {
              calls;
              type_;
              from;
              to_;
              value;
              gas;
              gas_used;
              input;
              output;
              error;
              revert_reason;
              logs;
            })
          (merge_objs
             (obj6
                (req "type" string)
                (req "from" Ethereum_types.address_encoding)
                (opt "to" Ethereum_types.address_encoding)
                (req "value" uint_as_hex_encoding)
                (opt "gas" uint_as_hex_encoding)
                (req "gasUsed" uint_as_hex_encoding))
             (obj6
                (req "input" Ethereum_types.hex_encoding)
                (opt "output" Ethereum_types.hex_encoding)
                (opt "error" string)
                (opt "revertReason" string)
                (opt "logs" (list logs_encoding))
                (req "calls" (list enc)))))

  let decode_logs item =
    let open Result_syntax in
    let open Ethereum_types in
    match item with
    | Rlp.List [address; topics; data] ->
        let* address = From_rlp.decode_address address in
        let* topics = Rlp.decode_list From_rlp.decode_hex topics in
        let* data = From_rlp.decode_hex data in
        return {address; topics; data}
    | _ -> tzfail (error_of_fmt "Invalid RLP encoding for the logs")

  let decode_call bytes =
    let open Result_syntax in
    let open Ethereum_types in
    let* rlp = Rlp.decode bytes in
    match rlp with
    | Rlp.List
        [
          type_;
          from;
          to_;
          value;
          gas;
          gas_used;
          input;
          output;
          error;
          logs;
          depth;
        ] ->
        let* type_ = From_rlp.decode_string type_ in
        let* from = From_rlp.decode_address from in
        let* to_ = Rlp.decode_option From_rlp.decode_address to_ in
        let* value = From_rlp.decode_z value in
        let* gas = Rlp.decode_option From_rlp.decode_z gas in
        let* gas_used = From_rlp.decode_z gas_used in
        let* input = From_rlp.decode_hex input in
        let* output = Rlp.decode_option From_rlp.decode_hex output in
        let* error = Rlp.decode_option From_rlp.decode_string error in
        let* error, revert_reason =
          (* Best guess to recover revert reason *)
          match (output, error) with
          | Some output, Some err when err = "Reverted" ->
              revert_reason_decoding output
          | _, Some err when not (String.is_valid_utf_8 err) ->
              (* We probably are propagating the error found in another call *)
              revert_reason_bytes_decoding (Bytes.unsafe_of_string err)
          | _ -> return (error, None)
        in
        let* logs = Rlp.decode_option (Rlp.decode_list decode_logs) logs in
        let* depth = From_rlp.decode_int depth in
        return
          ( {
              type_;
              from;
              to_;
              value;
              gas;
              gas_used;
              input;
              output;
              error;
              revert_reason;
              logs;
              calls = [];
            },
            depth )
    | List l ->
        let (`Hex rlp) = Hex.of_bytes bytes in
        tzfail
          (error_of_fmt
             "Invalid RLP encoding for a call, list with %n items. %s"
             (List.length l)
             rlp)
    | Value v ->
        let (`Hex rlp) = Hex.of_bytes bytes in
        tzfail
          (error_of_fmt
             "Invalid RLP encoding for a call, value \"%s\" %s"
             (Bytes.to_string v)
             rlp)

  let to_string call =
    Data_encoding.Json.to_string
      (Data_encoding.Json.construct output_encoding call)
end

type output =
  | StructLoggerOutput of StructLogger.output
  | CallTracerOutput of CallTracer.output

let output_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"structLogger"
        (Tag 0)
        StructLogger.output_encoding
        (function StructLoggerOutput output -> Some output | _ -> None)
        (fun output -> StructLoggerOutput output);
      case
        ~title:"callTracer"
        (Tag 1)
        CallTracer.output_encoding
        (function CallTracerOutput output -> Some output | _ -> None)
        (fun output -> CallTracerOutput output);
    ]

type block_output = (Ethereum_types.hash * output) list

let block_output_encoding =
  Data_encoding.list
    Data_encoding.(
      obj2
        (req "txHash" Ethereum_types.hash_encoding)
        (req "result" output_encoding))
