(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error +=
  | Tx_rollup_not_originated_in_the_given_block of
      Protocol.Alpha_context.Tx_rollup.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.not_originated_in_the_given_block"
    ~title:"transaction rollup not originated within the given block."
    ~description:
      "The transaction rollup was not originated inside the given block."
    ~pp:(fun ppf rollup_id ->
      Format.fprintf
        ppf
        "The transaction rollup %a was not originated inside the given block."
        Protocol.Alpha_context.Tx_rollup.pp
        rollup_id)
    `Permanent
    Data_encoding.(
      obj1 (req "rollup_id" Protocol.Alpha_context.Tx_rollup.encoding))
    (function
      | Tx_rollup_not_originated_in_the_given_block rollup_id -> Some rollup_id
      | _ -> None)
    (fun rollup_id -> Tx_rollup_not_originated_in_the_given_block rollup_id)

type error += Tx_rollup_block_predecessor_not_processed of Block_hash.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.block_predecessor_not_processed"
    ~title:"The predecessor of the block was not processed by the node"
    ~description:"The predecessor of the block was not processed by the node."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "The predecessor (%a) of the block was not processed by the node but \
         the node still tried to process (%a). This should not have happened \
         and it is definitely a bug. Please fill an issue at \
         <https://gitlab.com/tezos/tezos/-/issues>."
        Block_hash.pp
        hash
        Block_hash.pp
        hash)
    `Permanent
    Data_encoding.(obj1 (req "predecessor_hash" Block_hash.encoding))
    (function
      | Tx_rollup_block_predecessor_not_processed hash -> Some hash | _ -> None)
    (fun hash -> Tx_rollup_block_predecessor_not_processed hash)

type error +=
  | Tx_rollup_unable_to_encode_storable_value of string * Data_encoding.Json.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.unable_to_encode_storable_value"
    ~title:"Cannot encode value"
    ~description:
      "Cannot encode value which make it impossible to put it in the storage"
    ~pp:(fun ppf (key, value) ->
      Format.fprintf
        ppf
        "Cannot encode value (%a) which make it impossible to put it in the \
         storage at key: %s"
        Data_encoding.Json.pp
        value
        key)
    `Permanent
    Data_encoding.(
      obj2 (req "key" string) (req "value" Data_encoding.Json.encoding))
    (function
      | Tx_rollup_unable_to_encode_storable_value (key, value) ->
          Some (key, value)
      | _ -> None)
    (fun (key, value) -> Tx_rollup_unable_to_encode_storable_value (key, value))

type error += Tx_rollup_unable_to_decode_stored_value of string * string

let () =
  register_error_kind
    ~id:"tx_rollup.node.unable_to_decode_stored_value"
    ~title:"Cannot decode value"
    ~description:"We could not decode a value present in the storage"
    ~pp:(fun ppf (key, input) ->
      Format.fprintf
        ppf
        "We could not decode a stored value (%s) at key: %s"
        input
        key)
    `Permanent
    Data_encoding.(obj2 (req "key" string) (req "encoded_value" string))
    (function
      | Tx_rollup_unable_to_decode_stored_value (k, v) -> Some (k, v)
      | _ -> None)
    (fun (k, v) -> Tx_rollup_unable_to_decode_stored_value (k, v))

type error += Tx_rollup_irmin_error of string

let () =
  register_error_kind
    ~id:"tx_rollup.node.irmin_error"
    ~title:"Irmin error"
    ~description:"Error on Irmin side"
    ~pp:(fun ppf message ->
      Format.fprintf ppf "Error on Irmin side: %s" message)
    `Permanent
    Data_encoding.(obj1 (req "message" string))
    (function Tx_rollup_irmin_error message -> Some message | _ -> None)
    (fun message -> Tx_rollup_irmin_error message)

type error += Tx_rollup_configuration_file_does_not_exists of string

let () =
  register_error_kind
    ~id:"tx_rollup.node.configuration_file_does_not_exits"
    ~title:"Unable to find configuration file"
    ~description:"The configuration file does not seem to exist"
    ~pp:(fun ppf filepath ->
      Format.fprintf
        ppf
        "The configuration file '%s' does not seem to exist. Try giving \
         another '--data-dir' or running the 'config init on' subcommand"
        filepath)
    `Permanent
    Data_encoding.(obj1 (req "filepath" string))
    (function
      | Tx_rollup_configuration_file_does_not_exists path -> Some path
      | _ -> None)
    (fun path -> Tx_rollup_configuration_file_does_not_exists path)

type error += Tx_rollup_unable_to_write_configuration_file of string

let () =
  register_error_kind
    ~id:"tx_rollup.node.unable_to_write_configuration_file"
    ~title:"Unable to write configuration file"
    ~description:"Unable to write configuration file"
    ~pp:(fun ppf file ->
      Format.fprintf ppf "Unable to write the configuration file %s" file)
    `Permanent
    Data_encoding.(obj1 (req "file" string))
    (function
      | Tx_rollup_unable_to_write_configuration_file path -> Some path
      | _ -> None)
    (fun path -> Tx_rollup_unable_to_write_configuration_file path)

type error +=
  | Tx_rollup_invalid_history_mode of Tezos_shell_services.History_mode.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.invalid_history_mode"
    ~title:"The Tezos node has an invalid history mode"
    ~description:"Tezos node should be in 'archive' or 'full' history mode"
    ~pp:(fun ppf history_mode ->
      let open Tezos_shell_services.History_mode in
      Format.fprintf
        ppf
        "%a and %a are accepted mode. Not %a"
        pp
        Archive
        pp
        (Full None)
        pp
        history_mode)
    `Permanent
    Data_encoding.(
      obj1 (req "history_mode" Tezos_shell_services.History_mode.encoding))
    (function
      | Tx_rollup_invalid_history_mode history_mode -> Some history_mode
      | _ -> None)
    (fun history_mode -> Tx_rollup_invalid_history_mode history_mode)

type error +=
  | Tx_rollup_unsupported_context_version of {
      current : Protocol.Tx_rollup_l2_context_hash.Version.t;
      expected : Protocol.Tx_rollup_l2_context_hash.Version.t;
    }

let () =
  register_error_kind
    ~id:"tx_rollup.node.invalid_context_version"
    ~title:"The Tezos node has an invalid context version"
    ~description:"The Tezos node has an invalid context version"
    ~pp:(fun ppf (current, expected) ->
      Format.fprintf
        ppf
        "Tx rollup node has context version %a but was expected to have \
         version %a."
        Protocol.Tx_rollup_l2_context_hash.Version.pp
        current
        Protocol.Tx_rollup_l2_context_hash.Version.pp
        expected)
    `Permanent
    Data_encoding.(
      obj2
        (req "current" Protocol.Tx_rollup_l2_context_hash.Version.encoding)
        (req "expected" Protocol.Tx_rollup_l2_context_hash.Version.encoding))
    (function
      | Tx_rollup_unsupported_context_version {current; expected} ->
          Some (current, expected)
      | _ -> None)
    (fun (current, expected) ->
      Tx_rollup_unsupported_context_version {current; expected})
