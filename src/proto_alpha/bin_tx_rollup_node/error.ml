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

type error += Tx_rollup_originated_in_fork

let () =
  register_error_kind
    ~id:"tx_rollup.node.originated_in_fork"
    ~title:"transaction rollup was originated in another branch"
    ~description:"The transaction rollup was originated in another branch."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The transaction rollup was originated in another branch.")
    `Permanent
    Data_encoding.(unit)
    (function Tx_rollup_originated_in_fork -> Some () | _ -> None)
    (fun () -> Tx_rollup_originated_in_fork)

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

type error += Tx_rollup_invalid_l2_address of Micheline.canonical_location

let () =
  register_error_kind
    ~id:"tx_rollup.node.invalid_l2_address"
    ~title:"Invalid transaction rollup L2 address"
    ~description:"Not a valid transaction rollup L2 address"
    ~pp:(fun ppf loc ->
      Format.fprintf ppf "Not a valid transaction rollup l2 address at %d" loc)
    `Permanent
    Data_encoding.(obj1 (req "loc" int31))
    (function Tx_rollup_invalid_l2_address loc -> Some loc | _ -> None)
    (fun loc -> Tx_rollup_invalid_l2_address loc)

type error += Tx_rollup_invalid_ticket_amount of Z.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.invalid_ticket_amount"
    ~title:"Invalid transaction rollup ticket amount"
    ~description:"Not a valid transaction rollup ticket amount"
    ~pp:(fun ppf amount ->
      Format.fprintf
        ppf
        "Not a valid transaction rollup ticket amount: %a"
        Z.pp_print
        amount)
    `Permanent
    Data_encoding.(obj1 (req "amount" z))
    (function
      | Tx_rollup_invalid_ticket_amount amount -> Some amount | _ -> None)
    (fun amount -> Tx_rollup_invalid_ticket_amount amount)

type error += Tx_rollup_invalid_deposit

let () =
  let description = "Not a valid transaction rollup deposit" in
  register_error_kind
    ~id:"tx_rollup.node.invalid_deposit"
    ~title:"Invalid transaction rollup Deposit"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    `Permanent
    Data_encoding.empty
    (function Tx_rollup_invalid_deposit -> Some () | _ -> None)
    (fun () -> Tx_rollup_invalid_deposit)

type error +=
  | Tx_rollup_cannot_checkout_context of Protocol.Tx_rollup_l2_context_hash.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.cannot_checkout_context"
    ~title:"Cannot checkout the L2 context"
    ~description:"The rollup node cannot checkout the L2 context."
    ~pp:(fun ppf ctxt ->
      Format.fprintf
        ppf
        "Cannot checkout L2 context %a"
        Protocol.Tx_rollup_l2_context_hash.pp
        ctxt)
    `Permanent
    Data_encoding.(
      obj1 (req "context" Protocol.Tx_rollup_l2_context_hash.encoding))
    (function Tx_rollup_cannot_checkout_context c -> Some c | _ -> None)
    (fun c -> Tx_rollup_cannot_checkout_context c)

type error +=
  | Tx_rollup_no_rollup_origination_on_disk_and_no_rollup_genesis_given

let () =
  let description =
    "No rollup origination on disk and no rollup genesis provided"
  in
  register_error_kind
    ~id:"tx_rollup.node.no_rollup_origination_and_no_rollup_genesis_given"
    ~title:"No rollup origination on disk and none provided"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    `Permanent
    Data_encoding.empty
    (function
      | Tx_rollup_no_rollup_origination_on_disk_and_no_rollup_genesis_given ->
          Some ()
      | _ -> None)
    (fun () ->
      Tx_rollup_no_rollup_origination_on_disk_and_no_rollup_genesis_given)

type error +=
  | Tx_rollup_different_disk_stored_origination_rollup_and_given_rollup_genesis of {
      disk_rollup_origination : Block_hash.t;
      given_rollup_genesis : Block_hash.t;
    }

let () =
  register_error_kind
    ~id:
      "tx_rollup.node.different_disk_stored_origination_rollup_and_given_rollup_genesis"
    ~title:"Rollup origination on disk is different from the one provided"
    ~description:
      "Rollup origination on disk is different from the provided rollup genesis"
    ~pp:(fun ppf (disk_rollup, given_rollup) ->
      Format.fprintf
        ppf
        "Rollup origination on disk (%a) is different from the provided rollup \
         genesis (%a)"
        Block_hash.pp
        disk_rollup
        Block_hash.pp
        given_rollup)
    `Permanent
    Data_encoding.(
      obj2
        (req "disk_rollup" Block_hash.encoding)
        (req "given_rollup" Block_hash.encoding))
    (function
      | Tx_rollup_different_disk_stored_origination_rollup_and_given_rollup_genesis
          {disk_rollup_origination; given_rollup_genesis} ->
          Some (disk_rollup_origination, given_rollup_genesis)
      | _ -> None)
    (fun (disk_rollup_origination, given_rollup_genesis) ->
      Tx_rollup_different_disk_stored_origination_rollup_and_given_rollup_genesis
        {disk_rollup_origination; given_rollup_genesis})

type error += Tx_rollup_no_operation_metadata of Operation_hash.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.no_operation_metadata"
    ~title:"Operation receipt unavailable"
    ~description:"The operation receipt is unavailable."
    ~pp:(fun ppf op ->
      Format.fprintf
        ppf
        "The operation receipt of %a is unavailable. Please make sure that the \
         history mode of the Tezos node you are connecting to matches the \
         requirements."
        Operation_hash.pp
        op)
    `Permanent
    Data_encoding.(obj1 (req "context" Operation_hash.encoding))
    (function Tx_rollup_no_operation_metadata o -> Some o | _ -> None)
    (fun o -> Tx_rollup_no_operation_metadata o)

type error += Tx_rollup_mismatch

let () =
  register_error_kind
    ~id:"tx_rollup.node.different_disk_stored_rollup"
    ~title:"Rollup on disk is different from the one provided"
    ~description:"Rollup on disk is different from the provided rollup"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Rollup origination on disk is different from the provided rollup")
    `Permanent
    Data_encoding.unit
    (function Tx_rollup_mismatch -> Some () | _ -> None)
    (fun () -> Tx_rollup_mismatch)
