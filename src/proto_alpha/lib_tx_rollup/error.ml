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

type error += Tx_rollup_fatal

let () =
  register_error_kind
    ~id:"tx_rollup.node.fatal"
    ~title:"Fatal error in rollup node"
    ~description:
      "The node encountered a fatal error which prevents it from working \
       properly."
    ~pp:(fun ppf () -> Format.fprintf ppf "Fatal error in rollup node.")
    `Permanent
    Data_encoding.unit
    (function Tx_rollup_fatal -> Some () | _ -> None)
    (fun () -> Tx_rollup_fatal)

let trace_fatal p = trace Tx_rollup_fatal p

type error += Tx_rollup_internal of string

let () =
  register_error_kind
    ~id:"tx_rollup.node.internal"
    ~title:"Internal error in rollup node"
    ~description:"Internal error encountered"
    ~pp:(fun ppf loc ->
      Format.fprintf ppf "Internal error in rollup node at %s" loc)
    `Permanent
    Data_encoding.(obj1 (req "loc" string))
    (function Tx_rollup_internal loc -> Some loc | _ -> None)
    (fun loc -> Tx_rollup_internal loc)

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

type error += Tx_rollup_no_rollup_info_on_disk_and_no_rollup_genesis_given

let () =
  let description =
    "No rollup information on disk and no rollup genesis provided"
  in
  register_error_kind
    ~id:"tx_rollup.node.no_rollup_info_and_no_rollup_genesis_given"
    ~title:"No rollup information on disk and none provided"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    `Permanent
    Data_encoding.empty
    (function
      | Tx_rollup_no_rollup_info_on_disk_and_no_rollup_genesis_given -> Some ()
      | _ -> None)
    (fun () -> Tx_rollup_no_rollup_info_on_disk_and_no_rollup_genesis_given)

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

type error += Tx_rollup_cannot_fetch_tezos_block of Block_hash.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.cannot_fetch_tezos_block"
    ~title:"A Tezos block cannot be fetched"
    ~description:"A Tezos block cannot be fetched."
    ~pp:(fun ppf b ->
      Format.fprintf
        ppf
        "The Tezos block %a cannot be fetched from the node."
        Block_hash.pp
        b)
    `Permanent
    Data_encoding.(obj1 (req "block" Block_hash.encoding))
    (function Tx_rollup_cannot_fetch_tezos_block b -> Some b | _ -> None)
    (fun b -> Tx_rollup_cannot_fetch_tezos_block b)

type error += Tx_rollup_tree_not_found

let () =
  register_error_kind
    ~id:"tx_rollup.node.tree_not_found"
    ~title:"Tree not found in context"
    ~description:"The tree is not found in the context."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The tree was not found in the context. The merkle proof associated to \
         a message can not be produced, the rollup can not interpret the \
         message.")
    `Permanent
    Data_encoding.empty
    (function Tx_rollup_tree_not_found -> Some () | _ -> None)
    (fun () -> Tx_rollup_tree_not_found)

type error += Tx_rollup_tree_kinded_key_not_found

let () =
  register_error_kind
    ~id:"tx_rollup.node.tree_kinded_key_not_found"
    ~title:"Kinded key not found in tree"
    ~description:"The kinded key is not found in the tree."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The kinded key was not found in the tree. The merkle proof associated \
         to a message can not be produced, the rollup can not interpret the \
         message.")
    `Permanent
    Data_encoding.empty
    (function Tx_rollup_tree_kinded_key_not_found -> Some () | _ -> None)
    (fun () -> Tx_rollup_tree_kinded_key_not_found)

type error += Tx_rollup_invalid_message_position_in_inbox of int

let () =
  register_error_kind
    ~id:"tx_rollup.node.invalid_message_position"
    ~title:"Message position invalid in the inbox"
    ~description:"The message position is invalid the inbox."
    ~pp:(fun ppf i ->
      Format.fprintf ppf "The message position %d is invalid in the inbox" i)
    `Permanent
    Data_encoding.(obj1 (req "message_position" int31))
    (function
      | Tx_rollup_invalid_message_position_in_inbox i -> Some i | _ -> None)
    (fun i -> Tx_rollup_invalid_message_position_in_inbox i)

type error += No_worker_for_source of Signature.Public_key_hash.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.no_worker_for_source"
    ~title:"No injecting queue for source"
    ~description:
      "An L1 operation could not be queued because its source has no worker."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "No worker for source %a"
        Signature.Public_key_hash.pp
        s)
    `Permanent
    Data_encoding.(obj1 (req "source" Signature.Public_key_hash.encoding))
    (function No_worker_for_source s -> Some s | _ -> None)
    (fun s -> No_worker_for_source s)

type error += No_batcher

let () =
  register_error_kind
    ~id:"tx_rollup.node.no_batcher"
    ~title:"No batcher for this node"
    ~description:"This node does not have a batcher"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "This rollup node does not have batcher.")
    `Permanent
    Data_encoding.unit
    (function No_batcher -> Some () | _ -> None)
    (fun () -> No_batcher)

type error +=
  | Tx_rollup_unknown_ticket of
      Protocol.Tx_rollup_l2_context_sig.Ticket_indexable.either

let () =
  register_error_kind
    ~id:"tx_rollup.node.unknown_ticket"
    ~title:"No ticket registered for indexable ticket hash"
    ~description:"A ticket indexable has not ticket associated in the context."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Unknown ticket for ticket indexable %a"
        Protocol.Tx_rollup_l2_context_sig.Ticket_indexable.pp
        s)
    `Permanent
    Data_encoding.(
      obj1
        (req
           "ticket_index"
           Protocol.Tx_rollup_l2_context_sig.Ticket_indexable.encoding))
    (function Tx_rollup_unknown_ticket t -> Some t | _ -> None)
    (fun t -> Tx_rollup_unknown_ticket t)

type error +=
  | Tx_rollup_no_proto_inbox of
      Protocol.Alpha_context.Tx_rollup_level.t * Block_hash.t

let () =
  register_error_kind
    ~id:"tx_rollup.node.no_proto_inbox"
    ~title:"No inbox on L1 node"
    ~description:"Inbox on L1 node cannot be retrieved."
    ~pp:(fun ppf (l, b) ->
      Format.fprintf
        ppf
        "No inbox on L1 for rollup level %a at block %a"
        Protocol.Alpha_context.Tx_rollup_level.pp
        l
        Block_hash.pp
        b)
    `Permanent
    Data_encoding.(
      obj2
        (req "level" Protocol.Alpha_context.Tx_rollup_level.encoding)
        (req "block" Block_hash.encoding))
    (function Tx_rollup_no_proto_inbox (l, b) -> Some (l, b) | _ -> None)
    (fun (l, b) -> Tx_rollup_no_proto_inbox (l, b))

type error +=
  | Tx_rollup_inbox_mismatch of {
      level : Protocol.Alpha_context.Tx_rollup_level.t;
      reconstructed_inbox : Protocol.Alpha_context.Tx_rollup_inbox.t;
      protocol_inbox : Protocol.Alpha_context.Tx_rollup_inbox.t;
    }

let () =
  register_error_kind
    ~id:"tx_rollup.node.inbox_mismatch"
    ~title:"Inbox mismatch between L1 and L2"
    ~description:
      "Inbox reconstructed on L2 does not match the one stored on the L1 node."
    ~pp:(fun ppf (level, reconstructed_inbox, protocol_inbox) ->
      Format.fprintf
        ppf
        "@[<v 2>Inbox reconstructed for rollup level %a does not match the one \
         stored on the Tezos node.@,\
         @[<hov 2>Reconstructed:@ %a@]@,\
         @[<hov 2>Stored on Tezos:@ %a@]@,\
         @]"
        Protocol.Alpha_context.Tx_rollup_level.pp
        level
        Protocol.Alpha_context.Tx_rollup_inbox.pp
        reconstructed_inbox
        Protocol.Alpha_context.Tx_rollup_inbox.pp
        protocol_inbox)
    `Permanent
    Data_encoding.(
      obj3
        (req "level" Protocol.Alpha_context.Tx_rollup_level.encoding)
        (req
           "reconstructed_inbox"
           Protocol.Alpha_context.Tx_rollup_inbox.encoding)
        (req "protocol_inbox" Protocol.Alpha_context.Tx_rollup_inbox.encoding))
    (function
      | Tx_rollup_inbox_mismatch {level; reconstructed_inbox; protocol_inbox} ->
          Some (level, reconstructed_inbox, protocol_inbox)
      | _ -> None)
    (fun (level, reconstructed_inbox, protocol_inbox) ->
      Tx_rollup_inbox_mismatch {level; reconstructed_inbox; protocol_inbox})
