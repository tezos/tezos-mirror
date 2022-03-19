(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
  | Tx_rollup_already_exists of Tx_rollup_repr.t
  | Tx_rollup_does_not_exist of Tx_rollup_repr.t
  | Submit_batch_burn_exceeded of {burn : Tez_repr.t; limit : Tez_repr.t}
  | Inbox_does_not_exist of Tx_rollup_repr.t * Tx_rollup_level_repr.t
  | Inbox_size_would_exceed_limit of Tx_rollup_repr.t
  | Inbox_count_would_exceed_limit of Tx_rollup_repr.t
  | No_uncommitted_inbox
  | Message_size_exceeds_limit
  | Too_many_inboxes
  | Too_many_finalized_commitments
  | Wrong_batch_count
  | Commitment_too_early of {
      provided : Tx_rollup_level_repr.t;
      expected : Tx_rollup_level_repr.t;
    }
  | Level_already_has_commitment of Tx_rollup_level_repr.t
  | Wrong_inbox_hash
  | Bond_does_not_exist of Signature.public_key_hash
  | Bond_in_use of Signature.public_key_hash
  | No_commitment_to_finalize
  | No_commitment_to_remove
  | Invalid_rejection_level_argument
  | Commitment_does_not_exist of Tx_rollup_level_repr.t
  | Wrong_predecessor_hash of {
      provided : Tx_rollup_commitment_repr.Commitment_hash.t option;
      expected : Tx_rollup_commitment_repr.Commitment_hash.t option;
    }
  | Invalid_proof
  | Internal_error of string
  | Wrong_message_position of {
      level : Tx_rollup_level_repr.t;
      position : int;
      length : int;
    }
  | Wrong_message_path of {expected : Tx_rollup_inbox_repr.Merkle.root}
  | No_finalized_commitment_for_level of {
      level : Tx_rollup_level_repr.t;
      window : (Tx_rollup_level_repr.t * Tx_rollup_level_repr.t) option;
    }
  | Withdraw_invalid_path
  | Withdraw_already_consumed
  | Commitment_bond_negative of int
  | Cannot_reject_level of {
      provided : Tx_rollup_level_repr.t;
      accepted_range : (Tx_rollup_level_repr.t * Tx_rollup_level_repr.t) option;
    }
  | Wrong_rejection_hashes of {
      provided : Tx_rollup_commitment_repr.message_result;
      computed : Tx_rollup_commitment_repr.Message_result_hash.t;
      expected : Tx_rollup_commitment_repr.Message_result_hash.t;
    }
  | Ticket_payload_size_limit_exceeded of {payload_size : int; limit : int}
  | Deposit_wrong_ticketer of Tx_rollup_repr.t
  | Wrong_deposit_parameters

let () =
  let open Data_encoding in
  (* Tx_rollup_submit_batch_burn_exceeded *)
  register_error_kind
    `Temporary
    ~id:"operation.tx_rollup_submit_batch_burn_exceeded"
    ~title:"Submit batch exceeded burn limit"
    ~description:
      "The submit batch would exceed the burn limit, we withdraw the submit."
    ~pp:(fun ppf (burn, limit) ->
      Format.fprintf
        ppf
        "Cannot submit the batch of L2 operations as the cost (%a) would \
         exceed the burn limit (%a)"
        Tez_repr.pp
        burn
        Tez_repr.pp
        limit)
    Data_encoding.(
      obj2 (req "burn" Tez_repr.encoding) (req "limit" Tez_repr.encoding))
    (function
      | Submit_batch_burn_exceeded {burn; limit} -> Some (burn, limit)
      | _ -> None)
    (fun (burn, limit) -> Submit_batch_burn_exceeded {burn; limit}) ;
  (* Tx_rollup_inbox_does_not_exist *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_inbox_does_not_exist"
    ~title:"Missing transaction rollup inbox"
    ~description:"The transaction rollup does not have an inbox at this level"
    ~pp:(fun ppf (addr, level) ->
      Format.fprintf
        ppf
        "Transaction rollup %a does not have an inbox at level %a"
        Tx_rollup_repr.pp
        addr
        Tx_rollup_level_repr.pp
        level)
    (obj2
       (req "tx_rollup_address" Tx_rollup_repr.encoding)
       (req "raw_level" Tx_rollup_level_repr.encoding))
    (function
      | Inbox_does_not_exist (rollup, level) -> Some (rollup, level) | _ -> None)
    (fun (rollup, level) -> Inbox_does_not_exist (rollup, level)) ;
  register_error_kind
    `Temporary
    ~id:"tx_rollup_inbox_size_would_exceed_limit"
    ~title:"Transaction rollup inbox’s size would exceed the limit"
    ~description:
      "Transaction rollup inbox’s size in bytes would exceed the limit"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Adding the submitted message would make the inbox of %a exceed the \
         authorized size in bytes at this level"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "tx_rollup_address" Tx_rollup_repr.encoding))
    (function Inbox_size_would_exceed_limit rollup -> Some rollup | _ -> None)
    (fun rollup -> Inbox_size_would_exceed_limit rollup) ;
  (* Tx_rollup_message_count_would_exceed_limit *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_inbox_count_would_exceed_limit"
    ~title:"Transaction rollup inbox’s message count would exceed the limit"
    ~description:
      "Transaction rollup inbox’s message count would exceed the limit"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Adding the submitted message would make the inbox of %a exceed the \
         authorized message count at this level"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "tx_rollup_address" Tx_rollup_repr.encoding))
    (function
      | Inbox_count_would_exceed_limit rollup -> Some rollup | _ -> None)
    (fun rollup -> Inbox_count_would_exceed_limit rollup) ;
  (* Tx_rollup_message_size_exceed_limit *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_no_uncommitted_inbox"
    ~title:"There is no inbox awaiting a commitment."
    ~description:"There is no inbox awaiting a commitment."
    empty
    (function No_uncommitted_inbox -> Some () | _ -> None)
    (fun () -> No_uncommitted_inbox) ;
  (* Invalid_proof *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_invalid_proof"
    ~title:"The proof submitted for a rejection is invalid"
    ~description:"The proof submitted for a rejection is invalid"
    empty
    (function Invalid_proof -> Some () | _ -> None)
    (fun () -> Invalid_proof) ;
  (* Tx_rollup_message_size_exceed_limit *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_message_size_exceeds_limit"
    ~title:"A message submitted to a transaction rollup inbox exceeds limit"
    ~description:
      "A message submitted to a transaction rollup inbox exceeds limit"
    empty
    (function Message_size_exceeds_limit -> Some () | _ -> None)
    (fun () -> Message_size_exceeds_limit) ;
  (* Tx_rollup_too_many_inboxes *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_too_many_inboxes"
    ~title:"Cannot create a new inbox because there are too many already"
    ~description:"Cannot create a new inbox because there are too many already"
    empty
    (function Too_many_inboxes -> Some () | _ -> None)
    (fun () -> Too_many_inboxes) ;
  (* Tx_rollup_too_many_finalized_commitments *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_too_many_finalized_commitments"
    ~title:"Too many finalized commitments"
    ~description:
      "Cannot create a new commitment because there are too many finalized \
       commitments"
    empty
    (function Too_many_finalized_commitments -> Some () | _ -> None)
    (fun () -> Too_many_finalized_commitments) ;
  (* Wrong_batch_count *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_wrong_batch_count"
    ~title:"This commitment has the wrong number of batches"
    ~description:
      "This commitment has a different number of batches than its inbox"
    unit
    (function Wrong_batch_count -> Some () | _ -> None)
    (fun () -> Wrong_batch_count) ;
  (* Commitment_too_early *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_commitment_too_early"
    ~title:"Cannot submit a commitment for this level yet"
    ~description:
      "It is not possible to submit a commitment for this level just yet."
    (obj2
       (req "provided" Tx_rollup_level_repr.encoding)
       (req "expected" Tx_rollup_level_repr.encoding))
    (function
      | Commitment_too_early {provided; expected} -> Some (provided, expected)
      | _ -> None)
    (fun (provided, expected) -> Commitment_too_early {provided; expected}) ;
  (* Level_already_has_commitment *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_level_already_has_commitment"
    ~title:"This commitment is for a level that already has a commitment"
    ~description:"This commitment is for a level that already has a commitment"
    (obj1 (req "level" Tx_rollup_level_repr.encoding))
    (function Level_already_has_commitment level -> Some level | _ -> None)
    (fun level -> Level_already_has_commitment level) ;
  (* Wrong_inbox_hash *)
  register_error_kind
    `Branch
    ~id:"Wrong_inbox_hash"
    ~title:"This commitment has the wrong inbox hash"
    ~description:"This commitment has a different hash than its inbox"
    unit
    (function Wrong_inbox_hash -> Some () | _ -> None)
    (fun () -> Wrong_inbox_hash) ;
  (* Bond_does_not_exist *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_bond_does_not_exist"
    ~title:"This account does not have a bond for this rollup"
    ~description:"This account does not have a bond for this rollup"
    (obj1 (req "contract" Signature.Public_key_hash.encoding))
    (function Bond_does_not_exist contract -> Some contract | _ -> None)
    (fun contract -> Bond_does_not_exist contract) ;
  (* Bond_in_use *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_bond_in_use"
    ~title:"This account's bond is in use for one or more commitments"
    ~description:"This account's bond is in use for one or more commitments"
    (obj1 (req "contract" Signature.Public_key_hash.encoding))
    (function Bond_in_use contract -> Some contract | _ -> None)
    (fun contract -> Bond_in_use contract) ;
  (* No_commitment_to_finalize *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_no_commitment_to_finalize"
    ~title:"There is no commitment to finalize"
    ~description:"There is no commitment to finalize"
    empty
    (function No_commitment_to_finalize -> Some () | _ -> None)
    (fun () -> No_commitment_to_finalize) ;
  (* No_commitment_to_remove *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_no_commitment_to_remove"
    ~title:"There is no commitment to remove"
    ~description:"There is no commitment to remove"
    empty
    (function No_commitment_to_remove -> Some () | _ -> None)
    (fun () -> No_commitment_to_remove) ;
  (* Invalid_rejection_level_argument *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_invalid_rejection_level_argument"
    ~title:"Received a rejection with an incorrect level argument"
    ~description:"Received a rejection with an incorrect level argument"
    empty
    (function Invalid_rejection_level_argument -> Some () | _ -> None)
    (fun () -> Invalid_rejection_level_argument) ;
  (* Commitment_does_not_exist *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_commitment_does_not_exist"
    ~title:"There is no commitment at the requested level"
    ~description:"There is no commitment at the requested level"
    (obj1 (req "provided" Tx_rollup_level_repr.encoding))
    (function Commitment_does_not_exist l -> Some l | _ -> None)
    (fun l -> Commitment_does_not_exist l) ;
  (* Wrong_predecessor_hash *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_wrong_predecessor_hash"
    ~title:"The commitment refers to a commitment that is not in the context"
    ~description:
      "The commitment refers to a commitment that is not in the context"
    (obj2
       (req
          "provided"
          (option Tx_rollup_commitment_repr.Commitment_hash.encoding))
       (req
          "expected"
          (option Tx_rollup_commitment_repr.Commitment_hash.encoding)))
    (function
      | Wrong_predecessor_hash {provided; expected} -> Some (provided, expected)
      | _ -> None)
    (fun (provided, expected) -> Wrong_predecessor_hash {provided; expected}) ;
  (* Tx_rollup_already_exists *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_already_exists"
    ~title:"Transaction rollup was already created"
    ~description:
      "The protocol tried to originate the same transaction rollup twice"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Transaction rollup %a is already used for an existing transaction \
         rollup. This should not happen, and indicates there is a bug in the \
         protocol. If you can, please report this bug \
         (https://gitlab.com/tezos/tezos/-/issues.)"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "rollup_address" Tx_rollup_repr.encoding))
    (function Tx_rollup_already_exists rollup -> Some rollup | _ -> None)
    (fun rollup -> Tx_rollup_already_exists rollup) ;
  (* Tx_rollup_does_not_exist *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_does_not_exist"
    ~title:"Transaction rollup does not exist"
    ~description:"An invalid transaction rollup address was submitted"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Invalid transaction rollup address %a"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "rollup_address" Tx_rollup_repr.encoding))
    (function Tx_rollup_does_not_exist rollup -> Some rollup | _ -> None)
    (fun rollup -> Tx_rollup_does_not_exist rollup) ;
  (* Internal_error *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_internal_error"
    ~title:"An internal error occurred"
    ~description:"An internal error occurred"
    (obj1 (req "description" string))
    (function Internal_error str -> Some str | _ -> None)
    (fun str -> Internal_error str) ;
  (* Wrong_message_position *)
  register_error_kind
    `Branch
    ~id:"tx_rollup_wrong_message_position"
    ~title:"Wrong message index in rejection"
    ~description:
      "The rejection references the {position}^th message of the inbox {l} \
       which contains only {inbox_length} messages"
    (obj3
       (req "level" Tx_rollup_level_repr.encoding)
       (req "position" int31)
       (req "length" int31))
    (function
      | Wrong_message_position {level; position; length} ->
          Some (level, position, length)
      | _ -> None)
    (fun (level, position, length) ->
      Wrong_message_position {level; position; length}) ;
  (* Wrong_message_hash *)
  register_error_kind
    `Branch
    ~id:"tx_rollup_wrong_message_hash"
    ~title:"Wrong message path in rejection."
    ~description:
      "This rejection has sent a message  and a path that does not fit the \
       current merkle root hash in the corresponding inbox"
    (obj1
       (req "expected_merkle_root" Tx_rollup_inbox_repr.Merkle.root_encoding))
    (function Wrong_message_path {expected} -> Some expected | _ -> None)
    (fun expected -> Wrong_message_path {expected}) ;
  (* No_finalized_commitment_for_level *)
  register_error_kind
    `Temporary
    ~id:"operation.tx_rollup_no_finalized_commitment_for_level"
    ~title:"Operation is about a commitment that is not yet finalized"
    ~description:"This operation must be about a finalized commitment"
    ~pp:(fun ppf (level, window) ->
      match window with
      | Some (first, last) ->
          Format.fprintf
            ppf
            "This operation is only allowed on finalized and existing \
             commitments, but its level %a is not in the existing and \
             finalized window of commitments: [%a; %a]."
            Tx_rollup_level_repr.pp
            level
            Tx_rollup_level_repr.pp
            first
            Tx_rollup_level_repr.pp
            last
      | None ->
          Format.fprintf
            ppf
            "This operation was about level %a but no finalized commitment \
             exists yet."
            Tx_rollup_level_repr.pp
            level)
    Data_encoding.(
      obj2
        (req "received" Tx_rollup_level_repr.encoding)
        (req
           "commitment_head_level"
           (option
              (tup2 Tx_rollup_level_repr.encoding Tx_rollup_level_repr.encoding))))
    (function
      | No_finalized_commitment_for_level {level; window} -> Some (level, window)
      | _ -> None)
    (fun (level, window) -> No_finalized_commitment_for_level {level; window}) ;
  (* Withdraw_invalid_proof *)
  register_error_kind
    `Branch
    ~id:"tx_rollup_withdraw_invalid_path"
    ~title:"The validation path submitted for a withdrawal is invalid"
    ~description:
      "The validation path submitted for a withdrawal is not valid for the \
       given withdrawal and message index"
    empty
    (function Withdraw_invalid_path -> Some () | _ -> None)
    (fun () -> Withdraw_invalid_path) ;
  register_error_kind
    `Temporary
    ~id:"operation.withdraw_already_consumed"
    ~title:"withdraw already consumed"
    ~description:"The submitted withdraw has already been consumed"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The submitted withdraw exists but it has already been consumed \
         earlier.")
    Data_encoding.unit
    (function Withdraw_already_consumed -> Some () | _ -> None)
    (fun () -> Withdraw_already_consumed) ;
  register_error_kind
    `Permanent
    ~id:"tx_rollup_commitment_bond_negative"
    ~title:
      "The number of commitments associated with an implicit account is \
       negative"
    ~description:
      "A negative number of commitment is associated with an implicit account \
       and its associated bound. This error is internal and should never \
       happen."
    ~pp:(fun ppf count ->
      Format.fprintf
        ppf
        "The number of commitments %d associated with this implicit account is \
         negative"
        count)
    (obj1 (req "count" Data_encoding.int31))
    (function Commitment_bond_negative count -> Some count | _ -> None)
    (fun count -> Commitment_bond_negative count) ;
  (* Cannot_reject_level *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_cannot_reject_level"
    ~title:"Cannot reject a commitment at the requested level"
    ~description:"Cannot reject a commitment at the requested level"
    (obj2
       (req "provided" Tx_rollup_level_repr.encoding)
       (req
          "accepted_range"
          (option
             (obj2
                (req "min" Tx_rollup_level_repr.encoding)
                (req "max" Tx_rollup_level_repr.encoding)))))
    (function
      | Cannot_reject_level {provided; accepted_range} ->
          Some (provided, accepted_range)
      | _ -> None)
    (fun (provided, accepted_range) ->
      Cannot_reject_level {provided; accepted_range}) ;
  (* Wrong_rejection_hashes *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_wrong_rejection_hashes"
    ~title:
      "The message result hash recomputed from the rejection argument is \
       invalid"
    ~description:
      "The message result hash recomputed from the rejection argument is \
       invalid"
    (obj3
       (req "provided" Tx_rollup_commitment_repr.message_result_encoding)
       (req "computed" Tx_rollup_commitment_repr.Message_result_hash.encoding)
       (req "expected" Tx_rollup_commitment_repr.Message_result_hash.encoding))
    (function
      | Wrong_rejection_hashes {provided; computed; expected} ->
          Some (provided, computed, expected)
      | _ -> None)
    (fun (provided, computed, expected) ->
      Wrong_rejection_hashes {provided; computed; expected}) ;
  (* ticket_payload_size_limit_exceeded *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_ticket_payload_size_limit_exceeded"
    ~title:"The payload of the deposited ticket exceeded the size limit"
    ~description:"The payload of the deposited ticket exceeded the size limit"
    (obj2 (req "payload_size" int31) (req "limit" int31))
    (function
      | Ticket_payload_size_limit_exceeded {payload_size; limit} ->
          Some (payload_size, limit)
      | _ -> None)
    (fun (payload_size, limit) ->
      Ticket_payload_size_limit_exceeded {payload_size; limit}) ;
  register_error_kind
    `Permanent
    ~id:"tx_rollup_deposit_wrong_ticketer"
    ~title:
      "The ticketer submitted in the ticket is a tx rollup instead of a \
       contract."
    ~description:
      "The ticketer provided with the ticket on the deposit transaction is a \
       tx_rollup which is not possible."
    ~pp:(fun ppf tx_rollup ->
      Format.fprintf
        ppf
        "A tx_rollup (%a) can't be the ticketer of a ticket"
        Tx_rollup_repr.pp
        tx_rollup)
    (obj1 (req "tx_rollup" Tx_rollup_repr.encoding))
    (function Deposit_wrong_ticketer tx_rollup -> Some tx_rollup | _ -> None)
    (fun tx_rollup -> Deposit_wrong_ticketer tx_rollup)
