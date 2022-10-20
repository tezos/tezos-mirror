(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context

let tez_sym = "\xEA\x9C\xA9"

type error +=
  | Cannot_produce_proof of
      Sc_rollup.Inbox.t * Sc_rollup.Inbox.History.t * Raw_level.t
  | Missing_mode_operators of {mode : string; missing_operators : string list}
  | Bad_minimal_fees of string
  | Commitment_predecessor_should_be_LCC of Sc_rollup.Commitment.t
  | Unreliable_tezos_node_returning_inconsistent_game
  | Inconsistent_inbox of {
      layer1_inbox : Sc_rollup.Inbox.t;
      inbox : Sc_rollup.Inbox.t;
    }
  | Missing_PVM_state of Block_hash.t * Int32.t
  | Cannot_checkout_context of Block_hash.t * string option
  | Cannot_retrieve_reveal of Sc_rollup.Reveal_hash.t

type error +=
  | Lost_game of
      public_key_hash * Protocol.Alpha_context.Sc_rollup.Game.reason * Tez.t

let () =
  register_error_kind
    `Permanent
    ~id:"bad_minimal_fees_arg"
    ~title:"Bad -minimal-fees arg"
    ~description:"invalid fee threshold in -fee-threshold"
    ~pp:(fun ppf literal ->
      Format.fprintf ppf "invalid minimal fees '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_minimal_fees parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_minimal_fees parameter) ;

  register_error_kind
    `Permanent
    ~id:"internal.commitment_should_be_next_to_lcc"
    ~title:
      "Internal error: The next commitment should have the LCC as predecessor"
    ~description:
      "Internal error: The next commitment should have the LCC as predecessor"
    ~pp:(fun ppf commitment ->
      Format.fprintf
        ppf
        "invalid commitment '%a'"
        Sc_rollup.Commitment.pp
        commitment)
    Data_encoding.(obj1 (req "commitment" Sc_rollup.Commitment.encoding))
    (function
      | Commitment_predecessor_should_be_LCC commitment -> Some commitment
      | _ -> None)
    (fun commitment -> Commitment_predecessor_should_be_LCC commitment) ;

  register_error_kind
    `Permanent
    ~id:"internal.unreliable_tezos_node"
    ~title:"Internal error: Tezos node seems unreliable"
    ~description:
      "Internal error: The game invariant states that the dissection from the \
       opponent must contain a tick we disagree with. If the retrieved game \
       does not respect this, we cannot trust the Tezos node we are connected \
       to and prefer to stop here."
    ~pp:(fun _ppf () -> ())
    Data_encoding.unit
    (function
      | Unreliable_tezos_node_returning_inconsistent_game -> Some () | _ -> None)
    (fun () -> Unreliable_tezos_node_returning_inconsistent_game) ;

  register_error_kind
    `Permanent
    ~id:"internal.cannot_produce_proof"
    ~title:"Internal error: rollup node cannot produce refutation proof"
    ~description:
      "The rollup node is in a state that prevents it from producing \
       refutation proofs."
    ~pp:(fun ppf (inbox, history, level) ->
      Format.fprintf
        ppf
        "cannot produce proof for inbox %a of level %a with history %a"
        Sc_rollup.Inbox.pp
        inbox
        Raw_level.pp
        level
        Sc_rollup.Inbox.History.pp
        history)
    Data_encoding.(
      obj3
        (req "inbox" Sc_rollup.Inbox.encoding)
        (req "history" Sc_rollup.Inbox.History.encoding)
        (req "level" Raw_level.encoding))
    (function
      | Cannot_produce_proof (inbox, history, level) ->
          Some (inbox, history, level)
      | _ -> None)
    (fun (inbox, history, level) ->
      Cannot_produce_proof (inbox, history, level)) ;

  register_error_kind
    ~id:"sc_rollup.node.missing_mode_operators"
    ~title:"Missing operators for the chosen mode"
    ~description:"Missing operators for the chosen mode."
    ~pp:(fun ppf (mode, missing_operators) ->
      Format.fprintf
        ppf
        "@[<hov>Missing operators %a for mode %s.@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           Format.pp_print_string)
        missing_operators
        mode)
    `Permanent
    Data_encoding.(
      obj2 (req "mode" string) (req "missing_operators" (list string)))
    (function
      | Missing_mode_operators {mode; missing_operators} ->
          Some (mode, missing_operators)
      | _ -> None)
    (fun (mode, missing_operators) ->
      Missing_mode_operators {mode; missing_operators}) ;

  register_error_kind
    ~id:"internal.inconsistent_inbox"
    ~title:"Internal error: Rollup node has an inconsistent inbox"
    ~description:
      "The rollup node inbox should be the same as the layer 1 inbox."
    ~pp:(fun ppf (layer1_inbox, inbox) ->
      Format.fprintf
        ppf
        "@[Rollup inbox:@;%a@]@;should be equal to @[Layer1 inbox:@;%a@]"
        Sc_rollup.Inbox.pp
        inbox
        Sc_rollup.Inbox.pp
        layer1_inbox)
    `Permanent
    Data_encoding.(
      obj2
        (req "layer1_inbox" Sc_rollup.Inbox.encoding)
        (req "inbox" Sc_rollup.Inbox.encoding))
    (function
      | Inconsistent_inbox {layer1_inbox; inbox} -> Some (layer1_inbox, inbox)
      | _ -> None)
    (fun (layer1_inbox, inbox) -> Inconsistent_inbox {layer1_inbox; inbox}) ;

  register_error_kind
    `Permanent
    ~id:"internal.missing_pvm_state"
    ~title:"Internal error: Missing PVM state"
    ~description:"The rollup node cannot retrieve the state of the PVM."
    ~pp:(fun ppf (block, level) ->
      Format.fprintf
        ppf
        "Cannot retrieve PVM state for block %a at level %ld"
        Block_hash.pp
        block
        level)
    Data_encoding.(obj2 (req "block" Block_hash.encoding) (req "level" int32))
    (function
      | Missing_PVM_state (block, level) -> Some (block, level) | _ -> None)
    (fun (block, level) -> Missing_PVM_state (block, level)) ;

  register_error_kind
    `Permanent
    ~id:"internal.cannot_checkout_context"
    ~title:"Internal error: Cannot checkout context"
    ~description:
      "The rollup node cannot checkout the context registered for the block."
    ~pp:(fun ppf (block, context_hash) ->
      Format.fprintf
        ppf
        "The context %sfor block %a cannot be checkouted"
        (Option.fold
           ~none:""
           ~some:(fun c -> Hex.(show (of_string c)))
           context_hash)
        Block_hash.pp
        block)
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (opt "context" (conv Bytes.of_string Bytes.to_string bytes)))
    (function
      | Cannot_checkout_context (block, context) -> Some (block, context)
      | _ -> None)
    (fun (block, context) -> Cannot_checkout_context (block, context)) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup.node.lost_game"
    ~title:"Lost refutation game"
    ~description:"The rollup node lost a refutation game."
    ~pp:(fun ppf (loser, reason, slashed) ->
      Format.fprintf
        ppf
        "The rollup node lost the refutation game for operator %a and was \
         slashed %s%a, for reason: %a."
        Signature.Public_key_hash.pp
        loser
        tez_sym
        Tez.pp
        slashed
        Protocol.Alpha_context.Sc_rollup.Game.pp_reason
        reason)
    Data_encoding.(
      obj3
        (req "loser" Signature.Public_key_hash.encoding)
        (req "reason" Protocol.Alpha_context.Sc_rollup.Game.reason_encoding)
        (req "slashed" Tez.encoding))
    (function
      | Lost_game (loser, reason, slashed) -> Some (loser, reason, slashed)
      | _ -> None)
    (fun (loser, reason, slashed) -> Lost_game (loser, reason, slashed)) ;

  register_error_kind
    `Permanent
    ~id:"internal.cannot_retrieve_reveal"
    ~title:"Internal error: Cannot retrieve reveal of hash"
    ~description:"The rollup node cannot retrieve a reveal asked by the rollup."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "The node cannot retrieve a reveal for hash %a"
        Sc_rollup.Reveal_hash.pp
        hash)
    Data_encoding.(obj1 (req "hash" Sc_rollup.Reveal_hash.encoding))
    (function Cannot_retrieve_reveal hash -> Some hash | _ -> None)
    (fun hash -> Cannot_retrieve_reveal hash)
