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

let make_id id = String.concat "." [Protocol.name; id]

let register_error_kind ~id = register_error_kind ~id:(make_id id)

type error +=
  | Cannot_produce_proof of Sc_rollup.Inbox.t * Raw_level.t
  | Missing_mode_operators of {mode : string; missing_operators : string list}
  | Bad_minimal_fees of string
  | Disagree_with_cemented of {
      inbox_level : Raw_level.t;
      ours : Sc_rollup.Commitment.Hash.t option;
      on_l1 : Sc_rollup.Commitment.Hash.t;
    }
  | Unreliable_tezos_node_returning_inconsistent_game
  | Inconsistent_inbox of {
      layer1_inbox : Sc_rollup.Inbox.t;
      inbox : Sc_rollup.Inbox.t;
    }
  | Missing_PVM_state of Block_hash.t * Int32.t
  | Cannot_checkout_context of Block_hash.t * Sc_rollup_context_hash.t option
  | No_batcher
  | No_publisher

type error += Lost_game of Protocol.Alpha_context.Sc_rollup.Game.game_result

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
    ~id:"internal.node_disagrees_with_cemented"
    ~title:"Internal error: The node disagrees with a cemented commitment on L1"
    ~description:
      "Internal error: The node disagrees with a cemented commitment on L1"
    ~pp:(fun ppf (inbox_level, ours, on_l1) ->
      Format.fprintf
        ppf
        "Internal error: The node has commitment %a for inbox level %a but \
         this level is cemented on L1 with commitment %a"
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "[None]")
           Sc_rollup.Commitment.Hash.pp)
        ours
        Raw_level.pp
        inbox_level
        Sc_rollup.Commitment.Hash.pp
        on_l1)
    Data_encoding.(
      obj3
        (req "inbox_level" Raw_level.encoding)
        (req "ours" (option Sc_rollup.Commitment.Hash.encoding))
        (req "on_l1" Sc_rollup.Commitment.Hash.encoding))
    (function
      | Disagree_with_cemented {inbox_level; ours; on_l1} ->
          Some (inbox_level, ours, on_l1)
      | _ -> None)
    (fun (inbox_level, ours, on_l1) ->
      Disagree_with_cemented {inbox_level; ours; on_l1}) ;

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
    ~pp:(fun ppf (inbox, level) ->
      Format.fprintf
        ppf
        "cannot produce proof for inbox %a of level %a"
        Sc_rollup.Inbox.pp
        inbox
        Raw_level.pp
        level)
    Data_encoding.(
      obj2
        (req "inbox" Sc_rollup.Inbox.encoding)
        (req "level" Raw_level.encoding))
    (function
      | Cannot_produce_proof (inbox, level) -> Some (inbox, level) | _ -> None)
    (fun (inbox, level) -> Cannot_produce_proof (inbox, level)) ;

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
           ~some:Sc_rollup_context_hash.to_b58check
           context_hash)
        Block_hash.pp
        block)
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (opt "context" Sc_rollup_context_hash.encoding))
    (function
      | Cannot_checkout_context (block, context) -> Some (block, context)
      | _ -> None)
    (fun (block, context) -> Cannot_checkout_context (block, context)) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup.node.lost_game"
    ~title:"Lost refutation game"
    ~description:"The rollup node lost a refutation game."
    ~pp:(fun ppf result ->
      Format.fprintf
        ppf
        "The rollup node lost the refutation game (%a)"
        Protocol.Alpha_context.Sc_rollup.Game.pp_game_result
        result)
    Data_encoding.(
      obj1
        (req
           "result"
           Protocol.Alpha_context.Sc_rollup.Game.game_result_encoding))
    (function Lost_game result -> Some result | _ -> None)
    (fun result -> Lost_game result) ;

  register_error_kind
    ~id:"sc_rollup.node.no_batcher"
    ~title:"No batcher for this node"
    ~description:"This node does not have a batcher"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "This rollup node does not have batcher.")
    `Permanent
    Data_encoding.unit
    (function No_batcher -> Some () | _ -> None)
    (fun () -> No_batcher) ;

  register_error_kind
    ~id:"sc_rollup.node.no_publisher"
    ~title:"No publisher for this node"
    ~description:"This node does not have an operator to publish commitments"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "This rollup node does not have an operator to publish commitments.")
    `Permanent
    Data_encoding.unit
    (function No_publisher -> Some () | _ -> None)
    (fun () -> No_publisher)
