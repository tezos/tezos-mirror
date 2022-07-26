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

type error += Bad_minimal_fees of string

type error += Commitment_predecessor_should_be_LCC of Sc_rollup.Commitment.t

type error += Unreliable_tezos_node_returning_inconsistent_game

type error +=
  | Cannot_produce_proof of
      Sc_rollup.Inbox.t * Sc_rollup.Inbox.history * Raw_level.t

type error +=
  | Missing_mode_operators of {mode : string; missing_operators : string list}

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
    ~id:"internal.cannnot_produce_proof"
    ~title:"Internal error: Rollup node cannot produce refutation proof"
    ~description:
      "The rollup node is in a state that prevent it from produce refutation \
       proofs."
    ~pp:(fun ppf (inbox, history, level) ->
      Format.fprintf
        ppf
        "cannot produce proof for inbox %a of level %a with history %a"
        Sc_rollup.Inbox.pp
        inbox
        Raw_level.pp
        level
        Sc_rollup.Inbox.pp_history
        history)
    Data_encoding.(
      obj3
        (req "inbox" Sc_rollup.Inbox.encoding)
        (req "history" Sc_rollup.Inbox.history_encoding)
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
      Missing_mode_operators {mode; missing_operators})
