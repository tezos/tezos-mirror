(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
  | (* `Temporary *) Sc_rollup_disputed
  | (* `Temporary *) Sc_rollup_does_not_exist of Sc_rollup_repr.t
  | (* `Temporary *) Sc_rollup_no_conflict
  | (* `Temporary *) Sc_rollup_no_stakers
  | (* `Temporary *) Sc_rollup_not_staked
  | (* `Temporary *) Sc_rollup_not_staked_on_lcc
  | (* `Temporary *) Sc_rollup_parent_not_lcc
  | (* `Temporary *) Sc_rollup_remove_lcc
  | (* `Temporary *) Sc_rollup_staker_backtracked
  | (* `Temporary *) Sc_rollup_too_far_ahead
  | (* `Temporary *) Sc_rollup_too_recent
  | (* `Temporary *)
      Sc_rollup_unknown_commitment of
      Sc_rollup_commitment_repr.Hash.t
  | (* `Temporary *) Sc_rollup_bad_inbox_level
  | (* `Temporary *) Sc_rollup_max_number_of_available_messages_reached
  | (* `Temporary *) Sc_rollup_wrong_turn
  | (* `Temporary *) Sc_rollup_no_game
  | (* `Temporary *) Sc_rollup_staker_in_game
  | (* `Temporary *) Sc_rollup_timeout_level_not_reached
  | (* `Temporary *)
      Sc_rollup_max_number_of_messages_reached_for_commitment_period
  | (* `Temporary *) Sc_rollup_invalid_outbox_message_index
  | (* `Temporary *) Sc_rollup_outbox_level_expired
  | (* `Temporary *) Sc_rollup_outbox_message_already_applied

let () =
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_max_number_of_available_messages_reached"
    ~title:"Maximum number of available messages reached"
    ~description:"Maximum number of available messages reached"
    Data_encoding.unit
    (function
      | Sc_rollup_max_number_of_available_messages_reached -> Some ()
      | _ -> None)
    (fun () -> Sc_rollup_max_number_of_available_messages_reached) ;
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_staker_in_game"
    ~title:"Staker is already playing a game"
    ~description:"Attempt to start a game where one staker is already busy"
    Data_encoding.unit
    (function Sc_rollup_staker_in_game -> Some () | _ -> None)
    (fun () -> Sc_rollup_staker_in_game) ;
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_timeout_level_not_reached"
    ~title:"Attempt to timeout game too early"
    ~description:"Attempt to timeout game too early"
    Data_encoding.unit
    (function Sc_rollup_timeout_level_not_reached -> Some () | _ -> None)
    (fun () -> Sc_rollup_timeout_level_not_reached) ;
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_no_game"
    ~title:"Refutation game does not exist"
    ~description:"Refutation game does not exist"
    Data_encoding.unit
    (function Sc_rollup_no_game -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_game) ;
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_wrong_turn"
    ~title:"Attempt to play move but not staker's turn"
    ~description:"Attempt to play move but not staker's turn"
    Data_encoding.unit
    (function Sc_rollup_wrong_turn -> Some () | _ -> None)
    (fun () -> Sc_rollup_wrong_turn) ;
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_max_number_of_messages_reached_for_commitment_period"
    ~title:"Maximum number of messages reached for commitment period"
    ~description:"Maximum number of messages reached for commitment period"
    Data_encoding.unit
    (function
      | Sc_rollup_max_number_of_messages_reached_for_commitment_period ->
          Some ()
      | _ -> None)
    (fun () -> Sc_rollup_max_number_of_messages_reached_for_commitment_period) ;
  let description = "Attempted to cement a disputed commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_disputed"
    ~title:"Commitment disputed"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_disputed -> Some () | _ -> None)
    (fun () -> Sc_rollup_disputed) ;
  let description = "Attempted to use a rollup that has not been originated." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_does_not_exist"
    ~title:"Rollup does not exist"
    ~description
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Rollup %a does not exist" Sc_rollup_repr.pp x)
    Data_encoding.(obj1 (req "rollup" Sc_rollup_repr.encoding))
    (function Sc_rollup_does_not_exist x -> Some x | _ -> None)
    (fun x -> Sc_rollup_does_not_exist x) ;
  let description = "No conflict." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_no_conflict"
    ~title:"No conflict"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_conflict -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_conflict) ;
  let description = "No stakers." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_no_stakers"
    ~title:"No stakers"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_stakers -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_stakers) ;
  let description = "Unknown staker." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_not_staked"
    ~title:"Unknown staker"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_not_staked -> Some () | _ -> None)
    (fun () -> Sc_rollup_not_staked) ;
  let description =
    "Attempted to withdraw while not staked on the last cemented commitment."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_not_staked_on_lcc"
    ~title:"Rollup not staked on LCC"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_not_staked_on_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_not_staked_on_lcc) ;
  let description = "Parent is not cemented." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_parent_not_lcc"
    ~title:"Parent not cemented"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_parent_not_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_parent_not_lcc) ;
  let description = "Can not remove a cemented commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_remove_lcc"
    ~title:"Can not remove cemented"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_remove_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_remove_lcc) ;
  let description = "Staker backtracked." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_staker_backtracked"
    ~title:"Staker backtracked"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_staker_backtracked -> Some () | _ -> None)
    (fun () -> Sc_rollup_staker_backtracked) ;
  let description =
    "Commitment is too far ahead of the last cemented commitment."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_too_far_ahead"
    ~title:"Commitment too far ahead"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_too_far_ahead -> Some () | _ -> None)
    (fun () -> Sc_rollup_too_far_ahead) ;
  let description =
    "Attempted to cement a commitment before its refutation deadline."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_too_recent"
    ~title:"Commitment too recent"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_too_recent -> Some () | _ -> None)
    (fun () -> Sc_rollup_too_recent) ;
  let description = "Unknown commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_unknown_commitment"
    ~title:"Unknown commitment"
    ~description
    ~pp:(fun ppf x ->
      Format.fprintf
        ppf
        "Commitment %a does not exist"
        Sc_rollup_commitment_repr.Hash.pp
        x)
    Data_encoding.(
      obj1 (req "commitment" Sc_rollup_commitment_repr.Hash.encoding))
    (function Sc_rollup_unknown_commitment x -> Some x | _ -> None)
    (fun x -> Sc_rollup_unknown_commitment x) ;
  let description = "Attempted to commit to a bad inbox level." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_bad_inbox_level"
    ~title:"Committing to a bad inbox level"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_bad_inbox_level -> Some () | _ -> None)
    (fun () -> Sc_rollup_bad_inbox_level) ;
  let description = "Invalid rollup outbox message index" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_invalid_outbox_message_index"
    ~title:"Invalid rollup outbox message index"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_invalid_outbox_message_index -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_outbox_message_index) ;
  let description = "Outbox level expired" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_outbox_level_expired"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_outbox_level_expired -> Some () | _ -> None)
    (fun () -> Sc_rollup_outbox_level_expired) ;
  let description = "Outbox message already applied" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_outbox_message_already_applied"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_outbox_message_already_applied -> Some () | _ -> None)
    (fun () -> Sc_rollup_outbox_message_already_applied) ;
  ()
