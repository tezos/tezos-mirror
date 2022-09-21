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
  | (* `Temporary *)
      Sc_rollup_commitment_too_recent of {
      current_level : Raw_level_repr.t;
      min_level : Raw_level_repr.t;
    }
  | (* `Temporary *)
      Sc_rollup_unknown_commitment of
      Sc_rollup_commitment_repr.Hash.t
  | (* `Temporary *) Sc_rollup_bad_inbox_level
  | (* `Temporary *) Sc_rollup_game_already_started
  | (* `Temporary *) Sc_rollup_wrong_turn
  | (* `Temporary *) Sc_rollup_no_game
  | (* `Temporary *)
      Sc_rollup_staker_in_game of
      [ `Refuter of Signature.public_key_hash
      | `Defender of Signature.public_key_hash
      | `Both of Signature.public_key_hash * Signature.public_key_hash ]
  | (* `Temporary *)
      Sc_rollup_timeout_level_not_reached of
      int32 * Signature.public_key_hash
  | (* `Temporary *)
      Sc_rollup_max_number_of_messages_reached_for_commitment_period
  | (* `Permanent *) Sc_rollup_add_zero_messages
  | (* `Temporary *) Sc_rollup_invalid_outbox_message_index
  | (* `Temporary *) Sc_rollup_outbox_level_expired
  | (* `Temporary *) Sc_rollup_outbox_message_already_applied
  | (* `Temporary *) Sc_rollup_state_change_on_zero_tick_commitment
  | (* `Temporary *)
      Sc_rollup_staker_funds_too_low of {
      staker : Signature.public_key_hash;
      sc_rollup : Sc_rollup_repr.t;
      staker_balance : Tez_repr.t;
      min_expected_balance : Tez_repr.t;
    }
  | (* `Temporary *) Sc_rollup_bad_commitment_serialization
  | (* `Permanent *) Sc_rollup_address_generation

let () =
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_staker_in_game"
    ~title:"Staker is already playing a game"
    ~description:"Attempted to start a game where one staker is already busy"
    ~pp:(fun ppf staker ->
      let busy ppf = function
        | `Refuter sc ->
            Format.fprintf
              ppf
              "the refuter (%a) is"
              Signature.Public_key_hash.pp
              sc
        | `Defender sc ->
            Format.fprintf
              ppf
              "the defender (%a) is"
              Signature.Public_key_hash.pp
              sc
        | `Both (refuter, defender) ->
            Format.fprintf
              ppf
              "both the refuter (%a) and the defender (%a) are"
              Signature.Public_key_hash.pp
              refuter
              Signature.Public_key_hash.pp
              defender
      in
      Format.fprintf
        ppf
        "Attempted to start a game where %a already busy."
        busy
        staker)
    Data_encoding.(
      union
        [
          case
            (Tag 0)
            ~title:"Refuter"
            (obj1 (req "refuter" Signature.Public_key_hash.encoding))
            (function `Refuter sc -> Some sc | _ -> None)
            (fun sc -> `Refuter sc);
          case
            (Tag 1)
            ~title:"Defender"
            (obj1 (req "defender" Signature.Public_key_hash.encoding))
            (function `Defender sc -> Some sc | _ -> None)
            (fun sc -> `Defender sc);
          case
            (Tag 2)
            ~title:"Both"
            (obj2
               (req "refuter" Signature.Public_key_hash.encoding)
               (req "defender" Signature.Public_key_hash.encoding))
            (function
              | `Both (refuter, defender) -> Some (refuter, defender)
              | _ -> None)
            (fun (refuter, defender) -> `Both (refuter, defender));
        ])
    (function Sc_rollup_staker_in_game x -> Some x | _ -> None)
    (fun x -> Sc_rollup_staker_in_game x) ;
  let description = "Attempt to timeout game too early" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_timeout_level_not_reached"
    ~title:"Attempt to timeout game too early"
    ~description
    ~pp:(fun ppf (blocks_left, staker) ->
      Format.fprintf
        ppf
        "%s. The player %a has %ld left blocks to play."
        description
        Signature.Public_key_hash.pp_short
        staker
        blocks_left)
    Data_encoding.(
      obj2
        (req "level_timeout" int32)
        (req "staker" Signature.Public_key_hash.encoding))
    (function
      | Sc_rollup_timeout_level_not_reached (blocks_left, staker) ->
          Some (blocks_left, staker)
      | _ -> None)
    (fun (blocks_left, staker) ->
      Sc_rollup_timeout_level_not_reached (blocks_left, staker)) ;
  let description =
    "Refutation game already started, must play with is_opening_move = false."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_game_already_started"
    ~title:"Refutation game already started"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_game_already_started -> Some () | _ -> None)
    (fun () -> Sc_rollup_game_already_started) ;
  let description = "Refutation game does not exist" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_no_game"
    ~title:"Refutation game does not exist"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_no_game -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_game) ;
  let description = "Attempt to play move but not staker's turn" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_wrong_turn"
    ~title:"Attempt to play move but not staker's turn"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_wrong_turn -> Some () | _ -> None)
    (fun () -> Sc_rollup_wrong_turn) ;
  let description =
    "Maximum number of messages reached for commitment period"
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_max_number_of_messages_reached_for_commitment_period"
    ~title:"Maximum number of messages reached for commitment period"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function
      | Sc_rollup_max_number_of_messages_reached_for_commitment_period ->
          Some ()
      | _ -> None)
    (fun () -> Sc_rollup_max_number_of_messages_reached_for_commitment_period) ;
  let description = "Tried to add zero messages to a SC rollup" in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_errors.sc_rollup_add_zero_messages"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_add_zero_messages -> Some () | _ -> None)
    (fun () -> Sc_rollup_add_zero_messages) ;
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
  let description = "Parent is not the last cemented commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_parent_not_lcc"
    ~title:"Parent is not the last cemented commitment"
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
    ~id:"Sc_rollup_commitment_too_recent"
    ~title:"Commitment too recent"
    ~description
    ~pp:(fun ppf (current_level, min_level) ->
      Format.fprintf
        ppf
        "%s@ Current level: %a,@ minimal level: %a"
        description
        Raw_level_repr.pp
        current_level
        Raw_level_repr.pp
        min_level)
    Data_encoding.(
      obj2
        (req "current_level" Raw_level_repr.encoding)
        (req "min_level" Raw_level_repr.encoding))
    (function
      | Sc_rollup_commitment_too_recent {current_level; min_level} ->
          Some (current_level, min_level)
      | _ -> None)
    (fun (current_level, min_level) ->
      Sc_rollup_commitment_too_recent {current_level; min_level}) ;
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
  let description = "Attempt to commit zero ticks with state change" in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_state_change_on_zero_tick_commitment"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function
      | Sc_rollup_state_change_on_zero_tick_commitment -> Some () | _ -> None)
    (fun () -> Sc_rollup_state_change_on_zero_tick_commitment) ;
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_staker_funds_too_low"
    ~title:"Staker does not have enough funds to make a deposit"
    ~description:
      "Staker doesn't have enough funds to make a smart contract rollup \
       deposit."
    ~pp:(fun ppf (staker, sc_rollup, staker_balance, min_expected_balance) ->
      Format.fprintf
        ppf
        "Staker (%a) doesn't have enough funds to make the deposit for smart \
         contract rollup (%a). Staker's balance is %a while a balance of at \
         least %a is required."
        Signature.Public_key_hash.pp
        staker
        Sc_rollup_repr.pp
        sc_rollup
        Tez_repr.pp
        staker_balance
        Tez_repr.pp
        min_expected_balance)
    Data_encoding.(
      obj4
        (req "staker" Signature.Public_key_hash.encoding)
        (req "sc_rollup" Sc_rollup_repr.encoding)
        (req "staker_balance" Tez_repr.encoding)
        (req "min_expected_balance" Tez_repr.encoding))
    (function
      | Sc_rollup_staker_funds_too_low
          {staker; sc_rollup; staker_balance; min_expected_balance} ->
          Some (staker, sc_rollup, staker_balance, min_expected_balance)
      | _ -> None)
    (fun (staker, sc_rollup, staker_balance, min_expected_balance) ->
      Sc_rollup_staker_funds_too_low
        {staker; sc_rollup; staker_balance; min_expected_balance}) ;
  let description = "Could not serialize commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_bad_commitment_serialization"
    ~title:"Could not serialize commitment."
    ~description:"Unable to hash the commitment serialization."
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_bad_commitment_serialization -> Some () | _ -> None)
    (fun () -> Sc_rollup_bad_commitment_serialization) ;
  let description = "Error while generating rollup address" in
  register_error_kind
    `Permanent
    ~id:"rollup.error_smart_contract_rollup_address_generation"
    ~title:description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    ~description
    Data_encoding.empty
    (function Sc_rollup_address_generation -> Some () | _ -> None)
    (fun () -> Sc_rollup_address_generation) ;
  ()
