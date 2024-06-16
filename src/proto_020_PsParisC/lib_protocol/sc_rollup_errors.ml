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
  | (* `Temporary *) Sc_rollup_no_valid_commitment_to_cement
  | (* `Temporary *) Sc_rollup_does_not_exist of Sc_rollup_repr.t
  | (* `Temporary *) Sc_rollup_no_conflict
  | (* `Temporary *) Sc_rollup_no_stakers
  | (* `Temporary *) Sc_rollup_not_staked
  | (* `Temporary *) Sc_rollup_not_staked_on_lcc_or_ancestor
  | (* `Temporary *) Sc_rollup_parent_not_lcc
  | (* `Temporary *) Sc_rollup_remove_lcc_or_ancestor
  | (* `Temporary *) Sc_rollup_staker_double_stake
  | (* `Temporary *) Sc_rollup_too_far_ahead
  | (* `Temporary *)
      Sc_rollup_commitment_from_future of {
      current_level : Raw_level_repr.t;
      inbox_level : Raw_level_repr.t;
    }
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
  | (* `Temporary *)
      Sc_rollup_max_number_of_parallel_games_reached of
      Signature.Public_key_hash.t
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
  | (* `Temporary *)
      Sc_rollup_staker_funds_too_low of {
      staker : Signature.public_key_hash;
      sc_rollup : Sc_rollup_repr.t;
      staker_balance : Tez_repr.t;
      min_expected_balance : Tez_repr.t;
    }
  | (* `Temporary *) Sc_rollup_bad_commitment_serialization
  | (* `Permanent *) Sc_rollup_address_generation
  | (* `Permanent *) Sc_rollup_zero_tick_commitment
  | (* `Permanent *) Sc_rollup_commitment_past_curfew
  | (* `Permanent *)
      Sc_rollup_not_valid_commitments_conflict of
      Sc_rollup_commitment_repr.Hash.t
      * Signature.public_key_hash
      * Sc_rollup_commitment_repr.Hash.t
      * Signature.public_key_hash
  | (* `Permanent *)
      Sc_rollup_wrong_staker_for_conflict_commitment of
      Signature.public_key_hash * Sc_rollup_commitment_repr.Hash.t
  | (* `Permanent *)
      Sc_rollup_commitment_too_old of {
      last_cemented_inbox_level : Raw_level_repr.t;
      commitment_inbox_level : Raw_level_repr.t;
    }
  | (* `Temporary *)
      Sc_rollup_no_commitment_to_cement of Raw_level_repr.t
  | (* `Permanent *)
      Sc_rollup_double_publish of Sc_rollup_commitment_repr.Hash.t
  | Sc_rollup_empty_whitelist
  | Sc_rollup_whitelist_disabled
  | Sc_rollup_staker_not_in_whitelist
  | Sc_rollup_duplicated_key_in_whitelist
  | Sc_rollup_is_public

let () =
  register_error_kind
    `Temporary
    ~id:"smart_rollup_staker_in_game"
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
    ~id:"smart_rollup_timeout_level_not_reached"
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
    ~id:"smart_rollup_game_already_started"
    ~title:"Refutation game already started"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_game_already_started -> Some () | _ -> None)
    (fun () -> Sc_rollup_game_already_started) ;
  let description = "Refutation game does not exist" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_no_game"
    ~title:"Refutation game does not exist"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_no_game -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_game) ;
  let description = "Attempt to play move but not staker's turn" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_wrong_turn"
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
    ~id:"smart_rollup_max_number_of_messages_reached_for_commitment_period"
    ~title:"Maximum number of messages reached for commitment period"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function
      | Sc_rollup_max_number_of_messages_reached_for_commitment_period ->
          Some ()
      | _ -> None)
    (fun () -> Sc_rollup_max_number_of_messages_reached_for_commitment_period) ;
  let description = "Tried to add zero messages to a smart rollup" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_add_zero_messages"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_add_zero_messages -> Some () | _ -> None)
    (fun () -> Sc_rollup_add_zero_messages) ;
  let description =
    "Attempted to cement a commitment but there is no valid commitment to \
     cement."
  in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_no_valid_commitment_to_cement"
    ~title:"No valid commitment to cement"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_valid_commitment_to_cement -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_valid_commitment_to_cement) ;
  let description = "Attempted to cement a disputed commitment." in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_commitment_disputed"
    ~title:"Commitment disputed"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_disputed -> Some () | _ -> None)
    (fun () -> Sc_rollup_disputed) ;
  let description =
    "Attempted to use a smart rollup that has not been originated."
  in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_does_not_exist"
    ~title:"Smart rollup does not exist"
    ~description
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Smart rollup %a does not exist" Sc_rollup_repr.pp x)
    Data_encoding.(obj1 (req "rollup" Sc_rollup_repr.encoding))
    (function Sc_rollup_does_not_exist x -> Some x | _ -> None)
    (fun x -> Sc_rollup_does_not_exist x) ;
  let description = "No conflict." in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_no_conflict"
    ~title:"No conflict"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_conflict -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_conflict) ;
  let description = "No stakers for the targeted smart rollup." in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_no_stakers"
    ~title:"No stakers"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_stakers -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_stakers) ;
  let description =
    "This implicit account is not a staker of this smart rollup."
  in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_not_staked"
    ~title:"Unknown staker"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_not_staked -> Some () | _ -> None)
    (fun () -> Sc_rollup_not_staked) ;
  let description =
    "Attempted to withdraw while not staked on the last cemented commitment or \
     its ancestor."
  in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_not_staked_on_lcc_or_ancestor"
    ~title:"Smart rollup not staked on LCC or its ancestor"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_not_staked_on_lcc_or_ancestor -> Some () | _ -> None)
    (fun () -> Sc_rollup_not_staked_on_lcc_or_ancestor) ;
  let description = "Parent is not the last cemented commitment." in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_parent_not_lcc"
    ~title:"Parent is not the last cemented commitment"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_parent_not_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_parent_not_lcc) ;
  let description = "Can not remove a staker committed on cemented." in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_remove_lcc_or_ancestor"
    ~title:"Can not remove a staker"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_remove_lcc_or_ancestor -> Some () | _ -> None)
    (fun () -> Sc_rollup_remove_lcc_or_ancestor) ;
  let description = "Staker tried to double stake." in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_staker_double_stake"
    ~title:description
    ~description
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The staker tried to double stake, that is, it tried to publish a \
         commitment for an inbox level where it already published another \
         conflicting commitment. The staker is not allowed to changed its \
         mind.")
    Data_encoding.empty
    (function Sc_rollup_staker_double_stake -> Some () | _ -> None)
    (fun () -> Sc_rollup_staker_double_stake) ;
  let description =
    "Commitment is too far ahead of the last cemented commitment."
  in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_too_far_ahead"
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
    ~id:"smart_rollup_commitment_too_recent"
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
    ~id:"smart_rollup_unknown_commitment"
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
    ~id:"smart_rollup_bad_inbox_level"
    ~title:"Committing to a bad inbox level"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_bad_inbox_level -> Some () | _ -> None)
    (fun () -> Sc_rollup_bad_inbox_level) ;
  let description = "Invalid rollup outbox message index" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_invalid_outbox_message_index"
    ~title:"Invalid rollup outbox message index"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_invalid_outbox_message_index -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_outbox_message_index) ;
  let description = "Outbox level expired" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_outbox_level_expired"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_outbox_level_expired -> Some () | _ -> None)
    (fun () -> Sc_rollup_outbox_level_expired) ;
  let description = "Outbox message already applied" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_outbox_message_already_applied"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_outbox_message_already_applied -> Some () | _ -> None)
    (fun () -> Sc_rollup_outbox_message_already_applied) ;
  register_error_kind
    `Temporary
    ~id:"smart_rollup_staker_funds_too_low"
    ~title:"Staker does not have enough funds to make a deposit"
    ~description:
      "Staker doesn't have enough funds to make a smart rollup deposit."
    ~pp:(fun ppf (staker, sc_rollup, staker_balance, min_expected_balance) ->
      Format.fprintf
        ppf
        "Staker (%a) doesn't have enough funds to make the deposit for smart \
         rollup (%a). Staker's balance is %a while a balance of at least %a is \
         required."
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
        (req "smart_rollup" Sc_rollup_repr.encoding)
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
    ~id:"smart_rollup_bad_commitment_serialization"
    ~title:"Could not serialize commitment."
    ~description:"Unable to hash the commitment serialization."
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_bad_commitment_serialization -> Some () | _ -> None)
    (fun () -> Sc_rollup_bad_commitment_serialization) ;
  let description =
    "Commitment inbox level is greater or equal than current level"
  in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_commitment_from_future"
    ~title:"Commitment from future"
    ~description
    ~pp:(fun ppf (current_level, inbox_level) ->
      Format.fprintf
        ppf
        "%s@ Current level: %a,@ commitment inbox level: %a"
        description
        Raw_level_repr.pp
        current_level
        Raw_level_repr.pp
        inbox_level)
    Data_encoding.(
      obj2
        (req "current_level" Raw_level_repr.encoding)
        (req "inbox_level" Raw_level_repr.encoding))
    (function
      | Sc_rollup_commitment_from_future {current_level; inbox_level} ->
          Some (current_level, inbox_level)
      | _ -> None)
    (fun (current_level, inbox_level) ->
      Sc_rollup_commitment_from_future {current_level; inbox_level}) ;
  let description = "Commitment is past the curfew for this level." in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_commitment_past_curfew"
    ~title:"Commitment past curfew."
    ~description:
      "A commitment exists for this inbox level for longer than the curfew \
       period."
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_commitment_past_curfew -> Some () | _ -> None)
    (fun () -> Sc_rollup_commitment_past_curfew) ;
  let description = "Error while generating a smart rollup address" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_address_generation"
    ~title:description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    ~description
    Data_encoding.empty
    (function Sc_rollup_address_generation -> Some () | _ -> None)
    (fun () -> Sc_rollup_address_generation) ;
  let description = "Tried to publish a 0 tick commitment" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_zero_tick_commitment"
    ~title:description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    ~description
    Data_encoding.empty
    (function Sc_rollup_zero_tick_commitment -> Some () | _ -> None)
    (fun () -> Sc_rollup_zero_tick_commitment) ;
  let description = "Maximal number of parallel games reached" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_maximal_number_of_parallel_games_reached"
    ~title:description
    ~pp:(fun ppf staker ->
      Format.fprintf
        ppf
        "%a has reached the limit for number of parallel games"
        Signature.Public_key_hash.pp
        staker)
    ~description
    Data_encoding.(obj1 (req "staker" Signature.Public_key_hash.encoding))
    (function
      | Sc_rollup_max_number_of_parallel_games_reached staker -> Some staker
      | _ -> None)
    (fun staker -> Sc_rollup_max_number_of_parallel_games_reached staker) ;
  let description = "Conflicting commitments do not have a common ancestor" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_not_valid_commitments_conflict"
    ~title:description
    ~pp:(fun ppf (c1, s1, c2, s2) ->
      Format.fprintf
        ppf
        "The two commitments %a, staked by %a, and %a, staked by %a, does not \
         have a common predecessor. Two commitments are in conflict when there \
         direct predecessor is the same."
        Sc_rollup_commitment_repr.Hash.pp
        c1
        Signature.Public_key_hash.pp_short
        s1
        Sc_rollup_commitment_repr.Hash.pp
        c2
        Signature.Public_key_hash.pp_short
        s2)
    ~description
    Data_encoding.(
      obj4
        (req "commitment" Sc_rollup_commitment_repr.Hash.encoding)
        (req "player" Signature.Public_key_hash.encoding)
        (req "opponent_commitment" Sc_rollup_commitment_repr.Hash.encoding)
        (req "opponent" Signature.Public_key_hash.encoding))
    (function
      | Sc_rollup_not_valid_commitments_conflict (c1, s1, c2, s2) ->
          Some (c1, s1, c2, s2)
      | _ -> None)
    (fun (c1, s1, c2, s2) ->
      Sc_rollup_not_valid_commitments_conflict (c1, s1, c2, s2)) ;
  let description = "Given commitment is not staked by given staker" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_wrong_staker_for_conflict_commitment"
    ~title:description
    ~pp:(fun ppf (staker, commitment) ->
      Format.fprintf
        ppf
        "The staker %a has not staked commitment %a"
        Signature.Public_key_hash.pp
        staker
        Sc_rollup_commitment_repr.Hash.pp
        commitment)
    ~description
    Data_encoding.(
      obj2
        (req "player" Signature.Public_key_hash.encoding)
        (req "commitment" Sc_rollup_commitment_repr.Hash.encoding))
    (function
      | Sc_rollup_wrong_staker_for_conflict_commitment (staker, commitment) ->
          Some (staker, commitment)
      | _ -> None)
    (fun (staker, commitment) ->
      Sc_rollup_wrong_staker_for_conflict_commitment (staker, commitment)) ;

  let description = "Published commitment is too old" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_commitment_too_old"
    ~title:description
    ~pp:(fun ppf (last_cemented_inbox_level, commitment_inbox_level) ->
      Format.fprintf
        ppf
        "The published commitment is for the inbox level %a, the last cemented \
         commitment inbox level is %a. You cannot publish a commitment behind \
         the last cemented commitment."
        Raw_level_repr.pp
        last_cemented_inbox_level
        Raw_level_repr.pp
        commitment_inbox_level)
    ~description
    Data_encoding.(
      obj2
        (req "last_cemented_inbox_level" Raw_level_repr.encoding)
        (req "commitment_inbox_level" Raw_level_repr.encoding))
    (function
      | Sc_rollup_commitment_too_old
          {last_cemented_inbox_level; commitment_inbox_level} ->
          Some (last_cemented_inbox_level, commitment_inbox_level)
      | _ -> None)
    (fun (last_cemented_inbox_level, commitment_inbox_level) ->
      Sc_rollup_commitment_too_old
        {last_cemented_inbox_level; commitment_inbox_level}) ;
  let description = "No commitment to cement" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_no_commitment_to_cement"
    ~title:description
    ~pp:(fun ppf inbox_level ->
      Format.fprintf
        ppf
        "There is no commitment to cement at inbox level %a."
        Raw_level_repr.pp
        inbox_level)
    ~description
    Data_encoding.(obj1 (req "inbox_level" Raw_level_repr.encoding))
    (function
      | Sc_rollup_no_commitment_to_cement inbox_level -> Some inbox_level
      | _ -> None)
    (fun inbox_level -> Sc_rollup_no_commitment_to_cement inbox_level) ;
  register_error_kind
    `Permanent
    ~id:"smart_rollup_double_publish"
    ~title:"The commitment was published twice by the operator"
    ~pp:(fun ppf commitment_hash ->
      Format.fprintf
        ppf
        "The operator publishing %a already published this commitment."
        Sc_rollup_commitment_repr.Hash.pp
        commitment_hash)
    ~description
    Data_encoding.(
      obj1 (req "commitment_hash" Sc_rollup_commitment_repr.Hash.encoding))
    (function
      | Sc_rollup_double_publish commitment_hash -> Some commitment_hash
      | _ -> None)
    (fun commitment_hash -> Sc_rollup_double_publish commitment_hash) ;
  register_error_kind
    `Permanent
    ~id:"smart_rollup_empty_whitelist"
    ~title:"Invalid whitelist: whitelist cannot be empty"
    ~description:"Smart rollup whitelist cannot be empty"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Smart rollup whitelist cannot be empty.")
    Data_encoding.empty
    (function Sc_rollup_empty_whitelist -> Some () | _ -> None)
    (fun () -> Sc_rollup_empty_whitelist) ;
  register_error_kind
    `Permanent
    ~id:"smart_rollup_whitelist_disabled"
    ~title:"Invalid whitelist: must be None when the feature is deactivated"
    ~description:"The whitelist must be None when the feature is deactivated."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Private smart rollup with whitelist ACL is disabled.")
    Data_encoding.empty
    (function Sc_rollup_whitelist_disabled -> Some () | _ -> None)
    (fun () -> Sc_rollup_whitelist_disabled) ;
  register_error_kind
    `Temporary
    ~id:"smart_rollup_staker_not_in_whitelist"
    ~title:description
    ~description
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The rollup is private and the submitter of the commitment is not \
         present in the whitelist.")
    Data_encoding.empty
    (function Sc_rollup_staker_not_in_whitelist -> Some () | _ -> None)
    (fun () -> Sc_rollup_staker_not_in_whitelist) ;
  register_error_kind
    `Temporary
    ~id:"smart_rollup_duplicated_key_in_whitelist"
    ~title:description
    ~description
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The whitelist contains twice the same key. This is forbidden and all \
         keys in the whitelist should be disctinct.")
    Data_encoding.empty
    (function Sc_rollup_duplicated_key_in_whitelist -> Some () | _ -> None)
    (fun () -> Sc_rollup_duplicated_key_in_whitelist) ;
  register_error_kind
    `Permanent
    ~id:"smart_rollup_rollup_is_public"
    ~title:description
    ~description
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The rollup is public, no update whitelist message can be executed.")
    Data_encoding.empty
    (function Sc_rollup_is_public -> Some () | _ -> None)
    (fun () -> Sc_rollup_is_public)
