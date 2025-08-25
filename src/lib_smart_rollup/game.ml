(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module V1 = struct
  type dissection_chunk = {state_hash : State_hash.t option; tick : Z.t}

  type step = Dissection of dissection_chunk list | Proof of string

  type refutation =
    | Start of {
        player_commitment_hash : Commitment.Hash.t;
        opponent_commitment_hash : Commitment.Hash.t;
      }
    | Move of {choice : Z.t; step : step}

  type index = {
    alice : Signature.Public_key_hash.t;
    bob : Signature.Public_key_hash.t;
  }

  type timeout = {
    alice_timeout : int;
    bob_timeout : int;
    last_turn_level : int32;
  }

  let make_index a b =
    let alice, bob =
      if Signature.Public_key_hash.(a > b) then (b, a) else (a, b)
    in
    {alice; bob}

  let equal_dissection_chunk c1 c2 =
    Z.equal c1.tick c2.tick
    && Option.equal State_hash.equal c1.state_hash c2.state_hash

  let index_encoding =
    let open Data_encoding in
    conv
      (fun {alice; bob} -> (alice, bob))
      (fun (alice, bob) -> make_index alice bob)
      (obj2
         (req "alice" Signature.Public_key_hash.encoding)
         (req "bob" Signature.Public_key_hash.encoding))

  let dissection_chunk_encoding =
    let open Data_encoding in
    conv
      (fun {state_hash; tick} -> (state_hash, tick))
      (fun (state_hash, tick) -> {state_hash; tick})
      (obj2 (opt "state" State_hash.encoding) (req "tick" n))

  let dissection_encoding = Data_encoding.list dissection_chunk_encoding

  let step_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Dissection"
          (Tag 0)
          dissection_encoding
          (function Dissection d -> Some d | _ -> None)
          (fun d -> Dissection d);
        case
          ~title:"Proof"
          (Tag 1)
          (string' Hex)
          (function Proof p -> Some p | _ -> None)
          (fun p -> Proof p);
      ]

  let refutation_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Start"
          (Tag 0)
          (obj3
             (req "refutation_kind" (constant "start"))
             (req "player_commitment_hash" Commitment.Hash.encoding)
             (req "opponent_commitment_hash" Commitment.Hash.encoding))
          (function
            | Start {player_commitment_hash; opponent_commitment_hash} ->
                Some ((), player_commitment_hash, opponent_commitment_hash)
            | _ -> None)
          (fun ((), player_commitment_hash, opponent_commitment_hash) ->
            Start {player_commitment_hash; opponent_commitment_hash});
        case
          ~title:"Move"
          (Tag 1)
          (obj3
             (req "refutation_kind" (constant "move"))
             (req "choice" n)
             (req "step" step_encoding))
          (function
            | Move {choice; step} -> Some ((), choice, step) | _ -> None)
          (fun ((), choice, step) -> Move {choice; step});
      ]

  type conflict = {
    other : Signature.Public_key_hash.t;
    their_commitment : Commitment.t;
    our_commitment : Commitment.t;
    parent_commitment : Commitment.Hash.t;
  }

  let conflict_encoding =
    Data_encoding.(
      conv
        (fun {other; their_commitment; our_commitment; parent_commitment} ->
          (other, their_commitment, our_commitment, parent_commitment))
        (fun (other, their_commitment, our_commitment, parent_commitment) ->
          {other; their_commitment; our_commitment; parent_commitment})
        (obj4
           (req "other" Signature.Public_key_hash.encoding)
           (req "their_commitment" Commitment.encoding)
           (req "our_commitment" Commitment.encoding)
           (req "parent_commitment" Commitment.Hash.encoding)))

  type player = Alice | Bob

  type game_state =
    | Dissecting of {
        dissection : dissection_chunk list;
        default_number_of_sections : int;
      }
    | Final_move of {
        agreed_start_chunk : dissection_chunk;
        refuted_stop_chunk : dissection_chunk;
      }

  type t = {
    turn : player;
    inbox_snapshot : Inbox.V1.history_proof;
    dal_snapshot : Dal.Slot_history.t;
    start_level : int32;
    inbox_level : int32;
    game_state : game_state;
  }

  let game_state_equal gs1 gs2 =
    match (gs1, gs2) with
    | ( Dissecting
          {
            dissection = dissection1;
            default_number_of_sections = default_number_of_sections1;
          },
        Dissecting
          {
            dissection = dissection2;
            default_number_of_sections = default_number_of_sections2;
          } ) ->
        Compare.Int.equal
          default_number_of_sections1
          default_number_of_sections2
        && List.equal equal_dissection_chunk dissection1 dissection2
    | Dissecting _, _ -> false
    | ( Final_move
          {
            agreed_start_chunk = agreed_start_chunk1;
            refuted_stop_chunk = refuted_stop_chunk1;
          },
        Final_move
          {
            agreed_start_chunk = agreed_start_chunk2;
            refuted_stop_chunk = refuted_stop_chunk2;
          } ) ->
        equal_dissection_chunk agreed_start_chunk1 agreed_start_chunk2
        && equal_dissection_chunk refuted_stop_chunk1 refuted_stop_chunk2
    | Final_move _, _ -> false

  let player_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Alice"
          (Tag 0)
          (constant "alice")
          (function Alice -> Some () | _ -> None)
          (fun () -> Alice);
        case
          ~title:"Bob"
          (Tag 1)
          (constant "bob")
          (function Bob -> Some () | _ -> None)
          (fun () -> Bob);
      ]

  let game_state_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Dissecting"
          (Tag 0)
          (obj3
             (req "kind" (constant "Dissecting"))
             (req "dissection" dissection_encoding)
             (req "default_number_of_sections" uint8))
          (function
            | Dissecting {dissection; default_number_of_sections} ->
                Some ((), dissection, default_number_of_sections)
            | _ -> None)
          (fun ((), dissection, default_number_of_sections) ->
            Dissecting {dissection; default_number_of_sections});
        case
          ~title:"Final_move"
          (Tag 1)
          (obj3
             (req "kind" (constant "Final_move"))
             (req "agreed_start_chunk" dissection_chunk_encoding)
             (req "refuted_stop_chunk" dissection_chunk_encoding))
          (function
            | Final_move {agreed_start_chunk; refuted_stop_chunk} ->
                Some ((), agreed_start_chunk, refuted_stop_chunk)
            | _ -> None)
          (fun ((), agreed_start_chunk, refuted_stop_chunk) ->
            Final_move {agreed_start_chunk; refuted_stop_chunk});
      ]

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             turn;
             inbox_snapshot;
             dal_snapshot;
             start_level;
             inbox_level;
             game_state;
           }
         ->
        ( turn,
          inbox_snapshot,
          dal_snapshot,
          start_level,
          inbox_level,
          game_state ))
      (fun ( turn,
             inbox_snapshot,
             dal_snapshot,
             start_level,
             inbox_level,
             game_state )
         ->
        {
          turn;
          inbox_snapshot;
          dal_snapshot;
          start_level;
          inbox_level;
          game_state;
        })
      (obj6
         (req "turn" player_encoding)
         (req "inbox_snapshot" Inbox.V1.history_proof_encoding)
         (req "dal_snapshot" (dynamic_size Dal.Slot_history.encoding))
         (req "start_level" int32)
         (req "inbox_level" int32)
         (req "game_state" game_state_encoding))
end

type versioned = V1 of V1.t

let versioned_encoding =
  let open Data_encoding.V1 in
  union
    [
      case
        ~title:"smart_rollup_game.V1"
        (Tag 0)
        V1.encoding
        (function V1 game -> Some game)
        (fun game -> V1 game);
    ]

include V1

let of_versioned = function V1 g -> g [@@inline]

let to_versioned g = V1 g [@@inline]
