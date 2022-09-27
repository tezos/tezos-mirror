(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

type player = Alice | Bob

module V1 = struct
  type dissection_chunk = {
    state_hash : State_hash.t option;
    tick : Sc_rollup_tick_repr.t;
  }

  let pp_state_hash =
    let open Format in
    pp_print_option ~none:(fun ppf () -> fprintf ppf "None") State_hash.pp

  let pp_dissection_chunk ppf {state_hash; tick} =
    let open Format in
    fprintf
      ppf
      "State hash:%a@ Tick: %a"
      pp_state_hash
      state_hash
      Sc_rollup_tick_repr.pp
      tick

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
    inbox_snapshot : Sc_rollup_inbox_repr.history_proof;
    level : Raw_level_repr.t;
    pvm_name : string;
    game_state : game_state;
  }

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

  let player_equal p1 p2 =
    match (p1, p2) with
    | Alice, Alice -> true
    | Bob, Bob -> true
    | _, _ -> false

  let dissection_chunk_equal {state_hash; tick} chunk2 =
    Option.equal State_hash.equal state_hash chunk2.state_hash
    && Sc_rollup_tick_repr.equal tick chunk2.tick

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
        && List.equal dissection_chunk_equal dissection1 dissection2
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
        dissection_chunk_equal agreed_start_chunk1 agreed_start_chunk2
        && dissection_chunk_equal refuted_stop_chunk1 refuted_stop_chunk2
    | Final_move _, _ -> false

  let equal
      {
        turn = turn1;
        inbox_snapshot = inbox_snapshot1;
        level = level1;
        pvm_name = pvm_name1;
        game_state = game_state1;
      }
      {
        turn = turn2;
        inbox_snapshot = inbox_snapshot2;
        level = level2;
        pvm_name = pvm_name2;
        game_state = game_state2;
      } =
    player_equal turn1 turn2
    && Sc_rollup_inbox_repr.equal_history_proof inbox_snapshot1 inbox_snapshot2
    && Raw_level_repr.equal level1 level2
    && String.equal pvm_name1 pvm_name2
    && game_state_equal game_state1 game_state2

  let string_of_player = function Alice -> "alice" | Bob -> "bob"

  let pp_player ppf player = Format.fprintf ppf "%s" (string_of_player player)

  let opponent = function Alice -> Bob | Bob -> Alice

  let dissection_chunk_encoding =
    let open Data_encoding in
    conv
      (fun {state_hash; tick} -> (state_hash, tick))
      (fun (state_hash, tick) -> {state_hash; tick})
      (obj2
         (opt "state" State_hash.encoding)
         (req "tick" Sc_rollup_tick_repr.encoding))

  let dissection_encoding =
    let open Data_encoding in
    list dissection_chunk_encoding

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
      (fun {turn; inbox_snapshot; level; pvm_name; game_state} ->
        (turn, inbox_snapshot, level, pvm_name, game_state))
      (fun (turn, inbox_snapshot, level, pvm_name, game_state) ->
        {turn; inbox_snapshot; level; pvm_name; game_state})
      (obj5
         (req "turn" player_encoding)
         (req "inbox_snapshot" Sc_rollup_inbox_repr.history_proof_encoding)
         (req "level" Raw_level_repr.encoding)
         (req "pvm_name" string)
         (req "game_state" game_state_encoding))

  let pp_dissection ppf d =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n")
      pp_dissection_chunk
      ppf
      d

  let pp_game_state ppf game_state =
    let open Format in
    match game_state with
    | Dissecting {dissection; default_number_of_sections} ->
        fprintf
          ppf
          "Dissecting %a using %d number of sections"
          pp_dissection
          dissection
          default_number_of_sections
    | Final_move {agreed_start_chunk; refuted_stop_chunk} ->
        fprintf
          ppf
          "Final move to refute %a from %a, opponent failed to refute"
          pp_dissection_chunk
          agreed_start_chunk
          pp_dissection_chunk
          refuted_stop_chunk

  let pp ppf game =
    Format.fprintf
      ppf
      "%a playing; inbox snapshot = %a; level = %a; pvm_name = %s; game_state \
       = %a"
      pp_player
      game.turn
      Sc_rollup_inbox_repr.pp_history_proof
      game.inbox_snapshot
      Raw_level_repr.pp
      game.level
      game.pvm_name
      pp_game_state
      game.game_state
end

type versioned = V1 of V1.t

let versioned_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"V1"
        (Tag 0)
        V1.encoding
        (function V1 game -> Some game)
        (fun game -> V1 game);
    ]

include V1

let of_versioned = function V1 game -> game [@@inline]

let to_versioned game = V1 game [@@inline]

module Index = struct
  type t = {alice : Staker.t; bob : Staker.t}

  let make a b =
    let alice, bob =
      if Compare.Int.(Staker.compare a b > 0) then (b, a) else (a, b)
    in
    {alice; bob}

  let encoding =
    let open Data_encoding in
    conv
      (fun {alice; bob} -> (alice, bob))
      (fun (alice, bob) -> make alice bob)
      (obj2 (req "alice" Staker.encoding) (req "bob" Staker.encoding))

  let compare {alice = a; bob = b} {alice = c; bob = d} =
    match Staker.compare a c with 0 -> Staker.compare b d | x -> x

  let to_path {alice; bob} p =
    Staker.to_b58check alice :: Staker.to_b58check bob :: p

  let both_of_b58check_opt (a, b) =
    let ( let* ) = Option.bind in
    let* a_staker = Staker.of_b58check_opt a in
    let* b_staker = Staker.of_b58check_opt b in
    Some (make a_staker b_staker)

  let of_path = function [a; b] -> both_of_b58check_opt (a, b) | _ -> None

  let path_length = 2

  let rpc_arg =
    let descr =
      "A pair of stakers that index a smart contract rollup refutation game."
    in
    let construct {alice; bob} =
      Format.sprintf "%s-%s" (Staker.to_b58check alice) (Staker.to_b58check bob)
    in
    let destruct s =
      match String.split_on_char '-' s with
      | [a; b] -> (
          match both_of_b58check_opt (a, b) with
          | Some stakers -> ok stakers
          | None ->
              Result.error (Format.sprintf "Invalid game index notation %s" s))
      | _ -> Result.error (Format.sprintf "Invalid game index notation %s" s)
    in
    RPC_arg.make ~descr ~name:"game_index" ~construct ~destruct ()

  let staker {alice; bob} = function Alice -> alice | Bob -> bob
end

let make_chunk state_hash tick = {state_hash; tick}

let initial inbox ~pvm_name ~(parent : Sc_rollup_commitment_repr.t)
    ~(child : Sc_rollup_commitment_repr.t) ~refuter ~defender
    ~default_number_of_sections =
  let ({alice; _} : Index.t) = Index.make refuter defender in
  let alice_to_play = Staker.equal alice refuter in
  let open Sc_rollup_tick_repr in
  let tick = of_number_of_ticks child.number_of_ticks in
  let game_state =
    Dissecting
      {
        dissection =
          (if equal tick initial then
           [
             make_chunk (Some child.compressed_state) initial;
             make_chunk None (next initial);
           ]
          else
            [
              make_chunk (Some parent.compressed_state) initial;
              make_chunk (Some child.compressed_state) tick;
              make_chunk None (next tick);
            ]);
        default_number_of_sections;
      }
  in

  {
    turn = (if alice_to_play then Alice else Bob);
    inbox_snapshot = inbox;
    level = child.inbox_level;
    pvm_name;
    game_state;
  }

type step =
  | Dissection of dissection_chunk list
  | Proof of Sc_rollup_proof_repr.t

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
        Sc_rollup_proof_repr.encoding
        (function Proof p -> Some p | _ -> None)
        (fun p -> Proof p);
    ]

let pp_step ppf step =
  match step with
  | Dissection states ->
      Format.fprintf ppf "Dissection:@ " ;
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n\n")
        (fun ppf {state_hash; tick} ->
          Format.fprintf
            ppf
            "Tick: %a,@ State: %a\n"
            Sc_rollup_tick_repr.pp
            tick
            (Format.pp_print_option State_hash.pp)
            state_hash)
        ppf
        states
  | Proof proof -> Format.fprintf ppf "proof: %a" Sc_rollup_proof_repr.pp proof

type refutation = {choice : Sc_rollup_tick_repr.t; step : step}

let pp_refutation ppf {choice; step} =
  Format.fprintf
    ppf
    "Tick: %a@ Step: %a"
    Sc_rollup_tick_repr.pp
    choice
    pp_step
    step

let refutation_encoding =
  let open Data_encoding in
  conv
    (fun {choice; step} -> (choice, step))
    (fun (choice, step) -> {choice; step})
    (obj2
       (req "choice" Sc_rollup_tick_repr.encoding)
       (req "step" step_encoding))

type invalid_move =
  | Dissection_choice_not_found of Sc_rollup_tick_repr.t
  | Dissection_number_of_sections_mismatch of {expected : Z.t; given : Z.t}
  | Dissection_invalid_number_of_sections of Z.t
  | Dissection_start_hash_mismatch of {
      expected : State_hash.t option;
      given : State_hash.t option;
    }
  | Dissection_stop_hash_mismatch of State_hash.t option
  | Dissection_edge_ticks_mismatch of {
      dissection_start_tick : Sc_rollup_tick_repr.t;
      dissection_stop_tick : Sc_rollup_tick_repr.t;
      chunk_start_tick : Sc_rollup_tick_repr.t;
      chunk_stop_tick : Sc_rollup_tick_repr.t;
    }
  | Dissection_ticks_not_increasing
  | Dissection_invalid_distribution
  | Dissection_invalid_successive_states_shape
  | Proof_unexpected_section_size of Z.t
  | Proof_start_state_hash_mismatch of {
      start_state_hash : State_hash.t option;
      start_proof : State_hash.t;
    }
  | Proof_stop_state_hash_failed_to_refute of {
      stop_state_hash : State_hash.t option;
      stop_proof : State_hash.t option;
    }
  | Proof_stop_state_hash_failed_to_validate of {
      stop_state_hash : State_hash.t option;
      stop_proof : State_hash.t option;
    }
  | Proof_invalid of string

let pp_invalid_move fmt =
  let pp_hash_opt fmt = function
    | None -> Format.fprintf fmt "None"
    | Some x -> State_hash.pp fmt x
  in
  function
  | Dissection_choice_not_found tick ->
      Format.fprintf
        fmt
        "No section starting with tick %a found"
        Sc_rollup_tick_repr.pp
        tick
  | Dissection_number_of_sections_mismatch {expected; given} ->
      Format.fprintf
        fmt
        "The number of sections must be equal to %a instead of %a"
        Z.pp_print
        expected
        Z.pp_print
        given
  | Dissection_invalid_number_of_sections n ->
      Format.fprintf
        fmt
        "A dissection with %a sections can never be valid"
        Z.pp_print
        n
  | Dissection_start_hash_mismatch {given = None; _} ->
      Format.fprintf fmt "The start hash must not be None"
  | Dissection_start_hash_mismatch {given; expected} ->
      Format.fprintf
        fmt
        "The start hash should be equal to %a, but the provided hash is %a"
        pp_hash_opt
        expected
        pp_hash_opt
        given
  | Dissection_stop_hash_mismatch h ->
      Format.fprintf fmt "The stop hash should not be equal to %a" pp_hash_opt h
  | Dissection_edge_ticks_mismatch
      {
        dissection_start_tick;
        dissection_stop_tick;
        chunk_start_tick;
        chunk_stop_tick;
      } ->
      Sc_rollup_tick_repr.(
        Format.fprintf
          fmt
          "We should have dissection_start_tick(%a) = %a and \
           dissection_stop_tick(%a) = %a"
          pp
          dissection_start_tick
          pp
          chunk_start_tick
          pp
          dissection_stop_tick
          pp
          chunk_stop_tick)
  | Dissection_ticks_not_increasing ->
      Format.fprintf fmt "Ticks should only increase in dissection"
  | Dissection_invalid_successive_states_shape ->
      Format.fprintf
        fmt
        "Cannot return to a Some state after being at a None state"
  | Dissection_invalid_distribution ->
      Format.fprintf
        fmt
        "Maximum tick increment in a section cannot be more than half total \
         dissection length"
  | Proof_unexpected_section_size n ->
      Format.fprintf
        fmt
        "dist should be equal to 1 in a proof, but got %a"
        Z.pp_print
        n
  | Proof_start_state_hash_mismatch {start_state_hash; start_proof} ->
      Format.fprintf
        fmt
        "start(%a) should be equal to start_proof(%a)"
        pp_hash_opt
        start_state_hash
        State_hash.pp
        start_proof
  | Proof_stop_state_hash_failed_to_refute {stop_state_hash; stop_proof} ->
      Format.fprintf
        fmt
        "Trying to refute %a, the stop_proof must not be equal to %a"
        pp_hash_opt
        stop_state_hash
        pp_hash_opt
        stop_proof
  | Proof_stop_state_hash_failed_to_validate {stop_state_hash; stop_proof} ->
      Format.fprintf
        fmt
        "Trying to validate %a, the stop_proof must be equal to %a"
        pp_hash_opt
        stop_state_hash
        pp_hash_opt
        stop_proof
  | Proof_invalid s -> Format.fprintf fmt "Invalid proof: %s" s

let invalid_move_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"sc_rollup_dissection_choice_not_found"
        (Tag 0)
        (obj2
           (req "kind" (constant "dissection_choice_not_found"))
           (req "tick" Sc_rollup_tick_repr.encoding))
        (function
          | Dissection_choice_not_found tick -> Some ((), tick) | _ -> None)
        (fun ((), tick) -> Dissection_choice_not_found tick);
      case
        ~title:"sc_rollup_dissection_number_of_sections_mismatch"
        (Tag 1)
        (obj3
           (req "kind" (constant "dissection_number_of_sections_mismatch"))
           (req "expected" n)
           (req "given" n))
        (function
          | Dissection_number_of_sections_mismatch {expected; given} ->
              Some ((), expected, given)
          | _ -> None)
        (fun ((), expected, given) ->
          Dissection_number_of_sections_mismatch {expected; given});
      case
        ~title:"sc_rollup_dissection_invalid_number_of_sections"
        (Tag 2)
        (obj2
           (req "kind" (constant "dissection_invalid_number_of_sections"))
           (req "value" n))
        (function
          | Dissection_invalid_number_of_sections value -> Some ((), value)
          | _ -> None)
        (fun ((), value) -> Dissection_invalid_number_of_sections value);
      case
        ~title:"sc_rollup_dissection_unexpected_start_hash"
        (Tag 3)
        (obj3
           (req "kind" (constant "dissection_unexpected_start_hash"))
           (req "expected" (option State_hash.encoding))
           (req "given" (option State_hash.encoding)))
        (function
          | Dissection_start_hash_mismatch {expected; given} ->
              Some ((), expected, given)
          | _ -> None)
        (fun ((), expected, given) ->
          Dissection_start_hash_mismatch {expected; given});
      case
        ~title:"sc_rollup_dissection_stop_hash_mismatch"
        (Tag 4)
        (obj2
           (req "kind" (constant "dissection_stop_hash_mismatch"))
           (req "hash" (option State_hash.encoding)))
        (function
          | Dissection_stop_hash_mismatch hopt -> Some ((), hopt) | _ -> None)
        (fun ((), hopt) -> Dissection_stop_hash_mismatch hopt);
      case
        ~title:"sc_rollup_dissection_edge_ticks_mismatch"
        (Tag 5)
        (obj5
           (req "kind" (constant "dissection_edge_ticks_mismatch"))
           (req "dissection_start_tick" Sc_rollup_tick_repr.encoding)
           (req "dissection_stop_tick" Sc_rollup_tick_repr.encoding)
           (req "chunk_start_tick" Sc_rollup_tick_repr.encoding)
           (req "chunk_stop_tick" Sc_rollup_tick_repr.encoding))
        (function
          | Dissection_edge_ticks_mismatch e ->
              Some
                ( (),
                  e.dissection_start_tick,
                  e.dissection_stop_tick,
                  e.chunk_start_tick,
                  e.chunk_stop_tick )
          | _ -> None)
        (fun ( (),
               dissection_start_tick,
               dissection_stop_tick,
               chunk_start_tick,
               chunk_stop_tick ) ->
          Dissection_edge_ticks_mismatch
            {
              dissection_start_tick;
              dissection_stop_tick;
              chunk_start_tick;
              chunk_stop_tick;
            });
      case
        ~title:"sc_rollup_dissection_ticks_not_increasing"
        (Tag 6)
        (obj1 (req "kind" (constant "dissection_ticks_not_increasing")))
        (function Dissection_ticks_not_increasing -> Some () | _ -> None)
        (fun () -> Dissection_ticks_not_increasing);
      case
        ~title:"sc_rollup_dissection_invalid_distribution"
        (Tag 7)
        (obj1 (req "kind" (constant "dissection_invalid_distribution")))
        (function Dissection_invalid_distribution -> Some () | _ -> None)
        (fun () -> Dissection_invalid_distribution);
      case
        ~title:"sc_rollup_dissection_invalid_successive_states_shape"
        (Tag 8)
        (obj1
           (req "kind" (constant "dissection_invalid_successive_states_shape")))
        (function
          | Dissection_invalid_successive_states_shape -> Some () | _ -> None)
        (fun () -> Dissection_invalid_successive_states_shape);
      case
        ~title:"sc_rollup_proof_unexpected_section_size"
        (Tag 9)
        (obj2
           (req "kind" (constant "proof_unexpected_section_size"))
           (req "value" n))
        (function Proof_unexpected_section_size n -> Some ((), n) | _ -> None)
        (fun ((), n) -> Proof_unexpected_section_size n);
      case
        ~title:"sc_rollup_proof_start_state_hash_mismatch"
        (Tag 10)
        (obj3
           (req "kind" (constant "proof_start_state_hash_mismatch"))
           (req "start_state_hash" (option State_hash.encoding))
           (req "start_proof" State_hash.encoding))
        (function
          | Proof_start_state_hash_mismatch e ->
              Some ((), e.start_state_hash, e.start_proof)
          | _ -> None)
        (fun ((), start_state_hash, start_proof) ->
          Proof_start_state_hash_mismatch {start_state_hash; start_proof});
      case
        ~title:"sc_rollup_proof_stop_state_hash_failed_to_refute"
        (Tag 11)
        (obj3
           (req "kind" (constant "proof_stop_state_hash_failed_to_refute"))
           (req "stop_state_hash" (option State_hash.encoding))
           (req "stop_proof" (option State_hash.encoding)))
        (function
          | Proof_stop_state_hash_failed_to_refute e ->
              Some ((), e.stop_state_hash, e.stop_proof)
          | _ -> None)
        (fun ((), stop_state_hash, stop_proof) ->
          Proof_stop_state_hash_failed_to_refute {stop_state_hash; stop_proof});
      case
        ~title:"sc_rollup_proof_stop_state_hash_failed_to_validate"
        (Tag 12)
        (obj3
           (req "kind" (constant "proof_stop_state_hash_failed_to_validate"))
           (req "stop_state_hash" (option State_hash.encoding))
           (req "stop_proof" (option State_hash.encoding)))
        (function
          | Proof_stop_state_hash_failed_to_validate e ->
              Some ((), e.stop_state_hash, e.stop_proof)
          | _ -> None)
        (fun ((), stop_state_hash, stop_proof) ->
          Proof_stop_state_hash_failed_to_validate {stop_state_hash; stop_proof});
      case
        ~title:"sc_rollup_proof_invalid"
        (Tag 13)
        (obj2 (req "kind" (constant "proof_invalid")) (req "message" string))
        (function Proof_invalid s -> Some ((), s) | _ -> None)
        (fun ((), s) -> Proof_invalid s);
    ]

type reason = Conflict_resolved | Invalid_move of invalid_move | Timeout

let pp_reason ppf reason =
  match reason with
  | Conflict_resolved -> Format.fprintf ppf "conflict resolved"
  | Invalid_move mv -> Format.fprintf ppf "invalid move(%a)" pp_invalid_move mv
  | Timeout -> Format.fprintf ppf "timeout"

let reason_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Conflict_resolved"
        (Tag 0)
        (constant "conflict_resolved")
        (function Conflict_resolved -> Some () | _ -> None)
        (fun () -> Conflict_resolved);
      case
        ~title:"Invalid_move"
        (Tag 1)
        invalid_move_encoding
        (function Invalid_move reason -> Some reason | _ -> None)
        (fun s -> Invalid_move s);
      case
        ~title:"Timeout"
        (Tag 2)
        (constant "timeout")
        (function Timeout -> Some () | _ -> None)
        (fun () -> Timeout);
    ]

type game_result = Loser of {reason : reason; loser : Staker.t} | Draw

let pp_game_result ppf r =
  let open Format in
  match r with
  | Loser {reason; loser} ->
      fprintf ppf "%a lost because: %a" Staker.pp loser pp_reason reason
  | Draw -> fprintf ppf "Draw"

let game_result_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Loser"
        (Tag 0)
        (obj3
           (req "kind" (constant "loser"))
           (req "reason" reason_encoding)
           (req "player" Staker.encoding))
        (function
          | Loser {reason; loser} -> Some ((), reason, loser) | _ -> None)
        (fun ((), reason, loser) -> Loser {reason; loser});
      case
        ~title:"Draw"
        (Tag 1)
        (obj1 (req "kind" (constant "draw")))
        (function Draw -> Some () | _ -> None)
        (fun () -> Draw);
    ]

type status = Ongoing | Ended of game_result

let pp_status ppf status =
  match status with
  | Ongoing -> Format.fprintf ppf "Game ongoing"
  | Ended game_result ->
      Format.fprintf ppf "Game ended: %a" pp_game_result game_result

let status_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Ongoing"
        (Tag 0)
        (constant "ongoing")
        (function Ongoing -> Some () | _ -> None)
        (fun () -> Ongoing);
      case
        ~title:"Ended"
        (Tag 1)
        (obj1 (req "result" game_result_encoding))
        (function Ended r -> Some r | _ -> None)
        (fun r -> Ended r);
    ]

let invalid_move reason =
  let open Lwt_result_syntax in
  fail (Invalid_move reason)

let find_choice dissection tick =
  let open Lwt_result_syntax in
  let rec traverse states =
    match states with
    | ({state_hash = _; tick = state_tick} as curr) :: next :: others ->
        if Sc_rollup_tick_repr.(tick = state_tick) then return (curr, next)
        else traverse (next :: others)
    | _ -> invalid_move (Dissection_choice_not_found tick)
  in
  traverse dissection

let check pred reason =
  let open Lwt_result_syntax in
  if pred then return () else invalid_move reason

let check_dissection ~default_number_of_sections ~start_chunk ~stop_chunk
    dissection =
  let open Lwt_result_syntax in
  let len = Z.of_int @@ List.length dissection in
  let dist = Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick in
  let should_be_equal_to expected =
    Dissection_number_of_sections_mismatch {expected; given = len}
  in
  let num_sections = Z.of_int @@ default_number_of_sections in
  let* () =
    if Z.geq dist num_sections then
      check Z.(equal len num_sections) (should_be_equal_to num_sections)
    else if Z.(gt dist one) then
      check Z.(equal len (succ dist)) (should_be_equal_to Z.(succ dist))
    else invalid_move (Dissection_invalid_number_of_sections len)
  in
  let* () =
    match (List.hd dissection, List.last_opt dissection) with
    | Some {state_hash = a; tick = a_tick}, Some {state_hash = b; tick = b_tick}
      ->
        let* () =
          check
            (Option.equal State_hash.equal a start_chunk.state_hash
            && not (Option.is_none a))
            (Dissection_start_hash_mismatch
               {expected = start_chunk.state_hash; given = a})
        in
        let* () =
          check
            (not (Option.equal State_hash.equal b stop_chunk.state_hash))
            ((* If the [b] state is equal to [stop_chunk], that means we
                agree on the after state of the section. But, we're trying
                to dispute it, it doesn't make sense. *)
               Dissection_stop_hash_mismatch
               stop_chunk.state_hash)
        in
        Sc_rollup_tick_repr.(
          check
            (a_tick = start_chunk.tick && b_tick = stop_chunk.tick)
            (Dissection_edge_ticks_mismatch
               {
                 dissection_start_tick = a_tick;
                 dissection_stop_tick = b_tick;
                 chunk_start_tick = start_chunk.tick;
                 chunk_stop_tick = stop_chunk.tick;
               }))
    | _ ->
        (* This case is probably already handled by the
           [Dissection_invalid_number_of_sections] returned above *)
        invalid_move (Dissection_invalid_number_of_sections len)
  in
  let half_dist = Z.(div dist (of_int 2) |> succ) in
  let rec traverse states =
    match states with
    | {state_hash = None; _} :: {state_hash = Some _; _} :: _ ->
        invalid_move Dissection_invalid_successive_states_shape
    | {tick; _} :: ({tick = next_tick; state_hash = _} as next) :: others ->
        if Sc_rollup_tick_repr.(tick < next_tick) then
          let incr = Sc_rollup_tick_repr.distance tick next_tick in
          if Z.(leq incr half_dist) then traverse (next :: others)
          else invalid_move Dissection_invalid_distribution
        else invalid_move Dissection_ticks_not_increasing
    | _ -> return ()
  in
  traverse dissection

(** Check that the chosen interval is a single tick. *)
let check_proof_distance_is_one ~start_tick ~stop_tick =
  let dist = Sc_rollup_tick_repr.distance start_tick stop_tick in
  check Z.(equal dist one) (Proof_unexpected_section_size dist)

(** Check the proof begins with the correct state. *)
let check_proof_start_state ~start_state proof =
  let start_proof = Sc_rollup_proof_repr.start proof in
  check
    (Option.equal State_hash.equal start_state (Some start_proof))
    (Proof_start_state_hash_mismatch
       {start_state_hash = start_state; start_proof})

(** Check the proof stops with a different state than refuted one. *)
let check_proof_stop_state ~stop_state input_given
    (input_request : Sc_rollup_PVM_sig.input_request) proof validate =
  let stop_proof =
    match (input_given, input_request) with
    | None, No_input_required
    | Some _, Initial
    | Some _, First_after _
    | Some _, Needs_reveal _ ->
        Some (Sc_rollup_proof_repr.stop proof)
    | Some _, No_input_required
    | None, Initial
    | None, First_after _
    | None, Needs_reveal _ ->
        None
  in
  check
    (let b = Option.equal State_hash.equal stop_state stop_proof in
     if validate then b else not b)
    (if validate then
     Proof_stop_state_hash_failed_to_validate
       {stop_state_hash = stop_state; stop_proof}
    else
      Proof_stop_state_hash_failed_to_refute
        {stop_state_hash = stop_state; stop_proof})

(** Check the proof validates the stop state. *)
let check_proof_validate_stop_state ~stop_state input input_request proof =
  check_proof_stop_state ~stop_state input input_request proof true

(** Check the proof refutes the stop state. *)
let check_proof_refute_stop_state ~stop_state input input_request proof =
  check_proof_stop_state ~stop_state input input_request proof false

let validity_final_move ~first_move ~proof ~game ~start_chunk ~stop_chunk =
  let open Lwt_result_syntax in
  let*! res =
    let {inbox_snapshot; level; pvm_name; _} = game in
    let*! valid =
      Sc_rollup_proof_repr.valid inbox_snapshot level ~pvm_name proof
    in
    let* () =
      if first_move then
        check_proof_distance_is_one
          ~start_tick:start_chunk.tick
          ~stop_tick:stop_chunk.tick
      else return_unit
    in
    let* () =
      check_proof_start_state ~start_state:start_chunk.state_hash proof
    in
    match valid with
    | Ok (input, input_request) ->
        let* () =
          if first_move then
            check_proof_refute_stop_state
              ~stop_state:stop_chunk.state_hash
              input
              input_request
              proof
          else
            check_proof_validate_stop_state
              ~stop_state:stop_chunk.state_hash
              input
              input_request
              proof
        in
        return_true
    | _ -> return_false
  in
  Lwt.return @@ Result.value ~default:false res

(** Returns the validity of the first final move on top of a dissection.

    It is valid if and only:
    - The distance of the refuted dissection is [1].
    - The proof start on the agreed start state.
    - The proof stop on the state different than the refuted one.
    - The proof is correctly verified.
*)
let validity_first_final_move ~proof ~game ~start_chunk ~stop_chunk =
  validity_final_move ~first_move:true ~proof ~game ~start_chunk ~stop_chunk

(** Returns the validity of the second final move.

    It is valid if and only:
    - The proof start on the agreed start state.
    - The proof stop on the state validates the refuted one.
    - The proof is correctly verified.
*)
let validity_second_final_move ~agreed_start_chunk ~refuted_stop_chunk ~game
    ~proof =
  validity_final_move
    ~first_move:false
    ~proof
    ~game
    ~start_chunk:agreed_start_chunk
    ~stop_chunk:refuted_stop_chunk

let loser_of_results ~alice_result ~bob_result =
  match (alice_result, bob_result) with
  | true, true -> None
  | false, false -> None
  | false, true -> Some Alice
  | true, false -> Some Bob

let play ~stakers game refutation =
  let open Lwt_syntax in
  let mk_loser reason loser =
    let loser = Index.staker stakers loser in
    Either.Left (Loser {loser; reason})
  in
  let* result =
    let open Lwt_result_syntax in
    match (refutation.step, game.game_state) with
    | Dissection states, Dissecting {dissection; default_number_of_sections} ->
        let* start_chunk, stop_chunk =
          find_choice dissection refutation.choice
        in
        let* () =
          check_dissection
            ~default_number_of_sections
            ~start_chunk
            ~stop_chunk
            states
        in
        let new_game_state =
          Dissecting {dissection = states; default_number_of_sections}
        in
        return
          (Either.Right
             {
               turn = opponent game.turn;
               inbox_snapshot = game.inbox_snapshot;
               level = game.level;
               pvm_name = game.pvm_name;
               game_state = new_game_state;
             })
    | Dissection _, Final_move _ ->
        invalid_move
          (Proof_invalid "Final move has started, unexpected dissection")
    | Proof proof, Dissecting {dissection; default_number_of_sections = _} ->
        let* start_chunk, stop_chunk =
          find_choice dissection refutation.choice
        in
        let*! player_result =
          validity_first_final_move ~proof ~game ~start_chunk ~stop_chunk
        in
        if player_result then
          return @@ mk_loser Conflict_resolved (opponent game.turn)
        else
          let new_game_state =
            let agreed_start_chunk = start_chunk in
            let refuted_stop_chunk = stop_chunk in
            Final_move {agreed_start_chunk; refuted_stop_chunk}
          in
          return
            (Either.Right
               {
                 turn = opponent game.turn;
                 inbox_snapshot = game.inbox_snapshot;
                 level = game.level;
                 pvm_name = game.pvm_name;
                 game_state = new_game_state;
               })
    | Proof proof, Final_move {agreed_start_chunk; refuted_stop_chunk} ->
        let*! player_result =
          validity_second_final_move
            ~agreed_start_chunk
            ~refuted_stop_chunk
            ~game
            ~proof
        in
        if player_result then
          (* If we play when the final move started, the opponent provided
             a invalid proof. So if the defender manages to provide a valid
             proof, he wins. *)
          return @@ mk_loser Conflict_resolved (opponent game.turn)
        else return (Either.Left Draw)
  in
  match result with
  | Ok x -> return x
  | Error reason -> return @@ mk_loser reason game.turn

module Internal_for_tests = struct
  let find_choice = find_choice

  let check_dissection = check_dissection
end

type timeout = {alice : int; bob : int; last_turn_level : Raw_level_repr.t}

let timeout_encoding =
  let open Data_encoding in
  conv
    (fun {alice; bob; last_turn_level} -> (alice, bob, last_turn_level))
    (fun (alice, bob, last_turn_level) -> {alice; bob; last_turn_level})
    (obj3
       (req "alice" int31)
       (req "bob" int31)
       (req "last_turn_level" Raw_level_repr.encoding))
