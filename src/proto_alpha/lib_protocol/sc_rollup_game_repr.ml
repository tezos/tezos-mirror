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

(** {2 Refutation game errors} *)

type error +=
  | (* `Temporary *)
      Dissection_choice_not_found of Sc_rollup_tick_repr.t
  | (* `Permanent *) Proof_unexpected_section_size of Z.t
  | (* `Temporary *)
      Proof_start_state_hash_mismatch of {
      start_state_hash : Sc_rollup_repr.State_hash.t option;
      start_proof : Sc_rollup_repr.State_hash.t;
    }
  | (* `Temporary *)
      Proof_stop_state_hash_failed_to_refute of {
      stop_state_hash : Sc_rollup_repr.State_hash.t option;
      stop_proof : Sc_rollup_repr.State_hash.t option;
    }
  | (* `Temporary *)
      Proof_stop_state_hash_failed_to_validate of {
      stop_state_hash : Sc_rollup_repr.State_hash.t option;
      stop_proof : Sc_rollup_repr.State_hash.t option;
    }
  | (* `Temporary *) Dissecting_during_final_move

let pp_hash_opt fmt = function
  | None -> Format.fprintf fmt "None"
  | Some x -> Sc_rollup_repr.State_hash.pp fmt x

let () =
  let description = "Dissection choice not found" in
  register_error_kind
    `Temporary
    ~id:"Dissection_choice_not_found"
    ~title:description
    ~description
    ~pp:(fun ppf choice ->
      Format.fprintf
        ppf
        "No section starting with tick %a found"
        Sc_rollup_tick_repr.pp
        choice)
    Data_encoding.(obj1 (req "choice" Sc_rollup_tick_repr.encoding))
    (function Dissection_choice_not_found tick -> Some tick | _ -> None)
    (fun tick -> Dissection_choice_not_found tick) ;
  let description = "The distance for a proof should be equal to 1" in
  register_error_kind
    `Permanent
    ~id:"Dissection_unexpected_section_size"
    ~title:description
    ~description
    ~pp:(fun ppf n ->
      Format.fprintf
        ppf
        "Distance should be equal to 1 in a proof, but got %a"
        Z.pp_print
        n)
    Data_encoding.(obj1 (req "n" n))
    (function Proof_unexpected_section_size n -> Some n | _ -> None)
    (fun n -> Proof_unexpected_section_size n) ;
  let description = "The start state hash of the proof is invalid" in
  register_error_kind
    `Temporary
    ~id:"Proof_start_state_hash_mismatch"
    ~title:description
    ~description
    ~pp:(fun ppf (start_state_hash, start_proof) ->
      Format.fprintf
        ppf
        "start(%a) should be equal to start_proof(%a)"
        pp_hash_opt
        start_state_hash
        Sc_rollup_repr.State_hash.pp
        start_proof)
    Data_encoding.(
      obj2
        (req "start_state_hash" (option Sc_rollup_repr.State_hash.encoding))
        (req "start_proof" Sc_rollup_repr.State_hash.encoding))
    (function
      | Proof_start_state_hash_mismatch {start_state_hash; start_proof} ->
          Some (start_state_hash, start_proof)
      | _ -> None)
    (fun (start_state_hash, start_proof) ->
      Proof_start_state_hash_mismatch {start_state_hash; start_proof}) ;
  let description = "Failed to refute the stop state hash with the proof" in
  register_error_kind
    `Temporary
    ~id:"Proof_stop_state_hash_failed_to_refute"
    ~title:description
    ~description
    ~pp:(fun ppf (stop_state_hash, stop_proof) ->
      Format.fprintf
        ppf
        "Trying to refute %a, the stop_proof must not be equal to %a"
        pp_hash_opt
        stop_state_hash
        pp_hash_opt
        stop_proof)
    Data_encoding.(
      obj2
        (req "stop_state_hash" (option Sc_rollup_repr.State_hash.encoding))
        (req "stop_proof" (option Sc_rollup_repr.State_hash.encoding)))
    (function
      | Proof_stop_state_hash_failed_to_refute {stop_state_hash; stop_proof} ->
          Some (stop_state_hash, stop_proof)
      | _ -> None)
    (fun (stop_state_hash, stop_proof) ->
      Proof_stop_state_hash_failed_to_refute {stop_state_hash; stop_proof}) ;
  let description = "Failed to validate the stop state hash with the proof" in
  register_error_kind
    `Temporary
    ~id:"Proof_stop_state_hash_failed_to_validate"
    ~title:description
    ~description
    ~pp:(fun ppf (stop_state_hash, stop_proof) ->
      Format.fprintf
        ppf
        "Trying to validate %a, the stop_proof must not be equal to %a"
        pp_hash_opt
        stop_state_hash
        pp_hash_opt
        stop_proof)
    Data_encoding.(
      obj2
        (req "stop_state_hash" (option Sc_rollup_repr.State_hash.encoding))
        (req "stop_proof" (option Sc_rollup_repr.State_hash.encoding)))
    (function
      | Proof_stop_state_hash_failed_to_validate {stop_state_hash; stop_proof}
        ->
          Some (stop_state_hash, stop_proof)
      | _ -> None)
    (fun (stop_state_hash, stop_proof) ->
      Proof_stop_state_hash_failed_to_validate {stop_state_hash; stop_proof}) ;
  let description = "Tried to play a dissecting when the final move started" in
  register_error_kind
    `Temporary
    ~id:"Dissecting_during_final_move"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.pp_print_string ppf description)
    Data_encoding.empty
    (function Dissecting_during_final_move -> Some () | _ -> None)
    (fun () -> Dissecting_during_final_move) ;
  ()

type player = Alice | Bob

module V1 = struct
  type dissection_chunk = Sc_rollup_dissection_chunk_repr.t = {
    state_hash : State_hash.t option;
    tick : Sc_rollup_tick_repr.t;
  }

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
    dal_snapshot : Dal_slot_repr.History.t;
    start_level : Raw_level_repr.t;
    inbox_level : Raw_level_repr.t;
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
        && List.equal
             Sc_rollup_dissection_chunk_repr.equal
             dissection1
             dissection2
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
        Sc_rollup_dissection_chunk_repr.equal
          agreed_start_chunk1
          agreed_start_chunk2
        && Sc_rollup_dissection_chunk_repr.equal
             refuted_stop_chunk1
             refuted_stop_chunk2
    | Final_move _, _ -> false

  let equal
      {turn; inbox_snapshot; dal_snapshot; start_level; inbox_level; game_state}
      g2 =
    player_equal turn g2.turn
    && Sc_rollup_inbox_repr.equal_history_proof inbox_snapshot g2.inbox_snapshot
    && Dal_slot_repr.History.equal dal_snapshot g2.dal_snapshot
    && Raw_level_repr.equal start_level g2.start_level
    && Raw_level_repr.equal inbox_level g2.inbox_level
    && game_state_equal game_state g2.game_state

  let string_of_player = function Alice -> "alice" | Bob -> "bob"

  let pp_player ppf player =
    Format.pp_print_string ppf (string_of_player player)

  let opponent = function Alice -> Bob | Bob -> Alice

  let dissection_encoding =
    let open Data_encoding in
    list Sc_rollup_dissection_chunk_repr.encoding

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
             (req "agreed_start_chunk" Sc_rollup_dissection_chunk_repr.encoding)
             (req "refuted_stop_chunk" Sc_rollup_dissection_chunk_repr.encoding))
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
           } ->
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
             game_state ) ->
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
         (req "inbox_snapshot" Sc_rollup_inbox_repr.history_proof_encoding)
         (req "dal_snapshot" Dal_slot_repr.History.encoding)
         (req "start_level" Raw_level_repr.encoding)
         (req "inbox_level" Raw_level_repr.encoding)
         (req "game_state" game_state_encoding))

  let pp_dissection ppf d =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n")
      Sc_rollup_dissection_chunk_repr.pp
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
          Sc_rollup_dissection_chunk_repr.pp
          agreed_start_chunk
          Sc_rollup_dissection_chunk_repr.pp
          refuted_stop_chunk

  let pp ppf game =
    Format.fprintf
      ppf
      "%a playing; inbox snapshot = %a; start level = %a; inbox level = %a; \
       game_state = %a"
      pp_player
      game.turn
      Sc_rollup_inbox_repr.pp_history_proof
      game.inbox_snapshot
      Raw_level_repr.pp
      game.start_level
      Raw_level_repr.pp
      game.inbox_level
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
      "A pair of stakers that index a smart rollup refutation game."
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

let initial inbox dal_snapshot ~start_level
    ~(parent_commitment : Sc_rollup_commitment_repr.t)
    ~(defender_commitment : Sc_rollup_commitment_repr.t) ~refuter ~defender
    ~default_number_of_sections =
  let ({alice; _} : Index.t) = Index.make refuter defender in
  let alice_to_play = Staker.equal alice refuter in
  let open Sc_rollup_tick_repr in
  let tick = of_number_of_ticks defender_commitment.number_of_ticks in
  let game_state =
    Dissecting
      {
        dissection =
          [
            make_chunk (Some parent_commitment.compressed_state) initial;
            make_chunk (Some defender_commitment.compressed_state) tick;
            make_chunk None (next tick);
          ];
        default_number_of_sections;
      }
  in

  {
    turn = (if alice_to_play then Alice else Bob);
    inbox_snapshot = inbox;
    dal_snapshot;
    start_level;
    inbox_level = defender_commitment.inbox_level;
    game_state;
  }

type step =
  | Dissection of dissection_chunk list
  | Proof of Sc_rollup_proof_repr.serialized Sc_rollup_proof_repr.t

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

type refutation =
  | Start of {
      player_commitment_hash : Sc_rollup_commitment_repr.Hash.t;
      opponent_commitment_hash : Sc_rollup_commitment_repr.Hash.t;
    }
  | Move of {choice : Sc_rollup_tick_repr.t; step : step}

let pp_refutation ppf = function
  | Start {player_commitment_hash; opponent_commitment_hash} ->
      Format.fprintf
        ppf
        "Start game between commitment hashes %a and %a"
        Sc_rollup_commitment_repr.Hash.pp
        player_commitment_hash
        Sc_rollup_commitment_repr.Hash.pp
        opponent_commitment_hash
  | Move {choice; step} ->
      Format.fprintf
        ppf
        "Tick: %a@ Step: %a"
        Sc_rollup_tick_repr.pp
        choice
        pp_step
        step

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
           (req
              "player_commitment_hash"
              Sc_rollup_commitment_repr.Hash.encoding)
           (req
              "opponent_commitment_hash"
              Sc_rollup_commitment_repr.Hash.encoding))
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
           (req "choice" Sc_rollup_tick_repr.encoding)
           (req "step" step_encoding))
        (function Move {choice; step} -> Some ((), choice, step) | _ -> None)
        (fun ((), choice, step) -> Move {choice; step});
    ]

type reason = Conflict_resolved | Timeout

let pp_reason ppf reason =
  match reason with
  | Conflict_resolved -> Format.fprintf ppf "conflict resolved"
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
        ~title:"Timeout"
        (Tag 1)
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

let find_choice dissection tick =
  let open Result_syntax in
  let rec traverse states =
    match states with
    | ({state_hash = _; tick = state_tick} as curr) :: next :: others ->
        if Sc_rollup_tick_repr.(tick = state_tick) then return (curr, next)
        else traverse (next :: others)
    | _ -> tzfail (Dissection_choice_not_found tick)
  in
  traverse dissection

(** Check that the chosen interval is a single tick. *)
let check_proof_distance_is_one ~start_tick ~stop_tick =
  let dist = Sc_rollup_tick_repr.distance start_tick stop_tick in
  error_unless Z.(equal dist one) (Proof_unexpected_section_size dist)

(** Check the proof begins with the correct state. *)
let check_proof_start_state ~pvm ~start_state proof =
  let start_proof = Sc_rollup_proof_repr.start_of_pvm_step ~pvm proof in
  error_unless
    (Option.equal State_hash.equal start_state (Some start_proof))
    (Proof_start_state_hash_mismatch
       {start_state_hash = start_state; start_proof})

(** Check the proof stops with a different state than refuted one. *)
let check_proof_stop_state ~pvm ~stop_state input_given
    (input_request : Sc_rollup_PVM_sig.input_request) proof validate =
  let stop_proof =
    match (input_given, input_request) with
    | None, No_input_required
    | Some _, Initial
    | Some _, First_after _
    | Some _, Needs_reveal _ ->
        Some (Sc_rollup_proof_repr.stop_of_pvm_step ~pvm proof)
    | Some _, No_input_required
    | None, Initial
    | None, First_after _
    | None, Needs_reveal _ ->
        None
  in
  error_unless
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

(** Returns the validity of the first final move on top of a dissection. *)
let validity_final_move ~pvm ~dal_parameters ~dal_attestation_lag ~first_move
    ~metadata ~proof ~game ~start_chunk ~stop_chunk ~is_reveal_enabled =
  let open Lwt_result_syntax in
  let*! res =
    let {inbox_snapshot; inbox_level; dal_snapshot; _} = game in
    let*! valid =
      (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3997
         This function is not resilient to dal parameters changes
         (cryptobox parameters or dal_attestation_lag for instance). *)
      Sc_rollup_proof_repr.valid
        ~pvm
        ~metadata
        inbox_snapshot
        inbox_level
        dal_snapshot
        dal_parameters
        ~dal_attestation_lag
        ~is_reveal_enabled
        proof
    in
    let*? () =
      if first_move then
        check_proof_distance_is_one
          ~start_tick:start_chunk.tick
          ~stop_tick:stop_chunk.tick
      else ok ()
    in
    let*? () =
      check_proof_start_state
        ~pvm
        ~start_state:start_chunk.state_hash
        proof.pvm_step
    in
    match valid with
    | Ok (input, input_request) ->
        let*? () =
          if first_move then
            check_proof_refute_stop_state
              ~pvm
              ~stop_state:stop_chunk.state_hash
              input
              input_request
              proof.pvm_step
          else
            check_proof_validate_stop_state
              ~pvm
              ~stop_state:stop_chunk.state_hash
              input
              input_request
              proof.pvm_step
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
let validity_first_final_move ~pvm ~dal_parameters ~dal_attestation_lag
    ~metadata ~proof ~game ~start_chunk ~stop_chunk =
  validity_final_move
    ~pvm
    ~dal_parameters
    ~dal_attestation_lag
    ~first_move:true
    ~metadata
    ~proof
    ~game
    ~start_chunk
    ~stop_chunk

(** Returns the validity of the second final move.

    It is valid if and only:
    - The proof start on the agreed start state.
    - The proof stop on the state validates the refuted one.
    - The proof is correctly verified.
*)
let validity_second_final_move ~pvm ~dal_parameters ~dal_attestation_lag
    ~metadata ~agreed_start_chunk ~refuted_stop_chunk ~game ~proof =
  validity_final_move
    ~pvm
    ~dal_parameters
    ~dal_attestation_lag
    ~first_move:false
    ~metadata
    ~proof
    ~game
    ~start_chunk:agreed_start_chunk
    ~stop_chunk:refuted_stop_chunk

let cost_play ~step ~choice =
  match step with
  | Dissection states ->
      let number_of_states = List.length states in
      let hash_size = State_hash.size in
      let tick_size = Sc_rollup_tick_repr.size_in_bytes choice in
      Sc_rollup_costs.cost_check_dissection
        ~number_of_states
        ~tick_size
        ~hash_size
  | Proof _proof ->
      (*

         Proof verification is complex. We choose to follow a very
         rough overaproximation based on the idea that proof
         verification for both the inbox and the execution step is
         dominated by hash computation.

         Assuming that the worst case is a proof of the maximal
         operation data length, we consider the cost of hashing a
         balanced binary tree of this size (with a maximal size of
         leaves since the hashing of internal nodes can be neglected.

         We also consider the largest tick known. At the time of writing
         this comment, the largest tick is the origination tick of the
         PVM.

         If we assume the following worst-case for origination tick:
         - the origination has been done with a kernel of maximum size, and
         - most of the computation cost is consumed by importing this kernel
           in the PVM,

         We can simply consider, again, that the cost of hashing the imported
         kernel dominates everything else.

         We multiply this number by 10 for extra safety.

         At the time of writing this comment, this leads to 372940
         mgas for the proof wellformedness verification and 372940
         mgas for the cost of executing a tick.

      *)
      let open Saturation_repr in
      (* model N_IBlake2b *)
      (* Approximating 1.120804 x term *)
      let cost_N_IBlake2b size =
        let open Syntax in
        let v0 = safe_int size in
        safe_int 430 + v0 + (v0 lsr 3)
      in
      let overapproximated_hashing_size =
        2 * Constants_repr.max_operation_data_length
      in
      let scale10 x = Saturation_repr.(mul (safe_int 10) x) in
      scale10 @@ Gas_limit_repr.atomic_step_cost
      @@ cost_N_IBlake2b overapproximated_hashing_size

let play kind dal_parameters ~dal_attestation_lag ~stakers metadata game ~step
    ~choice ~is_reveal_enabled =
  let open Lwt_result_syntax in
  let (Packed ((module PVM) as pvm)) = Sc_rollups.Kind.pvm_of kind in
  let mk_loser loser =
    let loser = Index.staker stakers loser in
    Either.Left (Loser {loser; reason = Conflict_resolved})
  in
  match (step, game.game_state) with
  | Dissection states, Dissecting {dissection; default_number_of_sections} ->
      let*? start_chunk, stop_chunk = find_choice dissection choice in
      let*? () =
        PVM.check_dissection
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
             dal_snapshot = game.dal_snapshot;
             start_level = game.start_level;
             inbox_level = game.inbox_level;
             game_state = new_game_state;
           })
  | Dissection _, Final_move _ -> tzfail Dissecting_during_final_move
  | Proof proof, Dissecting {dissection; default_number_of_sections = _} ->
      let*? start_chunk, stop_chunk = find_choice dissection choice in
      let*? pvm_step =
        Sc_rollup_proof_repr.unserialize_pvm_step ~pvm proof.pvm_step
      in
      let proof = {proof with pvm_step} in
      let*! player_result =
        validity_first_final_move
          ~pvm
          ~dal_parameters
          ~dal_attestation_lag
          ~proof
          ~metadata
          ~game
          ~start_chunk
          ~stop_chunk
          ~is_reveal_enabled
      in
      if player_result then return @@ mk_loser (opponent game.turn)
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
               dal_snapshot = game.dal_snapshot;
               start_level = game.start_level;
               inbox_level = game.inbox_level;
               game_state = new_game_state;
             })
  | Proof proof, Final_move {agreed_start_chunk; refuted_stop_chunk} ->
      let*? pvm_step =
        Sc_rollup_proof_repr.unserialize_pvm_step ~pvm proof.pvm_step
      in
      let proof = {proof with pvm_step} in
      let*! player_result =
        validity_second_final_move
          ~pvm
          ~dal_parameters
          ~dal_attestation_lag
          ~metadata
          ~agreed_start_chunk
          ~refuted_stop_chunk
          ~game
          ~proof
          ~is_reveal_enabled
      in
      if player_result then
        (* If we play when the final move started, the opponent provided
           a invalid proof. So if the defender manages to provide a valid
           proof, he wins. *)
        return @@ mk_loser (opponent game.turn)
      else return (Either.Left Draw)

module Internal_for_tests = struct
  let find_choice = find_choice

  let check_dissection ~default_number_of_sections ~start_chunk ~stop_chunk =
    let open Sc_rollup_dissection_chunk_repr in
    let dist = Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick in
    let section_maximum_size = Z.div dist (Z.of_int 2) in
    Sc_rollup_dissection_chunk_repr.(
      default_check
        ~section_maximum_size
        ~check_sections_number:default_check_sections_number
        ~default_number_of_sections
        ~start_chunk
        ~stop_chunk)
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
