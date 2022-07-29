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

type dissection_chunk = {
  state_hash : State_hash.t option;
  tick : Sc_rollup_tick_repr.t;
}

module V1 = struct
  type t = {
    turn : player;
    inbox_snapshot : Sc_rollup_inbox_repr.history_proof;
    level : Raw_level_repr.t;
    pvm_name : string;
    dissection : dissection_chunk list;
    default_number_of_sections : int;
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

  let equal
      {
        turn = turn1;
        inbox_snapshot = inbox_snapshot1;
        level = level1;
        pvm_name = pvm_name1;
        dissection = dissection1;
        default_number_of_sections = default_number_of_sections1;
      }
      {
        turn = turn2;
        inbox_snapshot = inbox_snapshot2;
        level = level2;
        pvm_name = pvm_name2;
        dissection = dissection2;
        default_number_of_sections = default_number_of_sections2;
      } =
    player_equal turn1 turn2
    && Compare.Int.equal default_number_of_sections1 default_number_of_sections2
    && Sc_rollup_inbox_repr.equal_history_proof inbox_snapshot1 inbox_snapshot2
    && Raw_level_repr.equal level1 level2
    && String.equal pvm_name1 pvm_name2
    && List.equal dissection_chunk_equal dissection1 dissection2

  let string_of_player = function Alice -> "alice" | Bob -> "bob"

  let pp_player ppf player = Format.fprintf ppf "%s" (string_of_player player)

  let opponent = function Alice -> Bob | Bob -> Alice

  let dissection_encoding =
    let open Data_encoding in
    list
      (conv
         (fun {state_hash; tick} -> (state_hash, tick))
         (fun (state_hash, tick) -> {state_hash; tick})
         (obj2
            (opt "state" State_hash.encoding)
            (req "tick" Sc_rollup_tick_repr.encoding)))

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             turn;
             inbox_snapshot;
             level;
             pvm_name;
             dissection;
             default_number_of_sections;
           } ->
        ( turn,
          inbox_snapshot,
          level,
          pvm_name,
          dissection,
          default_number_of_sections ))
      (fun ( turn,
             inbox_snapshot,
             level,
             pvm_name,
             dissection,
             default_number_of_sections ) ->
        {
          turn;
          inbox_snapshot;
          level;
          pvm_name;
          dissection;
          default_number_of_sections;
        })
      (obj6
         (req "turn" player_encoding)
         (req "inbox_snapshot" Sc_rollup_inbox_repr.history_proof_encoding)
         (req "level" Raw_level_repr.encoding)
         (req "pvm_name" string)
         (req "dissection" dissection_encoding)
         (req "default_number_of_sections" uint8))

  let pp_dissection ppf d =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n")
      (fun ppf {state_hash; tick} ->
        Format.fprintf
          ppf
          "  %a @ %a"
          (Format.pp_print_option State_hash.pp)
          state_hash
          Sc_rollup_tick_repr.pp
          tick)
      ppf
      d

  let pp ppf game =
    Format.fprintf
      ppf
      "[%a] %a playing; inbox snapshot = %a; level = %a; pvm_name = %s;"
      pp_dissection
      game.dissection
      pp_player
      game.turn
      Sc_rollup_inbox_repr.pp_history_proof
      game.inbox_snapshot
      Raw_level_repr.pp
      game.level
      game.pvm_name
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
  {
    turn = (if alice_to_play then Alice else Bob);
    inbox_snapshot = inbox;
    level = child.inbox_level;
    pvm_name;
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
      Format.fprintf ppf "dissection:\n" ;
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n\n")
        (fun ppf {state_hash; tick} ->
          Format.fprintf
            ppf
            "tick = %a, state = %a\n"
            Sc_rollup_tick_repr.pp
            tick
            (Format.pp_print_option State_hash.pp)
            state_hash)
        ppf
        states
  | Proof proof -> Format.fprintf ppf "proof: %a" Sc_rollup_proof_repr.pp proof

type refutation = {choice : Sc_rollup_tick_repr.t; step : step}

let pp_refutation ppf refutation =
  Format.fprintf
    ppf
    "Refute from tick %a with %a.\n"
    Sc_rollup_tick_repr.pp
    refutation.choice
    pp_step
    refutation.step

let refutation_encoding =
  let open Data_encoding in
  conv
    (fun {choice; step} -> (choice, step))
    (fun (choice, step) -> {choice; step})
    (obj2
       (req "choice" Sc_rollup_tick_repr.encoding)
       (req "step" step_encoding))

type reason = Conflict_resolved | Invalid_move of string | Timeout

let pp_reason ppf reason =
  Format.fprintf
    ppf
    "%s"
    (match reason with
    | Conflict_resolved -> "conflict resolved"
    | Invalid_move reason -> Format.sprintf "invalid move(%s)" reason
    | Timeout -> "timeout")

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
        string
        (function Invalid_move reason -> Some reason | _ -> None)
        (fun s -> Invalid_move s);
      case
        ~title:"Timeout"
        (Tag 2)
        (constant "timeout")
        (function Timeout -> Some () | _ -> None)
        (fun () -> Timeout);
    ]

type status = Ongoing | Ended of (reason * Staker.t)

let pp_status ppf status =
  match status with
  | Ongoing -> Format.fprintf ppf "Game ongoing"
  | Ended (reason, staker) ->
      Format.fprintf
        ppf
        "Game ended due to %a, %a loses their stake"
        pp_reason
        reason
        Staker.pp
        staker

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
        (obj2 (req "reason" reason_encoding) (req "staker" Staker.encoding))
        (function Ended (r, s) -> Some (r, s) | _ -> None)
        (fun (r, s) -> Ended (r, s));
    ]

type outcome = {loser : player; reason : reason}

let pp_outcome ppf outcome =
  Format.fprintf
    ppf
    "Game outcome: %a - %a has lost.\n"
    pp_reason
    outcome.reason
    pp_player
    outcome.loser

let outcome_encoding =
  let open Data_encoding in
  conv
    (fun {loser; reason} -> (loser, reason))
    (fun (loser, reason) -> {loser; reason})
    (obj2 (req "loser" player_encoding) (req "reason" reason_encoding))

let invalid_move reason =
  let open Lwt_result_syntax in
  fail (Invalid_move reason)

let find_choice game tick =
  let open Lwt_result_syntax in
  let rec traverse states =
    match states with
    | {state_hash = state; tick = state_tick}
      :: ({state_hash = next_state; tick = next_tick} as next)
      :: others ->
        if Sc_rollup_tick_repr.(tick = state_tick) then
          return
            ( {state_hash = state; tick},
              {state_hash = next_state; tick = next_tick} )
        else traverse (next :: others)
    | _ -> invalid_move "This choice was not proposed"
  in
  traverse game.dissection

let check pred reason =
  let open Lwt_result_syntax in
  if pred then return () else invalid_move reason

let check_dissection ~default_number_of_sections ~start_chunk ~stop_chunk
    dissection =
  let open Lwt_result_syntax in
  let len = Z.of_int @@ List.length dissection in
  let dist = Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick in
  let should_be_equal_to what =
    Format.asprintf "The number of sections must be equal to %a" Z.pp_print what
  in
  let num_sections = Z.of_int @@ default_number_of_sections in
  let* () =
    if Z.geq dist num_sections then
      check Z.(equal len num_sections) (should_be_equal_to num_sections)
    else if Z.(gt dist one) then
      check Z.(equal len (succ dist)) (should_be_equal_to Z.(succ dist))
    else
      invalid_move
        (Format.asprintf "Cannot have a dissection of only one section")
  in
  let* () =
    match (List.hd dissection, List.last_opt dissection) with
    | Some {state_hash = a; tick = a_tick}, Some {state_hash = b; tick = b_tick}
      ->
        let* () =
          check
            (Option.equal State_hash.equal a start_chunk.state_hash
            && not (Option.is_none a))
            (match start_chunk.state_hash with
            | None -> "The start hash must not be None"
            | Some start ->
                Format.asprintf
                  "The start hash should be equal to %a"
                  State_hash.pp
                  start)
        in
        let* () =
          check
            (not (Option.equal State_hash.equal b stop_chunk.state_hash))
            (match stop_chunk.state_hash with
            (* If the [b] state is equal to [stop_chunk], that means we
               agree on the after state of the section. But, we're trying
               to dispute it, it doesn't make sense. *)
            | None -> "The stop hash should not be None."
            | Some stop ->
                Format.asprintf
                  "The stop hash should not be equal to %a"
                  State_hash.pp
                  stop)
        in
        Sc_rollup_tick_repr.(
          check
            (a_tick = start_chunk.tick && b_tick = stop_chunk.tick)
            (Format.asprintf
               "We should have section_start_tick(%a) = %a and \
                section_stop_tick(%a) = %a"
               pp
               a_tick
               pp
               start_chunk.tick
               pp
               b_tick
               pp
               stop_chunk.tick))
    | _ -> invalid_move "Dissection should contain at least 2 elements"
  in
  let rec traverse states =
    match states with
    | {state_hash = None; _} :: {state_hash = Some _; _} :: _ ->
        invalid_move "Cannot return to a Some state after being at a None state"
    | {tick; _} :: ({tick = next_tick; state_hash = _} as next) :: others ->
        if Sc_rollup_tick_repr.(tick < next_tick) then
          let incr = Sc_rollup_tick_repr.distance tick next_tick in
          if Z.(leq incr (div dist (of_int 2))) then traverse (next :: others)
          else
            invalid_move
              "Maximum tick increment in dissection must be less than half \
               total dissection length"
        else invalid_move "Ticks should only increase in dissection"
    | _ -> return ()
  in
  traverse dissection

(** We check firstly that the interval in question is a single tick.

    Then we check the proof begins with the correct state and ends
    with a different state to the one in the current dissection.

    Note: this does not check the proof itself is valid, just that it
    makes the expected claims about start and stop states. The function
    {!play} below has to call {!Sc_rollup_proof_repr.valid} separately
    to ensure the proof is actually valid. *)
let check_proof_start_stop ~start_chunk ~stop_chunk proof =
  let open Lwt_result_syntax in
  let dist = Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick in
  let* () = check Z.(equal dist one) "dist should be equal to 1" in
  let start_proof = Sc_rollup_proof_repr.start proof in
  let stop_proof = Sc_rollup_proof_repr.stop proof in
  let* () =
    check
      (Option.equal State_hash.equal start_chunk.state_hash (Some start_proof))
      (match start_chunk.state_hash with
      | None -> "Start is absent and should not."
      | Some start ->
          Format.asprintf
            "start(%a) should be equal to start_proof(%a)"
            State_hash.pp
            start
            State_hash.pp
            start_proof)
  in
  let option_pp pp fmt = function
    | None -> Format.fprintf fmt "None"
    | Some x -> pp fmt x
  in
  check
    (not (Option.equal State_hash.equal stop_chunk.state_hash stop_proof))
    (Format.asprintf
       "stop(%a) should not be equal to stop_proof(%a)"
       (option_pp State_hash.pp)
       stop_chunk.state_hash
       (option_pp State_hash.pp)
       stop_proof)

let play game refutation =
  let open Lwt_result_syntax in
  let*! result =
    let* start_chunk, stop_chunk = find_choice game refutation.choice in
    match refutation.step with
    | Dissection states ->
        let* () =
          check_dissection
            ~default_number_of_sections:game.default_number_of_sections
            ~start_chunk
            ~stop_chunk
            states
        in
        return
          (Either.Right
             {
               turn = opponent game.turn;
               inbox_snapshot = game.inbox_snapshot;
               level = game.level;
               pvm_name = game.pvm_name;
               dissection = states;
               default_number_of_sections = game.default_number_of_sections;
             })
    | Proof proof ->
        let* () = check_proof_start_stop ~start_chunk ~stop_chunk proof in
        let {inbox_snapshot; level; pvm_name; _} = game in
        let*! (proof_valid_tzresult : bool tzresult) =
          Sc_rollup_proof_repr.valid inbox_snapshot level ~pvm_name proof
        in
        let* () =
          match proof_valid_tzresult with
          | Ok true -> return ()
          | Ok false -> invalid_move "Invalid proof: no detail given"
          | Error e ->
              invalid_move (Format.asprintf "Invalid proof: %a" pp_trace e)
        in
        return
          (Either.Left {loser = opponent game.turn; reason = Conflict_resolved})
  in
  match result with
  | Ok x -> Lwt.return x
  | Error reason -> Lwt.return @@ Either.Left {loser = game.turn; reason}

module Internal_for_tests = struct
  let find_choice = find_choice

  let check_dissection = check_dissection
end
