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

type t = {
  turn : player;
  inbox_snapshot : Sc_rollup_inbox_repr.t;
  level : Raw_level_repr.t;
  pvm_name : string;
  dissection : (State_hash.t option * Sc_rollup_tick_repr.t) list;
}

let player_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Alice"
        (Tag 0)
        unit
        (function Alice -> Some () | _ -> None)
        (fun () -> Alice);
      case
        ~title:"Bob"
        (Tag 1)
        unit
        (function Bob -> Some () | _ -> None)
        (fun () -> Bob);
    ]

let string_of_player = function Alice -> "alice" | Bob -> "bob"

let pp_player ppf player = Format.fprintf ppf "%s" (string_of_player player)

let opponent = function Alice -> Bob | Bob -> Alice

let encoding =
  let open Data_encoding in
  conv
    (fun {turn; inbox_snapshot; level; pvm_name; dissection} ->
      (turn, inbox_snapshot, level, pvm_name, dissection))
    (fun (turn, inbox_snapshot, level, pvm_name, dissection) ->
      {turn; inbox_snapshot; level; pvm_name; dissection})
    (obj5
       (req "turn" player_encoding)
       (req "inbox_snapshot" Sc_rollup_inbox_repr.encoding)
       (req "level" Raw_level_repr.encoding)
       (req "pvm_name" string)
       (req
          "dissection"
          (list
             (tup2 (option State_hash.encoding) Sc_rollup_tick_repr.encoding))))

let pp_dissection ppf d =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n")
    (fun ppf (state, tick) ->
      Format.fprintf
        ppf
        "  %a @ %a"
        (Format.pp_print_option State_hash.pp)
        state
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
    Sc_rollup_inbox_repr.pp
    game.inbox_snapshot
    Raw_level_repr.pp
    game.level
    game.pvm_name

module Index = struct
  type t = Staker.t * Staker.t

  let encoding = Data_encoding.tup2 Staker.encoding Staker.encoding

  let compare (a, b) (c, d) =
    match Staker.compare a c with 0 -> Staker.compare b d | x -> x

  let to_path (a, b) p = Staker.to_b58check a :: Staker.to_b58check b :: p

  let both_of_b58check_opt (a, b) =
    Option.bind (Staker.of_b58check_opt b) (fun b_staker ->
        Option.bind (Staker.of_b58check_opt a) (fun a_staker ->
            Some (a_staker, b_staker)))

  let of_path = function [a; b] -> both_of_b58check_opt (a, b) | _ -> None

  let path_length = 2

  let rpc_arg =
    let descr =
      "A pair of stakers that index a smart contract rollup refutation game."
    in
    let construct (a, b) =
      Format.sprintf "%s-%s" (Staker.to_b58check a) (Staker.to_b58check b)
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

  let normalize (a, b) =
    match Staker.compare a b with 1 -> (b, a) | _ -> (a, b)

  let staker stakers player =
    let alice, bob = normalize stakers in
    match player with Alice -> alice | Bob -> bob
end

let initial inbox ~pvm_name ~(parent : Sc_rollup_commitment_repr.t)
    ~(child : Sc_rollup_commitment_repr.t) ~refuter ~defender =
  let alice, _ = Index.normalize (refuter, defender) in
  let alice_to_play = Staker.equal alice refuter in
  let tick = Sc_rollup_tick_repr.of_number_of_ticks child.number_of_ticks in
  {
    turn = (if alice_to_play then Alice else Bob);
    inbox_snapshot = inbox;
    level = child.inbox_level;
    pvm_name;
    dissection =
      [
        (Some parent.compressed_state, Sc_rollup_tick_repr.initial);
        (Some child.compressed_state, tick);
        (None, Sc_rollup_tick_repr.next tick);
      ];
  }

type step =
  | Dissection of (State_hash.t option * Sc_rollup_tick_repr.t) list
  | Proof of Sc_rollup_proof_repr.t

let step_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Dissection"
        (Tag 0)
        (list (tup2 (option State_hash.encoding) Sc_rollup_tick_repr.encoding))
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
        (fun ppf (hash, t) ->
          Format.fprintf
            ppf
            "tick = %a, state = %a\n"
            Sc_rollup_tick_repr.pp
            t
            (Format.pp_print_option State_hash.pp)
            hash)
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

type reason = Conflict_resolved | Invalid_move | Timeout

let pp_reason ppf reason =
  Format.fprintf
    ppf
    "%s"
    (match reason with
    | Conflict_resolved -> "conflict resolved"
    | Invalid_move -> "invalid move"
    | Timeout -> "timeout")

let reason_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Conflict_resolved"
        (Tag 0)
        unit
        (function Conflict_resolved -> Some () | _ -> None)
        (fun () -> Conflict_resolved);
      case
        ~title:"Invalid_move"
        (Tag 1)
        unit
        (function Invalid_move -> Some () | _ -> None)
        (fun () -> Invalid_move);
      case
        ~title:"Timeout"
        (Tag 2)
        unit
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
        unit
        (function Ongoing -> Some () | _ -> None)
        (fun () -> Ongoing);
      case
        ~title:"Ended"
        (Tag 1)
        (tup2 reason_encoding Staker.encoding)
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

let find_choice game tick =
  let open Lwt_result_syntax in
  let rec traverse states =
    match states with
    | (state, state_tick) :: (next_state, next_tick) :: others ->
        if Sc_rollup_tick_repr.(tick = state_tick) then
          return (state, tick, next_state, next_tick)
        else traverse ((next_state, next_tick) :: others)
    | _ -> fail ()
  in
  traverse game.dissection

let check pred =
  let open Lwt_result_syntax in
  if pred then return () else fail ()

let check_dissection start start_tick stop stop_tick dissection =
  let open Lwt_result_syntax in
  let len = Z.of_int @@ List.length dissection in
  let dist = Sc_rollup_tick_repr.distance start_tick stop_tick in
  let* _ =
    if Z.(geq dist (of_int 32)) then check Z.(equal len (of_int 32))
    else if Z.(gt dist one) then check Z.(equal len (succ dist))
    else fail ()
  in
  let* _ =
    match (List.hd dissection, List.last_opt dissection) with
    | Some (a, a_tick), Some (b, b_tick) ->
        check
          (Option.equal State_hash.equal a start
          && (not (Option.equal State_hash.equal b stop))
          && Sc_rollup_tick_repr.(a_tick = start_tick && b_tick = stop_tick))
    | _ -> fail ()
  in
  let rec traverse states =
    match states with
    | (Some _, tick) :: (next_state, next_tick) :: others ->
        if Sc_rollup_tick_repr.(tick < next_tick) then
          traverse ((next_state, next_tick) :: others)
        else fail ()
    | (None, _) :: _ :: _ -> fail ()
    | _ -> return ()
  in
  traverse dissection

(** We check firstly that the interval in question is a single tick.
    
    Then we check the proof begins with the correct state and ends
    with a different state to the one in the current dissection.

    Note: this does not check the proof itself is valid, just that it
    makes the expected claims about start and stop states. The function
    [play] below has to call [Sc_rollup_proof_repr.valid] separately
    to ensure the proof is actually valid. *)
let check_proof_start_stop start start_tick stop stop_tick proof =
  let open Lwt_result_syntax in
  let dist = Sc_rollup_tick_repr.distance start_tick stop_tick in
  let* _ = check Z.(equal dist one) in
  let* _ =
    check
    @@ Option.equal
         State_hash.equal
         start
         (Some (Sc_rollup_proof_repr.start proof))
  in
  check
  @@ not (Option.equal State_hash.equal stop (Sc_rollup_proof_repr.stop proof))

let play game refutation =
  let result =
    let open Lwt_result_syntax in
    let* start, start_tick, stop, stop_tick =
      find_choice game refutation.choice
    in
    match refutation.step with
    | Dissection states ->
        let* _ = check_dissection start start_tick stop stop_tick states in
        return
          (Either.Right
             {
               turn = opponent game.turn;
               inbox_snapshot = game.inbox_snapshot;
               level = game.level;
               pvm_name = game.pvm_name;
               dissection = states;
             })
    | Proof proof ->
        let* _ = check_proof_start_stop start start_tick stop stop_tick proof in
        let {inbox_snapshot; level; pvm_name; _} = game in
        let* proof_valid =
          Sc_rollup_proof_repr.valid inbox_snapshot level ~pvm_name proof
        in
        let* _ = check proof_valid in
        return
          (Either.Left {loser = opponent game.turn; reason = Conflict_resolved})
  in
  Lwt.map
    (fun r ->
      match Option.of_result r with
      | Some x -> x
      | None -> Either.Left {loser = game.turn; reason = Invalid_move})
    result
