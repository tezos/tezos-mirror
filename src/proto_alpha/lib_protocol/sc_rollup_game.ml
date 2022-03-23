(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context.Sc_rollup

module Make (PVM : Sc_rollup_PVM_sem.S) = struct
  module PVM = PVM

  type tick = Sc_rollup_tick_repr.t

  module Section = struct
    type section = {
      section_start_state : PVM.hash;
      section_start_at : tick;
      section_stop_state : PVM.hash;
      section_stop_at : tick;
    }

    and dissection = section Sc_rollup_tick_repr.Map.t

    let section_encoding =
      let open Data_encoding in
      conv
        (fun {
               section_start_state;
               section_start_at;
               section_stop_state;
               section_stop_at;
             } ->
          ( section_start_state,
            section_start_at,
            section_stop_state,
            section_stop_at ))
        (fun ( section_start_state,
               section_start_at,
               section_stop_state,
               section_stop_at ) ->
          {
            section_start_state;
            section_start_at;
            section_stop_state;
            section_stop_at;
          })
        (obj4
           (req "section_start_state" State_hash.encoding)
           (req "section_start_at" Sc_rollup_tick_repr.encoding)
           (req "section_stop_state" State_hash.encoding)
           (req "section_stop_at" Sc_rollup_tick_repr.encoding))

    let dissection_encoding =
      let open Data_encoding in
      let open Sc_rollup_tick_repr in
      option
      @@ conv
           (fun map -> List.of_seq @@ Map.to_seq map)
           (fun list -> Map.of_seq @@ List.to_seq list)
           (list @@ tup2 encoding section_encoding)

    let empty_dissection = Sc_rollup_tick_repr.Map.empty

    let add_section section dissection =
      Sc_rollup_tick_repr.Map.add section.section_start_at section dissection

    let find_section section (dissection : dissection) =
      let open Sc_rollup_tick_repr in
      Option.bind
        (Map.find section.section_start_at dissection)
        (fun {section_start_at; section_stop_at; _} ->
          if
            section_start_at = section.section_start_at
            && section_stop_at = section.section_stop_at
          then Some section
          else None)

    let fold_over_dissection f i dissection =
      Sc_rollup_tick_repr.Map.fold f i dissection

    let dissection_cardinal = Sc_rollup_tick_repr.Map.cardinal

    let last_section = Sc_rollup_tick_repr.Map.max_binding_opt

    let pp_section ppf (s : section) =
      Format.fprintf
        ppf
        "( %a ) -- [%a] \n ->\n  ( %a ) -- [%a]\n "
        State_hash.pp
        s.section_start_state
        Sc_rollup_tick_repr.pp
        s.section_start_at
        State_hash.pp
        s.section_stop_state
        Sc_rollup_tick_repr.pp
        s.section_stop_at

    let pp_dissection ppf d =
      let open Sc_rollup_tick_repr in
      let list = List.of_seq @@ Map.to_seq d in
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n\n")
        (fun ppf (key, b) ->
          Format.fprintf ppf "key = %a \n val = %a\n" pp key pp_section b)
        ppf
        list

    let pp_optional_dissection d =
      Format.pp_print_option
        ~none:(fun ppf () ->
          Format.pp_print_text ppf "no dissection at the moment")
        pp_dissection
        d

    let valid_section ({section_start_at; section_stop_at; _} : section) =
      Sc_rollup_tick_repr.(section_stop_at > section_start_at)

    let valid_dissection section dissection =
      let open Sc_rollup_tick_repr in
      let rec traverse last_pos = function
        | Seq.Nil -> section.section_stop_at = last_pos
        | Seq.Cons ((_, v), kvs) ->
            last_pos = v.section_start_at
            && valid_section v
            && traverse v.section_stop_at (kvs ())
      in
      traverse section.section_start_at (Map.to_seq dissection ())
  end

  type player = Committer | Refuter

  let string_of_player = function
    | Committer -> "committer"
    | Refuter -> "refuter"

  let pp_player ppf player = Format.fprintf ppf "%s" (string_of_player player)

  let player_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Commiter"
          (Tag 0)
          string
          (function Committer -> Some "committer" | _ -> None)
          (fun _ -> Committer);
        case
          ~title:"Refuter"
          (Tag 1)
          string
          (function Refuter -> Some "refuter" | _ -> None)
          (fun _ -> Refuter);
      ]

  let opponent = function Committer -> Refuter | Refuter -> Committer

  type t = {
    turn : player;
    start_state : PVM.hash;
    start_at : tick;
    player_stop_state : PVM.hash;
    opponent_stop_state : PVM.hash;
    stop_at : tick;
    current_dissection : Section.dissection option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             turn;
             start_state;
             start_at;
             player_stop_state;
             opponent_stop_state;
             stop_at;
             current_dissection;
           } ->
        ( turn,
          start_state,
          start_at,
          player_stop_state,
          opponent_stop_state,
          stop_at,
          current_dissection ))
      (fun ( turn,
             start_state,
             start_at,
             player_stop_state,
             opponent_stop_state,
             stop_at,
             current_dissection ) ->
        {
          turn;
          start_state;
          start_at;
          player_stop_state;
          opponent_stop_state;
          stop_at;
          current_dissection;
        })
      (obj7
         (req "turn" player_encoding)
         (req "start_state" State_hash.encoding)
         (req "start_at" Sc_rollup_tick_repr.encoding)
         (req "player_stop_state" State_hash.encoding)
         (req "oponent_stop_state" State_hash.encoding)
         (req "stop_at" Sc_rollup_tick_repr.encoding)
         (req "current_dissection" Section.dissection_encoding))

  type conflict_resolution_step =
    | Refine of {stop_state : PVM.hash; next_dissection : Section.dissection}
    | Conclude of Sc_rollup_PVM_sem.input option * PVM.proof

  type move =
    | ConflictInside of {
        choice : Section.section;
        conflict_resolution_step : conflict_resolution_step;
      }

  type commit = Commit of Section.section

  type refutation = RefuteByConflict of conflict_resolution_step

  type reason = InvalidMove | ConflictResolved

  let pp_reason ppf reason =
    Format.fprintf
      ppf
      "%s"
      (match reason with
      | InvalidMove -> "invalid move"
      | ConflictResolved -> "conflict resolved")

  type outcome = {winner : player option; reason : reason}

  let pp_winner winner =
    Format.pp_print_option
      ~none:(fun ppf () -> Format.pp_print_text ppf "no winner")
      pp_player
      winner

  let pp_outcome ppf {winner; reason} =
    Format.fprintf ppf "%a because of %a" pp_winner winner pp_reason reason

  type move_result = Over of outcome | Ongoing of t

  let pp ppf (g : t) =
    Format.fprintf
      ppf
      "%a @ %a -> %a / %a @ %a [%a] %s playing"
      State_hash.pp
      g.start_state
      Sc_rollup_tick_repr.pp
      g.start_at
      State_hash.pp
      g.player_stop_state
      State_hash.pp
      g.opponent_stop_state
      Sc_rollup_tick_repr.pp
      g.stop_at
      Section.pp_optional_dissection
      g.current_dissection
      (match g.turn with Committer -> "committer" | Refuter -> "refuter")

  let pp_move ppf = function
    | ConflictInside
        {
          choice;
          conflict_resolution_step = Refine {next_dissection; stop_state};
        } ->
        Format.fprintf
          ppf
          "conflict is inside %a, should end with %a, new dissection = %a"
          Section.pp_section
          choice
          State_hash.pp
          stop_state
          Section.pp_dissection
          next_dissection
    | ConflictInside
        {choice; conflict_resolution_step = Conclude (input, proof)} ->
        let using_optional_input =
          match input with
          | None -> ""
          | Some input ->
              Format.sprintf " (using optional input `%s')" input.payload
        in
        Format.fprintf
          ppf
          "atomic conflict found inside %a, we can verify that it starts with \
           %a and should end with %a%s"
          Section.pp_section
          choice
          State_hash.pp
          (PVM.proof_start_state proof)
          State_hash.pp
          (PVM.proof_stop_state proof)
          using_optional_input

  let conflict_found (game : t) =
    Sc_rollup_tick_repr.(Z.equal (distance game.stop_at game.start_at) Z.one)

  let stop_state = function
    | Refine {stop_state; _} -> stop_state
    | Conclude (_, proof) -> PVM.proof_stop_state proof

  let initial (Commit commit) (refutation : conflict_resolution_step) =
    let game =
      {
        start_state = commit.section_start_state;
        start_at = commit.section_start_at;
        opponent_stop_state = commit.section_stop_state;
        stop_at = commit.section_stop_at;
        player_stop_state = stop_state refutation;
        current_dissection = None;
        turn = Refuter;
      }
    in
    let choice = commit in
    let move = ConflictInside {choice; conflict_resolution_step = refutation} in
    (game, move)

  let resolve_conflict (game : t) (input, proof) =
    assert (conflict_found game) ;
    let player = game.turn in
    let opponent_state_valid =
      State_hash.equal (PVM.proof_stop_state proof) game.opponent_stop_state
    in
    let over winner = {winner; reason = ConflictResolved} in
    PVM.verify_proof ~input proof >>= fun player_state_valid ->
    let outcome =
      match (player_state_valid, opponent_state_valid) with
      | (true, true) -> over @@ Some Committer
      | (true, false) -> over @@ Some player
      | (false, true) -> over @@ Some (opponent player)
      | (false, false) -> over @@ None
    in
    Lwt.return outcome

  let apply_choice ~(game : t) ~(choice : Section.section) chosen_stop_state =
    let section =
      match game.current_dissection with
      | Some dissection -> Section.find_section choice dissection
      | None ->
          if State_hash.equal choice.section_start_state game.start_state then
            Some choice
          else None
    in
    let game =
      match section with
      | None -> None
      | Some
          {
            section_start_state;
            section_start_at;
            section_stop_state;
            section_stop_at;
          } ->
          if State_hash.equal chosen_stop_state section_stop_state then None
          else
            Some
              {
                game with
                start_state = section_start_state;
                start_at = section_start_at;
                opponent_stop_state = section_stop_state;
                player_stop_state = chosen_stop_state;
                stop_at = section_stop_at;
              }
    in
    Lwt.return game

  let apply_dissection ~(game : t) (next_dissection : Section.dissection) =
    let current_section : Section.section =
      {
        section_start_state = game.start_state;
        section_start_at = game.start_at;
        section_stop_state = game.opponent_stop_state;
        section_stop_at = game.stop_at;
      }
    in
    if Section.valid_dissection current_section next_dissection then
      Lwt.return @@ Some {game with current_dissection = Some next_dissection}
    else Lwt.return None

  let play game (ConflictInside {choice; conflict_resolution_step}) =
    let player = game.turn in

    let apply_move () =
      match conflict_resolution_step with
      | Refine {next_dissection; stop_state} -> (
          apply_choice ~game ~choice stop_state >>= function
          | None -> Lwt.return None
          | Some game -> (
              apply_dissection ~game next_dissection >>= function
              | None -> Lwt.return None
              | Some game -> Lwt.return @@ Some (Ongoing game)))
      | Conclude (input, proof) -> (
          apply_choice ~game ~choice (PVM.proof_stop_state proof) >>= function
          | None -> Lwt.return None
          | Some game ->
              if
                State_hash.equal (PVM.proof_start_state proof) game.start_state
                && conflict_found game
              then
                resolve_conflict game (input, proof) >>= fun x ->
                Lwt.return @@ Some (Over x)
              else Lwt.return None)
    in
    apply_move () >>= function
    | None ->
        Lwt.return
        @@ Over {winner = Some (opponent player); reason = InvalidMove}
    | Some state -> Lwt.return state
end
