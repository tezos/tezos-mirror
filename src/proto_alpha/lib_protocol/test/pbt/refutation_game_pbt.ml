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

(** Testing
    -------
    Component:    PBT for the SCORU refutation game
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/pbt/refutation_game_pbt.exe
    Subject:      SCORU refutation game
*)
open Protocol

open Alpha_context
open Sc_rollup
open Lwt_syntax

exception TickNotFound of Sc_rollup_tick_repr.t

open Lib_test.Qcheck_helpers

(**

   Helpers

*)
let assume_some opt f = match opt with Some x -> f x | None -> assert false

let hash_state state number =
  Digest.bytes @@ Bytes.of_string @@ state ^ string_of_int number

module type TestPVM = sig
  include Sc_rollup_PVM_sem.S

  module Internal_for_tests : sig
    val initial_state : state

    val random_state : int -> state -> state

    val make_proof : state -> state -> proof
  end
end

(**

   [MakeCountingPVM (P)] is a PVM whose state is an integer and that
   can count up to a certain [P.target].

   This PVM has no input states.

*)
module MakeCountingPVM (P : sig
  val target : int
end) : TestPVM with type state = int = struct
  type state = int

  type hash = State_hash.t

  type context = unit

  type proof = state * state

  let proof_start_state ((a, _) : proof) =
    State_hash.hash_string [Int.to_string a]

  let proof_stop_state (_, a) = State_hash.hash_string [Int.to_string a]

  let state_hash (x : state) =
    Lwt.return (State_hash.hash_string [Int.to_string x])

  let is_input_state _ = Lwt.return None

  let initial_state _ _ = Lwt.return P.target

  let set_input _ s = Lwt.return s

  module Internal_for_tests = struct
    let initial_state = P.target

    let random_state _ _ = Random.bits ()

    let make_proof s1 s2 = (s1, s2)
  end

  let equal_states = ( = )

  let encoding = Data_encoding.int16

  let proof_encoding = Data_encoding.tup2 encoding encoding

  let eval state =
    if state >= P.target then Lwt.return state else Lwt.return (state + 1)

  let verify_proof ~input:_ ((a, b) : proof) =
    eval a >>= fun x -> Lwt.return (equal_states x b)
end

(** This is a random PVM. Its state is a pair of a string and a
    list of integers. An evaluation step consumes the next integer
    of the list and concatenates its representation to the string. *)
module MakeRandomPVM (P : sig
  val initial_prog : int list
end) : TestPVM with type state = string * int list = struct
  type state = string * int list

  type context = unit

  type proof = state * state

  type hash = State_hash.t

  let to_string (a, b) =
    Format.sprintf "(%s, [%s])" a (String.concat ";" @@ List.map Int.to_string b)

  let proof_start_state (a, _) = State_hash.hash_string [to_string a]

  let proof_stop_state (_, a) = State_hash.hash_string [to_string a]

  let state_hash (x : state) =
    Lwt.return @@ State_hash.hash_string [to_string x]

  let initial_state _ _ = Lwt.return ("hello", P.initial_prog)

  let is_input_state _ = Lwt.return None

  let set_input _ state = Lwt.return state

  module Internal_for_tests = struct
    let initial_state = ("hello", P.initial_prog)

    let random_state length ((_, program) : state) =
      let remaining_program = TzList.drop_n length program in
      let (stop_state : state) =
        (hash_state "" (Random.bits ()), remaining_program)
      in
      stop_state

    let make_proof s1 s2 = (s1, s2)
  end

  let equal_states = ( = )

  let encoding =
    let open Data_encoding in
    conv
      (fun (value, list) -> (value, list))
      (fun (value, list) -> (value, list))
      (tup2 string (list int16))

  let proof_encoding = Data_encoding.tup2 encoding encoding

  let eval (hash, continuation) =
    match continuation with
    | [] -> Lwt.return (hash, continuation)
    | h :: tl -> Lwt.return (hash_state hash h, tl)

  let verify_proof ~input:_ ((a, b) : proof) =
    eval a >>= fun x -> Lwt.return (equal_states x b)
end

(** This module introduces some testing strategies for a game created from a PVM
*)
module Strategies (P : TestPVM) = struct
  module Game = Sc_rollup_game.Make (P)
  open Game

  (** [execute_until tick stat prop] runs eval until the tick satisfies pred.
      It returns the new tick and the modified state
  *)
  let execute_until tick state pred =
    let rec loop state tick =
      if pred tick state then Lwt.return (tick, state)
      else
        PVM.eval state >>= fun s ->
        if s = state then Lwt.return (tick, state)
        else loop s (Sc_rollup_tick_repr.next tick)
    in
    loop state tick

  let remember history t s = Sc_rollup_tick_repr.Map.add t s history

  (** [state_at history tick initial_state] is a lookup in the history.
  If no value at tick exists then it runs eval from the tick t0 to t starting
  from initial_state. Here t0 is the last known tick smaller that t
  (or the intial tick if no such exits) *)
  let state_at history tick initial_state =
    let (lower, ostate, _) = Sc_rollup_tick_repr.Map.split tick history in
    match ostate with
    | Some state -> Lwt.return (state, history)
    | None ->
        let (tick0, state0) =
          match Sc_rollup_tick_repr.Map.max_binding lower with
          | Some (t, s) -> (t, s)
          | None -> (Sc_rollup_tick_repr.initial, initial_state)
        in

        execute_until tick0 state0 (fun tick' _ ->
            Sc_rollup_tick_repr.(tick' = tick))
        >>= fun (t, s) -> Lwt.return (s, remember history t s)

  (** This function takes a section and an integer branching and creates a
   dissection with branching number of pieces that are (roughly) equal
   and whose states come from the history.
   Assume that length of the initial section is len
   and len mod branching = r.
   We have the following invariants:
    - if branching >len then we make branching = len
      (split the section into one tick sections)
    - valid_disection section
    (dissection_of_section history branching section)=true
    - The first r pieces are one tick longer than the rest
    (the alternative would have been for the last piece to be a lot longer)
    *)
  let dissection_of_section history (branching : int)
      (section : Section.section) =
    let open Section in
    if
      Sc_rollup_tick_repr.(
        next section.section_start_at >= section.section_stop_at)
    then Lwt.return (None, history)
    else
      assume_some (Sc_rollup_tick_repr.to_int section.section_start_at)
      @@ fun start ->
      assume_some (Sc_rollup_tick_repr.to_int section.section_stop_at)
      @@ fun stop ->
      let len = stop - start in
      let branching = min len branching in
      let bucket = len / branching in
      let reminder = len mod branching in
      let rec aux history index branch rem (map : dissection) =
        if index = branch then Lwt.return map
        else
          let start_at = start + (bucket * index) + min rem index in
          let stop_at = start + (bucket * (index + 1)) + min rem (index + 1) in
          let section_start_at =
            Option.value
              ~default:Sc_rollup_tick_repr.initial
              (Sc_rollup_tick_repr.of_int start_at)
          and section_stop_at =
            Option.value
              ~default:Sc_rollup_tick_repr.initial
              (Sc_rollup_tick_repr.of_int stop_at)
          in
          let* (starting_state, history) =
            state_at history section_start_at P.Internal_for_tests.initial_state
          in
          let* section_start_state = P.state_hash starting_state in
          let* (stoping_state, history) =
            state_at history section_stop_at P.Internal_for_tests.initial_state
          in
          let* section_stop_state = P.state_hash stoping_state in

          let section =
            {
              section_start_at;
              section_start_state;
              section_stop_at;
              section_stop_state;
            }
          in
          let new_map = add_section section map in
          aux history (index + 1) branching rem new_map
      in

      let* (dissection : dissection) =
        aux history 0 branching reminder empty_dissection
      in
      Lwt.return @@ (Some dissection, history)

  type ('from, 'initial) client = {
    initial : 'from -> 'initial Lwt.t;
    next_move : Section.dissection -> move Lwt.t;
  }

  let run ~start_at ~(start_state : PVM.state) ~committer ~refuter =
    let* (Commit commit) = committer.initial (start_at, start_state) in
    let* (RefuteByConflict refutation) =
      refuter.initial (start_state, Commit commit)
    in
    let outcome =
      let rec loop game move =
        play game move >>= function
        | Over outcome -> Lwt.return outcome
        | Ongoing game ->
            let game = {game with turn = opponent game.turn} in
            let* move =
              match game.turn with
              | Committer ->
                  committer.next_move
                    (Option.value
                       ~default:Game.Section.empty_dissection
                       game.current_dissection)
              | Refuter ->
                  refuter.next_move
                    (Option.value
                       ~default:Game.Section.empty_dissection
                       game.current_dissection)
            in
            loop game move
      in

      let (game, move) = initial (Commit commit) refutation in
      loop game move
    in
    outcome

  (** this exception is needed for a small that alows a "fold-with break"
*)
  exception Section of Game.Section.section

  let random_tick ?(from = 0) () =
    Option.value
      ~default:Sc_rollup_tick_repr.initial
      (Sc_rollup_tick_repr.of_int (from + Random.int 31))

  (** this picks a random section between start_at and stop_at. The states
  are determined by the random_state function.*)
  let random_section (start_at : Sc_rollup_tick_repr.t) start_state
      (stop_at : Sc_rollup_tick_repr.t) =
    let x =
      min 10000 @@ Z.to_int (Sc_rollup_tick_repr.distance start_at stop_at)
    in
    let length = 1 + try Random.int x with _ -> 0 in
    let stop_at =
      assume_some (Sc_rollup_tick_repr.to_int start_at) @@ fun start_at ->
      Sc_rollup_tick_repr.(of_int (start_at + length))
    in
    let section_stop_at = Option.value ~default:start_at stop_at in

    let random_stop_state =
      P.Internal_for_tests.(random_state length initial_state)
    in
    let* section_stop_state = P.state_hash random_stop_state in
    Lwt.return
      Section.
        {
          section_start_at = start_at;
          section_start_state = start_state;
          section_stop_at;
          section_stop_state;
        }

  (** this picks a random dissection of a given section.
  The sections involved are random and their states have no connection
  with the initial section.*)
  let random_dissection (gsection : Section.section) =
    let open Sc_rollup_tick_repr in
    let rec aux dissection start_at start_state =
      if start_at = gsection.section_stop_at then Lwt.return @@ Some dissection
      else
        let* section =
          random_section start_at start_state gsection.section_stop_at
        in
        if
          section.section_start_at = gsection.section_start_at
          && section.section_stop_at = gsection.section_stop_at
        then aux dissection start_at start_state
        else
          aux
            (Section.add_section section dissection)
            section.section_stop_at
            section.section_stop_state
    in
    if
      Compare.Int.(
        Z.to_int (distance gsection.section_stop_at gsection.section_start_at)
        > 1)
    then
      aux
        Section.empty_dissection
        gsection.section_start_at
        gsection.section_start_state
    else Lwt.return None

  (** this function assmbles a random decision from a given dissection.
    It first picks a random section from the dissection and modifies randomly
     its states.
    If the length of this section is one tick the returns a conclusion with
    the given modified states.
    If the length is longer it creates a random decision and outputs a Refine
     decisoon with this dissection.*)
  let random_decision d =
    let open Section in
    let cardinal = dissection_cardinal d in
    let x = Random.int cardinal in
    let (_, section) =
      try
        fold_over_dissection
          (fun _ s (n, _) -> if n = x then raise (Section s) else (n + 1, None))
          d
          (0, None)
      with Section sec -> (0, Some sec)
    in
    let section =
      match section with None -> raise Not_found | Some section -> section
    in
    let section_start_at = section.section_start_at in
    let section_stop_at = section.section_stop_at in
    let start_state = P.Internal_for_tests.(random_state 0 initial_state) in
    let* section_start_state = P.state_hash start_state in
    let stop_state =
      assume_some (Sc_rollup_tick_repr.to_int section_stop_at)
      @@ fun section_stop_at ->
      assume_some (Sc_rollup_tick_repr.to_int section_start_at)
      @@ fun section_start_at ->
      P.Internal_for_tests.(
        random_state (section_stop_at - section_start_at) initial_state)
    in
    let* section_stop_state = P.state_hash stop_state in

    let* next_dissection = random_dissection section in
    let section =
      {
        section_start_state;
        section_start_at;
        section_stop_state;
        section_stop_at;
      }
    in
    let conflict_resolution_step =
      match next_dissection with
      | None ->
          Conclude (None, P.Internal_for_tests.make_proof start_state stop_state)
      | Some next_dissection ->
          Refine {stop_state = section.section_stop_state; next_dissection}
    in
    Lwt.return @@ ConflictInside {choice = section; conflict_resolution_step}

  (** technical params for machine directed strategies, branching is the number
   of pieces for a dissection failing level*)
  type parameters = {
    branching : int;
    failing_level : int;
    max_failure : int option;
  }

  type checkpoint = Sc_rollup_tick_repr.t -> bool

  (** there are two kinds of strategies, random and machine dirrected by a
  params and a checkpoint*)
  type strategy = Random | MachineDirected of parameters * checkpoint

  (**
  checks that the stop state of a section conflicts with the one in the history.
  *)
  let conflicting_section history (section : Section.section) =
    let* (new_state, _) =
      state_at
        history
        section.section_stop_at
        P.Internal_for_tests.initial_state
    in
    let* new_hash = P.state_hash new_state in
    Lwt.return @@ not (section.section_stop_state = new_hash)

  (**
  Finds the section (if it exists) is a dissection that conflicts
  with the history. This is where the trick with the exception
  appears*)
  let find_conflict history dissection =
    try
      Section.fold_over_dissection
        (fun _ v _ ->
          let* conflict = conflicting_section history v in
          if conflict then raise (Section v) else Lwt.return None)
        dissection
        (Lwt.return None)
    with Section section -> Lwt.return @@ Some section

  (** [next_move history branching dissection]
  If finds the next move based on a dissection and history.
  It finds the first section of dissection that conflicts with the history.
  If the section has length one tick it returns a move with a Conclude
  conflict_resolution_step.
  If the section is longer it creates a new dissection with branching
  many pieces, updates the history based on this dissection and returns
   a move with a Refine type conflict_resolution_step.
   *)
  let next_move history branching dissection =
    let* section =
      find_conflict history dissection >>= function
      | None -> raise (TickNotFound Sc_rollup_tick_repr.initial)
      | Some s -> Lwt.return s
    in
    Game.Section.pp_section Format.std_formatter section ;
    let* (next_dissection, history) =
      dissection_of_section history branching section
    in
    let empty_history = Sc_rollup_tick_repr.Map.empty in
    let* (conflict_resolution_step, history) =
      match next_dissection with
      | None ->
          let* (stop_state, history) =
            state_at
              history
              (Sc_rollup_tick_repr.next section.section_start_at)
              P.Internal_for_tests.initial_state
          in
          let* (start_state, _) =
            state_at
              history
              section.section_start_at
              P.Internal_for_tests.initial_state
          in
          Lwt.return
            ( Conclude
                (None, P.Internal_for_tests.make_proof start_state stop_state),
              empty_history )
      | Some next_dissection ->
          let* (state, history) =
            state_at
              history
              section.section_stop_at
              P.Internal_for_tests.initial_state
          in
          let* stop_state = P.state_hash state in
          Lwt.return (Refine {stop_state; next_dissection}, history)
    in
    Lwt.return
      (ConflictInside {choice = section; conflict_resolution_step}, history)

  (** this is an outomatic commuter client. It generates a "perfect" client
  for the committer.*)
  let machine_directed_committer {branching; _} pred =
    let history = ref Sc_rollup_tick_repr.Map.empty in
    let initial ((section_start_at : Sc_rollup_tick_repr.t), start_state) =
      let* (section_stop_at, stop_state) =
        execute_until section_start_at start_state @@ fun tick _ -> pred tick
      in
      let* section_start_state = P.state_hash start_state in
      let* section_stop_state = P.state_hash stop_state in
      history := remember !history section_start_at start_state ;
      history := remember !history section_stop_at stop_state ;
      Lwt.return
      @@ Commit
           {
             section_start_state;
             section_start_at;
             section_stop_state;
             section_stop_at;
           }
    in
    let next_move dissection =
      let* (move, history') = next_move !history branching dissection in
      history := history' ;
      Lwt.return move
    in
    ({initial; next_move} : _ client)

  (** this is an outomatic refuter client. It generates a "perfect" client
  for the refuter.*)
  let machine_directed_refuter {branching; _} =
    let history = Sc_rollup_tick_repr.Map.empty in
    let initial (section_start_state, Commit section) =
      let ({section_start_at; section_stop_at; _} : Section.section) =
        section
      in
      let* (_stop_at, stop_state) =
        execute_until section_start_at section_start_state @@ fun tick _ ->
        tick >= section_stop_at
      in

      let history = remember history section_start_at section_start_state in
      let history = remember history section_stop_at stop_state in
      let* section_stop_state = P.state_hash stop_state in
      let* (next_dissection, history) =
        dissection_of_section
          history
          branching
          {section with section_stop_state}
      in
      let* conflict_resolution_step =
        match next_dissection with
        | None ->
            let* (state, _) =
              state_at
                history
                section_start_at
                P.Internal_for_tests.initial_state
            in
            Lwt.return
            @@ Conclude (None, P.Internal_for_tests.make_proof state stop_state)
        | Some next_dissection ->
            Lwt.return
            @@ Refine {stop_state = section_stop_state; next_dissection}
      in
      Lwt.return @@ RefuteByConflict conflict_resolution_step
    in
    let next_move dissection =
      let* (move, _) = next_move history branching dissection in
      Lwt.return move
    in
    ({initial; next_move} : _ client)

  (** This builds a commiter client from a strategy.
    If the strategy is MachineDirected it uses the above constructions.
    If the strategy is random then it usesa random section for the initial
    commitments and  the random decision for the next move.*)
  let committer_from_strategy : strategy -> _ client = function
    | Random ->
        {
          initial =
            (fun ((section_start_at : Sc_rollup_tick_repr.t), start_state) ->
              let section_stop_at =
                assume_some (Sc_rollup_tick_repr.to_int section_start_at)
                @@ fun start_at -> random_tick ~from:start_at ()
              in
              let* start_state = P.state_hash start_state in
              let* section =
                random_section section_start_at start_state section_stop_at
              in
              Lwt.return @@ Commit section);
          next_move = random_decision;
        }
    | MachineDirected (parameters, checkpoint) ->
        machine_directed_committer parameters checkpoint

  (** This builds a refuter client from a strategy.
    If the strategy is MachineDirected it uses the above constructions.
    If the strategy is random then it uses a randomdissection
    of the commited section for the initial refutation
     and  the random decision for the next move.*)
  let refuter_from_strategy : strategy -> _ client = function
    | Random ->
        {
          initial =
            (fun (_, Commit section) ->
              let* conflict_resolution_step =
                let* next_dissection =
                  random_dissection section >>= function
                  | None ->
                      Lwt.return
                      @@ Section.(add_section section empty_dissection)
                  | Some dissection -> Lwt.return dissection
                in
                let (_, section) =
                  Option.value
                    ~default:(Sc_rollup_tick_repr.initial, section)
                    (Section.last_section next_dissection)
                in
                Lwt.return
                @@ Refine
                     {stop_state = section.section_stop_state; next_dissection}
              in
              Lwt.return @@ RefuteByConflict conflict_resolution_step);
          next_move = random_decision;
        }
    | MachineDirected (parameters, _) -> machine_directed_refuter parameters

  (** [test_strategies committer_strategy refuter_strategy expectation]
    runs a game based oin the two given strategies and checks that the resulting
    outcome fits the expectations. *)
  let test_strategies committer_strategy refuter_strategy expectation =
    let start_state = P.Internal_for_tests.initial_state in
    let committer = committer_from_strategy committer_strategy in
    let refuter = refuter_from_strategy refuter_strategy in
    let outcome =
      run ~start_at:Sc_rollup_tick_repr.initial ~start_state ~committer ~refuter
    in
    expectation outcome

  (** This is a commuter client having a perfect strategy*)
  let perfect_committer =
    MachineDirected
      ( {failing_level = 0; branching = 2; max_failure = None},
        fun tick ->
          assume_some (Sc_rollup_tick_repr.to_int tick) @@ fun tick ->
          tick >= 20 + Random.int 100 )
  (** This is a refuter client having a perfect strategy*)

  let perfect_refuter =
    MachineDirected
      ( {failing_level = 0; branching = 2; max_failure = None},
        fun _ -> assert false )

  (** This is a commuter client having a strategy that forgets a tick*)
  let failing_committer max_failure =
    MachineDirected
      ( {failing_level = 1; branching = 2; max_failure},
        fun tick ->
          let s = match max_failure with None -> 20 | Some x -> x in
          assume_some (Sc_rollup_tick_repr.to_int tick) @@ fun tick -> tick >= s
      )

  (** This is a commuter client having a strategy that forgets a tick*)
  let failing_refuter max_failure =
    MachineDirected
      ({failing_level = 1; branching = 2; max_failure}, fun _ -> assert false)

  (** the possible expectation functions *)
  let commiter_wins x =
    Lwt_main.run
      (x >>= function
       | {winner = Some Committer; _} -> Lwt.return true
       | _ -> Lwt.return false)

  let refuter_wins x =
    Lwt_main.run
      (x >>= function
       | {winner = Some Refuter; _} -> Lwt.return true
       | _ -> Lwt.return false)

  let all_win _ = true
end

(** the following are the possible combinations of strategies*)
let perfect_perfect (module P : TestPVM) _max_failure =
  let module R = Strategies (P) in
  R.test_strategies R.perfect_committer R.perfect_refuter R.commiter_wins

let random_random (module P : TestPVM) _max_failure =
  let module S = Strategies (P) in
  S.test_strategies Random Random S.all_win

let random_perfect (module P : TestPVM) _max_failure =
  let module S = Strategies (P) in
  S.test_strategies Random S.perfect_refuter S.refuter_wins

let perfect_random (module P : TestPVM) _max_failure =
  let module S = Strategies (P) in
  S.test_strategies S.perfect_committer Random S.commiter_wins

let failing_perfect (module P : TestPVM) max_failure =
  let module S = Strategies (P) in
  S.test_strategies
    (S.failing_committer max_failure)
    S.perfect_refuter
    S.refuter_wins

let perfect_failing (module P : TestPVM) max_failure =
  let module S = Strategies (P) in
  S.test_strategies
    S.perfect_committer
    (S.failing_refuter max_failure)
    S.commiter_wins

(** this assembles a test from a RandomPVM and a function that choses the
type of strategies *)
let testing_randomPVM (f : (module TestPVM) -> int option -> bool) name =
  let open QCheck2 in
  Test.make
    ~name
    (Gen.list_size Gen.small_int (Gen.int_range 0 100))
    (fun initial_prog ->
      assume (initial_prog <> []) ;
      f
        (module MakeRandomPVM (struct
          let initial_prog = initial_prog
        end))
        (Some (List.length initial_prog)))

(** this assembles a test from a CountingPVM and a function that choses
the type of strategies *)
let testing_countPVM (f : (module TestPVM) -> int option -> bool) name =
  let open QCheck2 in
  Test.make ~name Gen.small_int (fun target ->
      assume (target > 0) ;
      f
        (module MakeCountingPVM (struct
          let target = target
        end))
        (Some target))

let test_random_dissection (module P : TestPVM) start_at length branching =
  let open P in
  let module S = Strategies (P) in
  let* section_start_state = P.state_hash @@ Internal_for_tests.initial_state in
  let section_stop_at =
    match Sc_rollup_tick_repr.of_int (start_at + length) with
    | None -> assert false
    | Some x -> x
  in
  let section_start_at =
    match Sc_rollup_tick_repr.of_int start_at with
    | None -> assert false
    | Some x -> x
  in
  let* section_stop_state =
    P.state_hash @@ Internal_for_tests.(random_state length initial_state)
  in
  let section =
    S.Game.Section.
      {
        section_start_at;
        section_start_state;
        section_stop_at;
        section_stop_state;
      }
  in
  let* (option_dissection, _) =
    let empty_history = Sc_rollup_tick_repr.Map.empty in
    S.dissection_of_section empty_history branching section
  in
  let dissection =
    match option_dissection with
    | None -> raise (Invalid_argument "no dissection")
    | Some x -> x
  in
  Lwt.return @@ S.Game.Section.valid_dissection section dissection

let testDissection =
  let open QCheck2 in
  [
    Test.make
      ~name:"randomVPN"
      (Gen.quad
         (Gen.list_size Gen.small_int (Gen.int_range 0 100))
         Gen.small_int
         Gen.small_int
         Gen.small_int)
      (fun (initial_prog, start_at, length, branching) ->
        assume
          (start_at >= 0 && length > 1
          && List.length initial_prog > start_at + length
          && 1 < branching) ;
        let module P = MakeRandomPVM (struct
          let initial_prog = initial_prog
        end) in
        Lwt_main.run
        @@ test_random_dissection (module P) start_at length branching);
    Test.make
      ~name:"count"
      (Gen.quad Gen.small_int Gen.small_int Gen.small_int Gen.small_int)
      (fun (target, start_at, length, branching) ->
        assume (start_at >= 0 && length > 1 && 1 < branching) ;
        let module P = MakeCountingPVM (struct
          let target = target
        end) in
        Lwt_main.run
        @@ test_random_dissection (module P) start_at length branching);
  ]

let () =
  Alcotest.run
    "Refutation Game"
    [
      ("Dissection tests", qcheck_wrap testDissection);
      ( "RandomPVM",
        qcheck_wrap
          [
            testing_randomPVM perfect_perfect "perfect-perfect";
            testing_randomPVM random_random "random-random";
            testing_randomPVM random_perfect "random-perfect";
            testing_randomPVM perfect_random "perfect-random";
          ] );
      ( "CountingPVM",
        qcheck_wrap
          [
            testing_countPVM perfect_perfect "perfect-perfect";
            testing_countPVM random_random "random-random";
            testing_countPVM random_perfect "random-perfect";
            testing_countPVM perfect_random "perfect-random";
          ] );
    ]
