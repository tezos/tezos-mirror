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
                src/proto_alpha/lib_protocol/test/pbt/test_refutation_game.exe
    Subject:      SCORU refutation game
*)
open Protocol

open Alpha_context
open Sc_rollup
open Lwt_syntax
open Tezos_context_memory

exception TickNotFound of Sc_rollup_tick_repr.t

open Lib_test.Qcheck_helpers
module Sc_rollup_repr = Protocol.Sc_rollup_repr

(**

   Helpers

*)
let assume_some opt f = match opt with Some x -> f x | None -> assert false

let hash_state state number =
  Digest.bytes @@ Bytes.of_string @@ state ^ string_of_int number

type dummy_proof = {
  start : Sc_rollup_repr.State_hash.t;
  stop : Sc_rollup_repr.State_hash.t option;
  valid : bool;
}

let dummy_proof_encoding : dummy_proof Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {start; stop; valid} -> (start, stop, valid))
    (fun (start, stop, valid) -> {start; stop; valid})
    (obj3
       (req "start" Sc_rollup_repr.State_hash.encoding)
       (req "stop" (option Sc_rollup_repr.State_hash.encoding))
       (req "valid" bool))

let proof_start_state proof = proof.start

let proof_stop_state proof = proof.stop

let check pred =
  let open Result_syntax in
  if pred then return () else error ()

let number_of_messages_exn n =
  match Sc_rollup_repr.Number_of_messages.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_messages"

let number_of_ticks_exn n =
  match Sc_rollup_repr.Number_of_ticks.of_int32 n with
  | Some x -> x
  | None -> Stdlib.failwith "Bad Number_of_ticks"

let get_comm pred inbox_level messages ticks state =
  Sc_rollup_commitment_repr.
    {
      predecessor = pred;
      inbox_level = Raw_level_repr.of_int32_exn inbox_level;
      number_of_messages = number_of_messages_exn messages;
      number_of_ticks = number_of_ticks_exn ticks;
      compressed_state = state;
    }

(** This picks a random section between start_at and stop_at. The states
  are determined by the random_state function.*)
let random_hash () = Sc_rollup_repr.State_hash.of_bytes_exn @@ Bytes.create 32

let of_int_exc n =
  match Sc_rollup_tick_repr.of_int n with None -> assert false | Some t -> t

(** This picks a random dissection of a given section.
  The sections involved are random and their states have no connection
  with the initial section.*)
let random_dissection start_at start_hash stop_at _stop_hash :
    (Sc_rollup_repr.State_hash.t option * Sc_rollup_tick_repr.t) list option
    Lwt.t =
  match start_hash with
  | Some _ ->
      let start_int =
        match Sc_rollup_tick_repr.to_int start_at with
        | None -> assert false
        | Some x -> x
      in
      let stop_int =
        match Sc_rollup_tick_repr.to_int stop_at with
        | None -> assert false
        | Some x -> x
      in
      let dist = stop_int - start_int in

      let branch = min (dist + 1) 32 in
      let size = (dist + 1) / branch in
      Lwt.return
      @@ Result.to_option
           (List.init branch ~when_negative_length:"error" (fun i ->
                if i = 0 then (start_hash, start_at)
                else if i = branch - 1 then (_stop_hash, stop_at)
                else (Some (random_hash ()), of_int_exc (start_int + (i * size)))))
  | None -> assert false

(** 
 `genlist` is a `correct list` generator. It generates a list of strings that
  are either integers or `+` to be consumed by the arithmetic PVM. 
  If a `+` is found then the previous two element of the stack are poped
   then added and the result is pushed to the stack. 
   In particular, lists like `[1 +]` are incorrect.
    
  To preserve the correctness invariant, genlist is a recursive generator that 
  produce a pair `(stack_size, state_list)` where  state_list is a correct list 
  of integers and `+` and consuming it will produce a `stack` of length 
  `stack_size`.
  For example a result can be `(3, [1; 2; +; 3; +; 2; 2; +; 1;]). 
  Consuming the list will produce the stack`[6; 4; 2]` which has length 3.
  The generator has two branches. 
  1. with frequency 1 adds integers to state_list and increases the 
  corresponding stack_size.
  2. With frequency 2, at each step, it looks at the inductive result
  `(self (n - 1))=(stack_size, state_list)`. 
  If the stack_size is smaller than 2 then it adds an integer to the state_list
   and increases the stack_size
  Otherwise it adds a plus to the state_list and decreases the stack_size.
  Remark: The algorithm is linear in the size of the generated list and 
  generates all kinds of inputs not only those that produce a stack of size 1.  
*)
let gen_list =
  QCheck2.Gen.(
    map (fun (_, l) -> List.rev l)
    @@ sized_size small_nat
    @@ fix (fun self n ->
           match n with
           | 0 -> map (fun x -> (1, [string_of_int x])) small_nat
           | n ->
               frequency
                 [
                   ( 2,
                     map2
                       (fun x (stack_size, state_list) ->
                         if stack_size >= 2 then
                           (stack_size - 1, "+" :: state_list)
                         else (stack_size + 1, string_of_int x :: state_list))
                       small_nat
                       (self (n - 1)) );
                   ( 1,
                     map2
                       (fun x (i, y) -> (i + 1, string_of_int x :: y))
                       small_nat
                       (self (n - 1)) );
                 ]))

(** This uses the above generator to produce a correct program with at 
    least 3 elements. 
*)
let rec correct_string () =
  let x = QCheck2.Gen.(generate1 gen_list) in
  if List.length x < 3 then correct_string () else x

module type TestPVM = sig
  include Sc_rollup_PVM_sem.S with type hash = Sc_rollup_repr.State_hash.t

  module Utils : sig
    (** This a post-boot state. It is used as default in many functions. *)
    val default_state : state

    (** [random_state n state] generates a random state. The integer n is 
        used in the generation *)
    val random_state : int -> state -> state

    (** [make_proof start_state stop_state] produces a proof that the eval of
         start_state is the stop_state.
        It will be used by the `verify_proof`. In the arithPVM we use 
        `produce_tree_proof` which only requires a starting state (tree) 
        and the transition function.*)
    val make_proof : hash option -> hash option -> bool -> proof Lwt.t
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

  type hash = Sc_rollup_repr.State_hash.t

  type context = unit

  type proof = dummy_proof

  let proof_start_state = proof_start_state

  let proof_stop_state = proof_stop_state

  let proof_input_given _ = None

  let proof_input_requested _ = Sc_rollup_PVM_sem.No_input_required

  let state_hash (x : state) =
    Lwt.return (Sc_rollup_repr.State_hash.hash_string [Int.to_string x])

  let is_input_state _ = Lwt.return Sc_rollup_PVM_sem.No_input_required

  let initial_state _ _ = Lwt.return P.target

  let set_input _ s = Lwt.return s

  module Utils = struct
    let default_state = P.target

    let random_state _ _ = Random.bits ()

    let make_proof s1 s2 v =
      match (s1, s2) with
      | None, _ -> assert false
      | Some start_hash, Some stop_hash ->
          Lwt.return @@ {start = start_hash; stop = Some stop_hash; valid = v}
      | Some start_hash, None ->
          Lwt.return @@ {start = start_hash; stop = None; valid = v}
  end

  let proof_encoding = dummy_proof_encoding

  let eval state =
    if state >= P.target then Lwt.return state else Lwt.return (state + 1)

  let verify_proof proof = Lwt.return proof.valid
end

(** This is a random PVM. Its state is a pair of a string and a
    list of integers. An evaluation step consumes the next integer
    of the list and concatenates its representation to the string. *)
module MakeRandomPVM (P : sig
  val initial_prog : int list
end) : TestPVM with type state = string * int list = struct
  type state = string * int list

  type context = unit

  type proof = dummy_proof

  type hash = Sc_rollup_repr.State_hash.t

  let to_string (a, b) =
    Format.sprintf "(%s, [%s])" a (String.concat ";" @@ List.map Int.to_string b)

  let proof_start_state = proof_start_state

  let proof_stop_state = proof_stop_state

  let proof_input_given _ = None

  let proof_input_requested _ = Sc_rollup_PVM_sem.No_input_required

  let state_hash (x : state) =
    Lwt.return @@ Sc_rollup_repr.State_hash.hash_string [to_string x]

  let initial_state _ _ = Lwt.return ("hello", P.initial_prog)

  let is_input_state _ = Lwt.return Sc_rollup_PVM_sem.No_input_required

  let set_input _ state = Lwt.return state

  module Utils = struct
    let default_state = ("hello", P.initial_prog)

    let random_state length ((_, program) : state) =
      let remaining_program = TzList.drop_n length program in
      let (stop_state : state) =
        (hash_state "" (Random.bits ()), remaining_program)
      in
      stop_state

    let make_proof s1 s2 v =
      match (s1, s2) with
      | None, _ -> assert false
      | Some start_hash, Some stop_hash ->
          Lwt.return @@ {start = start_hash; stop = Some stop_hash; valid = v}
      | Some start_hash, None ->
          Lwt.return @@ {start = start_hash; stop = None; valid = v}
  end

  let proof_encoding = dummy_proof_encoding

  let eval (hash, continuation) =
    match continuation with
    | [] -> Lwt.return (hash, continuation)
    | h :: tl -> Lwt.return (hash_state hash h, tl)

  let verify_proof proof = Lwt.return proof.valid
end

module ContextPVM = Sc_rollup_arith.Make (struct
  module Tree = struct
    include Context.Tree

    type tree = Context.tree

    type t = Context.t

    type key = string list

    type value = bytes
  end

  type tree = Context.tree

  let empty_tree = Tree.empty Context.empty

  type proof = Context.Proof.tree Context.Proof.t

  let verify_proof proof f =
    Lwt.map Result.to_option (Context.verify_tree_proof proof f)

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash ->
        Sc_rollup_repr.State_hash.hash_bytes [Context_hash.to_bytes hash]

  let proof_start_state proof =
    kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_stop_state proof =
    kinded_hash_to_state_hash proof.Context.Proof.after

  let proof_encoding =
    let open Data_encoding in
    conv (fun _ -> ()) (fun _ -> assert false) unit
end)

module TestArith (P : sig
  val inputs : string

  val evals : int
end) : TestPVM = struct
  include ContextPVM

  let init_context = Tezos_context_memory.Context.empty

  module Utils = struct
    let default_state =
      let promise =
        let* boot = initial_state init_context "" >>= eval in
        let input =
          Sc_rollup_PVM_sem.
            {
              inbox_level = Raw_level_repr.root;
              message_counter = Z.zero;
              payload = P.inputs;
            }
        in
        let prelim = set_input input boot in
        List.fold_left (fun acc _ -> acc >>= fun acc -> eval acc) prelim
        @@ List.repeat P.evals ()
      in
      Lwt_main.run promise

    let random_state i state =
      let program = correct_string () in
      let input =
        Sc_rollup_PVM_sem.
          {
            inbox_level = Raw_level_repr.root;
            message_counter = Z.zero;
            payload = String.concat " " program;
          }
      in
      let prelim = set_input input state in
      Lwt_main.run
      @@ List.fold_left (fun acc _ -> acc >>= fun acc -> eval acc) prelim
      @@ List.repeat (min i (List.length program - 2) + 1) ()

    let make_proof s1 s2 v =
      match (s1, s2) with
      | None, _ -> assert false
      | Some start_hash, Some stop_hash ->
          Lwt.return @@ {start = start_hash; stop = Some stop_hash; valid = v}
      | Some start_hash, None ->
          Lwt.return @@ {start = start_hash; stop = None; valid = v}
  end
end

(** This module introduces some testing strategies for a game created from a
 PVM.
*)
module Strategies
    (PVM : TestPVM
             with type hash = Sc_rollup_repr.State_hash.t
              and type proof = dummy_proof) =
struct
  module Game = Sc_rollup_game_repr

  (* Sc_rollup_game.Make (P) *)
  open Game

  (** [execute_until tick stat prop] runs eval until the tick satisfies pred or 
  until the state machine does not change anymore. It returns the new tick and 
  the modified state
  *)
  let execute_until tick state pred =
    let rec loop state tick =
      let* isinp = PVM.is_input_state state in
      match isinp with
      | Some _ -> Lwt.return (tick, state)
      | None ->
          if pred tick state then Lwt.return (tick, state)
          else
            let* s = PVM.eval state in
            let* hash1 = PVM.state_hash state in
            let* hash2 = PVM.state_hash s in

            if State_hash.equal hash1 hash2 then Lwt.return (tick, state)
            else loop s (Sc_rollup_tick_repr.next tick)
    in
    loop state tick

  (** [state_at tick default_state default_tick] is a computation of the state.
  If  runs eval from  default_tick to tick starting
  from default_state. *)
  let state_at tick default_state default_tick =
    execute_until default_tick default_state (fun tick' _ ->
        Sc_rollup_tick_repr.(tick' = tick))
    >>= fun (t, s) -> Lwt.return (Some s, t)

  (** [dissection_of_section start_tick start_state stop_tick] creates a 
  dissection with at most 32 pieces that are (roughly) equal spaced and whose 
  states are computed by running the eval function until the correct tick. Note 
  that  the last piece can be as much as 31 ticks longer than the others.
    *)
  let dissection_of_section start_tick start_state stop_tick =
    match start_state with
    | Some start_state ->
        let start_int =
          match Sc_rollup_tick_repr.to_int start_tick with
          | None -> assert false
          | Some x -> x
        in

        let stop_int =
          match Sc_rollup_tick_repr.to_int stop_tick with
          | None -> assert false
          | Some x -> x
        in
        let dist = stop_int - start_int in
        if dist = 1 then Lwt.return None
        else
          let branch = min (dist + 1) 32 in
          let size = (dist + 1) / branch in
          let tick_list =
            Result.to_option
            @@ List.init branch ~when_negative_length:"error" (fun i ->
                   if i = branch - 1 then stop_tick
                   else of_int_exc (start_int + (i * size)))
          in
          let a =
            Option.map
              (fun a ->
                List.map
                  (fun tick ->
                    let hash =
                      Lwt_main.run
                      @@ let* state, _ = state_at tick start_state start_tick in
                         match state with
                         | None -> Lwt.return None
                         | Some s ->
                             let* h = PVM.state_hash s in
                             Lwt.return (Some h)
                    in
                    (hash, tick))
                  a)
              tick_list
          in
          Lwt.return a
    | None -> assert false

  type client = {
    initial : (Sc_rollup_tick_repr.t * PVM.hash) option Lwt.t;
    next_move : t -> refutation option Lwt.t;
  }

  type outcome_for_tests = Defender_wins | Refuter_wins

  let equal_outcome a b =
    match (a, b) with
    | Defender_wins, Defender_wins -> true
    | Refuter_wins, Refuter_wins -> true
    | _ -> false

  let loser_to_outcome_for_tests loser alice_is_refuter =
    match loser with
    | Bob -> if alice_is_refuter then Refuter_wins else Defender_wins
    | Alice -> if alice_is_refuter then Defender_wins else Refuter_wins

  let run ~inbox ~refuter_client ~defender_client =
    let refuter, _, _ = Signature.generate_key () in
    let defender, _, _ = Signature.generate_key () in
    let alice_is_refuter = Staker.(refuter < defender) in
    let* start_hash = PVM.state_hash PVM.Utils.default_state in
    let* initial_data = defender_client.initial in
    let tick, initial_hash =
      match initial_data with None -> assert false | Some s -> s
    in
    let int_tick =
      match Sc_rollup_tick_repr.to_int tick with
      | None -> assert false
      | Some x -> x
    in
    let number_of_ticks = Int32.of_int int_tick in
    let parent =
      get_comm Sc_rollup_commitment_repr.Hash.zero 0l 3l 1l start_hash
    in
    let child =
      get_comm
        Sc_rollup_commitment_repr.Hash.zero
        0l
        3l
        number_of_ticks
        initial_hash
    in

    let initial_game = initial inbox ~parent ~child ~refuter ~defender in
    let outcome =
      let rec loop game refuter_move =
        let* move =
          if refuter_move then refuter_client.next_move game
          else defender_client.next_move game
        in

        match move with
        | None ->
            Lwt.return (if refuter_move then Defender_wins else Refuter_wins)
        | Some move -> (
            play game move |> function
            | Either.Left outcome ->
                Lwt.return
                  (loser_to_outcome_for_tests outcome.loser alice_is_refuter)
            | Either.Right game -> loop game (not refuter_move))
      in
      loop initial_game true
    in
    outcome

  let random_tick ?(from = 0) () =
    Option.value
      ~default:Sc_rollup_tick_repr.initial
      (Sc_rollup_tick_repr.of_int (from + Random.int 31))

  (**
  checks that the stop state of a section conflicts with the one computed by the
   evaluation.
  *)
  let conflicting_section tick state =
    let* new_state, _ =
      state_at tick PVM.Utils.default_state Sc_rollup_tick_repr.initial
    in
    let* new_hash =
      match new_state with
      | None -> Lwt.return None
      | Some state ->
          let* state = PVM.state_hash state in
          Lwt.return (Some state)
    in

    Lwt.return @@ not (Option.equal ( = ) state new_hash)

  (** This function assembles a random decision from a given dissection.
    It first picks a random section from the dissection and modifies randomly
     its states.
    If the length of this section is one tick the returns a conclusion with
    the given modified states.
    If the length is longer it creates a random decision and outputs a Refine
     decision with this dissection.*)
  let random_decision d =
    let cardinal = List.length d in
    let x = max 0 (Random.int (cardinal - 1)) in
    let start_state, start =
      match List.nth d x with Some (s, t) -> (s, t) | None -> assert false
    in
    let _, stop =
      match List.nth d (x + 1) with
      | Some (s, t) -> (s, t)
      | None -> assert false
    in
    let start_hash = start_state in
    let stop_hash = Some (random_hash ()) in
    let* random_dissection =
      random_dissection start start_hash stop stop_hash
    in

    match random_dissection with
    | None ->
        let new_hash = random_hash () in
        let* correct_state, _ =
          state_at stop PVM.Utils.default_state Sc_rollup_tick_repr.initial
        in
        let* valid =
          match correct_state with
          | None -> Lwt.return_false
          | Some x ->
              let* correct_hash = PVM.state_hash x in
              Lwt.return (correct_hash = new_hash)
        in
        let* proof =
          PVM.Utils.make_proof start_state (Some (random_hash ())) valid
        in
        Lwt.return
          (Some Sc_rollup_game_repr.{choice = start; step = Proof proof})
    | Some dissection ->
        Lwt.return
          (Some
             Sc_rollup_game_repr.{choice = start; step = Dissection dissection})

  (** There are two kinds of strategies, random and machine-directed. *)
  type strategy = Random | MachineDirected

  (**
  [find_conflict dissection] finds the section (if it exists) in a dissection that 
    conflicts  with the actual computation. *)
  let find_conflict dissection =
    let rec aux states =
      match states with
      | (start_state, start_tick) :: (next_state, next_tick) :: rest ->
          let* c0 = conflicting_section start_tick start_state in
          let* c = conflicting_section next_tick next_state in
          if c0 then assert false
          else if c then
            if next_state = None then Lwt.return None
            else
              Lwt.return
                (Some ((start_state, start_tick), (next_state, next_tick)))
          else aux ((next_state, next_tick) :: rest)
      | _ -> Lwt.return None
    in
    aux dissection

  (** [next_move  branching dissection] finds the next move based on a 
  dissection.
  It finds the first section of dissection that conflicts with the evaluation.
  If the section has length one tick it returns a move with a Conclude
  conflict_resolution_step.
  If the section is longer it creates a new dissection with branching
  many pieces and returns
   a move with a Refine type conflict_resolution_step.
   *)
  let next_move dissection =
    let* conflict = find_conflict dissection in

    match conflict with
    | Some ((_, start_tick), (_, next_tick)) ->
        let* start_state, _ =
          state_at
            start_tick
            PVM.Utils.default_state
            Sc_rollup_tick_repr.initial
        in
        let* next_dissection =
          dissection_of_section start_tick start_state next_tick
        in

        let* stop_state, _ =
          state_at next_tick PVM.Utils.default_state Sc_rollup_tick_repr.initial
        in
        let* refutation =
          match next_dissection with
          | None ->
              let* start_hash =
                match start_state with
                | None -> Lwt.return None
                | Some state ->
                    let* s = PVM.state_hash state in
                    Lwt.return (Some s)
              in
              let* stop_hash =
                match stop_state with
                | None -> Lwt.return None
                | Some state ->
                    let* s = PVM.state_hash state in
                    Lwt.return (Some s)
              in
              let* proof = PVM.Utils.make_proof start_hash stop_hash true in
              Lwt.return {choice = start_tick; step = Proof proof}
          | Some next_dissection ->
              Lwt.return
                {choice = start_tick; step = Dissection next_dissection}
        in

        Lwt.return (Some refutation)
    | None -> Lwt.return None

  (** This is an automatic client. It generates a "perfect" client.*)
  let machine_directed =
    let start_state = PVM.Utils.default_state in
    let initial =
      let* stop_at, stop_state =
        execute_until Sc_rollup_tick_repr.initial start_state @@ fun _ _ ->
        false
      in
      let* stop_hash = PVM.state_hash stop_state in
      Lwt.return (Some (stop_at, stop_hash))
    in

    let next_move game =
      let dissection = game.dissection in
      let* mv = next_move dissection in
      match mv with
      | Some move -> Lwt.return (Some move)
      | None -> Lwt.return None
    in
    {initial; next_move}

  (** This builds a client from a strategy.
    If the strategy is MachineDirected it uses the above constructions.
    If the strategy is random then it uses a random section for the initial
    commitments and  the random decision for the next move.*)
  let player_from_strategy = function
    | Random ->
        let initial =
          let random_state = PVM.Utils.default_state in
          let* stop_hash = PVM.state_hash random_state in
          let random_tick = random_tick ~from:1 () in
          Lwt.return (Some (random_tick, stop_hash))
        in

        {initial; next_move = (fun game -> random_decision game.dissection)}
    | MachineDirected -> machine_directed

  (** [test_strategies defender_strategy refuter_strategy expectation inbox]
    runs a game based oin the two given strategies and checks that the
     resulting outcome fits the expectations. *)
  let test_strategies defender_strategy refuter_strategy expectation inbox =
    let defender_client = player_from_strategy defender_strategy in
    let refuter_client = player_from_strategy refuter_strategy in
    let* outcome = run ~inbox ~defender_client ~refuter_client in
    return (expectation outcome)

  (** the possible expectation functions *)
  let defender_wins = equal_outcome Defender_wins

  let refuter_wins = equal_outcome Refuter_wins

  let all_win _ = true
end

(** the following are the possible combinations of strategies*)
let perfect_perfect (module P : TestPVM) inbox =
  let module R = Strategies (P) in
  R.test_strategies MachineDirected MachineDirected R.defender_wins inbox

let random_random (module P : TestPVM) inbox =
  let module S = Strategies (P) in
  S.test_strategies Random Random S.all_win inbox

let random_perfect (module P : TestPVM) inbox =
  let module S = Strategies (P) in
  S.test_strategies Random MachineDirected S.refuter_wins inbox

let perfect_random (module P : TestPVM) inbox =
  let module S = Strategies (P) in
  S.test_strategies MachineDirected Random S.defender_wins inbox

(** This assembles a test from a RandomPVM and a function that choses the
  type of strategies *)
let testing_randomPVM
    (f : (module TestPVM) -> Sc_rollup_inbox_repr.t -> bool Lwt.t) name =
  let open QCheck2 in
  Test.make
    ~name
    Gen.(list_size small_int (int_range 0 100))
    (fun initial_prog ->
      assume (initial_prog <> []) ;
      let rollup = Sc_rollup_repr.Address.hash_string [""] in
      let level =
        Raw_level_repr.of_int32 0l |> function Ok x -> x | _ -> assert false
      in
      let inbox = Sc_rollup_inbox_repr.empty rollup level in
      Lwt_main.run
      @@ f
           (module MakeRandomPVM (struct
             let initial_prog = initial_prog
           end))
           inbox)

(** This assembles a test from a CountingPVM and a function that choses
the type of strategies *)
let testing_countPVM
    (f : (module TestPVM) -> Sc_rollup_inbox_repr.t -> bool Lwt.t) name =
  let open QCheck2 in
  Test.make ~name Gen.small_int (fun target ->
      assume (target > 200) ;
      let rollup = Sc_rollup_repr.Address.hash_string [""] in
      let level =
        Raw_level_repr.of_int32 0l |> function Ok x -> x | _ -> assert false
      in
      let inbox = Sc_rollup_inbox_repr.empty rollup level in
      Lwt_main.run
      @@ f
           (module MakeCountingPVM (struct
             let target = target
           end))
           inbox)

let testing_arith (f : (module TestPVM) -> Sc_rollup_inbox_repr.t -> bool Lwt.t)
    name =
  let open QCheck2 in
  Test.make
    ~name
    Gen.(pair gen_list small_int)
    (fun (inputs, evals) ->
      assume (evals > 1 && evals < List.length inputs - 1) ;
      let rollup = Sc_rollup_repr.Address.hash_string [""] in
      let level =
        Raw_level_repr.of_int32 0l |> function Ok x -> x | _ -> assert false
      in
      let inbox = Sc_rollup_inbox_repr.empty rollup level in
      Lwt_main.run
      @@ f
           (module TestArith (struct
             let inputs = String.concat " " inputs

             let evals = evals
           end))
           inbox)

let test_random_dissection (module P : TestPVM) start_at length =
  let open P in
  let module S = Strategies (P) in
  let section_start_state = Utils.default_state in
  let rec aux hash =
    let new_hash = random_hash () in
    if hash = new_hash then aux hash else new_hash
  in
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
  let* option_dissection =
    S.dissection_of_section
      section_start_at
      (Some section_start_state)
      section_stop_at
  in
  let dissection =
    match option_dissection with
    | None -> raise (Invalid_argument "no dissection")
    | Some x -> x
  in
  let* start = state_hash section_start_state in
  let stop_hash = Some (aux start) in
  Lwt.return
    (Result.to_option
     @@ Sc_rollup_game_repr.check_dissection
          (Some start)
          section_start_at
          stop_hash
          section_stop_at
          dissection
    = Some ())

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
        Lwt_main.run @@ test_random_dissection (module P) start_at length);
    Test.make
      ~name:"count"
      (Gen.quad Gen.small_int Gen.small_int Gen.small_int Gen.small_int)
      (fun (target, start_at, length, branching) ->
        assume (start_at >= 0 && length > 1 && 1 < branching) ;
        let module P = MakeCountingPVM (struct
          let target = target
        end) in
        Lwt_main.run @@ test_random_dissection (module P) start_at length);
  ]

let testRandomDissection =
  let open QCheck2 in
  [
    Test.make
      ~name:"randomdissection"
      (Gen.pair Gen.small_int Gen.small_int)
      (fun (start_int, length) ->
        assume (start_int > 0 && length >= 10) ;
        let testing_lwt =
          let start_at =
            match Sc_rollup_tick_repr.of_int start_int with
            | None -> assert false
            | Some t -> t
          in
          let stop_at =
            match Sc_rollup_tick_repr.of_int (start_int + length) with
            | None -> assert false
            | Some t -> t
          in
          let start_hash = Some (random_hash ()) in
          let stop_hash = Some (random_hash ()) in

          let* dissection_opt =
            random_dissection start_at start_hash stop_at stop_hash
          in
          let dissection =
            match dissection_opt with None -> assert false | Some d -> d
          in
          let rec aux hash =
            let new_hash = Some (random_hash ()) in
            if hash = new_hash then aux hash else new_hash
          in
          let new_hash = aux stop_hash in
          Lwt.return
            (Result.to_option
             @@ Sc_rollup_game_repr.check_dissection
                  start_hash
                  start_at
                  new_hash
                  stop_at
                  dissection
            = Some ())
        in
        Lwt_main.run testing_lwt);
  ]

let () =
  Alcotest.run
    "Refutation Game"
    [
      ("Dissection tests", qcheck_wrap testDissection);
      ("Random disection", qcheck_wrap testRandomDissection);
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
      ( "ArithPVM",
        qcheck_wrap
          [
            testing_arith perfect_perfect "perfect-perfect";
            testing_arith random_random "random-random";
            testing_arith random_perfect "random-perfect";
            testing_arith perfect_random "perfect-random";
          ] );
    ]
