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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/pbt/main.exe \
                  -- --file test_refutation_game.ml
    Subject:      SCORU refutation game
*)
open Protocol

open Alpha_context
open Sc_rollup
open Qcheck2_helpers
open Sc_rollup_helpers

(** {2 Utils} *)

let qcheck_make_lwt = qcheck_make_lwt ~extract:Lwt_main.run

let qcheck_make_lwt_res ?print ?count ~name ~gen f =
  qcheck_make_result
    ~pp_error:Error_monad.pp_print_trace
    ?print
    ?count
    ~name
    ~gen
    (fun a -> Lwt_main.run (f a))

let tick_to_int_exn ?(__LOC__ = __LOC__) t =
  WithExceptions.Option.get ~loc:__LOC__ (Tick.to_int t)

let tick_of_int_exn ?(__LOC__ = __LOC__) n =
  WithExceptions.Option.get ~loc:__LOC__ (Tick.of_int n)

let number_of_ticks_of_int64_exn ?(__LOC__ = __LOC__) n =
  WithExceptions.Option.get ~loc:__LOC__ (Number_of_ticks.of_value n)

let game_status_of_refute_op_result = function
  | [
      Apply_results.Operation_metadata
        {
          contents =
            Single_result
              (Manager_operation_result
                {
                  operation_result =
                    Applied (Sc_rollup_refute_result {game_status; _});
                  _;
                });
        };
    ] ->
      game_status
  | _ -> assert false

let list_assoc (key : Tick.t) list = List.assoc ~equal:( = ) key list

let print_dissection_chunk = Format.asprintf "%a" Dissection_chunk.pp

let print_dissection = Format.asprintf "%a" Game.pp_dissection

let print_our_states _ = "<our states>"

(** Assert that the computation fails with the given message. *)
let assert_fails_with ~__LOC__ (res : unit Environment.Error_monad.tzresult)
    expected_err =
  match res with
  | Error trace ->
      let expected_trace =
        Environment.Error_monad.trace_of_error expected_err
      in
      if expected_trace = trace then Lwt.return true
      else
        let pp = Environment.Error_monad.pp_trace in
        QCheck2.Test.fail_reportf
          "@[Expected reason: %a@;Actual reason: %a@]"
          pp
          expected_trace
          pp
          trace
  | Ok () -> Lwt.return false

let initial_of_dissection dissection =
  List.hd dissection |> WithExceptions.Option.get ~loc:__LOC__

(** Modify the last section of a dissection. *)
let rec modify_stop f dissection =
  match dissection with
  | [] -> assert false
  | [chunk] -> [f chunk]
  | x :: xs ->
      let xs = modify_stop f xs in
      x :: xs

(** Modify the first section of a dissection. *)
let modify_start f dissection =
  match dissection with
  | chunk :: xs -> f chunk :: xs
  | [] -> (* The dissection can not be empty. *) assert false

(** Checks that the [dissection] is valid regarding the function
    {!Sc_rollup_game_repr.check_dissection}. *)
let valid_dissection ~default_number_of_sections ~start_chunk ~stop_chunk
    dissection =
  Game.Internal_for_tests.check_dissection
    ~default_number_of_sections
    ~start_chunk
    ~stop_chunk
    dissection
  |> Result.is_ok

(** [disputed_sections ~our_states dissection] returns the list of sections
    in the [dissection] on which the player dissecting disagree with.
    It uses [our_states], an assoc list between tick and state hashes to
    compare opponent's claims against our point of view. *)
let disputed_sections ~our_states dissection =
  let agree_on_state tick their_state =
    let our_state = list_assoc tick our_states in
    Option.equal State_hash.equal our_state their_state
  in
  let rec traverse acc = function
    | Dissection_chunk.(
        {state_hash = their_start_state; tick = start_tick} as a)
      :: ({state_hash = their_stop_state; tick = stop_tick} as b)
      :: dissection ->
        let rst = b :: dissection in
        if agree_on_state start_tick their_start_state then
          (* It's a disputed section if we agree on the start state but disagree
             on the stop. *)
          if agree_on_state stop_tick their_stop_state then traverse acc rst
          else
            let disputed_section = (a, b) in
            traverse (disputed_section :: acc) rst
        else traverse acc rst
    | _ -> acc
  in
  traverse [] dissection

let pick_disputed_sections disputed_sections =
  QCheck2.Gen.oneofl disputed_sections

let single_tick_disputed_sections disputed_sections =
  List.filter_map
    (fun disputed_section ->
      let Dissection_chunk.({tick = a_tick; _}, {tick = b_tick; _}) =
        disputed_section
      in
      let distance = Tick.distance a_tick b_tick in
      if Z.Compare.(distance = Z.one) then Some disputed_section else None)
    disputed_sections

let final_dissection ~our_states dissection =
  let disputed_sections = disputed_sections ~our_states dissection in
  let single_tick_disputed_sections =
    single_tick_disputed_sections disputed_sections
  in
  Compare.List_length_with.(single_tick_disputed_sections > 0)

(** Build a non-random dissection from [start_chunk] to [stop_chunk] using
    [our_states] as the state hashes for each tick. *)
let build_dissection ~number_of_sections ~start_chunk ~stop_chunk ~our_states =
  let open Lwt_result_syntax in
  let state_of_tick ?start_state:_ tick =
    return @@ list_assoc tick our_states
  in
  let state_hash_of_eval_state = Fun.id in
  let our_stop_chunk =
    Dissection_chunk.
      {stop_chunk with state_hash = list_assoc stop_chunk.tick our_states}
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3491

     This dissection's building does not check the number of sections. Checks should
     be added to verify that we don't generate invalid dissection and test the
     incorrect cases. *)
  Lwt_main.run
  @@ let*! r =
       Game_helpers.(
         make_dissection
           ~state_of_tick
           ~state_hash_of_eval_state
           ~start_chunk
           ~our_stop_chunk
         @@ default_new_dissection
              ~start_chunk
              ~our_stop_chunk
              ~default_number_of_sections:number_of_sections)
     in
     Lwt.return @@ WithExceptions.Result.get_ok ~loc:__LOC__ r

let originate_rollup originator block =
  let open Lwt_result_syntax in
  let* origination_operation, sc_rollup =
    Sc_rollup_helpers.origination_op (B block) originator Kind.Example_arith
  in
  let* block = Block.bake ~operations:[origination_operation] block in
  let* inbox = Context.Sc_rollup.inbox (B block) in
  let+ genesis_info = Context.Sc_rollup.genesis_info (B block) sc_rollup in
  (block, sc_rollup, inbox, genesis_info)

(** [create_ctxt ()] creates a context where an arith rollup was originated,
    and both [account1] and [account2] owns enough tez to stake on a
    commitment. *)
let create_ctxt () =
  WithExceptions.Result.get_ok ~loc:__LOC__
  @@ Lwt_main.run
  @@
  let open Lwt_result_syntax in
  let* block, (account1, account2, account3) =
    Context.init3
      ~sc_rollup_enable:true
      ~sc_rollup_arith_pvm_enable:true
      ~consensus_threshold:0
      ~bootstrap_balances:[100_000_000_000L; 100_000_000_000L; 100_000_000_000L]
      ()
  in
  let* block, sc_rollup, inbox, genesis_info =
    originate_rollup account3 block
  in
  return (block, sc_rollup, inbox, genesis_info, (account1, account2, account3))

(** {2 Context free generators} *)

(** Generate a {!State_hash.t}.

    We use a dirty hack {!QCheck2.Gen.make_primitive} to remove the
    automatic shrinking. Shrinking on the states in a dissection can
    be confusing, it can leads to a shrunk list with the same states in
    each cell.
*)
let gen_random_hash =
  let open QCheck2.Gen in
  let gen =
    let* x = bytes_fixed_gen 32 in
    return @@ State_hash.of_bytes_exn x
  in
  (* This is not beautiful, but there is currently no other way to
     remove the shrinker. *)
  make_primitive
    ~gen:(fun rand -> generate1 ~rand gen)
    ~shrink:(fun _ -> Seq.empty)

(** Generate the number of sections in the dissection. *)
let gen_num_sections =
  let open Tezos_protocol_alpha_parameters.Default_parameters in
  let testnet = constants_test.sc_rollup.number_of_sections_in_dissection in
  let mainnet = constants_mainnet.sc_rollup.number_of_sections_in_dissection in
  let sandbox = constants_sandbox.sc_rollup.number_of_sections_in_dissection in
  QCheck2.Gen.(
    frequency
      [(5, pure mainnet); (4, pure testnet); (2, pure sandbox); (1, 4 -- 100)])

(** Generate a tick. *)
let gen_tick ?(lower_bound = 0) ?(upper_bound = 10_000) () =
  let open QCheck2.Gen in
  let+ tick = lower_bound -- upper_bound in
  tick_of_int_exn ~__LOC__ tick

(** Generate two chunks consisting in valid boundaries for a dissection *)
let gen_wasm_pvm_dissection_boundaries kind =
  let open QCheck2.Gen in
  let open Alpha_context in
  let* broken = bool in
  let* state_hash = gen_random_hash in
  let* base = Z.of_int <$> 0 -- 10_000 in
  let* len =
    Z.of_int
    <$>
    match kind with
    | `Kernel_run -> pure 1
    | `Short -> 2 -- 32
    | `Large -> 1_000 -- 10_000
  in
  let+ offset =
    if broken then 1 -- Z.to_int Sc_rollup.Wasm_2_0_0PVM.ticks_per_snapshot
    else pure 0
  in
  let start_tick =
    Sc_rollup.Tick.of_z @@ Z.(base * Sc_rollup.Wasm_2_0_0PVM.ticks_per_snapshot)
  in
  let stop_tick =
    Sc_rollup.Tick.of_z
    @@ Z.(
         ((base + len) * Sc_rollup.Wasm_2_0_0PVM.ticks_per_snapshot)
         + Z.of_int offset)
  in
  let start_chunk =
    Sc_rollup.Dissection_chunk.
      {tick = start_tick; state_hash = Some State_hash.zero}
  in
  let stop_chunk =
    Sc_rollup.Dissection_chunk.{tick = stop_tick; state_hash = Some state_hash}
  in
  (start_chunk, stop_chunk)

(** [gen_arith_pvm_messages ~gen_size] is a `correct list` generator.
    It generates a list of strings that are either integers or `+` to be
    consumed by the arithmetic PVM.
    If a `+` is found then the previous two element of the stack are poped
    then added and the result is pushed to the stack. In particular,
    lists like `[1 +]` are incorrect. *)
let gen_arith_pvm_messages ~gen_size =
  let open QCheck2.Gen in
  (* To preserve the correctness invariant, genlist is a recursive generator
     that produce a pair `(stack_size, state_list)` where  state_list is a
     correct list of integers and `+` and consuming it will produce a `stack`
     of length `stack_size`.
     For example a result can be `(3, [1; 2; +; 3; +; 2; 2; +; 1;]).
     Consuming the list will produce the stack`[6; 4; 1]` which has length 3. *)
  let produce_inputs self fuel =
    match fuel with
    | 0 -> map (fun x -> (1, [string_of_int x])) small_nat
    | n ->
        (* The generator has two branches.
           1. with frequency 1 adds integers to state_list and increases the
              corresponding stack_size.
           2. With frequency 2, at each step, it looks at the inductive result
              [(self (n - 1)) = (stack_size, state_list)].

           If the stack_size is smaller than 2 then it adds an integer to the
           state_list and increases the stack_size. Otherwise, it adds a plus
           to the state_list and decreases the stack_size. *)
        frequency
          [
            ( 2,
              map2
                (fun x (stack_size, state_list) ->
                  if stack_size >= 2 then (stack_size - 1, "+" :: state_list)
                  else (stack_size + 1, string_of_int x :: state_list))
                small_nat
                (self (n / 2)) );
            ( 1,
              map2
                (fun x (i, y) -> (i + 1, string_of_int x :: y))
                small_nat
                (self (n / 2)) );
          ]
  in
  let+ inputs = sized_size gen_size @@ fix produce_inputs in
  snd inputs |> List.rev |> String.concat " "

(** Generate a list of level and associated arith pvm messages. *)
let gen_arith_pvm_payloads_for_levels ~start_level ~max_level =
  gen_payloads_for_levels
    ~start_level
    ~max_level
    (gen_arith_pvm_messages ~gen_size:(QCheck2.Gen.pure 0))

(** Dissection helpers and tests *)
module Dissection = struct
  (** Generate an initial *valid* dissection. The validity comes from a
      mirrored implementation of {!Sc_rollup_game_repr.initial}. *)
  let gen_initial_dissection ?ticks () =
    let open QCheck2.Gen in
    let* child_state = gen_random_hash and* parent_state = gen_random_hash in
    let* ticks =
      let+ ticks =
        match ticks with
        | None -> frequency [(1, pure 0); (9, 1 -- 1_000)]
        | Some distance -> pure distance
      in
      Z.of_int ticks
    in
    let* initial_tick = gen_tick () in
    if Z.Compare.(ticks = Z.zero) then
      pure
        [
          Dissection_chunk.{state_hash = Some child_state; tick = initial_tick};
          Dissection_chunk.{state_hash = None; tick = Tick.next initial_tick};
        ]
    else
      let tick = Tick.jump initial_tick ticks in
      pure
        [
          Dissection_chunk.{state_hash = Some parent_state; tick = initial_tick};
          Dissection_chunk.{state_hash = Some child_state; tick};
          Dissection_chunk.{state_hash = None; tick = Tick.next tick};
        ]

  (** Generate a *valid* dissection.
      It returns the dissection alongside the dissected start_chunk and
      stop_chunk, but also the number of sections used to generate the
      dissection. *)
  let gen_dissection ~number_of_sections ~our_states dissection =
    let open QCheck2.Gen in
    let disputed_sections = disputed_sections ~our_states dissection in
    assert (Compare.List_length_with.(disputed_sections > 0)) ;
    let+ start_chunk, stop_chunk = pick_disputed_sections disputed_sections in
    let dissection =
      build_dissection ~number_of_sections ~start_chunk ~stop_chunk ~our_states
    in
    (dissection, start_chunk, stop_chunk)

  let gen_initial_dissection_ticks = QCheck2.Gen.(0 -- 1_000)

  let gen_nonfinal_initial_dissection_ticks = QCheck2.Gen.(3 -- 1_000)

  (** Given an initial tick and state_hash: generates random state hashes for
      every others [ticks].
      Having [our_states] provide the state hashes you believe to
      be true. You can then generate a dissection from another one when
      you disagree with some sections. *)
  let gen_our_states start_chunk ticks =
    let open QCheck2.Gen in
    let Dissection_chunk.{tick = initial_tick; state_hash = initial_state_hash}
        =
      start_chunk
    in
    let initial_state_hash =
      WithExceptions.Option.get ~loc:__LOC__ initial_state_hash
    in
    let initial_tick_int = tick_to_int_exn initial_tick in
    let rec aux acc i =
      if i < 0 then return acc
      else if i = 0 then return ((initial_tick, initial_state_hash) :: acc)
      else
        let* state_hash = gen_random_hash in
        let tick = tick_of_int_exn (i + initial_tick_int) in
        aux ((tick, state_hash) :: acc) (i - 1)
    in
    aux [] ticks

  (** {3 Dissection tests} *)

  let count = 300

  (** Test the validity of dissection generated by {!gen_dissection} on
      an initial dissection generated by {!gen_initial_dissection}.
      It is a self test that'll help detect issues in subsequent tests;
      in case the generator does not produce valid dissections. *)
  let test_valid_gen_dissection =
    let open QCheck2 in
    let gen =
      let open Gen in
      let* number_of_sections = gen_num_sections in
      let* ticks = gen_initial_dissection_ticks in
      let* dissection = gen_initial_dissection ~ticks () in
      let* our_states =
        gen_our_states (initial_of_dissection dissection) (succ ticks)
      in
      if final_dissection ~our_states dissection then
        (* The initial dissection could not be dissected. *)
        return (dissection, None, number_of_sections, our_states)
      else
        let* new_dissection, start_hash, stop_hash =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return
          ( dissection,
            Some (new_dissection, start_hash, stop_hash),
            number_of_sections,
            our_states )
    in
    let print =
      Print.(
        quad
          print_dissection
          (option
             (triple
                print_dissection
                print_dissection_chunk
                print_dissection_chunk))
          int
          print_our_states)
    in
    qcheck_make_lwt
      ~count
      ~name:"gen_dissection produces a valid dissection"
      ~print
      ~gen
      (fun (dissection, new_dissection, default_number_of_sections, our_states)
      ->
        let open Lwt_syntax in
        match new_dissection with
        | None -> return (final_dissection ~our_states dissection)
        | Some (new_dissection, start_chunk, stop_chunk) ->
            return
            @@ valid_dissection
                 ~default_number_of_sections
                 ~start_chunk
                 ~stop_chunk
                 new_dissection)

  (** Truncate a [dissection] and expect the
      {!Sc_rollup_game_repr.check_dissection} to fail with an invalid
      number of sections, where [expected_number_of_sections] is expected. *)
  let truncate_and_check_error dissection start_chunk stop_chunk
      default_number_of_sections expected_number_of_sections =
    let truncated_dissection =
      match dissection with
      | x :: _ :: z :: rst -> x :: z :: rst
      | _ ->
          (* If the dissection is valid, this case can not be reached. *)
          assert false
    in
    let expected_len = Z.of_int expected_number_of_sections in
    let expected_reason =
      Dissection_chunk.Dissection_number_of_sections_mismatch
        {expected = expected_len; given = Z.pred expected_len}
    in
    assert_fails_with
      ~__LOC__
      (Game.Internal_for_tests.check_dissection
         ~default_number_of_sections
         ~start_chunk
         ~stop_chunk
         truncated_dissection)
      expected_reason

  (** Test that if a dissection is smaller than the default number of
      sections, the length is equal to (distance + 1) of the dissected
      section. *)
  let test_truncated_small_dissection =
    let open QCheck2 in
    qcheck_make_lwt
      ~count
      ~name:
        "distance < nb_of_sections => (len dissection = succ (dist dissection))"
      ~gen:
        (let open Gen in
        let* number_of_sections = gen_num_sections in
        let* ticks = 3 -- (number_of_sections - 1) in
        let* dissection = gen_initial_dissection ~ticks () in
        let* our_states =
          gen_our_states (initial_of_dissection dissection) (succ ticks)
        in
        let* new_dissection, start_hash, stop_hash =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_hash, stop_hash, number_of_sections, ticks))
      (fun ( dissection,
             start_chunk,
             stop_chunk,
             default_number_of_sections,
             distance ) ->
        let expected_len = distance in
        truncate_and_check_error
          dissection
          start_chunk
          stop_chunk
          default_number_of_sections
          expected_len)

  (** Test that if the distance in the dissected section is larger than
      the default number of sections, the dissection length is exactly the
      default number of sections. *)
  let test_truncated_large_dissection =
    let open QCheck2 in
    qcheck_make_lwt
      ~count
      ~name:"distance >= nb_of_sections => (len dissection = nb_of_sections"
      ~gen:
        (let open Gen in
        let* number_of_sections = gen_num_sections in
        let* ticks = number_of_sections -- 1_000 in
        let* dissection = gen_initial_dissection ~ticks () in
        let* our_states =
          gen_our_states (initial_of_dissection dissection) (succ ticks)
        in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_chunk, stop_chunk, number_of_sections))
      (fun (dissection, start_chunk, stop_chunk, default_number_of_sections) ->
        truncate_and_check_error
          dissection
          start_chunk
          stop_chunk
          default_number_of_sections
          default_number_of_sections)

  (** Test that we can not change the start chunk of a section when we produce
      a dissection. *)
  let test_immutable_start_chunk =
    let open QCheck2 in
    qcheck_make_lwt
      ~count
      ~name:"dissection.start_chunk can not change"
      ~gen:
        (let open Gen in
        let* number_of_sections = gen_num_sections in
        let* ticks = gen_nonfinal_initial_dissection_ticks in
        let* dissection = gen_initial_dissection ~ticks () in
        let* our_states =
          gen_our_states (initial_of_dissection dissection) (succ ticks)
        in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        let* new_state_hash = gen_random_hash in
        return
          ( new_dissection,
            start_chunk,
            stop_chunk,
            number_of_sections,
            new_state_hash ))
      (fun ( dissection,
             start_chunk,
             stop_chunk,
             default_number_of_sections,
             new_state_hash ) ->
        (* Check that we can not change the start hash. *)
        let dissection_with_different_start =
          modify_start
            (fun chunk ->
              Dissection_chunk.{chunk with state_hash = Some new_state_hash})
            dissection
        in
        assert_fails_with
          ~__LOC__
          (Game.Internal_for_tests.check_dissection
             ~default_number_of_sections
             ~start_chunk
             ~stop_chunk
             dissection_with_different_start)
          (Dissection_chunk.Dissection_start_hash_mismatch
             {expected = start_chunk.state_hash; given = Some new_state_hash}))

  (** Test that we can not produce a dissection that agrees with the stop hash.
      Otherwise, there would be nothing to dispute. *)
  let test_stop_hash_must_change =
    let open QCheck2 in
    qcheck_make_lwt
      ~count
      ~name:"dissection.stop_chunk must change"
      ~gen:
        (let open Gen in
        let* number_of_sections = gen_num_sections in
        let* ticks = gen_nonfinal_initial_dissection_ticks in
        let* dissection = gen_initial_dissection ~ticks () in
        let* our_states =
          gen_our_states (initial_of_dissection dissection) (succ ticks)
        in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_chunk, stop_chunk, number_of_sections))
      (fun (dissection, start_chunk, stop_chunk, default_number_of_sections) ->
        let open Lwt_syntax in
        let check_failure_on_same_stop_hash stop_hash =
          let invalid_dissection =
            modify_stop
              (fun chunk ->
                Dissection_chunk.{chunk with state_hash = stop_hash})
              dissection
          in
          let stop_chunk =
            Dissection_chunk.{stop_chunk with state_hash = stop_hash}
          in
          assert_fails_with
            ~__LOC__
            (Game.Internal_for_tests.check_dissection
               ~default_number_of_sections
               ~start_chunk
               ~stop_chunk
               invalid_dissection)
            (Dissection_chunk.Dissection_stop_hash_mismatch stop_hash)
        in
        let* b1 = check_failure_on_same_stop_hash None in
        let* b2 = check_failure_on_same_stop_hash stop_chunk.state_hash in
        return (b1 && b2))

  (** Test that we can not produce a dissection modifying the starting
      end last point of a section. *)
  let test_immutable_start_and_stop_ticks =
    let open QCheck2 in
    qcheck_make_lwt
      ~count
      ~name:
        "start_chunk.tick and stop_chunk.tick can not change in the dissection"
      ~gen:
        (let open Gen in
        let* number_of_sections = gen_num_sections in
        let* ticks = gen_nonfinal_initial_dissection_ticks in
        let* dissection = gen_initial_dissection ~ticks () in
        let* our_states =
          gen_our_states (initial_of_dissection dissection) (succ ticks)
        in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_chunk, stop_chunk, number_of_sections))
      (fun (dissection, start_chunk, stop_chunk, default_number_of_sections) ->
        let open Lwt_syntax in
        let expected_error dissection =
          match (List.hd dissection, List.last_opt dissection) with
          | Some Dissection_chunk.{tick = a_tick; _}, Some {tick = b_tick; _} ->
              Dissection_chunk.Dissection_edge_ticks_mismatch
                {
                  dissection_start_tick = a_tick;
                  dissection_stop_tick = b_tick;
                  chunk_start_tick = start_chunk.tick;
                  chunk_stop_tick = stop_chunk.tick;
                }
          | _ -> assert false
        in
        let modify_tick modify_X dissection =
          let invalid_dissection =
            modify_X
              (fun chunk ->
                Dissection_chunk.{chunk with tick = Tick.next chunk.tick})
              dissection
          in
          let expected_error = expected_error invalid_dissection in
          assert_fails_with
            ~__LOC__
            (Game.Internal_for_tests.check_dissection
               ~default_number_of_sections
               ~start_chunk
               ~stop_chunk
               invalid_dissection)
            expected_error
        in
        (* We modify the start tick and expect the failure. *)
        let* b1 = modify_tick modify_start dissection in
        (* We modify the stop tick and expect the failure. *)
        let* b2 = modify_tick modify_stop dissection in
        return (b1 && b2))

  (** Test that a valid dissection must have a proper distribution of the
      sections. That is, a section should not be geq than half of the
      dissected section's distance. *)
  let test_badly_distributed_dissection =
    let open QCheck2 in
    qcheck_make_lwt
      ~count
      ~name:"dissection must be well distributed"
      ~gen:
        (let open Gen in
        (* The test is not general enough to support all kind of number of
           sections. *)
        let number_of_sections =
          Tezos_protocol_alpha_parameters.Default_parameters.constants_mainnet
            .sc_rollup
            .number_of_sections_in_dissection
        in
        let* picked_section = 0 -- (number_of_sections - 2) in
        let* ticks = 100 -- 1_000 in
        let* dissection = gen_initial_dissection ~ticks () in
        let* our_states =
          gen_our_states (initial_of_dissection dissection) (succ ticks)
        in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return
          ( new_dissection,
            start_chunk,
            stop_chunk,
            number_of_sections,
            picked_section ))
      (fun ( dissection,
             start_chunk,
             stop_chunk,
             default_number_of_sections,
             picked_section ) ->
        (* We put a distance of [1] in every section. Then, we put the
           distance's left in the [picked_section], it will create
           an invalid section. *)
        let distance =
          Z.succ @@ Tick.distance start_chunk.tick stop_chunk.tick
        in
        let max_section_length =
          Z.(distance - of_int default_number_of_sections)
        in
        let section_length = Z.one in

        (* Replace the distance of the first [k] sections by [section_length].
           In practice, when [k = 0], we're at the last section of the
           dissection. *)
        let rec replace_distances tick k = function
          | a :: b :: xs ->
              let b, tick =
                if k = 0 then
                  let tick = Tick.jump tick max_section_length in
                  (Dissection_chunk.{b with tick}, tick)
                else
                  let tick = Tick.jump tick section_length in
                  (Dissection_chunk.{b with tick}, tick)
              in
              a :: replace_distances tick (k - 1) (b :: xs)
          | xs -> xs
        in
        let invalid_dissection =
          replace_distances start_chunk.tick picked_section dissection
        in
        let dist = Tick.distance start_chunk.tick stop_chunk.tick in
        let half_dist = Z.div dist (Z.of_int 2) in
        assert_fails_with
          ~__LOC__
          (Game.Internal_for_tests.check_dissection
             ~default_number_of_sections
             ~start_chunk
             ~stop_chunk
             invalid_dissection)
          (Dissection_chunk.Dissection_invalid_distribution half_dist))

  let tests =
    ( "Dissection",
      qcheck_wrap
        [
          test_valid_gen_dissection;
          test_truncated_small_dissection;
          test_truncated_large_dissection;
          test_immutable_start_chunk;
          test_stop_hash_must_change;
          test_immutable_start_and_stop_ticks;
          test_badly_distributed_dissection;
        ] )
end

(** {2. ArithPVM utils} *)

module ArithPVM = Arith_pvm

module Arith_test_pvm = struct
  include ArithPVM

  let initial_state () =
    let open Lwt_syntax in
    let empty = Sc_rollup_helpers.make_empty_tree () in
    let* state = initial_state ~empty in
    let* state = install_boot_sector state "" in
    return state

  let initial_hash =
    let open Lwt_syntax in
    let* state = initial_state () in
    state_hash state

  let consume_fuel = Option.map pred

  let continue_with_fuel ~our_states ~(tick : int) fuel state f =
    let open Lwt_syntax in
    match fuel with
    | Some 0 -> return (state, fuel, tick, our_states)
    | _ -> f tick our_states (consume_fuel fuel) state

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3498

     the following is almost the same code as in the rollup node, expect that it
     creates the association list (tick, state_hash). *)
  let eval_until_input ~fuel ~our_states start_tick state =
    let open Lwt_syntax in
    let rec go ~our_states fuel (tick : int) state =
      let* input_request = is_input_state state in
      match fuel with
      | Some 0 -> return (state, fuel, tick, our_states)
      | None | Some _ -> (
          match input_request with
          | No_input_required ->
              let* state = eval state in
              let* state_hash = state_hash state in
              let our_states = (tick, state_hash) :: our_states in
              go ~our_states (consume_fuel fuel) (tick + 1) state
          | Needs_reveal (Request_dal_page _pid) ->
              (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/4160
                 We assume that there are no confirmed Dal slots.
                 We'll reuse the infra to provide Dal pages in the future. *)
              let input = Sc_rollup.(Reveal (Dal_page None)) in
              let* state = set_input input state in
              let* state_hash = state_hash state in
              let our_states = (tick, state_hash) :: our_states in
              go ~our_states (consume_fuel fuel) (tick + 1) state
          | Needs_reveal (Reveal_raw_data _)
          | Needs_reveal Reveal_metadata
          | Initial | First_after _ ->
              return (state, fuel, tick, our_states))
    in
    go ~our_states fuel start_tick state

  let eval_metadata ~fuel ~our_states tick state ~metadata =
    let open Lwt_syntax in
    continue_with_fuel ~our_states ~tick fuel state
    @@ fun tick our_states fuel state ->
    let input = Sc_rollup.(Reveal (Metadata metadata)) in
    let* state = set_input input state in
    let* state_hash = state_hash state in
    let our_states = (tick, state_hash) :: our_states in
    let tick = succ tick in
    return (state, fuel, tick, our_states)

  let feed_input ~fuel ~our_states ~tick state input =
    let open Lwt_syntax in
    let* state, fuel, tick, our_states =
      eval_until_input ~fuel ~our_states tick state
    in
    continue_with_fuel ~our_states ~tick fuel state
    @@ fun tick our_states fuel state ->
    let* state = set_input input state in
    let* state_hash = state_hash state in
    let our_states = (tick, state_hash) :: our_states in
    let tick = tick + 1 in
    let* state, fuel, tick, our_states =
      eval_until_input ~fuel ~our_states tick state
    in
    return (state, fuel, tick, our_states)

  let eval_inbox ?fuel ~inputs ~tick state =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun (state, fuel, tick, our_states) input ->
        let*! state, fuel, tick, our_states =
          feed_input ~fuel ~our_states ~tick state input
        in
        return (state, fuel, tick, our_states))
      (state, fuel, tick, [])
      inputs

  let eval_inputs ~metadata ?fuel inputs_per_levels =
    let open Lwt_result_syntax in
    let*! state = initial_state () in
    let*! state_hash = state_hash state in
    let tick = 0 in
    let our_states = [(tick, state_hash)] in
    let tick = succ tick in
    (* 1. We evaluate the boot sector. *)
    let*! state, fuel, tick, our_states =
      eval_until_input ~fuel ~our_states tick state
    in
    (* 2. We evaluate the metadata. *)
    let*! state, fuel, tick, our_states =
      eval_metadata ~fuel ~our_states tick state ~metadata
    in
    (* 3. We evaluate the inbox. *)
    let* state, _fuel, tick, our_states =
      List.fold_left_es
        (fun (state, fuel, tick, our_states) inputs ->
          let* state, fuel, tick, our_states' =
            eval_inbox ?fuel ~inputs ~tick state
          in
          return (state, fuel, tick, our_states @ our_states'))
        (state, fuel, tick, our_states)
        inputs_per_levels
    in
    let our_states =
      List.sort (fun (x, _) (y, _) -> Compare.Int.compare x y) our_states
    in
    let our_states =
      List.map
        (fun (tick_int, state) -> (tick_of_int_exn tick_int, state))
        our_states
    in
    let tick = tick_of_int_exn tick in
    return (state, tick, our_states)
end

let construct_inbox_proto block list_of_messages contract =
  Sc_rollup_helpers.Protocol_inbox_with_ctxt.fill_inbox
    block
    list_of_messages
    contract

(** Construct the inbox for the protocol side. *)
let construct_inbox_proto block list_of_messages contract =
  WithExceptions.Result.get_ok ~loc:__LOC__
  @@ Lwt_main.run
  @@ construct_inbox_proto block list_of_messages contract

(** Kind of strategy a player can play

    The cheaters will have their own version of inputs. This way, they
    can produce valid proofs regarding their inboxes, but discarded by
    the protocol.
*)
type strategy =
  | Random  (** A random player will execute its own random vision of inputs. *)
  | Perfect
      (** A perfect player, never lies, always win.
          GSW 73-9 2014-2015 mindset. *)
  | Lazy  (** A lazy player will not execute all messages. *)
  | Eager  (** A eager player will not cheat until a certain point. *)
  | Keen  (** A keen player will execute more messages. *)
  | SOL_hater  (** A SOL hater will not execute the SOL input. *)
  | EOL_hater  (** A EOL hater will not execute the EOL input. *)
  | Info_hater  (** A Info per level hater will corrupt the infos. *)
  | Nostalgic
      (** A nostalgic player will execute messages at origination level. *)

let pp_strategy fmt = function
  | Random -> Format.pp_print_string fmt "Random"
  | Perfect -> Format.pp_print_string fmt "Perfect"
  | Lazy -> Format.pp_print_string fmt "Lazy"
  | Eager -> Format.pp_print_string fmt "Eager"
  | Keen -> Format.pp_print_string fmt "Keen"
  | SOL_hater -> Format.pp_print_string fmt "SOL hater"
  | EOL_hater -> Format.pp_print_string fmt "EOL hater"
  | Info_hater -> Format.pp_print_string fmt "Info per level hater"
  | Nostalgic -> Format.pp_print_string fmt "Nostalgic"

type player = {
  pkh : Signature.Public_key_hash.t;
  contract : Contract.t;
  strategy : strategy;
  game_player : Game.player;
}

let pp_player ppf {pkh; contract = _; strategy; game_player} =
  Format.fprintf
    ppf
    "pkh: %a@,strategy: %a@,game_player: %s"
    Signature.Public_key_hash.pp_short
    pkh
    pp_strategy
    strategy
    (if Game.player_equal game_player Alice then "Alice" else "Bob")

type player_client = {
  player : player;
  states : (Tick.t * State_hash.t) list;
  final_tick : Tick.t;
  inbox : Sc_rollup_helpers.Node_inbox.t;
  payloads_per_levels : payloads_per_level list;
  metadata : Metadata.t;
  context : Tezos_context_memory.Context_binary.t;
}

let pp_player_client ppf
    {
      player;
      states = _;
      final_tick;
      inbox = _;
      payloads_per_levels = _;
      metadata = _;
      context = _;
    } =
  Format.fprintf
    ppf
    "@[<v 2>player:@,%a@]@,final tick: %a@"
    pp_player
    player
    Tick.pp
    final_tick

module Player_client = struct
  let empty_memory_ctxt id =
    let open Lwt_syntax in
    Lwt_main.run
    @@ let+ index = Tezos_context_memory.Context_binary.init id in
       Tezos_context_memory.Context_binary.empty index

  (** Construct an inbox based on [list_of_messages] in the player context. *)
  let construct_inbox ~inbox list_of_messages =
    let history = Sc_rollup.Inbox.History.empty ~capacity:10000L in
    let payloads_histories = Payloads_histories.empty in
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Sc_rollup_helpers.Node_inbox.fill_inbox
         ~inbox_creation_level:Raw_level.root
         {inbox; history; payloads_histories}
         list_of_messages

  (** Generate [our_states] for [payloads_per_levels] based on the strategy.
      It needs [start_level] and [max_level] in case it will need to generate
      new inputs. *)
  let gen_our_states ~metadata strategy ~start_level ~max_level
      payloads_per_levels =
    let open QCheck2.Gen in
    let eval_inputs (payloads_per_levels : payloads_per_level list) =
      Lwt_main.run
      @@
      let open Lwt_result_syntax in
      let inputs_per_levels =
        List.map (fun {inputs; _} -> inputs) payloads_per_levels
      in
      let*! r = Arith_test_pvm.eval_inputs ~metadata inputs_per_levels in
      Lwt.return @@ WithExceptions.Result.get_ok ~loc:__LOC__ r
    in
    match strategy with
    | Perfect ->
        (* The perfect player does not lie, evaluates correctly the inputs. *)
        let _state, tick, our_states = eval_inputs payloads_per_levels in
        return (tick, our_states, payloads_per_levels)
    | Random ->
        (* Random player generates its own list of inputs. *)
        let* new_payloads_per_levels =
          gen_arith_pvm_payloads_for_levels ~start_level ~max_level
        in
        let _state, tick, our_states = eval_inputs new_payloads_per_levels in
        return (tick, our_states, new_payloads_per_levels)
    | Lazy ->
        (* Lazy player removes inputs from [payloads_per_levels]. *)
        let n = List.length payloads_per_levels in
        let* remove_k = 1 -- n in
        let new_inputs = List.take_n (n - remove_k) payloads_per_levels in
        let _state, tick, our_states = eval_inputs new_inputs in
        return (tick, our_states, new_inputs)
    | Eager ->
        (* Eager player executes correctly the inbox until a certain point. *)
        let* corrupt_at_level = 0 -- (List.length payloads_per_levels - 1) in
        let payloads_per_level =
          Stdlib.List.nth payloads_per_levels corrupt_at_level
          |> fun {payloads; _} -> List.length payloads
        in
        let* corrupt_at_k = 0 -- payloads_per_level in
        let payloads_per_levels =
          List.mapi
            (fun l payloads_per_level ->
              if l = corrupt_at_level then
                let inputs =
                  List.mapi
                    (fun k input ->
                      if k = corrupt_at_k then
                        make_input
                          ~inbox_level:(Raw_level.of_int32_exn 42l)
                          ~message_counter:(Z.of_int 42)
                          (make_external_inbox_message "foo")
                      else input)
                    payloads_per_level.inputs
                in
                {payloads_per_level with inputs}
              else payloads_per_level)
            payloads_per_levels
        in
        let _state, tick, our_states = eval_inputs payloads_per_levels in
        return (tick, our_states, payloads_per_levels)
    | Keen ->
        (* Keen player will add more inputs. *)
        let* offset = 1 -- 5 in
        let* new_payloads_per_levels =
          gen_arith_pvm_payloads_for_levels
            ~start_level:max_level
            ~max_level:(max_level + offset)
        in
        let new_payloads_per_levels =
          payloads_per_levels @ new_payloads_per_levels
        in
        let _state, tick, our_states = eval_inputs new_payloads_per_levels in
        return (tick, our_states, new_payloads_per_levels)
    | SOL_hater ->
        let new_payloads_per_levels =
          List.map
            (fun payloads_per_level ->
              {
                payloads_per_level with
                inputs = Stdlib.List.tl payloads_per_level.inputs;
              })
            payloads_per_levels
        in
        let _state, tick, our_states = eval_inputs new_payloads_per_levels in
        return (tick, our_states, new_payloads_per_levels)
    | EOL_hater ->
        let new_payloads_per_levels =
          List.map
            (fun payloads_per_level ->
              let inputs =
                let rev_inputs = List.rev payloads_per_level.inputs in
                let without_eol = Stdlib.List.tl rev_inputs in
                List.rev without_eol
              in
              {payloads_per_level with inputs})
            payloads_per_levels
        in
        let _state, tick, our_states = eval_inputs new_payloads_per_levels in
        return (tick, our_states, new_payloads_per_levels)
    | Info_hater ->
        let* corrupt_at_l = 0 -- List.length payloads_per_levels in
        let dumb_timestamp = Timestamp.of_seconds 42L in
        let dumb_predecessor = Block_hash.zero in

        let new_payloads_per_levels =
          List.mapi
            (fun l payloads_per_level ->
              if l = corrupt_at_l then
                {
                  payloads_per_level with
                  predecessor_timestamp = dumb_timestamp;
                  predecessor = dumb_predecessor;
                }
              else payloads_per_level)
            payloads_per_levels
        in
        let _state, tick, our_states = eval_inputs new_payloads_per_levels in
        return (tick, our_states, new_payloads_per_levels)
    | Nostalgic ->
        (* [payloads_per_levels] starts at [orignation_level + 1], the nostalgic
           player will execute messages at [origination_level]. *)
        let* messages =
          small_list (gen_arith_pvm_messages ~gen_size:(pure 0))
        in
        let payloads_at_origination =
          Sc_rollup_helpers.wrap_messages metadata.origination_level messages
        in
        let new_payloads_per_levels =
          payloads_at_origination :: payloads_per_levels
        in
        let _state, tick, our_states = eval_inputs new_payloads_per_levels in
        return (tick, our_states, new_payloads_per_levels)

  (** [gen ~inbox ~rollup ~origination_level ~start_level ~max_level player
      payloads_per_levels] generates a {!player_client} based on
      its {!player.strategy}. *)
  let gen ~inbox ~rollup ~origination_level ~start_level ~max_level player
      payloads_per_levels =
    let open QCheck2.Gen in
    let ctxt = empty_memory_ctxt "foo" in
    let metadata = Sc_rollup.Metadata.{address = rollup; origination_level} in
    let* tick, our_states, payloads_per_levels =
      gen_our_states
        ~metadata
        player.strategy
        ~start_level
        ~max_level
        payloads_per_levels
    in
    let inbox = construct_inbox ~inbox payloads_per_levels in
    return
      {
        player;
        final_tick = tick;
        states = our_states;
        inbox;
        payloads_per_levels;
        metadata;
        context = ctxt;
      }
end

(** [create_commitment ~predecessor ~inbox_level ~our_states] creates
    a commitment using [our_states] as the vision of ticks. *)
let create_commitment ~predecessor ~inbox_level ~our_states =
  let open Lwt_syntax in
  let inbox_level = Int32.of_int inbox_level |> Raw_level.of_int32_exn in
  let+ compressed_state =
    match List.last_opt our_states with
    | None ->
        (* No tick evaluated. *)
        Arith_test_pvm.initial_hash
    | Some (_, state) -> return state
  in

  let number_of_ticks =
    match our_states with
    | [] -> Number_of_ticks.zero
    | _ ->
        List.length our_states - 1
        |> Int64.of_int |> number_of_ticks_of_int64_exn
  in
  Commitment.{compressed_state; inbox_level; predecessor; number_of_ticks}

(** [operation_publish_commitment block rollup lcc inbox_level p1_client]
    creates a commitment and stake on it. *)
let operation_publish_commitment ctxt rollup predecessor inbox_level
    player_client =
  let open Lwt_result_syntax in
  let*! commitment =
    create_commitment ~predecessor ~inbox_level ~our_states:player_client.states
  in
  let* op =
    Op.sc_rollup_publish ctxt player_client.player.contract rollup commitment
  in
  return (op, commitment)

(** [build_proof ~player_client start_tick game] builds a valid proof
    regarding the vision [player_client] has. The proof refutes the
    [start_tick]. *)
let build_proof ~player_client start_tick (game : Game.t) =
  let open Lwt_result_syntax in
  (* No messages are added between [game.start_level] and the current level
     so we can take the existing inbox of players. Otherwise, we should find the
     inbox of [start_level]. *)
  let Sc_rollup_helpers.Node_inbox.{payloads_histories; history; inbox} =
    player_client.inbox
  in
  let get_payloads_history witness_hash =
    Payloads_histories.find witness_hash payloads_histories
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Lwt.return
  in
  let history_proof = Inbox.old_levels_messages inbox in
  (* We start a game on a commitment that starts at [Tick.initial], the fuel
     is necessarily [start_tick]. *)
  let fuel = tick_to_int_exn start_tick in
  let metadata = player_client.metadata in
  let inputs_per_levels =
    List.map (fun {inputs; _} -> inputs) player_client.payloads_per_levels
  in
  let*! r = Arith_test_pvm.eval_inputs ~metadata ~fuel inputs_per_levels in
  let state, _, _ = WithExceptions.Result.get_ok ~loc:__LOC__ r in
  let module P = struct
    include Arith_test_pvm

    let initial_state ~empty:_ = initial_state ()

    let context = player_client.context

    let state = state

    let reveal _ = assert false

    module Inbox_with_history = struct
      let inbox = history_proof

      let get_history inbox = Inbox.History.find inbox history |> Lwt.return

      let get_payloads_history = get_payloads_history
    end

    (* FIXME/DAL-REFUTATION: https://gitlab.com/tezos/tezos/-/issues/3992
       Extend refutation game to handle Dal refutation case. *)
    module Dal_with_history = struct
      let confirmed_slots_history = Dal.Slots_history.genesis

      let get_history _hash = Lwt.return_none

      let page_info = None

      let dal_parameters =
        Default_parameters.constants_test.dal.cryptobox_parameters

      let dal_attestation_lag =
        Default_parameters.constants_test.dal.attestation_lag
    end
  end in
  let*! proof = Sc_rollup.Proof.produce ~metadata (module P) game.inbox_level in
  return (WithExceptions.Result.get_ok ~loc:__LOC__ proof)

(** [next_move ~number_of_sections ~player_client game] produces
    the next move in the refutation game.

    If there is a disputed section where the distance is one tick, it
    produces a proof. Otherwise, provides another dissection.
*)
let next_move ~player_client (game : Game.t) =
  let open Lwt_result_syntax in
  match game.game_state with
  | Dissecting {dissection; default_number_of_sections} -> (
      let disputed_sections =
        disputed_sections ~our_states:player_client.states dissection
      in
      assert (Compare.List_length_with.(disputed_sections > 0)) ;
      let single_tick_disputed_sections =
        single_tick_disputed_sections disputed_sections
      in
      match single_tick_disputed_sections with
      | (start_chunk, _stop_chunk) :: _ ->
          let tick = start_chunk.tick in
          let+ proof = build_proof ~player_client tick game in
          Game.(Move {choice = tick; step = Proof proof})
      | [] ->
          (* If we reach this case, there is necessarily a disputed section. *)
          let start_chunk, stop_chunk = Stdlib.List.hd disputed_sections in
          let dissection =
            build_dissection
              ~number_of_sections:default_number_of_sections
              ~start_chunk
              ~stop_chunk
              ~our_states:player_client.states
          in
          return
            Game.(
              Move {choice = start_chunk.tick; step = Dissection dissection}))
  | Final_move {agreed_start_chunk; refuted_stop_chunk = _} ->
      let tick = agreed_start_chunk.tick in
      let+ proof = build_proof ~player_client tick game in
      Game.(Move {choice = tick; step = Proof proof})

type game_result_for_tests = Defender_wins | Refuter_wins

(** Play until there is an {!game_result_for_tests}.

    A game result can happen if:
    - A valid refutation was provided to the protocol and it succeeded to
      win the game.
    - A player played an invalid refutation and was rejected by the
      protocol.
*)
let play_until_game_result ~refuter_client ~defender_client ~rollup block =
  let rec play ~player_turn ~opponent block =
    let open Lwt_result_syntax in
    let* games =
      Context.Sc_rollup.ongoing_games_for_staker
        (B block)
        rollup
        player_turn.player.pkh
    in
    let game_opt = List.hd games in
    let game, _, _ = WithExceptions.Option.get ~loc:__LOC__ game_opt in
    let* refutation = next_move ~player_client:player_turn game in
    let* incr = Incremental.begin_construction block in
    let* operation_refutation =
      Op.sc_rollup_refute
        (I incr)
        player_turn.player.contract
        rollup
        opponent.player.pkh
        refutation
    in
    let* incr = Incremental.add_operation incr operation_refutation in
    match game_status_of_refute_op_result (Incremental.rev_tickets incr) with
    | Ongoing ->
        let* block = Incremental.finalize_block incr in
        play ~player_turn:opponent ~opponent:player_turn block
    | Ended (Loser {reason = _; loser}) ->
        if loser = Account.pkh_of_contract_exn refuter_client.player.contract
        then return Defender_wins
        else return Refuter_wins
    | Ended Draw ->
        QCheck2.Test.fail_reportf "Game ended in a draw, which is unexpected"
  in
  play ~player_turn:refuter_client ~opponent:defender_client block

(** Generate two {!player}s with a given strategy. *)
let make_players ~p1_strategy ~contract1 ~p2_strategy ~contract2 =
  let pkh1 = Account.pkh_of_contract_exn contract1 in
  let pkh2 = Account.pkh_of_contract_exn contract2 in
  let ({alice; bob = _} : Game.Index.t) = Game.Index.make pkh1 pkh2 in
  let player1, player2 =
    if Signature.Public_key_hash.equal alice pkh1 then Game.(Alice, Bob)
    else Game.(Bob, Alice)
  in
  ( {
      pkh = pkh1;
      contract = contract1;
      strategy = p1_strategy;
      game_player = player1;
    },
    {
      pkh = pkh2;
      contract = contract2;
      strategy = p2_strategy;
      game_player = player2;
    } )

(** [gen_game ~p1_strategy ~p2_strategy] generates a context where a rollup
    was originated.
    It generates inputs for the rollup, and creates the players' interpretation
    of these inputs in a {!player_client} for [p1_strategy] and [p2_strategy].
*)
let gen_game ~p1_strategy ~p2_strategy =
  let open QCheck2.Gen in
  (* If there is no good player, we do not care about the result. *)
  assert (p1_strategy = Perfect || p2_strategy = Perfect) ;
  let block, rollup, inbox, genesis_info, (contract1, contract2, contract3) =
    create_ctxt ()
  in
  let p1, p2 = make_players ~p1_strategy ~contract1 ~p2_strategy ~contract2 in

  (* Create a context with a rollup originated. *)
  let commitment_period =
    Tezos_protocol_alpha_parameters.Default_parameters.constants_mainnet
      .sc_rollup
      .commitment_period_in_blocks
  in
  let origination_level =
    Raw_level.to_int32 genesis_info.level |> Int32.to_int
  in
  let start_level = origination_level + 1 in
  let max_level = start_level + commitment_period in
  let* payloads_per_levels =
    gen_arith_pvm_payloads_for_levels ~start_level ~max_level
  in

  let block, payloads_per_levels =
    construct_inbox_proto block payloads_per_levels contract3
  in

  let* p1_client =
    Player_client.gen
      ~inbox
      ~origination_level:genesis_info.level
      ~start_level
      ~max_level
      ~rollup
      p1
      payloads_per_levels
  in
  let* p2_client =
    Player_client.gen
      ~inbox
      ~origination_level:genesis_info.level
      ~start_level
      ~max_level
      ~rollup
      p2
      payloads_per_levels
  in
  let* p1_start = bool in
  let commitment_level = origination_level + commitment_period in
  return
    ( block,
      rollup,
      commitment_level,
      genesis_info.commitment_hash,
      p1_client,
      p2_client,
      p1_start,
      payloads_per_levels )

(** Shrinker is really slow. Deactivating it. *)
let gen_game ~p1_strategy ~p2_strategy =
  let open QCheck2.Gen in
  make_primitive
    ~gen:(fun rand -> generate1 ~rand (gen_game ~p1_strategy ~p2_strategy))
    ~shrink:(fun _ -> Seq.empty)

(** [prepare_game block rollup lcc commitment_level p1_client p2_client contract
    list_of_messages] prepares a context where [p1_client] and [p2_client]
    are in conflict for one commitment. *)
let prepare_game ~p1_start block rollup lcc commitment_level p1_client p2_client
    =
  let open Lwt_result_syntax in
  let* p1_op, p1_commitment =
    operation_publish_commitment (B block) rollup lcc commitment_level p1_client
  in
  let* p2_op, p2_commitment =
    operation_publish_commitment (B block) rollup lcc commitment_level p2_client
  in
  let commit_then_commit_and_refute ~defender_op ~refuter_op refuter
      refuter_commitment defender defender_commitment =
    let refutation =
      Sc_rollup.Game.Start
        {
          player_commitment_hash =
            Sc_rollup.Commitment.hash_uncarbonated refuter_commitment;
          opponent_commitment_hash =
            Sc_rollup.Commitment.hash_uncarbonated defender_commitment;
        }
    in
    let* start_game =
      Op.sc_rollup_refute
        (B block)
        refuter.player.contract
        rollup
        defender.player.pkh
        refutation
    in
    let* refuter_batch =
      Op.batch_operations
        ~recompute_counters:true
        ~source:refuter.player.contract
        (B block)
        [refuter_op; start_game]
    in
    let* block = Block.bake ~operations:[defender_op; refuter_batch] block in
    return (block, refuter, defender)
  in
  if p1_start then
    commit_then_commit_and_refute
      ~defender_op:p2_op
      ~refuter_op:p1_op
      p1_client
      p1_commitment
      p2_client
      p2_commitment
  else
    commit_then_commit_and_refute
      ~defender_op:p1_op
      ~refuter_op:p2_op
      p2_client
      p2_commitment
      p1_client
      p1_commitment

let check_distribution = function
  | fst :: snd :: rst ->
      let open Dissection_chunk in
      let dist = Tick.distance fst.tick snd.tick in
      let _, min_len, max_len =
        List.fold_left
          (fun (previous_tick, min_len, max_len) chunk ->
            let dist = Tick.distance previous_tick chunk.tick in
            (* We only consider length that are greater or equal than
               the snapshot size. The last one may not be as big, if
               the PVM was stuck. *)
            if Compare.Z.(dist < Sc_rollup.Wasm_2_0_0PVM.ticks_per_snapshot)
            then (chunk.tick, min_len, max_len)
            else (chunk.tick, Z.min min_len dist, Z.max max_len dist))
          (snd.tick, dist, dist)
          rst
      in
      Z.(max_len - min_len <= Sc_rollup.Wasm_2_0_0PVM.ticks_per_snapshot)
  | _ -> true

let test_wasm_dissection name kind =
  qcheck_make_lwt_res
    ~count:1_000_000
    ~name
    ~print:(fun (start_chunk, stop_chunk) ->
      Format.asprintf
        "dissection from %a to %a"
        Dissection_chunk.pp
        start_chunk
        Dissection_chunk.pp
        stop_chunk)
    ~gen:(gen_wasm_pvm_dissection_boundaries kind)
    (fun (start_chunk, stop_chunk) ->
      let open Lwt_result_syntax in
      let+ dissection =
        Game_helpers.(
          make_dissection
            ~state_of_tick:(fun ?start_state:_ _ ->
              return_some Sc_rollup.State_hash.zero)
            ~state_hash_of_eval_state:Fun.id
            ~start_chunk
            ~our_stop_chunk:stop_chunk
          @@ Wasm.new_dissection
               ~start_chunk
               ~our_stop_chunk:stop_chunk
               ~default_number_of_sections:32)
      in
      if kind <> `Kernel_run then assert (check_distribution dissection) ;
      match
        Wasm_2_0_0PVM.Protocol_implementation.check_dissection
          ~default_number_of_sections:32
          ~start_chunk
          ~stop_chunk:{stop_chunk with state_hash = Some State_hash.zero}
          dissection
      with
      | Ok () -> true
      | Error e ->
          Format.printf
            "dissection %a caused errors %a\n"
            Game.pp_dissection
            dissection
            Environment.Error_monad.pp_trace
            e ;
          false)

(** Create a test of [p1_strategy] against [p2_strategy]. One of them
    must be a {!Perfect} player, otherwise, we do not care about which
    cheater wins. *)
let test_game ?(count = 10) ~p1_strategy ~p2_strategy () =
  let name =
    Format.asprintf
      "%a against %a"
      pp_strategy
      p1_strategy
      pp_strategy
      p2_strategy
  in
  qcheck_make_lwt_res
    ~print:
      (fun ( _block,
             _rollup,
             _commitment_level,
             _lcc,
             p1_client,
             p2_client,
             p1_start,
             _payloads_per_levels ) ->
      Format.asprintf
        "@[<v>@,@[<v 2>p1:@,%a@]@,@[<v 2>p2:@,%a@]@,%s@,@]"
        pp_player_client
        p1_client
        pp_player_client
        p2_client
        (if p1_start then "p1" else "p2"))
    ~count
    ~name
    ~gen:(gen_game ~p1_strategy ~p2_strategy)
    (fun ( block,
           rollup,
           commitment_level,
           lcc,
           p1_client,
           p2_client,
           p1_start,
           _list_of_messages ) ->
      let open Lwt_result_syntax in
      (* Otherwise, there is no conflict. *)
      QCheck2.assume
        (not
           (let p1_head = List.last_opt p1_client.states in
            let p2_head = List.last_opt p2_client.states in
            Option.equal
              (fun (t1, state_hash1) (t2, state_hash2) ->
                Tick.equal t1 t2 && State_hash.equal state_hash1 state_hash2)
              p1_head
              p2_head)) ;
      let* block, refuter, defender =
        prepare_game
          ~p1_start
          block
          rollup
          lcc
          commitment_level
          p1_client
          p2_client
      in
      let* game_result =
        play_until_game_result
          ~rollup
          ~refuter_client:refuter
          ~defender_client:defender
          block
      in
      match game_result with
      | Defender_wins -> return (defender.player.strategy = Perfect)
      | Refuter_wins -> return (refuter.player.strategy = Perfect))

let test_perfect_against_random =
  test_game ~p1_strategy:Perfect ~p2_strategy:Random ()

let test_perfect_against_lazy =
  test_game ~p1_strategy:Perfect ~p2_strategy:Lazy ()

let test_perfect_against_eager =
  test_game ~p1_strategy:Perfect ~p2_strategy:Eager ()

let test_perfect_against_keen =
  test_game ~p1_strategy:Perfect ~p2_strategy:Keen ()

let test_perfect_against_sol_hater =
  test_game ~p1_strategy:Perfect ~p2_strategy:SOL_hater ()

let test_perfect_against_eol_hater =
  test_game ~p1_strategy:Perfect ~p2_strategy:EOL_hater ()

let test_perfect_against_info_hater =
  test_game ~p1_strategy:Perfect ~p2_strategy:Info_hater ()

let test_perfect_against_nostalgic =
  test_game ~p1_strategy:Perfect ~p2_strategy:Nostalgic ~count:5 ()

(* This test will behave as a regression test. *)
let test_cut_at_level =
  let open QCheck2 in
  Test.make
    ~name:"cut at level properly cuts"
    ~print:(fun (origination_level, commit_inbox_level, input_level) ->
      Format.asprintf
        "origination_level: %a, commit_inbox_level: %a, input_level: %a"
        Raw_level_repr.pp
        origination_level
        Raw_level_repr.pp
        commit_inbox_level
        Raw_level_repr.pp
        input_level)
    Gen.(
      let level =
        map
          (fun i -> Raw_level_repr.of_int32_exn (Int32.of_int i))
          (0 -- 1_000_000)
      in
      triple level level level)
    (fun (origination_level, commit_inbox_level, input_level) ->
      let input : Sc_rollup_PVM_sig.input =
        Inbox_message
          {
            inbox_level = input_level;
            message_counter = Z.zero;
            payload = Sc_rollup_inbox_message_repr.unsafe_of_string "foo";
          }
      in
      let input_cut =
        Sc_rollup_proof_repr.Internal_for_tests.cut_at_level
          ~origination_level
          ~commit_inbox_level
          input
      in
      let should_be_none =
        Raw_level_repr.(
          input_level <= origination_level || commit_inbox_level < input_level)
      in
      match input_cut with
      | Some _input -> not should_be_none
      | None -> should_be_none)

let tests =
  ( "Refutation",
    qcheck_wrap
      [
        test_wasm_dissection "dissection is one kernel_run" `Kernel_run;
        test_wasm_dissection "dissection shorter than 32 kernel_run" `Short;
        test_wasm_dissection "dissection larger than 32 kernel_run" `Large;
        test_perfect_against_random;
        test_perfect_against_lazy;
        test_perfect_against_keen;
        test_perfect_against_eager;
        test_perfect_against_sol_hater;
        test_perfect_against_eol_hater;
        test_perfect_against_info_hater;
        test_perfect_against_nostalgic;
        test_cut_at_level;
      ] )

(** {2 Entry point} *)

let tests = [tests; Dissection.tests]

let () = Alcotest.run ~__FILE__ (Protocol.name ^ ": Refutation_game") tests
