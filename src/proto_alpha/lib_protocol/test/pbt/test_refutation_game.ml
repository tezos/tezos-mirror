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
open Lib_test.Qcheck2_helpers

(** {2 Utils} *)

let qcheck_make_lwt =
  Lib_test.Qcheck2_helpers.qcheck_make_lwt ~extract:Lwt_main.run

let tick_to_int_exn ?(__LOC__ = __LOC__) t =
  WithExceptions.Option.get ~loc:__LOC__ (Tick.to_int t)

let tick_of_int_exn ?(__LOC__ = __LOC__) n =
  WithExceptions.Option.get ~loc:__LOC__ (Tick.of_int n)

let list_assoc ?(__LOC__ = __LOC__) key list =
  match List.assoc ~equal:( = ) key list with
  | Some value -> value
  | None ->
      QCheck2.Test.fail_reportf "list_assoc failed when called at %s" __LOC__

let print_dissection_chunk = Format.asprintf "%a" Game.pp_dissection_chunk

let print_dissection = Format.asprintf "%a" Game.pp_dissection

let print_our_states _ = "<our states>"

let expect_invalid_move expected = function
  | Error (Game.Invalid_move reason) ->
      if reason = expected then true
      else
        let pp = Game.pp_invalid_move in
        QCheck2.Test.fail_reportf
          "@[Expected reason: %a@;Actual reason: %a@]"
          pp
          expected
          pp
          reason
  | _ -> false

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
  let open Lwt_syntax in
  let* res =
    Game.Internal_for_tests.check_dissection
      ~default_number_of_sections
      ~start_chunk
      ~stop_chunk
      dissection
  in
  return (Result.is_ok res)

(** [disputed_sections ~our_states dissection] returns the list of sections
    in the [dissection] on which the player dissecting disagree with.
    It uses [our_states], an assoc list between tick and state hashes to
    compare opponent's claims against our point of view. *)
let disputed_sections ~our_states dissection =
  let open Game in
  let agree_on_state start_tick their_state =
    let idx = tick_to_int_exn start_tick in
    let our_state = list_assoc ~__LOC__ idx our_states in
    Option.equal State_hash.equal our_state their_state
  in
  let rec traverse acc = function
    | ({state_hash = their_start_state; tick = start_tick} as a)
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

let final_dissection ~our_states dissection =
  let disputed_sections = disputed_sections ~our_states dissection in
  let single_disputed_sections =
    List.filter_map
      (fun disputed_section ->
        let Game.({tick = a_tick; _}, {tick = b_tick; _}) = disputed_section in
        let distance = Tick.distance a_tick b_tick in
        if Z.Compare.(distance = Z.one) then Some disputed_section else None)
      disputed_sections
  in
  Compare.List_length_with.(single_disputed_sections > 0)

(** Build a non-random dissection from [start_chunk] to [stop_chunk] using
    [our_states] as the state hashes for each tick. *)
let build_dissection ~number_of_sections ~start_chunk ~stop_chunk ~our_states =
  let open Lwt_result_syntax in
  let state_hash_from_tick tick =
    return @@ list_assoc ~__LOC__ (tick_to_int_exn tick) our_states
  in
  let our_stop_chunk =
    Game.
      {
        stop_chunk with
        state_hash =
          list_assoc ~__LOC__ (tick_to_int_exn stop_chunk.tick) our_states;
      }
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3491

     This dissection's building does not check the number of sections. Checks should
     be added to verify that we don't generate invalid dissection and test the
     incorrect cases. *)
  Lwt_main.run
  @@ let*! r =
       Game_helpers.new_dissection
         ~start_chunk
         ~our_stop_chunk
         ~default_number_of_sections:number_of_sections
         ~state_hash_from_tick
     in
     Lwt.return @@ WithExceptions.Result.get_ok ~loc:__LOC__ r

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
          Game.{state_hash = Some child_state; tick = initial_tick};
          Game.{state_hash = None; tick = Tick.next initial_tick};
        ]
    else
      let tick = Tick.jump initial_tick ticks in
      pure
        [
          Game.{state_hash = Some parent_state; tick = initial_tick};
          Game.{state_hash = Some child_state; tick};
          Game.{state_hash = None; tick = Tick.next tick};
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
  let gen_our_states dissection ticks =
    let open QCheck2.Gen in
    let Game.{tick = initial_tick; state_hash = initial_state_hash} =
      initial_of_dissection dissection
    in
    let initial_tick = tick_to_int_exn initial_tick in
    let rec aux acc i =
      if i < 0 then return acc
      else if i = 0 then return ((initial_tick, initial_state_hash) :: acc)
      else
        let* state_hash = gen_random_hash in
        aux ((i + initial_tick, Some state_hash) :: acc) (i - 1)
    in
    aux [] ticks

  (** {3 Dissection tests} *)

  let count = 1_000

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
      let* our_states = gen_our_states dissection (succ ticks) in
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
            valid_dissection
              ~default_number_of_sections
              ~start_chunk
              ~stop_chunk
              new_dissection)

  (** Truncate a [dissection] and expect the
      {!Sc_rollup_game_repr.check_dissection} to fail with an invalid
      number of sections, where [expected_number_of_sections] is expected. *)
  let truncate_and_check_error dissection start_chunk stop_chunk
      default_number_of_sections expected_number_of_sections =
    let open Lwt_syntax in
    let truncated_dissection =
      match dissection with
      | x :: _ :: z :: rst -> x :: z :: rst
      | _ ->
          (* If the dissection is valid, this case can not be reached. *)
          assert false
    in
    let* res =
      Game.Internal_for_tests.check_dissection
        ~default_number_of_sections
        ~start_chunk
        ~stop_chunk
        truncated_dissection
    in
    let expected_len = Z.of_int expected_number_of_sections in
    let expected_reason =
      Game.Dissection_number_of_sections_mismatch
        {expected = expected_len; given = Z.pred expected_len}
    in
    return (expect_invalid_move expected_reason res)

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
        let* our_states = gen_our_states dissection (succ ticks) in
        let* new_dissection, start_hash, stop_hash =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_hash, stop_hash, number_of_sections, ticks))
      (fun ( dissection,
             start_chunk,
             stop_chunk,
             default_number_of_sections,
             distance ) ->
        let expected_len = succ distance in
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
        let* our_states = gen_our_states dissection (succ ticks) in
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
        let* our_states = gen_our_states dissection (succ ticks) in
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
        let open Lwt_syntax in
        (* Check that we can not change the start hash. *)
        let dissection_with_different_start =
          modify_start
            (fun chunk -> Game.{chunk with state_hash = Some new_state_hash})
            dissection
        in
        let* res =
          Game.Internal_for_tests.check_dissection
            ~default_number_of_sections
            ~start_chunk
            ~stop_chunk
            dissection_with_different_start
        in
        let expected_reason =
          Game.Dissection_start_hash_mismatch
            {expected = start_chunk.state_hash; given = Some new_state_hash}
        in
        return (expect_invalid_move expected_reason res))

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
        let* our_states = gen_our_states dissection (succ ticks) in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_chunk, stop_chunk, number_of_sections))
      (fun (dissection, start_chunk, stop_chunk, default_number_of_sections) ->
        let open Lwt_syntax in
        let check_failure_on_same_stop_hash stop_hash =
          let invalid_dissection =
            modify_stop
              (fun chunk -> Game.{chunk with state_hash = stop_hash})
              dissection
          in
          let stop_chunk = Game.{stop_chunk with state_hash = stop_hash} in
          let* res =
            Game.Internal_for_tests.check_dissection
              ~default_number_of_sections
              ~start_chunk
              ~stop_chunk
              invalid_dissection
          in
          let expected_reason =
            Game.Dissection_stop_hash_mismatch stop_hash
            (* match stop_hash with
             * | None -> "The stop hash should not be None."
             * | Some stop ->
             *     Format.asprintf
             *       "The stop hash should not be equal to %a"
             *       State_hash.pp
             *       stop *)
          in
          return (expect_invalid_move expected_reason res)
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
        let* our_states = gen_our_states dissection (succ ticks) in
        let* new_dissection, start_chunk, stop_chunk =
          gen_dissection ~number_of_sections ~our_states dissection
        in
        return (new_dissection, start_chunk, stop_chunk, number_of_sections))
      (fun (dissection, start_chunk, stop_chunk, default_number_of_sections) ->
        let open Lwt_syntax in
        let expected_reason dissection =
          match (List.hd dissection, List.last_opt dissection) with
          | Some Game.{tick = a_tick; _}, Some {tick = b_tick; _} ->
              Game.Dissection_edge_ticks_mismatch
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
              (fun chunk -> Game.{chunk with tick = Tick.next chunk.tick})
              dissection
          in
          let* res =
            Game.Internal_for_tests.check_dissection
              ~default_number_of_sections
              ~start_chunk
              ~stop_chunk
              invalid_dissection
          in
          let expected_reason = expected_reason invalid_dissection in
          return (expect_invalid_move expected_reason res)
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
        let* our_states = gen_our_states dissection (succ ticks) in
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
        let open Lwt_syntax in
        (* We put a distance of [1] in every section. Then, we put the
           distance's left in the [picked_section], it will create
           an invalid section. *)
        let distance =
          Z.succ @@ Tick.distance start_chunk.tick stop_chunk.tick
        in
        let max_section_length =
          Z.(succ @@ (distance - of_int default_number_of_sections))
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
                  (Game.{b with tick}, tick)
                else
                  let tick = Tick.jump tick section_length in
                  (Game.{b with tick}, tick)
              in
              a :: replace_distances tick (k - 1) (b :: xs)
          | xs -> xs
        in
        let invalid_dissection =
          replace_distances start_chunk.tick picked_section dissection
        in
        let* res =
          Game.Internal_for_tests.check_dissection
            ~default_number_of_sections
            ~start_chunk
            ~stop_chunk
            invalid_dissection
        in
        let expected_reason = Game.Dissection_invalid_distribution in

        return (expect_invalid_move expected_reason res))

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

let tests = Dissection.tests :: Test_refutation_game_legacy.tests

let () = Alcotest.run "Refutation_game" tests
