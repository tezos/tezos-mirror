(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Log_helpers

exception Test_failed

(** A scenario is a succession of actions. We define a branching path as a way to create multiple tests
    from the same point. This allows easy compositionality of behaviors with minimal code sharing.
    The [Tag] allows to give meaningful identifiers to the branches. It is good practice to tag each
    case in a branch (it's not necessary, but since test names must be unique, at most one branch can
    remain unnamed, and even then it can create conflicting names.)
 *)
type ('input, 'output) scenarios =
  | Action : ('input -> 'output tzresult Lwt.t) -> ('input, 'output) scenarios
  | Empty : ('t, 't) scenarios
  | Concat : (('a, 'b) scenarios * ('b, 'c) scenarios) -> ('a, 'c) scenarios
  | Branch : (('a, 'b) scenarios * ('a, 'b) scenarios) -> ('a, 'b) scenarios
  | Tag : (* Name for test branch *) string -> ('t, 't) scenarios
  | Slow : (* If in scenario branch, makes the test `Slow *)
      ('t, 't) scenarios

(** Unfolded scenario type *)
type ('input, 'output) single_scenario =
  | End_scenario : ('t, 't) single_scenario
  | Cons :
      (('input -> 't tzresult Lwt.t) * ('t, 'output) single_scenario)
      -> ('input, 'output) single_scenario

let rec cat_ss :
    type a b c.
    (a, b) single_scenario -> (b, c) single_scenario -> (a, c) single_scenario =
 fun a b ->
  match a with End_scenario -> b | Cons (act, a') -> Cons (act, cat_ss a' b)

let combine f l1 l2 =
  List.map (fun a -> List.map (fun b -> f a b) l2) l1 |> List.flatten

let rec unfold_scenarios :
    type input output.
    (input, output) scenarios ->
    ((input, output) single_scenario * string list * bool) list = function
  | Slow -> [(End_scenario, [], true)]
  | Tag s -> [(End_scenario, [s], false)]
  | Empty -> [(End_scenario, [], false)]
  | Action a -> [(Cons (a, End_scenario), [], false)]
  | Branch (left, right) -> unfold_scenarios left @ unfold_scenarios right
  | Concat (left, right) ->
      let l = unfold_scenarios left in
      let r = unfold_scenarios right in
      combine
        (fun (sl, tl, bl) (sr, tr, br) -> (cat_ss sl sr, tl @ tr, bl || br))
        l
        r

let rec run_scenario :
    type input output.
    (input, output) single_scenario -> input -> output tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun scenario input ->
    match scenario with
    | End_scenario -> return input
    | Cons (action, next) ->
        let* result = action input in
        run_scenario next result

type test_closure = string * bool * (Tezt_tezos.Protocol.t -> unit Lwt.t)

let unfolded_to_test :
    (unit, unit) single_scenario * string list * bool -> test_closure =
  let open Lwt_syntax in
  fun (s, title, is_slow) ->
    let title =
      match title with
      | [] -> ""
      | [n] -> n
      | header :: tags ->
          (* We chose to separate all tags with a comma, and use the head tag as a header for the test *)
          header ^ ": " ^ String.concat ", " tags
    in
    ( title,
      is_slow,
      fun _proto ->
        let* r = (run_scenario s) () in
        match r with
        | Ok () -> return_unit
        | Error e ->
            Log.error "%a@." Error_monad.pp_print_trace e ;
            raise Test_failed )

let register_test ~__FILE__ ~tags ((title, is_slow, test) : test_closure) : unit
    =
  let tags = if is_slow then Tezos_test_helpers.Tag.slow :: tags else tags in
  Tezt_tezos.Protocol.(
    register_test
      ~__FILE__
      ~title
      ~tags
      ~uses:(fun _ -> [])
      ~uses_node:false
      ~uses_client:false
      ~uses_admin_client:false
      test
      [Alpha])

let register_tests ~__FILE__ ~tags (l : test_closure list) : unit =
  List.iter (register_test ~__FILE__ ~tags) l

(** Useful aliases and operators *)

(* Aliases for [Empty]. Can be used as first component of a scenario instead of a tag if its not needed. *)
let noop = Empty

let no_tag = Empty

let concat :
    type a b c. (a, b) scenarios -> (b, c) scenarios -> (a, c) scenarios =
 fun a b ->
  match (a, b) with
  | Empty, Empty -> Empty
  | _, Empty -> a
  | Empty, _ -> b
  | _ -> Concat (a, b)

let branch : type a b. (a, b) scenarios -> (a, b) scenarios -> (a, b) scenarios
    =
 fun a b -> match (a, b) with Empty, Empty -> Empty | _ -> Branch (a, b)

(** Continuation connector: execute a then b *)
let ( --> ) a b = concat a b

(** Branching connector: creates two tests with different execution paths *)
let ( |+ ) a b = branch a b

(** Ends the test. Dump the state, returns [unit] *)
let end_test : ('a, unit) scenarios =
  let open Lwt_result_syntax in
  Action
    (fun _ ->
      Log.info ~color:begin_end_color "-- End test --" ;
      return_unit)

(** Transforms scenarios into tests *)
let tests_of_scenarios :
    (string * (unit, 't) scenarios) list -> test_closure list =
 fun scenarios ->
  List.map (fun (s, x) -> Tag s --> x --> end_test) scenarios |> function
  | [] -> []
  | a :: t ->
      List.fold_left ( |+ ) a t |> unfold_scenarios |> List.map unfolded_to_test

(** Arbitrary execution *)
let exec f = Action f

(** Execute a function that does not modify the block, only the state *)
let exec_state f =
  let open Lwt_result_syntax in
  Action
    (fun ((block, _state) as input) ->
      let* state = f input in
      return (block, state))

(** Execute a function that does not modify neither the block nor the state.
    Usually used for checks/asserts *)
let exec_unit f =
  let open Lwt_result_syntax in
  Action
    (fun input ->
      let* () = f input in
      return input)

(** [fold f l] folds [f] over [l], fails on empty list *)
let rec fold : ('a -> ('b, 'c) scenarios) -> 'a list -> ('b, 'c) scenarios =
 fun f list ->
  match list with
  | [] -> Stdlib.failwith "Scenario_dsl.fold: empty list"
  | [x] -> f x
  | h :: t -> f h |+ fold f t

(** [fold_tag f l] folds [f] over [l], [l] has a tag for each of its elements.
    Fails on empty list. *)
let fold_tag :
    ('a -> ('b, 'c) scenarios) -> (string * 'a) list -> ('b, 'c) scenarios =
 fun f ->
  let f (s, x) = Tag s --> f x in
  fold f

(** [fold_tag_f f tag l] folds [f] over [l], [tag] returns a tag for each element of [l].
    Fails on empty list. *)
let fold_tag_f :
    ('a -> ('b, 'c) scenarios) ->
    ('a -> string) ->
    'a list ->
    ('b, 'c) scenarios =
 fun f tag ->
  let f x = Tag (tag x) --> f x in
  fold f

(** [unfold f l] maps [f] over [l], and runs them in order *)
let rec unfold : ('a -> ('b, 'b) scenarios) -> 'a list -> ('b, 'b) scenarios =
 fun f -> function [] -> Empty | [x] -> f x | h :: t -> f h --> unfold f t
