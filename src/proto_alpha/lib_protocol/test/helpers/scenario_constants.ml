(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Scenario_dsl

type constants = Constants_helpers.t

(* Not a Set *)
include Constants_helpers.Set

let set :
    (constants -> 'a -> constants) -> 'a -> (constants, constants) scenarios =
 fun f x -> Action (fun csts -> return @@ f csts x)

let sets :
    (constants -> 'a -> constants) ->
    (string * 'a) list ->
    (constants, constants) scenarios =
 fun f -> fold_tag (set f)

let sets_f :
    (constants -> 'a -> constants) ->
    ('a -> string) ->
    'a list ->
    (constants, constants) scenarios =
 fun f f_tag -> fold_tag_f (set f) f_tag

let any_flag :
    (constants -> bool -> constants) -> (constants, constants) scenarios =
 fun f -> sets f [("true", true); ("false", false)]

let any_flags :
    (constants -> bool -> constants) list -> (constants, constants) scenarios =
  unfold any_flag

let sets_int :
    (constants -> int -> constants) ->
    int list ->
    (constants, constants) scenarios =
 fun f -> sets_f f string_of_int
