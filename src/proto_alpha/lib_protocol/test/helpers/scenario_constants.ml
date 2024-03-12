(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Scenario_dsl

type constants = Constants_helpers.t

(* Not a Set *)
module S = Constants_helpers.Set

let set :
    ('a -> constants -> constants) -> 'a -> (constants, constants) scenarios =
 fun f x -> Action (fun csts -> return @@ f x csts)

let set_opt :
    ('a -> constants -> constants) ->
    'a option ->
    (constants, constants) scenarios =
 fun f -> function None -> Empty | Some x -> set f x

let sets :
    ('a -> constants -> constants) ->
    (string * 'a) list ->
    (constants, constants) scenarios =
 fun f -> fold_tag (set f)

let sets_f :
    ('a -> constants -> constants) ->
    ('a -> string) ->
    'a list ->
    (constants, constants) scenarios =
 fun f f_tag -> fold_tag_f (set f) f_tag

let branch_flag :
    (bool -> constants -> constants) -> (constants, constants) scenarios =
 fun f -> sets f [("true", true); ("false", false)]

let branch_flags :
    (bool -> constants -> constants) list -> (constants, constants) scenarios =
  unfold branch_flag

let sets_int :
    (int -> constants -> constants) ->
    int list ->
    (constants, constants) scenarios =
 fun f -> sets_f f string_of_int
