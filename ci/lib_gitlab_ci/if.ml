(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Base

type term = Str of string | Null

type t =
  | And of t * t
  | Or of t * t
  | Eq of Var.t * term
  | Neq of Var.t * term
  | Match of Var.t * string
  | Unmatch of Var.t * string

let var = Var.make

let rec encode expr =
  let prio = function
    | Eq _ -> 1
    | Neq _ -> 1
    | Match _ -> 1
    | Unmatch _ -> 1
    | And _ -> 2
    | Or _ -> 3
  in
  (* Precedence: not > and > or > string, var

     And (Or a b) c -> (a || b) && c
     Or a (And b c) -> a || b && c
     Or (And a b) c -> a && b || c *)
  let paren_opt sub_expr =
    let s = encode sub_expr in
    if prio expr < prio sub_expr then "(" ^ s ^ ")" else s
  in
  let encode_term = function
    | Null -> "null"
    | Str s ->
        (* Strings can be enclosed by single or double quotes, see
           https://docs.gitlab.com/ee/ci/jobs/job_control.html#compare-a-variable-to-a-string.

           To encode strings that do not contain double quotes, we
           enclose them with double quotes. To encode strings that do
           not contain single quotes, we enclose with single
           quotes. The smart constructor {!str} for strings ensure
           that strings do not contain both single and double quotes,
           as such strings cannot be encoded. *)
        if not (String.contains s '"') then sf {|"%s"|} s
        else if not (String.contains s '\'') then sf {|'%s'|} s
        else
          (* This branch cannot be reached for [Str]s created by
             {!str}. *)
          assert false
  in
  match expr with
  | And (a, b) -> sf "%s && %s" (paren_opt a) (paren_opt b)
  | Or (a, b) -> sf "%s || %s" (paren_opt a) (paren_opt b)
  | Eq (a, b) -> sf "%s == %s" (Var.encode a) (encode_term b)
  | Neq (a, b) -> sf "%s != %s" (Var.encode a) (encode_term b)
  | Match (a, b) -> sf "%s =~ %s" (Var.encode a) b
  | Unmatch (a, b) -> sf "%s !~ %s" (Var.encode a) b

let eq a b = Eq (a, b)

let neq a b = Neq (a, b)

let and_ a b = And (a, b)

let or_ a b = Or (a, b)

let str s =
  (* See {{:https://gitlab.com/gitlab-org/gitlab/blob/master/lib/gitlab/ci/pipeline/expression/lexeme/string.rb#L9}string.rb}}. *)
  if String.contains s '"' && String.contains s '\'' then
    raise
      (Invalid_argument
         (sf
            "[If.str] literal strings cannot mix single and double-quotes, got \
             %S."
            s)) ;
  Str s

let null = Null

let match_ a b = Match (a, b)

let unmatch a b = Unmatch (a, b)

let ( == ) = eq

let ( != ) = neq

let ( && ) = and_

let ( || ) = or_

let ( =~ ) = match_

let ( =~! ) = unmatch
