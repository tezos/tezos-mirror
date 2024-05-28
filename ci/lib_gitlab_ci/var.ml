(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Base

type t = string

let encode = Fun.id

let make variable_name =
  (* See
     {{:https://gitlab.com/gitlab-org/gitlab/blob/master/lib/gitlab/ci/pipeline/expression/lexeme/variable.rb#L9}string.rb}}
     for valid variable names. *)
  if
    String.exists
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> false | _ -> true)
      variable_name
  then
    raise
      (Invalid_argument (sf "[Var.t] invalid variable name '%s'" variable_name)) ;
  "$" ^ variable_name
