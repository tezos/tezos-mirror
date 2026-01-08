(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

{
  type t =
    {
      component: string option;
      variant: string option;
      index: int option;
    }
}

let digit = ['0'-'9']
let identifier = ['a'-'z' '0'-'9' '_' '-']+

rule parse_ = parse
  | ((identifier as component) '.')?
    "tezt"
    ('-' (identifier as variant))?
    (' ' (digit+ as index) '/' digit+)?
    eof
    {
      let index =
        match index with
          | None -> None
          | Some index -> int_of_string_opt index
      in
      Some { component; variant; index }
    }
  | _
    { None }
  | eof
    { None }
