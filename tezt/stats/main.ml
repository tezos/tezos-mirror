(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezt_core

let () =
  let command =
    Clap.subcommand
      [
        ( Clap.case
            "stats"
            ~description:"Display statistics about a selection of tests."
        @@ fun () ->
          let recursive =
            Clap.flag
              ~set_long:"recursive"
              ~set_short:'r'
              ~unset_long:"not-recursive"
              ~unset_short:'R'
              ~description:
                "If set (which is the default), recurse into PATHs when they \
                 are directories, to input all files with extension '.json'."
              true
          in
          let filter =
            let tsl_expression_type =
              Clap.typ
                ~name:"TSL expression"
                ~dummy:TSL_AST.True
                ~parse:TSL.parse
                ~show:TSL.show
            in
            Clap.optional
              tsl_expression_type
              ~long:"where"
              ~short:'w'
              ~placeholder:"TSL"
              ~description:
                "TSL expression denoting the tests to select for analysis."
              ()
          in
          let paths =
            Clap.list_string
              ~placeholder:"PATH"
              ~description:
                "Path to a record file or directory to read. If no PATH is \
                 specified, defaults to 'tezt/records'. If PATH is a \
                 directory, read all JSON files from PATH (recursively if -r \
                 is set, which is the default)."
              ()
          in
          let paths = if paths = [] then ["tezt/records"] else [] in
          `stats (recursive, filter, paths) );
        ( Clap.case
            "balance"
            ~description:
              "Output how many jobs of each kind should lead to better \
               balancing."
        @@ fun () ->
          let target =
            Clap.default_float
              ~long:"target"
              ~short:'t'
              ~placeholder:"MINUTES"
              ~description:
                "Recommend job counts such that each job runs for about \
                 MINUTES."
              5.
          in
          let verbose =
            Clap.flag
              ~set_long:"verbose"
              ~set_short:'v'
              ~description:"Output more information about each job."
              false
          in
          `balance (target, verbose) );
      ]
  in
  Clap.close () ;
  match command with
  | `stats (recursive, filter, paths) -> Cmd_stats.run ~recursive ~filter ~paths
  | `balance (target, verbose) -> Cmd_balance.run ~target ~verbose
