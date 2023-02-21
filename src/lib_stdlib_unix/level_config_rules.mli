(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module handles set of rules for log sections specific configurations.
    For instance, if one want enable section "node.context" at level debug, it
    would be written "node.context -> debug". The complete syntax is the
    following

      a pattern is a <string> containing a "*"

      <rules> := <rule> ; <rules>
      <rule> := (<pattern> ->)? <level>

    A value of this form can be found in environment variables TEZOS_LOG and
    LWT_LOG.
*)

exception Incorrect_log_rules_syntax

exception Incorrect_log_rules_not_a_level

exception Incorrect_log_rules_missing_pattern

exception Incorrect_log_rules_missing_level

type rules = (string * Internal_event.Level.t) list

(** [find_log_rules default] checks above environment variables for set of rules
    - returns the origin (environment variable or config file) and value if
      found
    - returns the [default] given configuration otherwise
*)
val find_log_rules : string option -> string * string option

(** [parse_rules s] parses the string s representing a set of rules *)
val parse_rules : string -> rules
