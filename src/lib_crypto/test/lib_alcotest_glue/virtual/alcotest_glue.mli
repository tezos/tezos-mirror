(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module type TESTABLE = sig
  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

val testable : 'a Fmt.t -> ('a -> 'a -> bool) -> 'a testable

module Source_code_position : sig
  type here = Lexing.position

  type pos = string * int * int * int
end

type 'a extra_info =
  ?here:Source_code_position.here -> ?pos:Source_code_position.pos -> 'a

type return = unit

type speed_level = [`Quick | `Slow]

type 'a test_case = string * speed_level * ('a -> return)

(*exception Test_error of unit Fmt.t*)

type 'a test = string * 'a test_case list

type 'a with_options =
  ?and_exit:bool ->
  ?verbose:bool ->
  ?compact:bool ->
  ?tail_errors:[`Unlimited | `Limit of int] ->
  ?quick_only:bool ->
  ?show_errors:bool ->
  ?json:bool ->
  ?filter:Re.re option * int list option ->
  ?log_dir:string ->
  ?bail:bool ->
  'a

val bool : bool testable

val int : int testable

val string : string testable

val option : 'a testable -> 'a option testable

val bytes : bytes testable

val pp : 'a testable -> 'a Fmt.t

val equal : 'a testable -> 'a -> 'a -> bool

val check : ('a testable -> string -> 'a -> 'a -> unit) extra_info

val fail : (string -> 'a) extra_info

val failf : (('a, Format.formatter, unit, 'b) format4 -> 'a) extra_info

val run :
  (?argv:string array -> string -> unit test list -> return) with_options
