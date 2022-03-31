(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** [assert_true msg b] checks that [b] is [true] and fails otherwise,
    displaying [msg]. *)
val assert_true : string -> bool -> unit

(** [assert_true msg b] checks that [b] is [false] and fails otherwise,
    displaying [msg]. *)
val assert_false : string -> bool -> unit

(** [assert_none msg opt] checks that [opt] is [None], will fail with message [msg] otherwise *)
val assert_none : ?msg:string -> 'a option -> unit

(** Alcotest version of [assert false]. *)
val impossible : string -> unit

(** [check_any msg f l] asserts that at least one value in [l] satisfies [f],
    otherwise it fails and displays [msg] *)
val check_any : ?msg:string -> ('a -> bool) -> 'a list -> unit

(** [contains m msg x ls] asserts that one testable in [ls] equals
    [x], and otherwise fails with [msg] *)
val contains : 'a Alcotest.testable -> string -> 'a -> 'a list -> unit

(** [fail exp giv msg] triggers a failure, displaying that [exp] was expected,
    [giv] was given, and the custom message [msg] *)
val fail : msg:string -> string -> string -> 'a

(** [fail_msg fmt] triggers a failure, but instead of expecting three messages
    like {!fail}; it takes as parameter a single format string. *)
val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** [equal eq prn msg val0 val1] checks that the operation [eq val0 val1]
    returns [true], will fail otherwise, using [prn] to format [val0] and [val1]
    and adds the custom message [msg] *)
val equal :
  ?eq:('a -> 'a -> bool) ->
  ?prn:('a -> string) ->
  ?msg:string ->
  'a ->
  'a ->
  unit

(** [equal_bytes msg b0 b1] checks that [b0] equals [b1], where
    [b0] and [b1] are [bytes], and will fail with message [msg] otherwise *)
val equal_bytes : ?msg:string -> bytes -> bytes -> unit

(** [equal_bytes_option msg opt0 opt1] checks that [opt0] equals [opt1], where
    [opt0] and [opt1] are [bytes option], and will fail with message [msg] otherwise *)
val equal_bytes_option : ?msg:string -> bytes option -> bytes option -> unit

(** [equal_bool msg b0 b1] checks that [b0] equals [b1], where
    [b0] and [b1] are [bool], and will fail with message [msg] otherwise *)
val equal_bool : ?msg:string -> bool -> bool -> unit

(** [equal_string_option msg opt0 opt1] checks that [opt0] equals [opt1], where
    [opt0] and [opt1] are [string option], and will fail with message [msg] otherwise *)
val equal_string_option : ?msg:string -> string option -> string option -> unit

(** [make_equal_list eq prn msg l0 l1] checks that lists [l0] and [l1] are equal using the
    function [eq] provided, fails with the values displayed using the function [prn] along
    with the message [msg] otherwise *)
val make_equal_list :
  ('a -> 'a -> bool) ->
  ('a -> string) ->
  ?msg:string ->
  'a list ->
  'a list ->
  unit

(** [equal_string_list msg l0 l1] checks that the lists of [string] [l0] and [l1] are equal,
    will fail with message [msg] otherwise *)
val equal_string_list : ?msg:string -> string list -> string list -> unit

(** [equal_string_list_list msg l0 l1] checks that the lists of [string list] [l0] and [l1] are equal,
    will fail with message [msg] otherwise *)
val equal_string_list_list :
  ?msg:string -> string list list -> string list list -> unit
