(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Dependent booleans *)

type no = private DNo

type yes = private DYes

(** 
    ['b dbool] is a boolean whose value depends on its type parameter ['b].
    [yes dbool] can only be [Yes]. [no dbool] can only be [No].
*)
type _ dbool = No : no dbool | Yes : yes dbool

(** 
    [('a, 'b, 'r) dand] is a witness of the logical conjunction of dependent
    booleans. ['r] is the result of ['a] and ['b].
*)
type ('a, 'b, 'r) dand =
  | NoNo : (no, no, no) dand
  | NoYes : (no, yes, no) dand
  | YesNo : (yes, no, no) dand
  | YesYes : (yes, yes, yes) dand

type ('a, 'b) ex_dand = Ex_dand : ('a, 'b, _) dand -> ('a, 'b) ex_dand
[@@unboxed]

(** Logical conjunction of dependent booleans. *)
val dand : 'a dbool -> 'b dbool -> ('a, 'b) ex_dand

(** Result of the logical conjunction of dependent booleans. *)
val dbool_of_dand : ('a, 'b, 'r) dand -> 'r dbool

(** Type equality witness. *)
type (_, _) eq = Eq : ('a, 'a) eq

(**
    [merge_dand] proves that the type [dand] represents a function, i.e. that
    there is a unique ['r] such that [('a, 'b, 'r) dand] is inhabited for a
    given ['a] and a given ['b].
*)
val merge_dand : ('a, 'b, 'c1) dand -> ('a, 'b, 'c2) dand -> ('c1, 'c2) eq
