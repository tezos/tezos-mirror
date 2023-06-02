(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Type representing frozen deposits for a cycle.

    It is used both for frozen deposits and unstaked frozen deposits with
    slightly different behaviours.

    [initial_amount] is the amount on which slashing should be based.
    [current_amount] is the current amount after slashing has happened. 

    For frozen deposits, a single record is maintained with the invariant
    [initial_amount = current_amount] at the beginning of a cycle.

    For unstaked frozen deposits, there is a record per cycle. 
    The values of all unslashable cycles are squashed together at cycle ends.
    The [initial_amount] may be increased during the current cycle only, when
    an unstake is requested.

    TODO #5788: possibly rename fields
*)
type t = {initial_amount : Tez_repr.t; current_amount : Tez_repr.t}

val encoding : t Data_encoding.t

val zero : t
