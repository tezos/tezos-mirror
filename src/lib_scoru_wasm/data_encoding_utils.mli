(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(** [case_incr] is like [Data_encoding.case] but shall be used in unions
    where the case tags are monotonically increasing. *)
type 'a case_incr

(** [case_incr title encoding probe extract] creates a [case_incr] similar to
    [Data_encoding.case ~title auto_tag encoding probe extract] where the
    [auto_tag] is automatically determined later. *)
val case_incr :
  string ->
  'a Data_encoding.t ->
  ('b -> 'a option) ->
  ('a -> 'b) ->
  'b case_incr

(** [unit_case_incr title value] is a convenience function around [case_incr]
    to simplify construction of cases with unit-isomorphic bodies. *)
val unit_case_incr : string -> 'a -> 'a case_incr

(** [union_incr cases] creates a data encoding like [Data_encoding.union] for
    [case_incr] items. *)
val union_incr : 'a case_incr list -> 'a Data_encoding.t

module Little_endian : sig
  val int32 : int32 Data_encoding.t
end
