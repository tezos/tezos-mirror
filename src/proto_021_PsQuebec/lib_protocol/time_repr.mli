(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include module type of struct
  include Time
end

(** Internal timestamp representation. *)
type time = t

(** Pretty-prints the time stamp using RFC3339 format. *)
val pp : Format.formatter -> t -> unit

(** Parses RFC3339 representation and returns a timestamp. *)
val of_seconds_string : string -> time option

(** Returns the timestamp encoded in RFC3339 format. *)
val to_seconds_string : time -> string

(** Adds a time span to a timestamp.
    This function fails on integer overflow *)
val ( +? ) : time -> Period_repr.t -> time tzresult

(** Returns the difference between two timestamps as a time span.
    This function fails when the difference is negative *)
val ( -? ) : time -> time -> Period_repr.t tzresult

(** [t - p] Returns a timestamps [p] seconds before [t].

    TODO: https://gitlab.com/tezos/tezos/-/issues/2054
    This function should be made available in the environment.
 *)
val ( - ) : time -> Period_repr.t -> time
