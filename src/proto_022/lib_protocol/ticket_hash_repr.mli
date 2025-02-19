(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

(** Ticket hashes are used to uniquely identify pairs made of
    Michelson ticktes and their owner.

    They are used by the protocol to keep record of a tickets ledger,
    that is how many tickets smart contracts own. More precisely, they
    are used as keys for the {!Storage.Ticket_balance} table.  *)

(** A ticket hash is computed by the function [make] and is a
    combination of a [ticketer], a [content type], a [content], and an
    [owner].

    {b Note:} This invariant can be invalidated if the [key_hash] is
    created from the [encoding]. *)
type t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val to_b58check : t -> string

val of_b58check_opt : string -> t option

val of_b58check_exn : string -> t

val of_bytes_exn : bytes -> t

val of_bytes_opt : bytes -> t option

include Compare.S with type t := t

val zero : t

val of_script_expr_hash : Script_expr_hash.t -> t

module Index : Storage_description.INDEX with type t = t
