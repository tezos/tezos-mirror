(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [x >>= f] is an infix notation for [apply ~f x] *)
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option

(** [x >>| f] is an infix notation for [map ~f x] *)
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option

(** [unopt_exn exn x] is [y] if [x] is [Some y], or raises [exn] if [x] is [None] *)
val unopt_exn : exn -> 'a option -> 'a

(** [unopt_assert ~loc x] is [y] if [x] is [Some y], or raises [Assert_failure loc] if [x] is [None] *)
val unopt_assert : loc:string * int * int * 'a -> 'b option -> 'b

(** First input of form [Some x], or [None] if both are [None] *)
val first_some : 'a option -> 'a option -> 'a option

(** [pp ~default pp fmt x] pretty-print value [x] using [pp]
    or [default] (["None"] by default) string if there is no value. *)
val pp :
  ?default:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a option ->
  unit
