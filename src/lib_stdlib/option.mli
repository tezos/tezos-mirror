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

(** [Some (f x)] if input is [Some x], or [None] if it's [None] *)
val map : f:('a -> 'b) -> 'a option -> 'b option

(** [(f x)] if input is [Some x], or [None] if it's [None] *)
val apply : f:('a -> 'b option) -> 'a option -> 'b option

(** [x >>= f] is an infix notation for [apply ~f x] *)
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option

(** [x >>| f] is an infix notation for [map ~f x] *)
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option

(** Call [(f x)] if input is [Some x], noop if it's [None] *)
val iter : f:('a -> unit) -> 'a option -> unit

(** [x] if input is [Some x], [default] if it's [None] *)
val unopt : default:'a -> 'a option -> 'a

(** [unopt_map ~f ~default x] is [f y] if [x] is [Some y], [default] if [x] is [None] *)
val unopt_map : f:('a -> 'b) -> default:'b -> 'a option -> 'b

(** [unopt_exn exn x] is [y] if [x] is [Some y], or raises [exn] if [x] is [None] *)
val unopt_exn : exn -> 'a option -> 'a

(** [unopt_assert ~loc x] is [y] if [x] is [Some y], or raises [Assert_failure loc] if [x] is [None] *)
val unopt_assert : loc:string * int * int * 'a -> 'b option -> 'b

(** First input of form [Some x], or [None] if both are [None] *)
val first_some : 'a option -> 'a option -> 'a option

(** Make an option of a value *)
val some : 'a -> 'a option

(** [pp ~default pp fmt x] pretty-print value [x] using [pp]
    or [default] (["None"] by default) string if there is no value. *)
val pp :
  ?default:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a option ->
  unit
