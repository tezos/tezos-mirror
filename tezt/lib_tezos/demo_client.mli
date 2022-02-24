(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe  <gb.fefe@protonmail.com>                    *)
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

(** Run Tezos client with commands related to `demo_protocol`. *)

(** Run [tezos-client activate protocol] with `demo_protocol`.

    If [timestamp] is not specified explicitely, it is set to [now
    -. timestamp_delay]. Default value for [timestamp_delay] is 365
    days, which allows to bake plenty of blocks before their timestamp
    reach the present (at which point one would have to wait between
    each block so that peers do not reject them for being in the
    future). *)
val activate :
  ?endpoint:Client.endpoint ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:string ->
  ?timestamp_delay:float ->
  Client.t ->
  unit Lwt.t

(** Run [tezos-client bake]. *)
val bake : ?msg:string -> Client.t -> unit Lwt.t

(** Run [tezos-client get a], get the current value of counter `a`. *)
val get_a : Client.t -> int Lwt.t

(** Run [tezos-client get a], get the current value of counter `b`. *)
val get_b : Client.t -> int Lwt.t

(** Run [tezos-client increment a], add one to counter `a`. *)
val increment_a : Client.t -> unit Lwt.t

(** Run [tezos-client increment b], add one to counter `a`.. *)
val increment_b : Client.t -> unit Lwt.t

(** Run [tezos-client transfer amount], when [amount] is positive,
    transfer [amount] from counter `a` to counter `b. When [amount] is
    negative, transfer [-amount] from `b` to `a`. *)
val transfer : Client.t -> int -> unit Lwt.t
