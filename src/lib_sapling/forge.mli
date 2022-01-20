(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

module Core = Core.Client
module S = Storage

module Input : sig
  type t

  val encoding : t Data_encoding.t

  val pos : t -> int64

  val amount : t -> int64

  val address : t -> Core.Viewing_key.address

  val compare : t -> t -> int

  val is_spent : t -> S.state -> Core.Viewing_key.t -> bool

  val get : S.state -> int64 -> Core.Viewing_key.t -> (Bytes.t * t) option

  val get_out :
    S.state -> int64 -> Core.Spending_key.ovk -> (Bytes.t * t) option

  val mem : S.state -> int64 -> bool
end

type output

val make_output : Core.Viewing_key.address -> int64 -> Bytes.t -> output

(* @raise Invalid_argument if eitner of [number_dummy_inputs] or
   [number_dummy_outputs] is strictly negative *)
val forge_transaction :
  ?number_dummy_inputs:int ->
  ?number_dummy_outputs:int ->
  Input.t list ->
  output list ->
  Core.Spending_key.t ->
  string ->
  S.state ->
  Core.UTXO.transaction

(* @raise Invalid_argument if eitner of [number_dummy_inputs] or
   [number_dummy_outputs] is strictly negative *)
val forge_shield_transaction :
  ?number_dummy_inputs:int ->
  ?number_dummy_outputs:int ->
  output list ->
  int64 ->
  string ->
  S.state ->
  Core.UTXO.transaction
