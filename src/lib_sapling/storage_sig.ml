(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module type STORAGE = sig
  type state

  val state_encoding : state Data_encoding.t

  val empty : memo_size:int -> state

  val size : state -> int64 * int64

  val get_memo_size : state -> int

  val get_root : state -> Core.Validator.Hash.t

  val mem_root : state -> Core.Validator.Hash.t -> bool

  val mem : state -> int64 -> bool

  val get :
    state -> int64 -> Core.Validator.Commitment.t * Core.Validator.Ciphertext.t

  val get_witness : state -> int64 -> Bytes.t

  val mem_nullifier : state -> Core.Validator.Nullifier.t -> bool

  val add :
    state ->
    (Core.Validator.Commitment.t * Core.Validator.Ciphertext.t) list ->
    state

  val add_nullifier : state -> Core.Validator.Nullifier.t -> state
end
