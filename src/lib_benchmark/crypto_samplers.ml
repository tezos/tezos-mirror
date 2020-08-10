(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@tezos.com>                       *)
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

(* Primitives for sampling crypto-related data *)

module type Param_S = sig
  (** Random state to feed the crypto samplers.
      /!\ This will be muted by sampling /!\ *)
  val state : Random.State.t

  val size : int

  val algo : [`Algo of Signature.algo | `Default]
end

module type Finite_key_pool_S = sig
  val pk : unit -> Signature.public_key

  val pkh : unit -> Signature.public_key_hash

  val sk : unit -> Signature.secret_key

  val all :
    unit ->
    Signature.public_key_hash * Signature.public_key * Signature.secret_key
end

module Make_finite_key_pool (Arg : Param_S) : Finite_key_pool_S = struct
  (* Hardcoded bc not directly accessible through the Tezos_crypto API. *)
  let minimal_seed_length = 32

  let (key_pool, current) =
    let next_seed () =
      Base_samplers.uniform_bytes Arg.state ~nbytes:minimal_seed_length
    in
    let algo = match Arg.algo with `Default -> None | `Algo a -> Some a in
    let array =
      Array.init Arg.size (fun _i ->
          Signature.generate_key ?algo ~seed:(next_seed ()) ())
    in
    (array, ref 0)

  let pk () =
    let (_, pk, _) = key_pool.(!current) in
    current := (!current + 1) mod Array.length key_pool ;
    pk

  let pkh () =
    let (pkh, _, _) = key_pool.(!current) in
    current := (!current + 1) mod Array.length key_pool ;
    pkh

  let sk () =
    let (_, _, sk) = key_pool.(!current) in
    current := (!current + 1) mod Array.length key_pool ;
    sk

  let all () =
    let res = key_pool.(!current) in
    current := (!current + 1) mod Array.length key_pool ;
    res
end
