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
  val size : int

  val algo : [`Algo of Signature.algo | `Default]
end

module type Finite_key_pool_S = sig
  val pk : Signature.public_key Base_samplers.sampler

  val pkh : Signature.public_key_hash Base_samplers.sampler

  val sk : Signature.secret_key Base_samplers.sampler

  val all :
    (Signature.public_key_hash * Signature.public_key * Signature.secret_key)
    Base_samplers.sampler
end

module Make_finite_key_pool (Arg : Param_S) : Finite_key_pool_S = struct
  let () = if Arg.size < 1 then invalid_arg "Make_finite_key_pool" else ()

  (* Hardcoded bc not directly accessible through the Tezos_crypto API. *)
  let minimal_seed_length = 32

  let key_pool = Queue.create ()

  let all_algos = [|Signature.Ed25519; Signature.Secp256k1; Signature.P256|]

  let uniform_algo state =
    let i = Random.State.int state (Array.length all_algos) in
    all_algos.(i)

  let get_next state =
    if Queue.length key_pool < Arg.size then (
      let algo =
        match Arg.algo with
        | `Default -> uniform_algo state
        | `Algo algo -> algo
      in
      let seed =
        Base_samplers.uniform_bytes ~nbytes:minimal_seed_length state
      in
      let triple = Signature.generate_key ~algo ~seed () in
      Queue.add triple key_pool ;
      triple)
    else
      match Queue.take_opt key_pool with
      | None ->
          (* Queue.length >= Arg.size >= 1 *)
          assert false
      | Some triple ->
          Queue.add triple key_pool ;
          triple

  let pk state =
    let _, pk, _ = get_next state in
    pk

  let pkh state =
    let pkh, _, _ = get_next state in
    pkh

  let sk state =
    let _, _, sk = get_next state in
    sk

  let all = get_next
end
