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
  type algo

  val size : int

  val algo : [`Algo of algo | `Default]
end

module type P_Finite_key_pool_S = sig
  type public_key_hash

  type public_key

  type secret_key

  val pk : public_key Base_samplers.sampler

  val pkh : public_key_hash Base_samplers.sampler

  val sk : secret_key Base_samplers.sampler

  val all : (public_key_hash * public_key * secret_key) Base_samplers.sampler
end

module type Signature_S = sig
  include Tezos_crypto.Intfs.SIGNATURE

  type algo

  val algos : algo list

  val generate_key :
    ?algo:algo ->
    ?seed:Bytes.t ->
    unit ->
    Public_key_hash.t * Public_key.t * Secret_key.t
end

module Make_p_finite_key_pool
    (Signature : Signature_S)
    (Arg : Param_S with type algo := Signature.algo) :
  P_Finite_key_pool_S
    with type public_key_hash := Signature.Public_key_hash.t
     and type public_key := Signature.Public_key.t
     and type secret_key := Signature.Secret_key.t = struct
  let () = if Arg.size < 1 then invalid_arg "Make_finite_key_pool" else ()

  (* Hardcoded bc not directly accessible through the Tezos_crypto API. *)
  let minimal_seed_length = 32

  let key_pool = Queue.create ()

  let all_algos = Array.of_list Signature.algos

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

module V0 = struct
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V0.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V0.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V0.Secret_key.t

  module Make_finite_key_pool =
    Make_p_finite_key_pool (Tezos_crypto.Signature.V0)
end

module V1 = struct
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V1.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V1.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V1.Secret_key.t

  module Make_finite_key_pool =
    Make_p_finite_key_pool (Tezos_crypto.Signature.V1)
end

module V2 = struct
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V2.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V2.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V2.Secret_key.t

  module Make_finite_key_pool =
    Make_p_finite_key_pool (Tezos_crypto.Signature.V2)
end

module V3 = struct
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V3.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V3.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V3.Secret_key.t

  module Make_finite_key_pool =
    Make_p_finite_key_pool (Tezos_crypto.Signature.V3)
end

module V_latest = struct
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash :=
        Tezos_crypto.Signature.V_latest.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V_latest.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V_latest.Secret_key.t

  module Make_finite_key_pool =
    Make_p_finite_key_pool (Tezos_crypto.Signature.V_latest)
end

include V_latest
