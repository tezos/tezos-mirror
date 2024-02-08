(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type srs_verifier = {
  shards : Kzg.Bls.G2.t;
  pages : Kzg.Bls.G2.t;
  commitment : Kzg.Bls.G2.t;
}

val get_verifier_srs1 : unit -> Kzg.Bls.Srs_g1.t

val get_verifier_srs2 :
  max_polynomial_length:int ->
  page_length_domain:int ->
  shard_length:int ->
  srs_verifier

val ensure_srs_validity :
  test:bool ->
  mode:[< `Prover | `Verifier] ->
  slot_size:int ->
  page_size:int ->
  redundancy_factor:int ->
  number_of_shards:int ->
  (unit, [> `Fail of string]) result

exception Failed_to_load_trusted_setup of string

val read_srs_g1 :
  ?len:int ->
  path:string ->
  unit ->
  (Kzg.Bls.Srs_g1.t, [> `End_of_file of string | `Invalid_point of int]) result
  Lwt.t

module Internal_for_tests : sig
  val fake_srs : ?size:int -> unit -> Kzg.Bls.Srs_g1.t

  val get_verifier_srs1 : unit -> Kzg.Bls.Srs_g1.t

  val get_verifier_srs2 :
    max_polynomial_length:int ->
    page_length_domain:int ->
    shard_length:int ->
    srs_verifier
end
