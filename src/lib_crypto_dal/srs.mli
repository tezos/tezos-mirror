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

(** SRS points needed by the verifier of the DAL *)
type srs_verifier = {
  shards : Kzg.Bls.G2.t;
  pages : Kzg.Bls.G2.t;
  commitment : Kzg.Bls.G2.t;
}

(** Returns ZCash SRS₁ resized for the verifier, according to mainnet parameters *)
val get_verifier_srs1 : unit -> Kzg.Bls.Srs_g1.t

(** Return the ZCash SRS₂ points needed by the verifier, according to mainnet
   parameters *)
val get_verifier_srs2 :
  max_polynomial_length:int ->
  page_length_domain:int ->
  shard_length:int ->
  srs_verifier

(** Fails if and only if the SRS does not suits the parameters, ie SRS₁ would
    be too short to commit or SRS₂ does not contain the needed points *)
val ensure_srs_validity :
  is_fake:bool ->
  mode:[< `Prover | `Verifier] ->
  slot_size:int ->
  page_size:int ->
  redundancy_factor:int ->
  number_of_shards:int ->
  (unit, [> `Fail of string]) result

exception Failed_to_load_trusted_setup of string

(** Returns {m SRS_{1}} of size [len] (by default, {m 2^{21}}) read from the
    file given by [path]. May raise [Failed_to_load_trusted_setup] exception *)
val read_srs :
  ?len:int ->
  srs_g1_path:string ->
  srs_g2_path:string ->
  unit ->
  ( Kzg.Bls.Srs_g1.t * Kzg.Bls.Srs_g2.t,
    [> `End_of_file of string | `Invalid_point of int] )
  result
  Lwt.t

(** Returns {m SRS_{1}} of size [len] (by default, {m 2^{21}}) read from the
    file given by [path]. May raise [Failed_to_load_trusted_setup] exception *)
val read_uncompressed_srs :
  ?len:int ->
  srsu_g1_path:string ->
  srsu_g2_path:string ->
  unit ->
  ( Kzg.Bls.Srs_g1.t * Kzg.Bls.Srs_g2.t,
    [> `Close | `Open] Lwt_utils_unix.io_error )
  result
  Lwt.t

(** This module is used to handle a fake SRS in the DAL. This is more flexible
    and easier to handle in the CI than loading ZCash SRS *)
module Internal_for_tests : sig
  (** Generates an unsafe SRS₁ of size 2¹⁶ from a known seed *)
  val fake_srs1 : Kzg.Bls.Srs_g1.t Lazy.t

  (** Generates an unsafe SRS₂ of size 2¹⁶ from a known seed *)
  val fake_srs2 : Kzg.Bls.Srs_g2.t Lazy.t

  (** Same as [fake_srs] but the returned SRS₁ is of size 2⁸, which is enough
       to suit the verifier needs in our tests *)
  val get_verifier_srs1 : unit -> Kzg.Bls.Srs_g1.t

  (** Returns the fake SRS₂ points needed by the verifier *)
  val get_verifier_srs2 :
    max_polynomial_length:int ->
    page_length_domain:int ->
    shard_length:int ->
    srs_verifier

  (** Prints the minimal srs needed for mainnet params, in an OCaml-readable
        format. Run with
         [let _ = Lwt_main.run
           @@ Srs.Internal_for_tests.(
            print_verifier_srs_from_file ~srs_g1_path ~srs_g2_path ~dest_path) ()]
        This function writes points of srs_g1 & srs_g2 in [dest_path] that can
        be added in Zcash_srs *)
  val print_verifier_srs_from_file :
    ?max_srs_size:int ->
    srs_g1_path:string ->
    srs_g2_path:string ->
    dest_path:string ->
    unit ->
    (unit, [> `End_of_file of string | `Invalid_point of int]) result Lwt.t
end
