(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module SMap : Map.S with type key = string

(** Representation of a ZK Rollup account. *)

(** Static part of a ZKRU account. These are set at origination,
    after which they cannot be modified. *)
type static = {
  public_parameters : Plonk.public_parameters;
      (** Input to the Plonk verifier that are fixed once the circuits
          are decided. *)
  state_length : int;  (** Number of scalars in the state. *)
  circuits_info : [`Public | `Private | `Fee] SMap.t;
      (** Circuit names, alongside a tag indicating its kind. *)
  nb_ops : int;  (** Valid op codes of L2 operations must be in \[0, nb_ops) *)
}

(**  Dynamic part of a ZKRU account. *)
type dynamic = {
  state : Zk_rollup_state_repr.t;
      (** Array of scalars representing the state of the rollup
          at a given level. *)
  paid_l2_operations_storage_space : Z.t;
      (** Number of bytes for storage of L2 operations that have
          been already paid for. *)
  used_l2_operations_storage_space : Z.t;
      (** Number of bytes for storage of L2 operations that are
          being used. *)
}

type t = {static : static; dynamic : dynamic}

val encoding : t Data_encoding.t

(* Encoding for the [circuits_info] field.
   Checks that keys are not duplicated in serialized representation. *)
val circuits_info_encoding : [`Public | `Private | `Fee] SMap.t Data_encoding.t
