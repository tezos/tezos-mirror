(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Here is the list of PVMs available in this protocol. *)

module PVM : sig
  type boot_sector = string

  module type S = sig
    val name : string

    val parse_boot_sector : string -> boot_sector option

    val pp_boot_sector : Format.formatter -> boot_sector -> unit

    include Sc_rollup_PVM_sem.S
  end

  type t = (module S)
end

(** A smart contract rollup has a kind, which assigns meaning to
   rollup operations. *)
module Kind : sig
  (**

     The list of available rollup kinds.

     This list must only be appended for backward compatibility.
  *)
  type t = Example_arith | Wasm_2_0_0

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  (** [pvm_of kind] returns the [PVM] of the given [kind]. *)
  val pvm_of : t -> PVM.t

  (** [of_pvm pvm] returns the [kind] of the given [PVM]. *)
  val of_pvm : PVM.t -> t

  (** [pvm_of_name ~name] is [Some (module I)] if an implemented PVM
      called [name]. This function returns [None] otherwise. *)
  val pvm_of_name : name:string -> PVM.t option

  (** [all] returns all implemented PVM. *)
  val all : t list

  (** [all_names] returns all implemented PVM names. *)
  val all_names : string list

  (** [of_name name] returns the kind of the PVM of the specified [name]. *)
  val of_name : string -> t option

  (** [name_of kind] returns a human-readable representation of [kind]. *)
  val name_of : t -> string
end

(** A module signature we can use to form first-class modules that carry
    a specific proof a long with the PVM module interface. *)
module type PVM_with_proof = sig
  include PVM.S

  val proof : proof
end

(** A wrapper for first-class modules [(module PVM_with_proof)]. We need
    this in order to implement an encoding function. The [Unencodable]
    case is provided so that tests can provide their own PVM interfaces
    without having to include proof encodings here. *)
type wrapped_proof =
  | Unencodable of (module PVM_with_proof)
  | Arith_pvm_with_proof of
      (module PVM_with_proof
         with type proof = Sc_rollup_arith.ProtocolImplementation.proof)
  | Wasm_2_0_0_pvm_with_proof of
      (module PVM_with_proof
         with type proof = Sc_rollup_wasm.V2_0_0.ProtocolImplementation.proof)

(** Unwrap a [wrapped_proof] into a first-class module. *)
val wrapped_proof_module : wrapped_proof -> (module PVM_with_proof)

val wrapped_proof_encoding : wrapped_proof Data_encoding.t

(** Wrap a PVM module with proof into a [wrapped_proof]. This matches on
    the [name] in the module---if that is recognisable as a [Kind], this
    function will encode and decode to coerce the proof to a proof in
    the protocol implementation of the PVM. If the [name] is not
    recognised this will fall back to using [Unencodable], so the value
    can still be used in tests but won't work as part of a
    [Sc_rollup_refute] operation. *)
val wrap_proof : (module PVM_with_proof) -> wrapped_proof option
