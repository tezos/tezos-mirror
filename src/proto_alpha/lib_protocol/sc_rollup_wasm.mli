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

module V2_0_0 : sig
  (** This module provides Proof-Generating Virtual Machine (PVM) running
    WebAssembly (version 2.0.0). *)

  module type S = sig
    include Sc_rollup_PVM_sem.S

    (** [name] is "wasm_2_0_0".

      WebAssembly is an "evergreen" specification. We aim to track
      the latest major version, 2.0 at the time of writing. We
      use the minor version number to track changes to our fork.
   *)
    val name : string

    (** [parse_boot_sector s] builds a boot sector from its human
      writable description. *)
    val parse_boot_sector : string -> string option

    (** [pp_boot_sector fmt s] prints a human readable representation of
     a boot sector. *)
    val pp_boot_sector : Format.formatter -> string -> unit

    (* Required by L2 node: *)

    (** [get_tick state] gets the total tick counter for the given PVM state. *)
    val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

    (** PVM status *)
    type status = Computing | WaitingForInputMessage

    (** [get_status state] gives you the current execution status for the PVM. *)
    val get_status : state -> status Lwt.t
  end

  module type P = sig
    module Tree :
      Context.TREE with type key = string list and type value = bytes

    type tree = Tree.tree

    type proof

    val proof_encoding : proof Data_encoding.t

    val proof_before : proof -> Sc_rollup_repr.State_hash.t

    val proof_after : proof -> Sc_rollup_repr.State_hash.t

    val verify_proof :
      proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t

    val produce_proof :
      Tree.t -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t
  end

  type 'a proof = {
    tree_proof : 'a;
    given : Sc_rollup_PVM_sem.input option;
    requested : Sc_rollup_PVM_sem.input_request;
  }

  val proof_encoding : 'a Data_encoding.t -> 'a proof Data_encoding.t

  (** Build a WebAssembly PVM using the given proof-supporting context. *)
  module Make (Context : P) :
    S
      with type context = Context.Tree.t
       and type state = Context.tree
       and type proof = Context.proof proof

  (** This PVM is used for verification in the Protocol. [produce_proof] always returns [None]. *)
  module ProtocolImplementation :
    S
      with type context = Context.t
       and type state = Context.tree
       and type proof = Context.Proof.tree Context.Proof.t proof
end
