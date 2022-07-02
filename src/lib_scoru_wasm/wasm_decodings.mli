(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_webassembly_interpreter.Instance

(** A module type representing WASM specific decodings. *)
module type S = sig
  (** Represents decoders. *)
  type 'a t

  (** Represents values encoded as trees. *)
  type tree

  (** [run decoder tree] runs the tree decoder against the tree. *)
  val run : 'a t -> tree -> 'a Lwt.t

  (** [module_instance_encoding modules] allows you to decode a module instance.
      It requires a vector of previously decoded modules for references. *)
  val module_instance_decoding : module_inst Vector.t -> module_inst t

  (** [module_instances_decoding] decodes module instances.  *)
  val module_instances_decoding : module_inst Vector.t t
end

(** Creates a WASM decoding module given a {!Tree.S} implementation. *)
module Make (T : Tree.S) : S with type tree = T.tree
