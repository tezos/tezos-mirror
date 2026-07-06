(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

open Tezos_context_sigs.Context

(** V1: using vanilla Data_encoding. Easier to parse by non-OCaml programs
    but less efficient *)
module V1 : sig
  (** Encoding for 32-tree proofs *)
  module Tree32 : PROOF_ENCODING

  (** Encoding for binary tree proofs *)
  module Tree2 : PROOF_ENCODING
end

(** V2 : using Compact_encoding.  Smaller than V1 but more complex parser
    is required. *)
module V2 : sig
  (** [PROOF_ENCODING] extended with the compact underlying
      [tree_proof_encoding] ([Compact.make ~tag_size:`Uint8] of it), so
      composite proof formats can join its tag space in a
      {!Data_encoding.Compact.union} while keeping the tree-proof case
      byte-identical. *)
  module type PROOF_ENCODING_COMPACT = sig
    include PROOF_ENCODING

    val tree_proof_compact :
      Proof_types.tree Proof_types.t Data_encoding.Compact.t
  end

  (** Encoding for 32-tree proofs *)
  module Tree32 : PROOF_ENCODING_COMPACT

  (** Encoding for binary tree proofs *)
  module Tree2 : PROOF_ENCODING_COMPACT
end
