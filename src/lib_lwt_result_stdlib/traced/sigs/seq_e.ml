(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The [S] signature is similar to [Seq.S] except that suspended nodes are
    wrapped in a result.

    This allows some additional traversors to be applied lazily.

    The functions [of_seq] and [of_seq_e] allow conversion from vanilla
    sequences. *)
module type S =
  Bare_sigs.Seq_e.S
    with type ('a, 'e) node = ('a, 'e) Bare_structs.Seq_e.node
     and type ('a, 'e) t = ('a, 'e) Bare_structs.Seq_e.t

(* Developer note:

   Due to the type of sequences and the availability of traces, we can add the
   following traversors that are not supported in the [Bare] version. We will
   add those in future versions.

   - [iter_p] which returns a [Trace.cons] of the sequence-interruption and the
     [Trace.conp_list] of the iterator errors. We need to decide on a semantic:
     which error is consed "above" the other?

   - [stitch] which "concatenates" multiple sequences, stacking the interrupting
     errors for the end.

   - [recover] which feeds the interruption error into an unfold to continue the
     sequence.

   Also note that those can be made available in [Seq_es] too. *)
