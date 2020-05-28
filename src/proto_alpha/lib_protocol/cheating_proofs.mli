(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
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

open Alpha_context

(* A proof is a valid and verified Double_X_evidence operation.

   No distinction is make between proof created from a double_baking_evidence
   and double_endorsement evidence because the penalty is the same and only the
   proof level is stored in Storage.Contract.Proof_level.

 *)
type error +=
  | (* `Permanent *)
      Inconsistent_evidence of {
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }
  | (* `Permanent *)
      Outdated_evidence of {
      level : Raw_level.t;
      last : Raw_level.t;
    }
  | (* `Temporary *)
      Too_early_evidence of {
      level : Raw_level.t;
      current : Raw_level.t;
    }
  | (* `Permanent *)
      Invalid_double_baking_evidence of {
      hash1 : Block_hash.t;
      level1 : Raw_level.t;
      hash2 : Block_hash.t;
      level2 : Raw_level.t;
    }
  | (* `Permanent *)
      Invalid_double_endorsement_evidence of {
      hash1 : Operation_hash.t;
      level1 : Raw_level.t;
      hash2 : Operation_hash.t;
      level2 : Raw_level.t;
    }

val prove_double_baking :
  context ->
  Chain_id.t ->
  Kind.double_baking_evidence contents ->
  (Level.t * Signature.public_key_hash) tzresult Lwt.t

val prove_double_endorsement :
  context ->
  Chain_id.t ->
  Kind.double_endorsement_evidence contents ->
  (Level.t * Signature.public_key_hash) tzresult Lwt.t
