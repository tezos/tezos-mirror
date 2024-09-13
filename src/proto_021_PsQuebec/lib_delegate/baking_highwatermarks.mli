(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol.Alpha_context

type highwatermark = {round : Round.t; level : int32}

type error += Block_previously_baked of highwatermark

type error += Block_previously_preattested of highwatermark

type error += Block_previously_attested of highwatermark

type t

val load :
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  t tzresult Lwt.t

val may_sign_block :
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  delegate:Signature.public_key_hash ->
  level:int32 ->
  round:Round.t ->
  bool tzresult Lwt.t

val may_sign_preattestation :
  t ->
  delegate:Signature.public_key_hash ->
  level:int32 ->
  round:Round.t ->
  bool

val may_sign_attestation :
  t ->
  delegate:Signature.public_key_hash ->
  level:int32 ->
  round:Round.t ->
  bool

val record_block :
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  delegate:Signature.public_key_hash ->
  level:int32 ->
  round:Round.t ->
  unit tzresult Lwt.t

val record_preattestation :
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  delegate:Signature.public_key_hash ->
  level:int32 ->
  round:Round.t ->
  unit tzresult Lwt.t

val record_attestation :
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  delegate:Signature.public_key_hash ->
  level:int32 ->
  round:Round.t ->
  unit tzresult Lwt.t

val record_all_preattestations :
  t ->
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  delegates:public_key_hash list ->
  level:int32 ->
  round:Round.t ->
  unit tzresult Lwt.t

val record_all_attestations :
  t ->
  #Protocol_client_context.full ->
  [`Highwatermarks] Baking_files.location ->
  delegates:public_key_hash list ->
  level:int32 ->
  round:Round.t ->
  unit tzresult Lwt.t
