(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(** [send_preimage coordinator_cctxt b] sends a [PUT /preimage] request with
    body [b] (hex-encoded) to the coordinator of [coordinator_cctxt]. *)
val send_preimage :
  #Dac_node_client.cctxt -> bytes -> Dac_plugin.raw_hash tzresult Lwt.t

(** [wait_for_certificate coordinator_cctxt root_hash threshold] monitors
    certificate updates for [root_hash] via the endpoint
    [GET /monitor/certificates] of the coordinator referenced by
    [coordinator_cctxt]. *)
val wait_for_certificate :
  #Dac_node_client.cctxt ->
  Dac_plugin.raw_hash ->
  int ->
  (Tezos_stdlib.Hex.t option, tztrace) result Lwt.t

(** [get_certificate coordinator_cctxt root_hash] returns the most up-to-date
    certificate available for [root_hash] from the coordinator of
    [coordinator_cctxt]. *)
val get_certificate :
  #Dac_node_client.cctxt ->
  Dac_plugin.raw_hash ->
  (Tezos_stdlib.Hex.t option, tztrace) result Lwt.t
