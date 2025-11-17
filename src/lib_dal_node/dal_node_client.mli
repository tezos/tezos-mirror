(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

(** Instance of [Tezos_client_base.Client_context] that only handles IOs and
    RPCs. Can be used for keys and RPCs related commands. *)
type cctxt

val call :
  cctxt ->
  ([< Resto.meth], unit, 'a, 'b, 'c, 'd) Tezos_rpc__.RPC_service.t ->
  'a ->
  'b ->
  'c ->
  'd Tezos_error_monad.Error_monad.tzresult Lwt.t

(** [make_unix_context endpoint] generates a cctxt with the provided
    [endpoint]. *)
val make_unix_cctxt : Uri.t -> cctxt

(** [get_slot_pages cctxt slot_id] fetches from the dal node the pages
    of the slot with id [slot_id]. *)
val get_slot_pages :
  cctxt -> Tezos_dal_node_services.Types.slot_id -> bytes list tzresult Lwt.t

(** [get_slot_page_proof cctxt slot_id page_index] computes and
    returns the proof of the page whose index and slot are given. *)
val get_slot_page_proof :
  cctxt ->
  Tezos_dal_node_services.Types.slot_id ->
  int ->
  Cryptobox.page_proof tzresult Lwt.t

(** [post_slot cctxt slot_data] posts the given data as a slot to the DAL node,
    and returns the corresponding commitment hash alongside its proof. *)
val post_slot :
  cctxt ->
  ?slot_index:Tezos_dal_node_services.Types.slot_index ->
  string ->
  (Cryptobox.commitment * Cryptobox.commitment_proof) tzresult Lwt.t

(** [get_slot_status cctxt slot_id] returns the attestation
    info (status and lag) of the slot whose id is given. *)
val get_slot_status :
  cctxt ->
  Tezos_dal_node_services.Types.slot_id ->
  Tezos_dal_node_services.Types.header_status tzresult Lwt.t
