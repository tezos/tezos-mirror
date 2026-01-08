(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Node_context

(** [lock ~data_dir] locks the [data_dir]. *)
val lock : data_dir:string -> Lwt_unix.file_descr tzresult Lwt.t

(** [init cctxt ~data_dir mode l1_ctxt genesis_info protocol configuration]
    initializes the rollup representation. The rollup origination level and kind
    are fetched via an RPC call to the layer1 node that [cctxt] uses for RPC
    requests.
*)
val init :
  #Client_context.full ->
  data_dir:string ->
  irmin_cache_size:int ->
  ?last_whitelist_update:Z.t * Int32.t ->
  store_access:([< `Read | `Write > `Read] as 'store) Access_mode.t ->
  context_access:([< `Read | `Write > `Read] as 'context) Access_mode.t ->
  Layer1.t ->
  genesis_info ->
  lcc:lcc ->
  lpc:Commitment.t option ->
  Kind.t ->
  current_protocol ->
  Configuration.t ->
  < store : 'store ; context : 'context > t tzresult Lwt.t

(** Closes the store, context and Layer 1 monitor. *)
val close : _ t -> unit tzresult Lwt.t

module For_snapshots : sig
  (** [create_node_context cctxt protocol store context ~data_dir
      ~apply_unsafe_patches] creates a node context which does not monitor the
      L1 chain but which can be used to reconstruct the context from historical
      data. This function is used by the {!Snapshots} module. *)
  val create_node_context :
    #Client_context.full ->
    current_protocol ->
    'store Store.t ->
    'context Context.t ->
    data_dir:string ->
    apply_unsafe_patches:bool ->
    < store : 'store ; context : 'context > t tzresult Lwt.t
end

(**/**)

module Internal_for_tests : sig
  (** Create a node context which really stores data on disk but does not
      connect to any layer 1 node. It is meant to be used in unit tests for the
      rollup node functions. *)
  val create_node_context :
    #Client_context.full ->
    current_protocol ->
    data_dir:string ->
    Kind.t ->
    rw tzresult Lwt.t

  (** Create a dummy context to generate OpenAPI specification. *)
  val openapi_context :
    #Client_context.full -> Protocol_hash.t -> rw tzresult Lwt.t
end
