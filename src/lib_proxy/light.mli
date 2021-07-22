(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Code related to the light mode that is protocol-independent.
    See [src/proto_*/lib_client/light.ml] files for protocol-dependent code. *)

(** See [mk_sources_config] smart constructor to build values. *)
type sources_config = private {
  min_agreement : float;
      (** A float between 0 (exclusive) and 1 (inclusive), representing
          the minimum ratio of endpoints that must agree on data
          for said data to be accepted. 1 means "require all enpoints
          to agree" (the default). *)
  uris : Uri.t list;
      (** The list of endpoint URIs used for Light mode consensus.
          This list must contain at least 2 endpoints (one for data retrieval,
          one for check). *)
}

(** See [sources_config_to_sources] to build values. *)
type sources = private {
  min_agreement : float;
      (** A float between 0 (exclusive) and 1 (inclusive), representing
          the minimum ratio of endpoints that must agree on data
          for said data to be accepted. 1 means "require all enpoints
          to agree" (the default). *)
  endpoints : (Uri.t * RPC_context.simple) list;
      (** The list of endpoint URIs used for Light mode consensus.
          This list must contain at least 2 endpoints
          (one for data retrieval, one for check). *)
}

(** Ad-hoc type safe JSON parsing function until [json-data-encoding]
    library provides a [result]-returning alternative. *)
val destruct_sources_config :
  Data_encoding.json -> (sources_config, string) result

(** [sources_config_to_uris sources] returns the {!Uri}s contained
 *  in [sources]. *)
val sources_config_to_uris : sources_config -> Uri.t list

(** [sources_config_to_sources f config] transforms the value [config]
    (which was obtained by parsing the CLI) into a value used
    by core algorithms of the light mode. *)
val sources_config_to_sources :
  (Uri.t -> RPC_context.simple) -> sources_config -> sources

(** [None] if the given block is symbolic, otherwise its concrete hash. *)
val hash_of_block :
  Tezos_shell_services.Block_services.block -> Block_hash.t option
