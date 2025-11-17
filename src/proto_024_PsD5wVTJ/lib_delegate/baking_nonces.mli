(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context

type state

type t = state

(** [nonces] is a hash map corresponding to the data which can be found in the
    file from [nonces_location] *)
type nonces

(** [load wallet location] loads the file corresponding to [location] and
    returns a data structure containing the stored information. *)
val load :
  #Client_context.wallet ->
  stateful_location:[`Stateful_nonce] Baking_files.location ->
  nonces tzresult Lwt.t

(** [generate_seed_nonce nonce_config delegate level] computes a nonce via a
    [Deterministic] or [Random] approach, depending on the [nonce_config]
    argument; the deterministic case uses as parameters [delegate] and [level]. *)
val generate_seed_nonce :
  ?timeout:float ->
  Baking_configuration.nonce_config ->
  Baking_state_types.Key.t ->
  Raw_level.t ->
  (Nonce_hash.t * Nonce.t) tzresult Lwt.t

(** [register_nonce cctxt ~chain_id block_hash nonce ~cycle ~level ~round] updates
    the contents from the nonces file located using [cctxt] and [~chain_id] by
    adding a new entry or replacing an existing one of the form
    [block_hash] : [nonce] * [~cycle] * [~level] * [~round]. *)
val register_nonce :
  #Protocol_client_context.full ->
  chain_id:Chain_id.t ->
  Block_hash.t ->
  Nonce.t ->
  cycle:Cycle.t ->
  level:Raw_level.t ->
  round:Round.t ->
  unit tzresult Lwt.t

(** [start_revelation_worker cctxt config chain_id constants block_stream]
    represents the continuous process of receiving new proposal from [block_stream]
    and applying them to the internal state created from [cctxt], [config],
    [chain_id] and [constants]; each new proposal can produce a nonce, to be stored
    in a nonce file location, facilitating the nonce revelation process. *)
val start_revelation_worker :
  Protocol_client_context.full ->
  Baking_configuration.nonce_config ->
  Chain_id.t ->
  Constants.t ->
  Baking_state_types.proposal Lwt_stream.t ->
  Lwt_canceler.t Lwt.t
