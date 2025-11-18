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

type baking_mode = Local of {local_data_dir_path : string} | Remote

val get_delegates :
  Protocol_client_context.full ->
  Environment.Signature.public_key_hash trace ->
  Baking_state_types.Key.t list tzresult Lwt.t

val run_baker :
  string option
  * bool
  * string option
  * Protocol.Alpha_context.Tez.t
  * Q.t
  * Q.t
  * int option
  * bool
  * Protocol.Per_block_votes_repr.per_block_vote option
  * Protocol.Per_block_votes_repr.per_block_vote option
  * string option
  * Baking_configuration.Operations_source.t option
  * Uri.t option
  * bool
  * Baking_configuration.state_recorder_config
  * Q.t option
  * Q.t option
  * bool ->
  baking_mode ->
  Environment.Signature.public_key_hash list ->
  Protocol_client_context.full ->
  unit Error_monad.tzresult Lwt.t

val delegate_commands :
  unit -> Protocol_client_context.full Tezos_clic.command list

val baker_commands :
  unit -> Protocol_client_context.full Tezos_clic.command list

val accuser_commands :
  unit -> Protocol_client_context.full Tezos_clic.command list
