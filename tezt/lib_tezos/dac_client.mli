(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

(** The type of a dac client. *)
type t

(** [create ?runner ?name ?base_dir ?color dac_node] creates a new
    Dac_client.t that can be used to send commands to [dac_node]. *)
val create :
  ?runner:Runner.t ->
  ?name:string ->
  ?base_dir:string ->
  ?color:Log.Color.t ->
  Dac_node.t ->
  t

(** The output of a dac client command.
    Can be either a root hash or a certificate. *)
type output = Root_hash of Hex.t | Certificate of Hex.t

(** [send_payload ?hooks ?threshold dac_client hex] sends the Hex payload [hex]
    to the coordinator of [dac_client]. If the [threshold] value is specified,
    then the command will wait for a  [Certificate] with an amount of
    signatures greater or equal to [threshold], before returning. Otherwise,
    the [Root_hash] of the payload is returned. *)
val send_payload :
  ?hooks:Process_hooks.t -> ?threshold:int -> t -> Hex.t -> output Lwt.t

(** [get_certificate ?hooks dac_client root_hash] returns the certificate
    available to the coordinator of [dac_client] for [root_hash], if any.
    Otherwise, [None] is returned. *)
val get_certificate :
  ?hooks:Process_hooks.t -> t -> Hex.t -> output option Lwt.t
