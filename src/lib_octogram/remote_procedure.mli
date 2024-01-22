(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Jingoo.Jg_types

type ('a, 'uri) t = ..

type ('a, 'uri) remote_procedure = ('a, 'uri) t

type 'uri packed = Packed : ('a, 'uri) t -> 'uri packed

type ('a, 'b) eq = Eq : ('a, 'a) eq | Neq : ('a, 'b) eq

(** The module type to implement in order to be able to make agents capable of
    executing a new kind of remote procedure *)
module type S = sig
  (** The name of the remote procedure, as referred to in the experience script
      when defining a new job. *)
  val name : string

  (** The request sent by the orchestrator to the agent responsible to execute
      it. *)
  type 'a t

  (** The result returned by the agent to the orchestrator. *)
  type r

  val of_remote_procedure : ('a, 'uri) remote_procedure -> 'uri t option

  val to_remote_procedure : 'uri t -> (r, 'uri) remote_procedure

  val unify : ('a, 'uri) remote_procedure -> ('a, r) eq

  val encoding : 'uri Data_encoding.t -> 'uri t Data_encoding.t

  val r_encoding : r Data_encoding.t

  val tvalue_of_r : r -> tvalue

  val expand :
    self:Agent_name.t -> run:(string -> string) -> string t -> Uri.global_uri t

  val resolve :
    self:Agent_name.t -> Uri_resolver.t -> Uri.global_uri t -> Uri.agent_uri t

  val run : Agent_state.t -> Uri.agent_uri t -> r Lwt.t

  val on_completion :
    on_new_service:
      (string ->
      Services_cache.node_kind ->
      Services_cache.service_kind ->
      int ->
      unit) ->
    on_new_metrics_source:(string -> Services_cache.node_kind -> int -> unit) ->
    r ->
    unit
end

val packed_encoding : 'uri Data_encoding.t -> 'uri packed Data_encoding.t

val encode_response : ('a, 'uri) t -> 'a -> string

val response_encoding : ('a, 'uri) t -> 'a Data_encoding.t

val merged_encoding :
  'a Data_encoding.t ->
  'uri Data_encoding.t ->
  ('a * 'uri packed) Data_encoding.t

val tvalue_of_response : ('a, 'uri) t -> 'a -> tvalue

val expand :
  self:Agent_name.t ->
  vars:Global_variables.t ->
  agent:tvalue ->
  re:tvalue ->
  item:tvalue ->
  string packed ->
  Uri.global_uri packed

val resolve_global_uris :
  self:Agent_name.t ->
  Uri_resolver.t ->
  Uri.global_uri packed ->
  Uri.agent_uri packed

val run : Agent_state.t -> ('a, Uri.agent_uri) t -> 'a Lwt.t

val on_completion :
  on_new_service:
    (string ->
    Services_cache.node_kind ->
    Services_cache.service_kind ->
    int ->
    unit) ->
  on_new_metrics_source:(string -> Services_cache.node_kind -> int -> unit) ->
  ('a, 'uri) t ->
  'a ->
  unit

(** [register (module P)] plugs the remote procedure specified in [P] into
    Octogram machinery.

    {v
module P = struct
  let name = _

  type 'uri t = _

  type r = _

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | _ _ args -> Some args
    | _ -> None

  let to_remote_procedure args = _ args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | _ -> Eq
    | _ -> Neq

  let encoding uri_encoding = _

  let r_encoding = _

  let tvalue_of_r args = _

  let expand ~self ~run args = _

  let resolve ~self resolver args = _

  let run state args = _

  let on_completion ~on_new_service ~on_new_metrics_source args = _
end
    v} *)
val register : (module S) -> unit

val file_agent_uri :
  self:Agent_name.t ->
  resolver:Uri_resolver.t ->
  Uri.global_uri ->
  Uri.agent_uri

val global_uri_of_string :
  self:Agent_name.t -> run:(string -> string) -> string -> Uri.global_uri
