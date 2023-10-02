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

(** This module introduces the definition of a Octogram job, that is an
    interaction between the orchestrator and one agent.

    Note that one job can result in several interactions, depending on how the
    [with_item] field of the job [header] is used. *)

type item = Global_variables.var

type items = item list

type with_items = Seq of items | Prod of items list

val expand_item :
  vars:Global_variables.t -> agent:tvalue -> re:tvalue -> item -> tvalue Seq.t

type header = {
  name : string;
  with_items : with_items option;
      (** The job’ body will be executed once for every item listed in the
          [with_items] field, where each execution has its [{{ item }}]
          variable customized for the current value.

          If [with_item] is [None], then the job’s body is executed once, with
          [{{ item }}] equal to [null]. *)
  mode : Execution_params.mode;
  vars_updates : Global_variables.update list;
}

type 'uri body =
  | Remote_procedure of {procedure : 'uri Remote_procedure.packed}
      (** Request the targeted agent to execute the [procedure]. *)
  | Copy of {source : string; destination : string}
      (** Copy [destination] to the targeted agent, relatively to
          [destination]. *)

val expand_body :
  self:Agent_name.t ->
  vars:Global_variables.t ->
  agent:tvalue ->
  re:tvalue ->
  item:tvalue ->
  string body ->
  Uri.global_uri body

type 'uri t = {header : header; body : 'uri body}

(** Require to use the [Register] functor *)
val encoding : 'uri Data_encoding.t -> 'uri t Data_encoding.t
