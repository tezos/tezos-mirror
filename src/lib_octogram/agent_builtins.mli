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

(** This module introduces the remote procedure allowing to manage the
    lifecycle of a remote agent. *)

val agent_should_continue : Agent_state.t -> bool

type start_http_server = {directory : string; http_port : string option}

type start_http_server_r = {port : int}

type action = Create | Extract

type 'uri tar = {contents : string; archive : 'uri; action : action}

type ('a, 'uri) Remote_procedure.t +=
  | Quit : (unit, 'uri) Remote_procedure.t
        (** [Quit] can be used to terminate an agent gracefully. *)
  | Start_http_server :
      start_http_server
      -> (start_http_server_r, 'uri) Remote_procedure.t
        (** [Start_http_server] spawns a HTTP server on the agent, which can
            later be used to fetch files from it.

            Each agent can spawn at most one HTTP server. *)
  | Tar : 'uri tar -> (unit, 'uri) Remote_procedure.t
        (** [Tar] requests an agent to [Create] or [Extract] an archive.

            When [Extract]ing, the [archive] can be on a remote agent. In such
            a case, the HTTP server of this agent will be used as a means to
            fetch the archive.*)
