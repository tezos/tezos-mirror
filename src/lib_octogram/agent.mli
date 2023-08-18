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

(** This module introduces the business logic run by an Octogram agent on a
    remote runner. *)

type request = {
  proc_id : int;
  procedure : Uri.agent_uri Remote_procedure.packed;
}

(** [run ~input ~output state] changes the current working directory of the
    process to the [state]’s home directory (see {!initial_state}), starts an
    event loop, reading request from [input] and writing the results to
    [output].

    For a request [{proc_id; procedure}], the expected encoding to be read from
    the standard input is the Json encoding specified by {!request_encoding}.
    
    The agent responds with the [<<[{proc_id}] :] (that is, if [proc_id] is
    equal to 1, the magic string is [<<[1]: ], followed by a Json response as
    specified by the encodings defined in the {!Remote_procedure} module).*)
val run :
  input:Lwt_io.input_channel ->
  output:Lwt_io.output_channel ->
  Agent_state.t ->
  unit Lwt.t

(** {2 Encodings} *)

val request_encoding : request Data_encoding.t
