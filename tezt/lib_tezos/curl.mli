(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** [get url] returns a runnable requesting [url] with curl.

    The response is parsed and returned as JSON.
    Fails if [curl] is not found in path.
  *)
val get :
  ?name:string ->
  ?runner:Runner.t ->
  ?args:string list ->
  string ->
  JSON.t Runnable.process

(** Same as [get] but does not parse the returned value *)
val get_raw :
  ?name:string ->
  ?runner:Runner.t ->
  ?args:string list ->
  string ->
  string Runnable.process

(** [post url data] returns a runnable posting [data] to [url] with curl.

    The response is parsed and returned as JSON.
    Fails if [curl] is not found in path. *)
val post :
  ?name:string ->
  ?runner:Runner.t ->
  ?args:string list ->
  string ->
  JSON.t ->
  JSON.t Runnable.process

(** Same as [post] but does not parse the returned value *)
val post_raw :
  ?name:string ->
  ?runner:Runner.t ->
  ?args:string list ->
  string ->
  JSON.t ->
  string Runnable.process

(** [put url data] returns a runnable putting [data] to [url] with curl.

    The response is parsed and returned as JSON.
    Fails if [curl] is not found in path. *)
val put :
  ?name:string ->
  ?runner:Runner.t ->
  ?args:string list ->
  string ->
  JSON.t ->
  JSON.t Runnable.process

(** Same as [put] but does not parse the returned value *)
val put_raw :
  ?name:string ->
  ?runner:Runner.t ->
  ?args:string list ->
  string ->
  JSON.t ->
  string Runnable.process
