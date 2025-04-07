(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Return type for service handler *)
type 'o t =
  [ `Ok of 'o (* 200 *)
  | `OkChunk of 'o (* 200 but send answer as chunked transfer encoding *)
  | `OkStream of 'o stream (* 200 *)
  | `Created of string option (* 201 *)
  | `No_content (* 204 *)
  | `Unauthorized of error list option (* 401 *)
  | `Forbidden of error list option (* 403 *)
  | `Not_found of error list option (* 404 *)
  | `Conflict of error list option (* 409 *)
  | `Error of error list option (* 500 *) ]

and 'a stream = {next : unit -> 'a option Lwt.t; shutdown : unit -> unit}

val return : 'o -> 'o t Lwt.t

(** [return_chunked] is identical to [return] but it indicates to the server
    that the result might be long and that the serialisation should be done in
    mutliple chunks.

    You should use [return_chunked] when returning an (unbounded or potentially
    large) list, array, map, or other such set. *)
val return_chunked : 'o -> 'o t Lwt.t

val return_stream : 'o stream -> 'o t Lwt.t

val not_found : 'o t Lwt.t

val fail : error list -> 'a t Lwt.t
