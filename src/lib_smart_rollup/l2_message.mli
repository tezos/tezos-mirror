(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Type of L2 messages.  *)
type t

(** [make ~unique message] constructs a message with content
    [message]. If [unique = true] the returned value will be unique
    (multiple identical calls to [make] will return different values)
    because it will be given a unique id. Using [unique = false], on the
    contrary, makes the call idempotent. *)
val make : unique:bool -> string -> t

(** [content message] returns the string content of [message], i.e.
    [content (make s) = s]. *)
val content : t -> string

(** Hash with b58check encoding scmsg(55), for ids of L2 messages. *)
module Id : Tezos_crypto.Intfs.HASH

(** Alias for message id *)
type id = Id.t

(**  {2 Serialization} *)

val content_encoding : string Data_encoding.t

val encoding : t Data_encoding.t

val id : t -> Id.t
