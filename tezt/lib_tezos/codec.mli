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

(** Run octez-codec commands. *)

(** In all functions below, [path] can be used to override the path
    to [octez-codec], which is [Constant.tezos_codec] by default. *)

(** Run [octez-codec encode <name> from <json>]. *)
val encode :
  ?path:string -> ?hooks:Process.hooks -> name:string -> JSON.u -> string Lwt.t

(** Same as [encode], but do not wait for the process to exit. *)
val spawn_encode :
  ?path:string -> ?hooks:Process.hooks -> name:string -> JSON.u -> Process.t

(** Run [octez-codec decode <name> from <binary>]. *)
val decode :
  ?path:string -> ?hooks:Process.hooks -> name:string -> string -> JSON.t Lwt.t

(** Same as [decode], but do not wait for the process to exit. *)
val spawn_decode :
  ?path:string -> ?hooks:Process.hooks -> name:string -> string -> Process.t

(** Run [octez-codec dump encodings]. *)
val dump_encodings : ?path:string -> unit -> JSON.t Lwt.t

(** Same as [dump_encodings], but do not wait for the process to exit. *)
val spawn_dump_encodings : ?path:string -> unit -> Process.t

(** Run [octez-codec dump encoding]. *)
val dump_encoding : ?path:string -> id:string -> unit -> JSON.t Lwt.t

(** Same as [dump_encoding], but do not wait for the process to exit. *)
val spawn_dump_encoding : ?path:string -> id:string -> unit -> Process.t

val describe_binary_schema : ?path:string -> id:string -> unit -> string Lwt.t

val describe_json_schema : ?path:string -> id:string -> unit -> string Lwt.t
