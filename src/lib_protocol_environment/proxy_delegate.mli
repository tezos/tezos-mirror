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

(** The module that needs to be implemented for providing the delegation
    feature to {!Proxy_context}. Implementors willing to add a new backend
    to the client's [--mode proxy] and [tezos-proxy-server] should likely
    add a new implementation of this module type. *)
module type T = sig
  (** [proxy_dir_mem key] returns whether {!proxy_get} would return a non-leaf tree. *)
  val proxy_dir_mem : string list -> bool tzresult Lwt.t

  (** [proxy_get key] returns the tree associated to [key]. *)
  val proxy_get :
    string list -> Tezos_context_memory.Context.tree option tzresult Lwt.t

  (** [proxy_mem key] returns whether [proxy_get key] would return a leaf tree. *)
  val proxy_mem : string list -> bool tzresult Lwt.t
end

(** {!T} as a type, to make it easier to pass it around *)
type t = (module T)
