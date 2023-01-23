(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4651
   Integrate the pages backend with preimage data manager. *)

type error +=
  | Cannot_write_dac_page_to_page_storage of {key : string; contents : bytes}
  | Cannot_read_dac_page_from_page_storage of string

(** [S] is the module type defining the backend required for
    persisting DAC pages data onto the page storage. *)
module type S = sig
  (** [t] represents an instance of a storage backened. *)
  type t

  (** [key] is identifier for storing pages contents. *)
  type key

  (** [write_page key contents t] writes [contents] of page onto storage
      backend [t] under given [key].
      
      When writing fails it returns a [Cannot_write_dac_page_to_page_storage]
      error.

      We require [write_page] to be atomic.*)
  val write_page : key:key -> contents:bytes -> t -> unit tzresult Lwt.t

  (** [read_page key t] returns [contents] of the storage backend [t]
      represented by a key. When reading fails it returns a 
      [Cannot_read_dac_page_from_page_storage] error. *)
  val read_page : key:key -> t -> bytes tzresult Lwt.t
end

(* This is the default implementation of the signature. *)
module Default : S
