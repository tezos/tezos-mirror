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

(** Mockup file abstraction *)

(** File path *)
type t = private string

(** [mockup_directory ~dirname] gets the full name of the mockup subdirectory
    located inside [dirname] *)
val get_mockup_directory : dirname:string -> t

(** [has_mockup_directory ~dirname] checks whether a mockup subdirectory exists
    in directory [dirname] *)
val exists_mockup_directory : dirname:string -> bool Lwt.t

(** Generic signature to access files located inside mockup subdirectory *)
module type ACCESSOR = sig
  (** [get ~dirname] retrieves the full file name of [file] under [dirname]. It
     is expected to be <base_dir>/<mockup_directory>/<basename> *)
  val get : dirname:string -> t

  (** [exists ~dirname] checks whether [file] is present under [dirname] *)
  val exists : dirname:string -> bool Lwt.t
end

(** Accessor for [context.json] *)
module Context : ACCESSOR

(** Accessor for [mempool.json]*)
module Mempool : ACCESSOR

(** Accessor for [thrashpool.json] *)
module Trashpool : ACCESSOR
