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

(** Functions that raise exceptions are hidden by the main modules of Lwtreslib
    but available here. These functions are either: very practical or are safe
    in some specific uses (e.g., [List.init] when used with a literal length).

    Functions that take a [loc] parameter raise {!Invalid_argument} with the
    location included in the exception's message. It is intended to be used with
    the {!__LOC__} value, but it can be used with different messages. *)
module type S = sig
  module Option : sig
    val get : loc:string -> 'a option -> 'a

    val to_exn : none:exn -> 'a option -> 'a

    val to_exn_f : none:(unit -> exn) -> 'a option -> 'a
  end

  module Result : sig
    val get_ok : loc:string -> ('a, 'trace) result -> 'a

    val get_error : loc:string -> ('a, 'trace) result -> 'trace

    (* [to_exn (Ok v)] is [v], [to_exn (Error e)] raises [e] *)
    val to_exn : ('a, exn) result -> 'a

    val to_exn_f : error:('b -> exn) -> ('a, 'b) result -> 'a
  end

  module List : sig
    val combine : loc:string -> 'a list -> 'b list -> ('a * 'b) list

    val rev_combine : loc:string -> 'a list -> 'b list -> ('a * 'b) list

    val map2 : loc:string -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  end
end
