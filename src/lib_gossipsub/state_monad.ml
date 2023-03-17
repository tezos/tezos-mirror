(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** See documentation of the interface below in the mli file. *)

type ('state, 'a) t = 'state -> 'state * 'a

type ('state, 'pass, 'fail) check =
  ('state, [`Pass of 'pass | `Fail of 'fail]) t

module type S = sig
  val bind : ('state, 'a) t -> ('a -> ('state, 'b) t) -> ('state, 'b) t

  val get : ('state -> 'a) -> ('a -> ('state, 'b) t) -> ('state, 'b) t

  val return : 'a -> ('state, 'a) t

  val check :
    ('state, 'pass, 'fail) check ->
    ('pass -> ('state, 'fail) t) ->
    ('state, 'fail) t

  val return_pass : 'pass -> ('state, 'pass, 'fail) check

  val return_fail : 'fail -> ('state, 'pass, 'fail) check

  module Syntax : sig
    val ( let* ) : ('state, 'a) t -> ('a -> ('state, 'b) t) -> ('state, 'b) t

    val ( let*? ) :
      ('state, 'pass, 'fail) check ->
      ('pass -> ('state, 'fail) t) ->
      ('state, 'fail) t

    val ( let*! ) : ('state -> 'a) -> ('a -> ('state, 'b) t) -> ('state, 'b) t

    val return : 'a -> ('state, 'a) t

    val pass : 'pass -> ('state, 'pass, 'fail) check

    val unit : ('state, unit, 'fail) check

    val fail : 'fail -> ('state, 'a, 'fail) check
  end
end

module M : S = struct
  let bind m f state =
    let state, res = m state in
    f res state

  let get p f state =
    let res = p state in
    f res state

  let return x state = (state, x)

  let check c f =
    bind c (function `Pass res -> f res | `Fail output -> return output)

  let return_pass x = `Pass x |> return

  let return_fail x = `Fail x |> return

  module Syntax = struct
    let ( let* ) = bind

    let ( let*? ) = check

    let ( let*! ) = get

    let return = return

    let pass = return_pass

    let unit state = pass () state

    let fail = return_fail
  end
end
