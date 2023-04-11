(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Gossipsub_intf

(** This module allows to compute a score for each peers. *)

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4967

   This is incomplete *)

module Make (Span : SPAN) (Time : TIME with type span = Span.t) = struct
  type span = Span.t

  type time = Time.t

  type peer_status =
    | Connected
    | Disconnected of {
        expires : Time.t;  (** The time after which the score can be cleared. *)
      }

  type t = {
    behaviour_penalty : int;  (** The score associated to a peer. *)
    peer_status : peer_status;
  }

  let newly_connected () = {behaviour_penalty = 0; peer_status = Connected}

  let float {behaviour_penalty; _} = -behaviour_penalty |> float_of_int

  let penalty score penalty =
    {score with behaviour_penalty = score.behaviour_penalty + penalty}

  let is_connected ps =
    match ps.peer_status with Connected -> true | Disconnected _ -> false

  let set_connected score = {score with peer_status = Connected}

  let expires ps =
    match ps.peer_status with
    | Connected -> None
    | Disconnected {expires} -> Some expires

  let compare s1 s2 =
    let f1 = float s1 in
    let f2 = float s2 in
    Float.compare f1 f2

  include Compare.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
