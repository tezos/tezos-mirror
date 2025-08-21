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

module Request = struct
  type ('a, 'b) t =
    | Play : Game.t -> (unit, error trace) t
    | Play_opening : Game.conflict -> (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Play"
          (obj2 (req "request" (constant "play")) (req "game" Game.encoding))
          (function View (Play g) -> Some ((), g) | _ -> None)
          (fun ((), g) -> View (Play g));
        case
          (Tag 1)
          ~title:"Play opening"
          (obj2
             (req "request" (constant "play_opening"))
             (req "conflict" Game.conflict_encoding))
          (function View (Play_opening c) -> Some ((), c) | _ -> None)
          (fun ((), c) -> View (Play_opening c));
      ]

  let pp ppf (View r) =
    match r with
    | Play game ->
        Format.fprintf
          ppf
          "Playing game %a"
          Data_encoding.Json.pp
          (Data_encoding.Json.construct Game.encoding game)
    | Play_opening conflict ->
        Format.fprintf
          ppf
          "Playing opening move for conflict against staker %a at our \
           commitment %a"
          Signature.Public_key_hash.pp
          conflict.other
          Data_encoding.Json.pp
          (Data_encoding.Json.construct
             Commitment.encoding
             conflict.our_commitment)

  let name : type a b. (a, b) t -> string = function
    | Play _ -> "Play"
    | Play_opening _ -> "Play_opening"
end
