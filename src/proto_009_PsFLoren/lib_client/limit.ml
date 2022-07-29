(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type 'a t = 'a option

let unknown = None

let known x = Some x

let of_option = Fun.id

let is_unknown = Option.is_none

let join (type a) ~where eq (l1 : a t) (l2 : a t) =
  match (l1, l2) with
  | None, None -> Result.return_none
  | Some x, None | None, Some x -> Result.return_some x
  | Some x, Some y ->
      if eq x y then Result.return_some x
      else error_with "Limit.join: error (%s)" where

let%test "join" =
  let check res y =
    match res with Ok x -> Option.equal Bool.equal x y | Error _ -> false
  in
  check (join ~where:__LOC__ Bool.equal (Some true) (Some true)) (Some true)
  && check (join ~where:__LOC__ Bool.equal None None) None
  && check (join ~where:__LOC__ Bool.equal None (Some true)) (Some true)
  && check (join ~where:__LOC__ Bool.equal (Some true) None) (Some true)
  && not
       (Result.is_ok (join ~where:__LOC__ Bool.equal (Some true) (Some false)))

let get ~when_unknown = function
  | None -> error_with "Limit.get: %s" when_unknown
  | Some x -> ok x

let%test "get" =
  match get ~when_unknown:"" (Some true) with Ok true -> true | _ -> false

let fold ~unknown ~known x = match x with None -> unknown | Some x -> known x

let value ~when_unknown = function None -> when_unknown | Some x -> x
