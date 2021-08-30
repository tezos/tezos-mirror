(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module S = Saturation_repr

let ( + ) = S.add

let ( lsl ) = S.shift_left

let ( lsr ) = S.shift_right

(* Approximating 31.8578476398 * number of bytes *)
let expr_to_address_in_context_cost bytes =
  let v0 = Bytes.length bytes |> S.safe_int in
  Gas_limit_repr.atomic_step_cost
  @@ ((v0 lsl 4) + (v0 lsl 3) + (v0 lsl 2) + (v0 lsl 1) + v0 + (v0 lsr 1))

(* Approximating 4156.4530516 *)
let expand_constants_branch_cost =
  Gas_limit_repr.atomic_step_cost @@ S.safe_int 4156

(* Approximating 24.534511699 * number of nodes *)
let expand_no_constants_branch_cost node =
  let size = Script_repr.micheline_nodes node |> S.safe_int in
  Gas_limit_repr.atomic_step_cost @@ ((size lsl 4) + (size lsl 3) + (size lsl 1))
