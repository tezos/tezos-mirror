(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error += Tx_rollup_negative_message_size

module S = Saturation_repr

(** The same model as in {!Michelson_v1_gas.N_IBlake2b}. *)
let message_hash_cost msg_size =
  if Compare.Int.(0 <= msg_size) then
    let ( + ) = S.add in
    let v0 = Saturation_repr.safe_int msg_size in
    let cost_N_IBlake2b = S.safe_int 430 + v0 + S.shift_right v0 3 in
    ok @@ Gas_limit_repr.atomic_step_cost cost_N_IBlake2b
  else error Tx_rollup_negative_message_size

let () =
  let open Data_encoding in
  (* Tx_rollup_negative_message_size *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_negative_message_size"
    ~title:"The protocol has computed a negative size for an inbox messages"
    ~description:
      "The protocol has computed a negative size for an inbox messages. This \
       is an internal error, and denotes a bug in the protocol implementation."
    unit
    (function Tx_rollup_negative_message_size -> Some () | _ -> None)
    (fun () -> Tx_rollup_negative_message_size)
