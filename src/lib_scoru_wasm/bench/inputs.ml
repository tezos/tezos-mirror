(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(** Defines interface to access / document inputs *)

module Kernels = struct
  let tx_kernel_dac =
    "src/lib_scoru_wasm/bench/kernels/tx_kernel/tx_kernel_dac_20221216.wasm"

  (** taken from unit test,
      reads input, execute without taking them into account *)
  let computation_kernel = "src/lib_scoru_wasm/bench/kernels/computation.wasm"

  (** taken from unit tests, supposed to get stuck at the first execution*)
  let unreachable_kernel = "src/lib_scoru_wasm/bench/kernels/unreachable.wasm"
end

module Messages = struct
  module Demo = struct
    let deposit_blue = "gen_messages/00-deposit-blue.out"

    let deposit_green = "gen_messages/00-deposit-green.out"

    let deposit_red = "gen_messages/00-deposit-red.out"
  end
end
