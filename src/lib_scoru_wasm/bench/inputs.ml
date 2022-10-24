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
  (** taken from integration tests.*)
  let tx_kernel_old =
    "src/lib_scoru_wasm/bench/kernels/tx_kernel/tx_kernel.wasm"

  (** does not use durable storage, does not check signatures.
      To recompute, checkout commit deb95799cc in kernel repo,
      compile according to README instruction *)
  let tx_kernel_vRAM_nosig =
    "src/lib_scoru_wasm/bench/kernels/tx_kernel/tx_kernal_deb95799cc_nosig.wasm"

  (** does not use durable storage, checks signatures.
      To recompute, checkout commit 12bf6a994 in kernel repo,
      compile according to README instruction *)
  let tx_kernel_vRAM_sig =
    "src/lib_scoru_wasm/bench/kernels/tx_kernel/tx_kernel_12bf6a994.wasm"

  (** Taken directly from pipeline in kernel repo.
      https://gitlab.com/trili/kernel/-/commit/e759f43ec27ef5fa2cebb567780a61702e851c33/pipelines?ref=main *)
  let tx_kernal_vRam_latest =
    "src/lib_scoru_wasm/bench/kernels/tx_kernel/tx_kernel_e759f43e.wasm"

  (** taken from unit test,
      reads input, execute without taking them into account *)
  let computation_kernel = "src/lib_scoru_wasm/bench/kernels/computation.wasm"

  (** taken from unit tests, supposed to get stuck at the first execution*)
  let unreachable_kernel = "src/lib_scoru_wasm/bench/kernels/unreachable.wasm"
end

module Messages = struct
  (** messages extracted from end 2 end test (trili/kernel) deposit_transfer_withdraw
      https://gitlab.com/trili/kernel/-/blob/303400a8a286d268d73debddee1390472315146c/e2e_kernel_tests/tests/tx_kernel.rs
      *)
  module Deposit_transfer_withdraw = struct
    (** deposit of first actor *)
    let fst_deposit =
      "tx_kernel/deposit_transfer_withdraw/fst_deposit_message.out"

    (** deposit of second actor *)
    let snd_deposit =
      "tx_kernel/deposit_transfer_withdraw/snd_deposit_message.out"

    (** Invalid batch, as account 2 has not transferred the 'snd_ticket' to account 1
        before account 1 withdraws.

        operation 1:
        - actor 1 makes transfer
        - actor 1 withdraws fst ticket
        - actor 1 withdraws snd ticket
        operation 2:
        - actor 2 makes transfer
        - actor 2 withdraws fst ticket
        - actor 2 withdraws snd ticket
        *)
    let invalid_message =
      "tx_kernel/deposit_transfer_withdraw/invalid_external_message.out"

    (** Valid batch

        operation 1:
        - actor 1 makes transfer
        - actor 1 withdraws fst ticket
        operation 2:
        - actor 2 makes transfer
        - actor 2 withdraws snd ticket
        operation 3:
        - actor 1 withdraws snd ticket
        operation 4:
        - actor 2 withdraws fst ticket
        *)
    let valid_message =
      "tx_kernel/deposit_transfer_withdraw/valid_external_message.out"
  end

  (** large messages (4KB) *)
  module Large = struct
    (** lots of transfers, between to actors only *)
    let transfer_two_actors =
      "tx_kernel/deposit_transfer_withdraw/big_external_message.out"
  end

  (** taken from end 2 end test (trili/kernel) deposit_then_withdraw_to_same_address
      https://gitlab.com/trili/kernel/-/blob/303400a8a286d268d73debddee1390472315146c/e2e_kernel_tests/tests/tx_kernel.rs
      corresponds to first tests with tx_kernel_old
      *)
  module Old = struct
    let deposit = "tx_kernel/deposit_then_withdraw_to_same_address/deposit.out"

    let withdrawal =
      "tx_kernel/deposit_then_withdraw_to_same_address/withdrawal.out"
  end
end
