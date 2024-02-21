(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
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

type path = string

let evm rst = sf "/evm%s" rst

let kernel rst = sf "/kernel%s" rst

let eth_accounts = evm "/eth_accounts"

let eth_account addr = sf "%s/%s" eth_accounts (Helpers.normalize addr)

let balance addr = sf "%s/balance" (eth_account addr)

let code addr = sf "%s/code" (eth_account addr)

let storage addr ?key () =
  sf
    "%s/storage%s"
    (eth_account addr)
    (match key with None -> "" | Some key -> "/" ^ key)

let admin = evm "/admin"

let kernel_governance = evm "/kernel_governance"

let sequencer_admin = evm "/sequencer_admin"

let ticketer = evm "/ticketer"

let sequencer = evm "/sequencer"

let kernel_boot_wasm = kernel "/boot.wasm"

let delayed_bridge_path = evm "/delayed_bridge"

let da_fee_per_byte_path = evm "/fees/da_fee_per_byte"

let minimum_base_fee_per_gas = evm "/fees/minimum_base_fee_per_gas"

let delayed_inbox_timeout = evm "/delayed_inbox_timeout"

let delayed_inbox_min_levels = evm "/delayed_inbox_min_levels"

let config_root_hash = "/__tmp/config_root_hash"
