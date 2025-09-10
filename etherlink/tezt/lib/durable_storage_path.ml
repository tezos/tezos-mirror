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

let no_0x s =
  if String.starts_with ~prefix:"0x" s then String.sub s 2 (String.length s - 2)
  else s

let normalize s = String.lowercase_ascii @@ no_0x s

let evm rst = sf "/evm%s" rst

let world_state rst = evm (sf "/world_state%s" rst)

let delayed_inbox = evm "/delayed-inbox"

let kernel rst = sf "/kernel%s" rst

let kernel_root_hash = evm "/kernel_root_hash"

let indexes = world_state "/indexes"

let eth_accounts = world_state "/eth_accounts"

let eth_account addr = sf "%s/%s" eth_accounts (normalize addr)

let balance addr = sf "%s/balance" (eth_account addr)

let nonce addr = sf "%s/nonce" (eth_account addr)

let code addr = sf "%s/code" (eth_account addr)

let storage addr ?key () =
  sf
    "%s/storage%s"
    (eth_account addr)
    (match key with None -> "" | Some key -> "/" ^ key)

let admin = evm "/admin"

let kernel_governance = evm "/kernel_governance"

let kernel_security_governance = evm "/kernel_security_governance"

let sequencer_governance = evm "/sequencer_governance"

let ticketer = evm "/ticketer"

let sequencer = evm "/sequencer"

let sequencer_pool_address = evm "/sequencer_pool_address"

let kernel_boot_wasm = kernel "/boot.wasm"

let delayed_bridge_path = evm "/delayed_bridge"

let da_fee_per_byte_path = world_state "/fees/da_fee_per_byte"

let minimum_base_fee_per_gas = world_state "/fees/minimum_base_fee_per_gas"

let delayed_inbox_timeout = evm "/delayed_inbox_timeout"

let delayed_inbox_min_levels = evm "/delayed_inbox_min_levels"

let reveal_config = "/__tmp/reveal_config"

let enable_fa_bridge = evm "/feature_flags/enable_fa_bridge"

let enable_multichain = evm "/feature_flags/enable_multichain"

let enable_fast_withdrawal =
  evm "/world_state/feature_flags/enable_fast_withdrawal"

let storage_version = evm "/storage_version"

module Ticket_table = struct
  let ticket_table =
    sf
      "%s/ticket_table"
      (eth_account "0x0000000000000000000000000000000000000000")

  let balance ~ticket_hash ~account =
    String.concat "/" [ticket_table; ticket_hash; account]
end

module Ghostnet = struct
  let eth_accounts = evm "/eth_accounts"

  let eth_account addr = sf "%s/%s" eth_accounts (normalize addr)

  let balance addr = sf "%s/balance" (eth_account addr)

  let code addr = sf "%s/code" (eth_account addr)

  let storage addr ?key () =
    sf
      "%s/storage%s"
      (eth_account addr)
      (match key with None -> "" | Some key -> "/" ^ key)

  let da_fee_per_byte_path = evm "/fees/da_fee_per_byte"

  let minimum_base_fee_per_gas = evm "/fees/minimum_base_fee_per_gas"
end
