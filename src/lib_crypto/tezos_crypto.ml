(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Intfs = S

module Hashed = struct
  module Block_hash = Block_hash
  module Block_metadata_hash = Block_metadata_hash
  module Chain_id = Chain_id
  module Context_hash = Context_hash
  module Operation_hash = Operation_hash
  module Operation_list_hash = Operation_list_hash
  module Operation_list_list_hash = Operation_list_list_hash
  module Operation_metadata_hash = Operation_metadata_hash
  module Operation_metadata_list_hash = Operation_metadata_list_hash
  module Operation_metadata_list_list_hash = Operation_metadata_list_list_hash
  module Protocol_hash = Protocol_hash
  module Smart_rollup_address = Smart_rollup_address
  module Smart_rollup_commitment_hash = Smart_rollup_commitment_hash
  module Smart_rollup_state_hash = Smart_rollup_state_hash
  module Smart_rollup_inbox_hash = Smart_rollup_inbox_hash
  module Smart_rollup_merkelized_payload_hashes_hash =
    Smart_rollup_merkelized_payload_hashes_hash
end

module Signature = struct
  module Bls = Bls
  module Ed25519 = Ed25519
  module P256 = P256
  module Secp256k1 = Secp256k1
  include Signature
end

module Aggregate_signature = Aggregate_signature
module Base58 = Base58
module Blake2B = Blake2B
module Crypto_box = Crypto_box
module Hacl = Hacl
module Helpers = Helpers
module Rand = Rand
module Timelock_legacy = Timelock_legacy
module Timelock = Timelock
