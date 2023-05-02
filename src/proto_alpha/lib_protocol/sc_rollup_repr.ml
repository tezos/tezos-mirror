(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
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

module Address = struct
  include Smart_rollup.Address

  let prefix = "sr1"

  let () = Base58.check_encoded_prefix b58check_encoding prefix 36

  let of_b58data = function Smart_rollup.Address.Data h -> Some h | _ -> None
end

module Internal_for_tests = struct
  let originated_sc_rollup nonce =
    let data =
      Data_encoding.Binary.to_bytes_exn Origination_nonce.encoding nonce
    in
    Address.hash_bytes [data]
end

(* 32 *)
let state_hash_prefix = "\017\165\235\240" (* srs1(54) *)

module State_hash = struct
  let prefix = "srs1"

  let encoded_size = 54

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Smart_rollup_state_hash"

        let title = "The hash of the VM state of a smart rollup"

        let b58check_prefix = state_hash_prefix

        (* defaults to 32 *)
        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)

  let context_hash_to_state_hash =
    (* Both State_hash and Context_hash's hashes are supposed to have the
       same size. This top-level check enforces this invariant, in which case,
       no exception could be thrown by [of_bytes_exn] below *)
    let () = assert (Compare.Int.equal size Context_hash.size) in
    fun h -> of_bytes_exn @@ Context_hash.to_bytes h

  (* Hackish way to disable hash_bytes and hash_string to force people to use
     context_hash_to_state_hash (without changing content of HASH.S) *)
  type unreachable = |

  let hash_bytes = function (_ : unreachable) -> .

  let hash_string = function (_ : unreachable) -> .
end

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5506
   Remove type and module aliases for Smart_rollup.Address. *)

type t = Address.t

let pp = Address.pp

let encoding = Address.encoding

let rpc_arg = Address.rpc_arg

let in_memory_size (_ : t) =
  let open Cache_memory_helpers in
  h1w +! string_size_gen Address.size

module Staker = struct
  include Signature.Public_key_hash

  let rpc_arg_staker1 =
    RPC_arg.like rpc_arg ?descr:(RPC_arg.descr rpc_arg).descr "staker1_pkh"

  let rpc_arg_staker2 =
    RPC_arg.like rpc_arg ?descr:(RPC_arg.descr rpc_arg).descr "staker2_pkh"
end

module Index = struct
  type t = Address.t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt encoding)
    | _ -> None

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = Address.compare
end

module Number_of_ticks = struct
  include Bounded.Int64 (struct
    let min_value = 0L

    let max_value = Int64.max_int
  end)

  let zero =
    match of_value 0L with
    | Some zero -> zero
    | None -> assert false (* unreachable case, since [min_int = 0l] *)
end
