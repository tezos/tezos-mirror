(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

module Hash = struct
  include Smart_rollup.Commitment_hash
  include Path_encoding.Make_hex (Smart_rollup.Commitment_hash)
end

module V1 = struct
  type t = {
    compressed_state : State_hash.t;
    inbox_level : Raw_level_repr.t;
    predecessor : Hash.t;
    number_of_ticks : Number_of_ticks.t;
  }

  let pp fmt {compressed_state; inbox_level; predecessor; number_of_ticks} =
    Format.fprintf
      fmt
      "compressed_state: %a@,\
       inbox_level: %a@,\
       predecessor: %a@,\
       number_of_ticks: %Ld"
      State_hash.pp
      compressed_state
      Raw_level_repr.pp
      inbox_level
      Hash.pp
      predecessor
      (Number_of_ticks.to_value number_of_ticks)

  let encoding =
    let open Data_encoding in
    conv
      (fun {compressed_state; inbox_level; predecessor; number_of_ticks} ->
        (compressed_state, inbox_level, predecessor, number_of_ticks))
      (fun (compressed_state, inbox_level, predecessor, number_of_ticks) ->
        {compressed_state; inbox_level; predecessor; number_of_ticks})
      (obj4
         (req "compressed_state" State_hash.encoding)
         (req "inbox_level" Raw_level_repr.encoding)
         (req "predecessor" Hash.encoding)
         (req "number_of_ticks" Number_of_ticks.encoding))

  let hash_uncarbonated commitment =
    let commitment_bytes =
      Data_encoding.Binary.to_bytes_exn encoding commitment
    in
    Hash.hash_bytes [commitment_bytes]

  (* For [number_of_messages] and [number_of_ticks] min_value is equal to zero. *)
  let genesis_commitment ~origination_level ~genesis_state_hash =
    let open Sc_rollup_repr in
    let number_of_ticks = Number_of_ticks.zero in
    {
      compressed_state = genesis_state_hash;
      inbox_level = origination_level;
      predecessor = Hash.zero;
      number_of_ticks;
    }

  type genesis_info = {level : Raw_level_repr.t; commitment_hash : Hash.t}

  let genesis_info_encoding =
    let open Data_encoding in
    conv
      (fun {level; commitment_hash} -> (level, commitment_hash))
      (fun (level, commitment_hash) -> {level; commitment_hash})
      (obj2
         (req "level" Raw_level_repr.encoding)
         (req "commitment_hash" Hash.encoding))
end

type versioned = V1 of V1.t

let versioned_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"V1"
        (Tag 0)
        V1.encoding
        (function V1 commitment -> Some commitment)
        (fun commitment -> V1 commitment);
    ]

include V1

let of_versioned = function V1 commitment -> commitment [@@inline]

let to_versioned commitment = V1 commitment [@@inline]
