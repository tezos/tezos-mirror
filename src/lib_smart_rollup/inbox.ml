(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module Hash = Inbox_hash

module Skip_list_parameters = struct
  let basis = 4
end

module Skip_list = Skip_list.Make (Skip_list_parameters)

module V1 = struct
  type level_proof = {
    hash : Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.t;
    level : int32;
  }

  let level_proof_encoding =
    let open Data_encoding in
    conv
      (fun {hash; level} -> (hash, level))
      (fun (hash, level) -> {hash; level})
      (obj2
         (req
            "hash"
            Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash
            .encoding)
         (req "level" int32))

  let equal_level_proof {hash; level} level_proof_2 =
    Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.equal
      hash
      level_proof_2.hash
    && Int32.equal level level_proof_2.level

  type history_proof = (level_proof, Hash.t) Skip_list.cell

  let hash_history_proof cell =
    let {hash; level} = Skip_list.content cell in
    let back_pointers_hashes = Skip_list.back_pointers cell in
    Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.to_bytes
      hash
    :: (level |> Int32.to_string |> Bytes.of_string)
    :: List.map Hash.to_bytes back_pointers_hashes
    |> Hash.hash_bytes

  let equal_history_proof = Skip_list.equal Hash.equal equal_level_proof

  let history_proof_encoding : history_proof Data_encoding.t =
    Skip_list.encoding Hash.encoding level_proof_encoding

  let pp_level_proof fmt {hash; level} =
    Format.fprintf
      fmt
      "hash: %a@,level: %ld"
      Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.pp
      hash
      level

  let pp_history_proof fmt history_proof =
    (Skip_list.pp ~pp_content:pp_level_proof ~pp_ptr:Hash.pp) fmt history_proof

  (* An inbox is composed of a metadata of type {!t}, and a [level witness]
     representing the messages of the current level (held by the
     [Raw_context.t] in the protocol).

     The metadata contains :
     - [level] : the inbox level ;
     - [old_levels_messages] : a witness of the inbox history.
  *)
  type t = {level : int32; old_levels_messages : history_proof}

  let equal inbox1 inbox2 =
    (* To be robust to addition of fields in [t]. *)
    let {level; old_levels_messages} = inbox1 in
    Int32.equal level inbox2.level
    && equal_history_proof old_levels_messages inbox2.old_levels_messages

  let pp fmt {level; old_levels_messages} =
    Format.fprintf
      fmt
      "@[<hov 2>{ level = %ld@;old_levels_messages = %a@;}@]"
      level
      pp_history_proof
      old_levels_messages

  let hash inbox = hash_history_proof inbox.old_levels_messages

  let inbox_level inbox = inbox.level

  let old_levels_messages inbox = inbox.old_levels_messages

  let current_witness inbox =
    let {hash; _} = Skip_list.content inbox.old_levels_messages in
    hash

  let encoding =
    Data_encoding.(
      conv
        (fun {level; old_levels_messages} -> (level, old_levels_messages))
        (fun (level, old_levels_messages) -> {level; old_levels_messages})
        (obj2
           (req "level" int32)
           (req "old_levels_messages" history_proof_encoding)))
end

type versioned = V1 of V1.t

let versioned_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"smart_rollup_inbox.v1"
        (Tag 0)
        V1.encoding
        (function V1 inbox -> Some inbox)
        (fun inbox -> V1 inbox);
    ]

include V1

let of_versioned = function V1 inbox -> inbox [@@inline]

let to_versioned inbox = V1 inbox [@@inline]
