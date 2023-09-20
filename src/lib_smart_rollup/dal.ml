(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module Slot_index = struct
  type t = int

  let encoding = Data_encoding.uint8
end

module Page_index = struct
  type t = int

  let encoding = Data_encoding.int16
end

module Commitment = struct
  include Tezos_crypto_dal.Cryptobox.Verifier.Commitment

  type t = Tezos_crypto_dal.Cryptobox.Verifier.commitment
end

module Slot_header = struct
  module V1 = struct
    type id = {published_level : int32; index : Slot_index.t}

    type t = {id : id; commitment : Commitment.t}

    let id_encoding =
      let open Data_encoding in
      conv
        (fun {published_level; index} -> (published_level, index))
        (fun (published_level, index) -> {published_level; index})
        (obj2 (req "level" int32) (req "index" Slot_index.encoding))

    let encoding =
      let open Data_encoding in
      conv
        (fun {id; commitment} -> (id, commitment))
        (fun (id, commitment) -> {id; commitment})
        (merge_objs id_encoding (obj1 (req "commitment" Commitment.encoding)))
  end

  type versioned = V1 of V1.t

  let versioned_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"smart_rollup_dal_slot_header.v1"
          (Tag 0)
          V1.encoding
          (function V1 header -> Some header)
          (fun header -> V1 header);
      ]

  include V1

  let of_versioned = function V1 header -> header [@@inline]

  let to_versioned header = V1 header [@@inline]
end

module Slot_history = struct
  module V1 = struct
    (* Serialized representation for protocol agnostic slot history *)
    type t = bytes

    let encoding = Data_encoding.Variable.bytes
  end

  type versioned = V1 of V1.t

  let versioned_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"smart_rollup_slot_history.v1"
          (Tag 0)
          V1.encoding
          (function V1 x -> Some x)
          (fun x -> V1 x);
      ]

  include V1

  let of_versioned = function V1 x -> x [@@inline]

  let to_versioned x = V1 x [@@inline]
end

module Slot_history_cache = struct
  module V1 = struct
    (* Serialized representation for protocol agnostic slot history cache *)
    type t = bytes

    let encoding = Data_encoding.Variable.bytes
  end

  type versioned = V1 of V1.t

  let versioned_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"smart_rollup_slot_history_cache.v1"
          (Tag 0)
          V1.encoding
          (function V1 x -> Some x)
          (fun x -> V1 x);
      ]

  include V1

  let of_versioned = function V1 x -> x [@@inline]

  let to_versioned x = V1 x [@@inline]
end
