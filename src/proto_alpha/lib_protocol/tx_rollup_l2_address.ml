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

let address_size = 20

include
  Blake2B.Make
    (Base58)
    (struct
      let name = "Tx_rollup_l2_address"

      let title =
        "The hash of a BLS public key used to identify a L2 ticket holders"

      let b58check_prefix = "\001\127\181\224" (* tru2(37) *)

      let size = Some address_size
    end)

include Compare.Make (struct
  type nonrec t = t

  let compare = compare
end)

type address = t

let () = Base58.check_encoded_prefix b58check_encoding "tru2" 37

let of_bls_pk : Bls_signature.pk -> t =
 fun pk -> hash_bytes [Bls_signature.pk_to_bytes pk]

let in_memory_size : t -> Cache_memory_helpers.sint =
 fun _ ->
  let open Cache_memory_helpers in
  header_size +! word_size +! string_size_gen address_size

let size _ = address_size

module Indexable = struct
  include Indexable.Make (struct
    type nonrec t = t

    let encoding = encoding

    let compare = compare

    let pp = pp
  end)

  let in_memory_size x = Indexable.in_memory_size in_memory_size x

  let size x = Indexable.size size x
end
