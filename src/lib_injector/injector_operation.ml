(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Injector_sigs

module Make (O : PARAM_OPERATION) :
  INJECTOR_OPERATION with type operation = O.t = struct
  module Hash =
    Tezos_crypto.Blake2B.Make
      (Tezos_crypto.Base58)
      (struct
        let name = "injector_operation_hash"

        let title = "An identifier (hash) for an operation in the injector"

        let b58check_prefix = "\064\007\206" (* iop(53) *)

        let size = None
      end)

  let () =
    Tezos_crypto.Base58.check_encoded_prefix Hash.b58check_encoding "iop" 53

  type operation = O.t

  type hash = Hash.t

  type t = {hash : hash; operation : O.t}

  let hash_inner_operation op =
    Hash.hash_bytes [Data_encoding.Binary.to_bytes_exn O.encoding op]

  let make operation =
    let hash = hash_inner_operation operation in
    {hash; operation}

  let encoding =
    let open Data_encoding in
    conv
      (fun {hash; operation} -> (hash, operation))
      (fun (hash, operation) -> {hash; operation})
    @@ obj2 (req "hash" Hash.encoding) (req "operation" O.encoding)

  let pp ppf {hash; operation} =
    Format.fprintf ppf "%a (%a)" O.pp operation Hash.pp hash
end
