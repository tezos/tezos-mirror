(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include
  Blake2B.Make
    (Base58)
    (struct
      let name = "Context_hash"

      let title = "A hash of context"

      let b58check_prefix = Base58.Prefix.context_hash

      let size = None
    end)

let () = Base58.check_encoded_prefix b58check_encoding "Co" 52

module Version = struct
  include Compare.Int

  let pp = Format.pp_print_int

  let encoding =
    let open Data_encoding in
    def
      "context_hash_version"
      ~description:"A version number for the context hash computation"
      uint16

  let of_int v =
    if 0 <= v && v <= 0xffff then v
    else
      Format.kasprintf
        Stdlib.invalid_arg
        "Context_hash.Version.of_int: hash version must be uint16 (got %d)"
        v

  let () = Data_encoding.Registration.register ~pp encoding
end

type version = Version.t
