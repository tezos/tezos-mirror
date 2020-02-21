(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = bytes

let create = Bytes.create

let length = Bytes.length

let copy = Bytes.copy

let sub = Bytes.sub

let blit = Bytes.blit

let blit_of_string = Bytes.blit_string

let blit_to_bytes = Bytes.blit

let of_string = Bytes.of_string

let to_string = Bytes.to_string

let sub_string = Bytes.sub_string

let get_char = Bytes.get

let set_char = Bytes.set

include Tezos_stdlib.TzEndian

module LE = struct
  let get_uint16 = Bytes.get_uint16_le

  let get_int16 = Bytes.get_int16_le

  let get_int32 = Bytes.get_int32_le

  let get_int64 = Bytes.get_int64_le

  let set_int16 = Bytes.set_int16_le

  let set_int32 = Bytes.set_int32_le

  let set_int64 = Bytes.set_int64_le
end

let ( = ) = Stdlib.( = )

let ( <> ) = Stdlib.( <> )

let ( < ) = Stdlib.( < )

let ( <= ) = Stdlib.( <= )

let ( >= ) = Stdlib.( >= )

let ( > ) = Stdlib.( > )

let compare = Bytes.compare

let concat s bs = Bytes.concat (Bytes.of_string s) bs

let to_hex t = Hex.of_bytes t

let of_hex hex = Hex.to_bytes hex
