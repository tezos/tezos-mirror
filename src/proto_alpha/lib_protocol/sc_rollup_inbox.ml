(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* 32 *)
let inbox_hash = "\003\250\174\238\208" (* scib1(55) *)

module Inbox_hash = struct
  let prefix = "scib1"

  let encoded_size = 55

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "inbox_hash"

        let title = "The hash of a the inbox of a smart contract rollup"

        let b58check_prefix = inbox_hash

        (* defaults to 32 *)
        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)
end

type hash = Inbox_hash.t

type t = {hash : hash; inbox_size : int64}

let pp_hash = Inbox_hash.pp

let pp fmtr {hash; inbox_size} =
  Format.fprintf
    fmtr
    "@[<v 2>{ hash = %a;@,inbox_size = %Ld }@]"
    pp_hash
    hash
    inbox_size

let hash_encoding = Inbox_hash.encoding

let encoding =
  let open Data_encoding in
  conv
    (fun {hash; inbox_size} -> (hash, inbox_size))
    (fun (hash, inbox_size) -> {hash; inbox_size})
    (obj2 (req "hash" hash_encoding) (req "inbox_size" int64))

let message_encoding =
  let open Data_encoding in
  obj3
    (req "parent" hash_encoding)
    (req "payload" string)
    (req "level" Raw_level_repr.encoding)

let number_of_available_messages {inbox_size; hash = _} = Z.of_int64 inbox_size

let empty = {hash = Inbox_hash.zero; inbox_size = 0L}

let add_message {hash; inbox_size} message level =
  let message_bytes =
    Data_encoding.Binary.to_bytes_exn message_encoding (hash, message, level)
  in
  let hash = Inbox_hash.hash_bytes [message_bytes] in
  {hash; inbox_size = Int64.succ inbox_size}

let add_messages messages level inbox =
  List.fold_left
    (fun inbox message -> add_message inbox message level)
    inbox
    messages

let consume_n_messages n {hash; inbox_size} =
  if Compare.Int.(n < 0) then None
  else if Compare.Int64.(Int64.of_int n > inbox_size) then None
  else Some {hash; inbox_size = Int64.sub inbox_size (Int64.of_int n)}
