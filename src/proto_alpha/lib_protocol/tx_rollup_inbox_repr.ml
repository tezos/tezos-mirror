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

let hash_size = Tx_rollup_prefixes.inbox_hash.hash_size

module Inbox_hash =
  Blake2B.Make
    (Base58)
    (struct
      let name = "Tx_rollup_inbox_hash"

      let title = "The hash of a transaction rollup inbox"

      let b58check_prefix = Tx_rollup_prefixes.inbox_hash.b58check_prefix

      let size = Some hash_size
    end)

let () =
  Tx_rollup_prefixes.(check_encoding inbox_hash Inbox_hash.b58check_encoding)

type hash = Inbox_hash.t

let compare_hash = Inbox_hash.compare

let equal_hash = Inbox_hash.equal

let pp_hash = Inbox_hash.pp

let hash_of_bytes_exn = Inbox_hash.of_bytes_exn

let hash_of_bytes_opt = Inbox_hash.of_bytes_opt

let hash_of_b58check_exn = Inbox_hash.of_b58check_exn

let hash_of_b58check_opt = Inbox_hash.of_b58check_opt

let hash_encoding = Inbox_hash.encoding

let hash_to_bytes = Inbox_hash.to_bytes

let hash_to_b58check = Inbox_hash.to_b58check

let extend_hash inbox_hash msg_hash =
  let open Data_encoding.Binary in
  let inbox_hash = to_bytes_exn hash_encoding inbox_hash in
  let msg_hash = to_bytes_exn Tx_rollup_message_repr.hash_encoding msg_hash in
  Inbox_hash.hash_bytes [inbox_hash; msg_hash]

let hash_hashed_inbox message_hashes =
  List.fold_left
    extend_hash
    (Inbox_hash.hash_bytes [Bytes.make hash_size @@ Char.chr 0])
    message_hashes

let hash_inbox messages =
  let message_hashes =
    List.map Tx_rollup_message_repr.hash_uncarbonated messages
  in
  hash_hashed_inbox message_hashes

type t = {
  contents : Tx_rollup_message_repr.hash list;
  cumulated_size : int;
  hash : hash;
}

let pp fmt {contents; cumulated_size; hash} =
  Format.fprintf
    fmt
    "tx rollup inbox: %d messages using %d bytes with hash %a"
    (List.length contents)
    cumulated_size
    pp_hash
    hash

let encoding =
  let open Data_encoding in
  conv
    (fun {contents; cumulated_size; hash} -> (contents, cumulated_size, hash))
    (fun (contents, cumulated_size, hash) -> {contents; cumulated_size; hash})
    (obj3
       (req "contents" @@ list Tx_rollup_message_repr.hash_encoding)
       (req "cumulated_size" int31)
       (req "hash" hash_encoding))

type metadata = {inbox_length : int32; cumulated_size : int; hash : hash}

let metadata_encoding =
  let open Data_encoding in
  conv
    (fun {inbox_length; cumulated_size; hash} ->
      (inbox_length, cumulated_size, hash))
    (fun (inbox_length, cumulated_size, hash) ->
      {inbox_length; cumulated_size; hash})
    (obj3
       (req "inbox_length" int32)
       (req "cumulated_size" int31)
       (req "hash" hash_encoding))

let empty_metadata =
  {
    inbox_length = 0l;
    cumulated_size = 0;
    hash = Inbox_hash.hash_bytes [Bytes.make hash_size @@ Char.chr 0];
  }
