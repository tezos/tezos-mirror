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

module El = struct
  type t = Tx_rollup_message_hash_repr.t

  let to_bytes =
    Data_encoding.Binary.to_bytes_exn Tx_rollup_message_hash_repr.encoding
end

module Prefix = struct
  let name = "Inbox_list_hash"

  let title = "A merkle root hash for inboxes"

  let b58check_prefix = Tx_rollup_prefixes.inbox_list_hash.b58check_prefix

  let size = Some Tx_rollup_prefixes.inbox_list_hash.hash_size
end

module H = Blake2B.Make (Base58) (Prefix)
module Merkle_list = Merkle_list.Make (El) (H)

module Merkle = struct
  type tree = Merkle_list.t

  type root = Merkle_list.h

  type path = Merkle_list.path

  let empty = Merkle_list.nil

  let root = Merkle_list.root

  let ( = ) = H.( = )

  let compare = H.compare

  let root_encoding = H.encoding

  let root_of_b58check_opt = H.of_b58check_opt

  let pp_root = H.pp

  let path_encoding = Merkle_list.path_encoding

  let add_message = Merkle_list.snoc

  let tree_of_messages = List.fold_left Merkle_list.snoc Merkle_list.nil

  let compute_path messages position =
    let tree = tree_of_messages messages in
    Merkle_list.compute_path tree position

  let check_path = Merkle_list.check_path

  let path_depth = Merkle_list.path_depth

  let merklize_list messages =
    let tree = tree_of_messages messages in
    root tree
end

type t = {inbox_length : int; cumulated_size : int; merkle_root : Merkle.root}

let ( = )
    {
      inbox_length = inbox_length_left;
      cumulated_size = cumulated_size_left;
      merkle_root = merkle_root_left;
    }
    {
      inbox_length = inbox_length_right;
      cumulated_size = cumulated_size_right;
      merkle_root = merkle_root_right;
    } =
  Compare.Int.(inbox_length_left = inbox_length_right)
  && Compare.Int.(cumulated_size_left = cumulated_size_right)
  && Merkle.(merkle_root_left = merkle_root_right)

let encoding =
  let open Data_encoding in
  conv
    (fun {inbox_length; cumulated_size; merkle_root} ->
      (inbox_length, cumulated_size, merkle_root))
    (fun (inbox_length, cumulated_size, merkle_root) ->
      {inbox_length; cumulated_size; merkle_root})
    (obj3
       (req "inbox_length" int31)
       (req "cumulated_size" int31)
       (req "merkle_root" Merkle.root_encoding))

let empty =
  {inbox_length = 0; cumulated_size = 0; merkle_root = Merkle_list.empty}

let size = Z.of_int @@ Data_encoding.Binary.length encoding empty

let pp fmt {inbox_length; cumulated_size; merkle_root} =
  Format.fprintf
    fmt
    "Inbox with length %d, size %d, merkle root %a"
    inbox_length
    cumulated_size
    Merkle.pp_root
    merkle_root
