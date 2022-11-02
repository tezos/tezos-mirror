(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

module Hash = struct
  let commitment_hash = Tx_rollup_prefixes.commitment_hash.b58check_prefix

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Commitment_hash"

        let title = "A commitment ID"

        let b58check_prefix = commitment_hash

        let size = Some Tx_rollup_prefixes.commitment_hash.hash_size
      end)

  include H

  let () = Tx_rollup_prefixes.(check_encoding commitment_hash b58check_encoding)

  include Path_encoding.Make_hex (H)

  let rpc_arg =
    let construct = Data_encoding.Binary.to_string_exn encoding in
    let destruct str =
      Option.value_e ~error:"Failed to decode commitment"
      @@ Data_encoding.Binary.of_string_opt encoding str
    in
    RPC_arg.make
      ~descr:"A tx_rollup commitment."
      ~name:"tx_rollup_commitment"
      ~construct
      ~destruct
      ()
end

module Merkle_hash = struct
  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Message_result_list_hash"

        let title = "A merklised message result list hash"

        let b58check_prefix =
          Tx_rollup_prefixes.message_result_list_hash.b58check_prefix

        let size = Some Tx_rollup_prefixes.message_result_list_hash.hash_size
      end)

  include H
  include Path_encoding.Make_hex (H)

  let () =
    Tx_rollup_prefixes.(
      check_encoding message_result_list_hash b58check_encoding)
end

module Merkle =
  Merkle_list.Make (Tx_rollup_message_result_hash_repr) (Merkle_hash)

type 'a template = {
  level : Tx_rollup_level_repr.t;
  messages : 'a;
  predecessor : Hash.t option;
  inbox_merkle_root : Tx_rollup_inbox_repr.Merkle.root;
}

let map_template f x = {x with messages = f x.messages}

let pp_template :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a template -> unit
    =
 fun pp_messages fmt t ->
  Format.fprintf
    fmt
    "Level: %a@,Messages: %a@,Predecessor: %a@,Inbox merkle root: %a"
    Tx_rollup_level_repr.pp
    t.level
    pp_messages
    t.messages
    (Format.pp_print_option
       ~none:(fun fmt () -> Format.pp_print_string fmt "None")
       Hash.pp)
    t.predecessor
    Tx_rollup_inbox_repr.Merkle.pp_root
    t.inbox_merkle_root

let encoding_template encoding =
  let open Data_encoding in
  conv
    (fun {level; messages; predecessor; inbox_merkle_root} ->
      (level, messages, predecessor, inbox_merkle_root))
    (fun (level, messages, predecessor, inbox_merkle_root) ->
      {level; messages; predecessor; inbox_merkle_root})
    (obj4
       (req "level" Tx_rollup_level_repr.encoding)
       (req "messages" encoding)
       (req "predecessor" (option Hash.encoding))
       (req "inbox_merkle_root" Tx_rollup_inbox_repr.Merkle.root_encoding))

module Compact = struct
  type excerpt = {
    count : int;
    root : Merkle.h;
    last_result_message_hash : Tx_rollup_message_result_hash_repr.t;
  }

  type t = excerpt template

  let pp =
    pp_template (fun fmt {count; root; last_result_message_hash} ->
        Format.fprintf
          fmt
          "Count: %d@, Merkle root hash: %a@,Last result message hash: %a"
          count
          Merkle_hash.pp
          root
          Tx_rollup_message_result_hash_repr.pp
          last_result_message_hash)

  let encoding =
    encoding_template
      Data_encoding.(
        conv
          (fun {count; root; last_result_message_hash} ->
            (count, root, last_result_message_hash))
          (fun (count, root, last_result_message_hash) ->
            {count; root; last_result_message_hash})
        @@ obj3
             (req "count" int31)
             (req "root" Merkle_hash.encoding)
             (req
                "last_message_result_hash"
                Tx_rollup_message_result_hash_repr.encoding))

  let hash t =
    let bytes = Data_encoding.Binary.to_bytes_exn encoding t in
    Hash.hash_bytes [bytes]
end

module Full = struct
  type t = Tx_rollup_message_result_hash_repr.t list template

  let pp =
    pp_template (Format.pp_print_list Tx_rollup_message_result_hash_repr.pp)

  let encoding : t Data_encoding.t =
    encoding_template
      (Data_encoding.list Tx_rollup_message_result_hash_repr.encoding)

  let compact full =
    map_template
      (fun list ->
        let root = Merkle.compute list in
        List.fold_left
          (fun (acc, _) m -> (acc + 1, m))
          (0, Tx_rollup_message_result_hash_repr.zero)
          list
        |> fun (count, last_result_message_hash) ->
        Compact.{count; root; last_result_message_hash})
      full
end

module Index = struct
  type t = Hash.t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn Hash.encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt Hash.encoding)
    | _ -> None

  let rpc_arg = Hash.rpc_arg

  let encoding = Hash.encoding

  let compare = Hash.compare
end

module Submitted_commitment = struct
  type nonrec t = {
    commitment : Compact.t;
    commitment_hash : Hash.t;
    committer : Signature.Public_key_hash.t;
    submitted_at : Raw_level_repr.t;
    finalized_at : Raw_level_repr.t option;
  }

  let encoding =
    let compact = Compact.encoding in
    let open Data_encoding in
    conv
      (fun {commitment; commitment_hash; committer; submitted_at; finalized_at} ->
        (commitment, commitment_hash, committer, submitted_at, finalized_at))
      (fun (commitment, commitment_hash, committer, submitted_at, finalized_at) ->
        {commitment; commitment_hash; committer; submitted_at; finalized_at})
      (obj5
         (req "commitment" compact)
         (req "commitment_hash" Hash.encoding)
         (req "committer" Signature.Public_key_hash.encoding)
         (req "submitted_at" Raw_level_repr.encoding)
         (opt "finalized_at" Raw_level_repr.encoding))
end
