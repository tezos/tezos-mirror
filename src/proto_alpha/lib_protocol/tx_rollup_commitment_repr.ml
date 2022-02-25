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

module Commitment_hash = struct
  let commitment_hash = "\017\249\195\013" (* toc1(54) *)

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Commitment_hash"

        let title = "A commitment ID"

        let b58check_prefix = commitment_hash

        let size = Some 32
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding "toc1" 54

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

type batch_commitment = {
  (* TODO: add effects and replace bytes with Irmin:
     https://gitlab.com/tezos/tezos/-/issues/2444
  *)
  root : bytes;
}

module Batch = struct
  type t = batch_commitment

  let encoding =
    Data_encoding.(
      conv (fun {root} -> root) (fun root -> {root}) (obj1 (req "root" bytes)))

  let pp : Format.formatter -> t -> unit =
   fun fmt {root} -> Hex.pp fmt (Hex.of_bytes root)

  include Compare.Make (struct
    type nonrec t = t

    let compare {root = root1} {root = root2} = Bytes.compare root1 root2
  end)
end

let batch_commitment_equal : batch_commitment -> batch_commitment -> bool =
  Batch.equal

type t = {
  level : Tx_rollup_level_repr.t;
  batches : batch_commitment list;
  predecessor : Commitment_hash.t option;
  inbox_hash : Tx_rollup_inbox_repr.hash;
}

let compare_or cmp c1 c2 f = match cmp c1 c2 with 0 -> f () | diff -> diff

include Compare.Make (struct
  type nonrec t = t

  module Compare_root_list = Compare.List (Batch)

  let compare r1 r2 =
    compare_or Tx_rollup_level_repr.compare r1.level r2.level (fun () ->
        compare_or Compare_root_list.compare r1.batches r2.batches (fun () ->
            compare_or
              (Option.compare Commitment_hash.compare)
              r1.predecessor
              r2.predecessor
              (fun () ->
                Tx_rollup_inbox_repr.compare_hash r1.inbox_hash r2.inbox_hash)))
end)

let pp : Format.formatter -> t -> unit =
 fun fmt t ->
  Format.fprintf
    fmt
    "commitment %a : batches = %a predecessor %a for inbox %a"
    Tx_rollup_level_repr.pp
    t.level
    (Format.pp_print_list Batch.pp)
    t.batches
    (Format.pp_print_option Commitment_hash.pp)
    t.predecessor
    Tx_rollup_inbox_repr.pp_hash
    t.inbox_hash

(* FIXME/TORU: https://gitlab.com/tezos/tezos/-/issues/2470

   This encoding is not bounded, and maybe it is an issue. *)
let encoding =
  let open Data_encoding in
  conv
    (fun {level; batches; predecessor; inbox_hash} ->
      (level, batches, predecessor, inbox_hash))
    (fun (level, batches, predecessor, inbox_hash) ->
      {level; batches; predecessor; inbox_hash})
    (obj4
       (req "level" Tx_rollup_level_repr.encoding)
       (req "batches" (list Batch.encoding))
       (req "predecessor" (option Commitment_hash.encoding))
       (req "inbox_hash" Tx_rollup_inbox_repr.hash_encoding))

let hash c =
  let bytes = Data_encoding.Binary.to_bytes_exn encoding c in
  Commitment_hash.hash_bytes [bytes]

module Index = struct
  type t = Commitment_hash.t

  let path_length = 1

  let to_path c l =
    let raw_key =
      Data_encoding.Binary.to_bytes_exn Commitment_hash.encoding c
    in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt Commitment_hash.encoding)
    | _ -> None

  let rpc_arg = Commitment_hash.rpc_arg

  let encoding = Commitment_hash.encoding

  let compare = Commitment_hash.compare
end

module Submitted_commitment = struct
  type nonrec t = {
    commitment : t;
    commitment_hash : Commitment_hash.t;
    committer : Signature.Public_key_hash.t;
    submitted_at : Raw_level_repr.t;
    finalized_at : Raw_level_repr.t option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {commitment; commitment_hash; committer; submitted_at; finalized_at} ->
        (commitment, commitment_hash, committer, submitted_at, finalized_at))
      (fun (commitment, commitment_hash, committer, submitted_at, finalized_at) ->
        {commitment; commitment_hash; committer; submitted_at; finalized_at})
      (obj5
         (req "commitment" encoding)
         (req "commitment_hash" Commitment_hash.encoding)
         (req "committer" Signature.Public_key_hash.encoding)
         (req "submitted_at" Raw_level_repr.encoding)
         (opt "finalized_at" Raw_level_repr.encoding))
end
