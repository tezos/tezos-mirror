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

type error += (* `Branch *) Commitment_hash_already_submitted

type error += (* `Branch *) Two_commitments_from_one_committer

type error += (* `Branch *) Wrong_commitment_predecessor_level

type error += (* `Temporary *) Missing_commitment_predecessor

type error += (* `Branch *) Wrong_batch_count

type error += (* `Temporary *) Commitment_too_early

let () =
  let open Data_encoding in
  (* Commitment_hash_already_submitted *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_commit_hash_already_submitted"
    ~title:"Someone already made this commitment"
    ~description:"The requested commitment is a duplicate"
    unit
    (function Commitment_hash_already_submitted -> Some () | _ -> None)
    (fun () -> Commitment_hash_already_submitted) ;
  (* Two_commitments_from_one_committer *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_two_commitments_from_one_committer"
    ~title:"This contract already made a different commitment at this level"
    ~description:
      "This contract already made a different commitment at this level"
    unit
    (function Two_commitments_from_one_committer -> Some () | _ -> None)
    (fun () -> Two_commitments_from_one_committer) ;
  (* Wrong_commitment_predecessor_level *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_wrong_commitment_predecessor_level"
    ~title:"This commitment's predecessor is invalid"
    ~description:
      "This commitment has predecessor but shouldn't, or doesn't but should"
    unit
    (function Wrong_commitment_predecessor_level -> Some () | _ -> None)
    (fun () -> Wrong_commitment_predecessor_level) ;
  (* Missing_commitment_predecessor *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_missing_commitment_predecessor"
    ~title:"This commitment refers to a predecessor that doesn't exist"
    ~description:"This commitment refers to a predecessor that doesn't exist"
    unit
    (function Missing_commitment_predecessor -> Some () | _ -> None)
    (fun () -> Missing_commitment_predecessor) ;
  (* Wrong_batch_count *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_wrong_batch_count"
    ~title:"This commitment has the wrong number of batches"
    ~description:
      "This commitment has a different number of batches than its inbox"
    unit
    (function Wrong_batch_count -> Some () | _ -> None)
    (fun () -> Wrong_batch_count) ;
  (* Commitment_too_early *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_commitment_too_early"
    ~title:"This commitment is for a level that hasn't finished yet"
    ~description:"This commitment is for a level that hasn't finished yet"
    unit
    (function Commitment_too_early -> Some () | _ -> None)
    (fun () -> Commitment_too_early)

let compare_or cmp c1 c2 f = match cmp c1 c2 with 0 -> f () | diff -> diff

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

module Commitment = struct
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
    level : Raw_level_repr.t;
    batches : batch_commitment list;
    predecessor : Commitment_hash.t option;
  }

  include Compare.Make (struct
    type nonrec t = t

    module Compare_root_list = Compare.List (Batch)

    let compare r1 r2 =
      compare_or Raw_level_repr.compare r1.level r2.level (fun () ->
          compare_or Compare_root_list.compare r1.batches r2.batches (fun () ->
              Option.compare
                Commitment_hash.compare
                r1.predecessor
                r2.predecessor))
  end)

  let pp : Format.formatter -> t -> unit =
   fun fmt t ->
    Format.fprintf
      fmt
      "commitment %a : batches = %a predecessor %a"
      Raw_level_repr.pp
      t.level
      (Format.pp_print_list Batch.pp)
      t.batches
      (Format.pp_print_option Commitment_hash.pp)
      t.predecessor

  (* FIXME/TORU: https://gitlab.com/tezos/tezos/-/issues/2470

     This encoding is not bounded, and maybe it is an issue. *)
  let encoding =
    let open Data_encoding in
    conv
      (fun {level; batches; predecessor} -> (level, batches, predecessor))
      (fun (level, batches, predecessor) -> {level; batches; predecessor})
      (obj3
         (req "level" Raw_level_repr.encoding)
         (req "batches" (list Batch.encoding))
         (req "predecessor" (option Commitment_hash.encoding)))

  let hash t =
    let to_bytes_exn = Data_encoding.Binary.to_bytes_exn in
    let level_bytes = to_bytes_exn Raw_level_repr.encoding t.level in
    let predecessor_bytes =
      Option.fold
        ~none:Bytes.empty
        ~some:(fun pred -> Commitment_hash.to_bytes pred)
        t.predecessor
    in
    let batches_bytes =
      to_bytes_exn (Data_encoding.list Batch.encoding) t.batches
    in
    Commitment_hash.hash_bytes [level_bytes; predecessor_bytes; batches_bytes]

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
end

type pending_commitment = {
  commitment : Commitment.t;
  hash : Commitment_hash.t;
  committer : Signature.Public_key_hash.t;
  submitted_at : Raw_level_repr.t;
}

let pp_pending_commitment : Format.formatter -> pending_commitment -> unit =
 fun fmt {commitment; hash; committer; submitted_at} ->
  Format.fprintf
    fmt
    "pending_commitment %a; hash = %a; committer = %a; submitted_at = %a"
    Commitment.pp
    commitment
    Commitment_hash.pp
    hash
    Signature.Public_key_hash.pp_short
    committer
    Raw_level_repr.pp
    submitted_at

let pending_commitment_encoding =
  Data_encoding.(
    conv
      (fun {commitment; committer; submitted_at; _} ->
        (commitment, committer, submitted_at))
      (fun (commitment, committer, submitted_at) ->
        let hash = Commitment.hash commitment in
        {hash; commitment; committer; submitted_at})
      (obj3
         (req "commitment" Commitment.encoding)
         (req "commiter" Signature.Public_key_hash.encoding)
         (req "submitted_at" Raw_level_repr.encoding)))

type t = pending_commitment list

let encoding = Data_encoding.(list pending_commitment_encoding)

let empty = []

let commitment_exists : t -> Commitment_hash.t -> bool =
 fun t hash -> List.exists (fun {hash = h; _} -> Commitment_hash.(h = hash)) t

let commitment_with_committer_exists : t -> Signature.Public_key_hash.t -> bool
    =
 fun t contract ->
  List.exists
    (fun {committer; _} -> Signature.Public_key_hash.(committer = contract))
    t

let append :
    t ->
    Signature.Public_key_hash.t ->
    Commitment.t ->
    Raw_level_repr.t ->
    t tzresult =
 fun t contract commitment level ->
  let hash = Commitment.hash commitment in
  (* We fail if this contract already has a commitment at this level,
     or if anyone has already made this commitment at this level; a
     bond entitles you to at most one commitment per level. *)
  error_when (commitment_exists t hash) Commitment_hash_already_submitted
  >>? fun () ->
  error_when
    (commitment_with_committer_exists t contract)
    Two_commitments_from_one_committer
  >>? fun () ->
  Ok ({hash; commitment; committer = contract; submitted_at = level} :: t)
