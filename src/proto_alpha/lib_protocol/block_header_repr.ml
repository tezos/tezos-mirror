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

(** Block header *)

type contents = {
  payload_hash : Block_payload_hash.t;
  payload_round : Round_repr.t;
  seed_nonce_hash : Nonce_hash.t option;
  proof_of_work_nonce : bytes;
  per_block_votes : Per_block_votes_repr.per_block_votes;
}

type protocol_data = {contents : contents; signature : Signature.t}

type t = {shell : Block_header.shell_header; protocol_data : protocol_data}

type block_header = t

type raw = Block_header.t

type shell_header = Block_header.shell_header

let raw_encoding = Block_header.encoding

let shell_header_encoding = Block_header.shell_header_encoding

type block_watermark = Block_header of Chain_id.t

let bytes_of_block_watermark = function
  | Block_header chain_id ->
      Bytes.cat (Bytes.of_string "\x11") (Chain_id.to_bytes chain_id)

let to_watermark b = Signature.Custom (bytes_of_block_watermark b)

let of_watermark = function
  | Signature.Custom b ->
      if Compare.Int.(Bytes.length b > 0) then
        match Bytes.get b 0 with
        | '\x11' ->
            Option.map
              (fun chain_id -> Block_header chain_id)
              (Chain_id.of_bytes_opt (Bytes.sub b 1 (Bytes.length b - 1)))
        | _ -> None
      else None
  | _ -> None

let contents_encoding =
  let open Data_encoding in
  let json =
    conv
      (fun {
             payload_hash;
             payload_round;
             seed_nonce_hash;
             proof_of_work_nonce;
             per_block_votes = {liquidity_baking_vote; adaptive_inflation_vote};
           } ->
        ( payload_hash,
          payload_round,
          proof_of_work_nonce,
          seed_nonce_hash,
          liquidity_baking_vote,
          adaptive_inflation_vote ))
      (fun ( payload_hash,
             payload_round,
             proof_of_work_nonce,
             seed_nonce_hash,
             liquidity_baking_vote,
             adaptive_inflation_vote ) ->
        {
          payload_hash;
          payload_round;
          seed_nonce_hash;
          proof_of_work_nonce;
          per_block_votes = {liquidity_baking_vote; adaptive_inflation_vote};
        })
      (obj6
         (req "payload_hash" Block_payload_hash.encoding)
         (req "payload_round" Round_repr.encoding)
         (req
            "proof_of_work_nonce"
            (Fixed.bytes Hex Constants_repr.proof_of_work_nonce_size))
         (opt "seed_nonce_hash" Nonce_hash.encoding)
         (req
            "liquidity_baking_toggle_vote"
            Per_block_votes_repr.liquidity_baking_vote_encoding)
         (req
            "adaptive_inflation_vote"
            Per_block_votes_repr.adaptive_inflation_vote_encoding))
  in
  let binary =
    conv
      (fun {
             payload_hash;
             payload_round;
             seed_nonce_hash;
             proof_of_work_nonce;
             per_block_votes;
           } ->
        ( payload_hash,
          payload_round,
          proof_of_work_nonce,
          seed_nonce_hash,
          per_block_votes ))
      (fun ( payload_hash,
             payload_round,
             proof_of_work_nonce,
             seed_nonce_hash,
             per_block_votes ) ->
        {
          payload_hash;
          payload_round;
          seed_nonce_hash;
          proof_of_work_nonce;
          per_block_votes;
        })
      (obj5
         (req "payload_hash" Block_payload_hash.encoding)
         (req "payload_round" Round_repr.encoding)
         (req
            "proof_of_work_nonce"
            (Fixed.bytes Hex Constants_repr.proof_of_work_nonce_size))
         (opt "seed_nonce_hash" Nonce_hash.encoding)
         (req "per_block_votes" Per_block_votes_repr.per_block_votes_encoding))
  in
  def "block_header.alpha.unsigned_contents" @@ splitted ~binary ~json

let protocol_data_encoding =
  let open Data_encoding in
  def "block_header.alpha.signed_contents"
  @@ conv
       (fun {contents; signature} -> (contents, signature))
       (fun (contents, signature) -> {contents; signature})
       (merge_objs
          contents_encoding
          (obj1 (req "signature" Signature.encoding)))

let raw {shell; protocol_data} =
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn protocol_data_encoding protocol_data
  in
  {Block_header.shell; protocol_data}

let unsigned_encoding =
  let open Data_encoding in
  merge_objs Block_header.shell_header_encoding contents_encoding

let encoding =
  let open Data_encoding in
  def "block_header.alpha.full_header"
  @@ conv
       (fun {shell; protocol_data} -> (shell, protocol_data))
       (fun (shell, protocol_data) -> {shell; protocol_data})
       (merge_objs Block_header.shell_header_encoding protocol_data_encoding)

(** Constants *)

let max_header_length =
  let fake_level = Raw_level_repr.root in
  let fake_round = Round_repr.zero in
  let fake_fitness =
    Fitness_repr.create_without_locked_round
      ~level:fake_level
      ~predecessor_round:fake_round
      ~round:fake_round
  in
  let fake_shell =
    {
      Block_header.level = 0l;
      proto_level = 0;
      predecessor = Block_hash.zero;
      timestamp = Time.of_seconds 0L;
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.zero;
      fitness = Fitness_repr.to_raw fake_fitness;
      context = Context_hash.zero;
    }
  and fake_contents =
    {
      payload_hash = Block_payload_hash.zero;
      payload_round = Round_repr.zero;
      proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '0';
      seed_nonce_hash = Some Nonce_hash.zero;
      per_block_votes =
        {
          liquidity_baking_vote = Per_block_vote_pass;
          adaptive_inflation_vote = Per_block_vote_pass;
        };
    }
  in
  Data_encoding.Binary.length
    encoding
    {
      shell = fake_shell;
      protocol_data = {contents = fake_contents; signature = Signature.zero};
    }

(** Header parsing entry point  *)

let hash_raw = Block_header.hash

let hash {shell; protocol_data} =
  Block_header.hash
    {
      shell;
      protocol_data =
        Data_encoding.Binary.to_bytes_exn protocol_data_encoding protocol_data;
    }

type error +=
  | (* Permanent *)
      Invalid_block_signature of
      Block_hash.t * Signature.Public_key_hash.t
  | (* Permanent *) Invalid_stamp
  | (* Permanent *)
      Invalid_payload_round of {
      payload_round : Round_repr.t;
      round : Round_repr.t;
    }
  | (* Permanent *) Invalid_commitment of {expected : bool}
  | (* Permanent *) Wrong_timestamp of Time.t * Time.t

let () =
  register_error_kind
    `Permanent
    ~id:"block_header.invalid_block_signature"
    ~title:"Invalid block signature"
    ~description:"A block was not signed with the expected private key."
    ~pp:(fun ppf (block, pkh) ->
      Format.fprintf
        ppf
        "Invalid signature for block %a. Expected: %a."
        Block_hash.pp_short
        block
        Signature.Public_key_hash.pp_short
        pkh)
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (req "expected" Signature.Public_key_hash.encoding))
    (function
      | Invalid_block_signature (block, pkh) -> Some (block, pkh) | _ -> None)
    (fun (block, pkh) -> Invalid_block_signature (block, pkh)) ;
  register_error_kind
    `Permanent
    ~id:"block_header.invalid_stamp"
    ~title:"Insufficient block proof-of-work stamp"
    ~description:"The block's proof-of-work stamp is insufficient"
    ~pp:(fun ppf () -> Format.fprintf ppf "Insufficient proof-of-work stamp")
    Data_encoding.empty
    (function Invalid_stamp -> Some () | _ -> None)
    (fun () -> Invalid_stamp) ;
  register_error_kind
    `Permanent
    ~id:"block_header.invalid_payload_round"
    ~title:"Invalid payload round"
    ~description:"The given payload round is invalid."
    ~pp:(fun ppf (payload_round, round) ->
      Format.fprintf
        ppf
        "The provided payload round (%a) is after the block round (%a)."
        Round_repr.pp
        payload_round
        Round_repr.pp
        round)
    Data_encoding.(
      obj2
        (req "payload_round" Round_repr.encoding)
        (req "round" Round_repr.encoding))
    (function
      | Invalid_payload_round {payload_round; round} ->
          Some (payload_round, round)
      | _ -> None)
    (fun (payload_round, round) -> Invalid_payload_round {payload_round; round}) ;
  register_error_kind
    `Permanent
    ~id:"block_header.invalid_commitment"
    ~title:"Invalid commitment in block header"
    ~description:"The block header has invalid commitment."
    ~pp:(fun ppf expected ->
      if expected then
        Format.fprintf ppf "Missing seed's nonce commitment in block header."
      else
        Format.fprintf ppf "Unexpected seed's nonce commitment in block header.")
    Data_encoding.(obj1 (req "expected" bool))
    (function Invalid_commitment {expected} -> Some expected | _ -> None)
    (fun expected -> Invalid_commitment {expected}) ;
  register_error_kind
    `Permanent
    ~id:"block_header.wrong_timestamp"
    ~title:"Wrong timestamp"
    ~description:"Block timestamp not the expected one."
    ~pp:(fun ppf (block_ts, expected_ts) ->
      Format.fprintf
        ppf
        "Wrong timestamp: block timestamp (%a) not the expected one (%a)"
        Time.pp_hum
        block_ts
        Time.pp_hum
        expected_ts)
    Data_encoding.(
      obj2
        (req "block_timestamp" Time.encoding)
        (req "expected_timestamp" Time.encoding))
    (function Wrong_timestamp (t1, t2) -> Some (t1, t2) | _ -> None)
    (fun (t1, t2) -> Wrong_timestamp (t1, t2))

let check_signature (block : t) (chain_id : Chain_id.t)
    (key : Signature.Public_key.t) =
  let check_signature key ({shell; protocol_data = {contents; signature}} : t) =
    let unsigned_header =
      Data_encoding.Binary.to_bytes_exn unsigned_encoding (shell, contents)
    in
    Signature.check
      ~watermark:(to_watermark (Block_header chain_id))
      key
      signature
      unsigned_header
  in
  if check_signature key block then ok ()
  else
    error (Invalid_block_signature (hash block, Signature.Public_key.hash key))

let check_payload_round ~round ~payload_round =
  error_when
    Round_repr.(payload_round > round)
    (Invalid_payload_round {payload_round; round})

let check_timestamp round_durations ~timestamp ~round ~predecessor_timestamp
    ~predecessor_round =
  Round_repr.timestamp_of_round
    round_durations
    ~predecessor_timestamp
    ~predecessor_round
    ~round
  >>? fun expected_timestamp ->
  if Time_repr.(expected_timestamp = timestamp) then Error_monad.ok ()
  else error (Wrong_timestamp (timestamp, expected_timestamp))

module Proof_of_work = struct
  let check_hash hash stamp_threshold =
    let bytes = Block_hash.to_bytes hash in
    let word = TzEndian.get_int64 bytes 0 in
    Compare.Uint64.(word <= stamp_threshold)

  let check_header_proof_of_work_stamp shell contents stamp_threshold =
    let hash =
      hash {shell; protocol_data = {contents; signature = Signature.zero}}
    in
    check_hash hash stamp_threshold

  let check_proof_of_work_stamp ~proof_of_work_threshold block =
    if
      check_header_proof_of_work_stamp
        block.shell
        block.protocol_data.contents
        proof_of_work_threshold
    then ok ()
    else error Invalid_stamp
end

let begin_validate_block_header ~(block_header : t) ~(chain_id : Chain_id.t)
    ~(predecessor_timestamp : Time.t) ~(predecessor_round : Round_repr.t)
    ~(fitness : Fitness_repr.t) ~(timestamp : Time.t)
    ~(delegate_pk : Signature.Public_key.t)
    ~(round_durations : Round_repr.Durations.t)
    ~(proof_of_work_threshold : int64) ~(expected_commitment : bool) =
  (* Level relationship between current node and the predecessor is
     done by the shell. We know that level is predecessor level + 1.
     The predecessor block hash is guaranteed by the shell to be the
     one in the shell header.  The operations are guaranteed to
     correspond to the shell_header.operations_hash by the shell *)
  let {payload_round; seed_nonce_hash; _} =
    block_header.protocol_data.contents
  in
  let raw_level = block_header.shell.level in
  Proof_of_work.check_proof_of_work_stamp ~proof_of_work_threshold block_header
  >>? fun () ->
  Raw_level_repr.of_int32 raw_level >>? fun level ->
  check_signature block_header chain_id delegate_pk >>? fun () ->
  let round = Fitness_repr.round fitness in
  check_payload_round ~round ~payload_round >>? fun () ->
  check_timestamp
    round_durations
    ~predecessor_timestamp
    ~predecessor_round
    ~timestamp
    ~round
  >>? fun () ->
  Fitness_repr.check_except_locked_round fitness ~level ~predecessor_round
  >>? fun () ->
  let has_commitment =
    match seed_nonce_hash with None -> false | Some _ -> true
  in
  error_unless
    Compare.Bool.(has_commitment = expected_commitment)
    (Invalid_commitment {expected = expected_commitment})
