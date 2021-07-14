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

open Alpha_context
open Misc

type error += Invalid_fitness_gap of int64 * int64 (* `Permanent *)

type error +=
  | Timestamp_too_early of {
      minimal_time : Timestamp.t;
      provided_time : Timestamp.t;
      priority : int;
      endorsing_power_opt : int option;
    }

(* `Permanent *)

type error += Unexpected_endorsement (* `Permanent *)

type error += Invalid_endorsement_slot of int (* `Permanent *)

type error += Unexpected_endorsement_slot of int (* `Permanent *)

type error +=
  | Invalid_block_signature of Block_hash.t * Signature.Public_key_hash.t

(* `Permanent *)

type error += Invalid_signature (* `Permanent *)

type error += Invalid_stamp (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"baking.timestamp_too_early"
    ~title:"Block forged too early"
    ~description:"The block timestamp is before the minimal valid one."
    ~pp:(fun ppf (minimal_time, provided_time, priority, endorsing_power) ->
      let message_regarding_endorsements =
        match endorsing_power with
        | None -> ""
        | Some power -> Format.asprintf " and endorsing power %d" power
      in
      Format.fprintf
        ppf
        "Block forged too early: %a is before the minimal time %a for priority \
         %d%s)"
        Time.pp_hum
        provided_time
        Time.pp_hum
        minimal_time
        priority
        message_regarding_endorsements)
    Data_encoding.(
      obj4
        (req "minimal_time" Time.encoding)
        (req "provided_time" Time.encoding)
        (req "priority" int31)
        (opt "endorsing_power" int31))
    (function
      | Timestamp_too_early
          {minimal_time; provided_time; priority; endorsing_power_opt} ->
          Some (minimal_time, provided_time, priority, endorsing_power_opt)
      | _ -> None)
    (fun (minimal_time, provided_time, priority, endorsing_power_opt) ->
      Timestamp_too_early
        {minimal_time; provided_time; priority; endorsing_power_opt}) ;
  register_error_kind
    `Permanent
    ~id:"baking.invalid_fitness_gap"
    ~title:"Invalid fitness gap"
    ~description:"The gap of fitness is out of bounds"
    ~pp:(fun ppf (m, g) ->
      Format.fprintf ppf "The gap of fitness %Ld is not between 0 and %Ld" g m)
    Data_encoding.(obj2 (req "maximum" int64) (req "provided" int64))
    (function Invalid_fitness_gap (m, g) -> Some (m, g) | _ -> None)
    (fun (m, g) -> Invalid_fitness_gap (m, g)) ;
  register_error_kind
    `Permanent
    ~id:"baking.invalid_block_signature"
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
    ~id:"baking.invalid_signature"
    ~title:"Invalid block signature"
    ~description:"The block's signature is invalid"
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid block signature")
    Data_encoding.empty
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature) ;
  register_error_kind
    `Permanent
    ~id:"baking.insufficient_proof_of_work"
    ~title:"Insufficient block proof-of-work stamp"
    ~description:"The block's proof-of-work stamp is insufficient"
    ~pp:(fun ppf () -> Format.fprintf ppf "Insufficient proof-of-work stamp")
    Data_encoding.empty
    (function Invalid_stamp -> Some () | _ -> None)
    (fun () -> Invalid_stamp) ;
  register_error_kind
    `Permanent
    ~id:"baking.unexpected_endorsement"
    ~title:"Endorsement from unexpected delegate"
    ~description:
      "The operation is signed by a delegate without endorsement rights."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The endorsement is signed by a delegate without endorsement rights.")
    Data_encoding.unit
    (function Unexpected_endorsement -> Some () | _ -> None)
    (fun () -> Unexpected_endorsement) ;
  register_error_kind
    `Permanent
    ~id:"baking.invalid_endorsement_slot"
    ~title:"Endorsement slot out of range"
    ~description:"The endorsement slot provided is negative or too high."
    ~pp:(fun ppf v ->
      Format.fprintf
        ppf
        "Endorsement slot %d provided is negative or too high."
        v)
    Data_encoding.(obj1 (req "slot" uint16))
    (function Invalid_endorsement_slot v -> Some v | _ -> None)
    (fun v -> Invalid_endorsement_slot v) ;
  register_error_kind
    `Permanent
    ~id:"baking.unexpected_endorsement_slot"
    ~title:"Endorsement slot not the smallest possible"
    ~description:"The endorsement slot provided is not the smallest possible."
    ~pp:(fun ppf v ->
      Format.fprintf
        ppf
        "Endorsement slot %d provided is not the smallest possible."
        v)
    Data_encoding.(obj1 (req "slot" uint16))
    (function Unexpected_endorsement_slot v -> Some v | _ -> None)
    (fun v -> Unexpected_endorsement_slot v)

(* The function implements the fast-path case in [minimal_time]. (See
   [minimal_valid_time] for the definition of the fast-path.) *)
let minimal_time_fastpath_case minimal_block_delay pred_timestamp =
  Timestamp.(pred_timestamp +? minimal_block_delay)

(* The function implements the slow-path case in [minimal_time]. (See
   [minimal_valid_time] for the definition of the slow-path.) *)
let minimal_time_slowpath_case time_between_blocks priority pred_timestamp =
  let[@coq_struct "durations"] rec cumsum_time_between_blocks acc durations p =
    if Compare.Int32.( <= ) p 0l then ok acc
    else
      match durations with
      | [] -> cumsum_time_between_blocks acc [Period.one_minute] p
      | [last] -> Period.mult p last >>? fun period -> Timestamp.(acc +? period)
      | first :: durations ->
          Timestamp.(acc +? first) >>? fun acc ->
          let p = Int32.pred p in
          cumsum_time_between_blocks acc durations p
  in
  cumsum_time_between_blocks
    pred_timestamp
    time_between_blocks
    (Int32.succ priority)

let minimal_time constants ~priority pred_timestamp =
  let priority = Int32.of_int priority in
  if Compare.Int32.(priority = 0l) then
    minimal_time_fastpath_case
      constants.Constants.minimal_block_delay
      pred_timestamp
  else
    minimal_time_slowpath_case
      constants.time_between_blocks
      priority
      pred_timestamp

let earlier_predecessor_timestamp ctxt level =
  let current = Level.current ctxt in
  let current_timestamp = Timestamp.current ctxt in
  let gap = Level.diff level current in
  let step = Constants.minimal_block_delay ctxt in
  if Compare.Int32.(gap < 1l) then
    failwith "Baking.earlier_block_timestamp: past block."
  else
    Period.mult (Int32.pred gap) step >>? fun delay ->
    Timestamp.(current_timestamp +? delay)

let check_timestamp c ~priority pred_timestamp =
  minimal_time (Constants.parametric c) ~priority pred_timestamp
  >>? fun minimal_time ->
  let timestamp = Timestamp.current c in
  record_trace
    (Timestamp_too_early
       {
         minimal_time;
         provided_time = timestamp;
         priority;
         endorsing_power_opt = None;
       })
    Timestamp.(timestamp -? minimal_time)
  >>? fun _block_delay -> ok ()

type error += Incorrect_priority (* `Permanent *)

type error += Incorrect_number_of_endorsements (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"incorrect_priority"
    ~title:"Incorrect priority"
    ~description:"Block priority must be non-negative."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The block priority must be non-negative.")
    Data_encoding.unit
    (function Incorrect_priority -> Some () | _ -> None)
    (fun () -> Incorrect_priority)

let () =
  let description =
    "The number of endorsements must be non-negative and at most the \
     endorsers_per_block constant."
  in
  register_error_kind
    `Permanent
    ~id:"incorrect_number_of_endorsements"
    ~title:"Incorrect number of endorsements"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Incorrect_number_of_endorsements -> Some () | _ -> None)
    (fun () -> Incorrect_number_of_endorsements)

let rec reward_for_priority reward_per_prio prio =
  match reward_per_prio with
  | [] ->
      (* Empty reward list in parameters means no rewards *)
      Tez.zero
  | [last] -> last
  | first :: rest ->
      if Compare.Int.(prio <= 0) then first
      else reward_for_priority rest (pred prio)

let baking_reward ctxt ~block_priority ~included_endorsements =
  error_unless Compare.Int.(block_priority >= 0) Incorrect_priority
  >>? fun () ->
  error_unless
    Compare.Int.(
      included_endorsements >= 0
      && included_endorsements <= Constants.endorsers_per_block ctxt)
    Incorrect_number_of_endorsements
  >>? fun () ->
  let reward_per_endorsement =
    reward_for_priority
      (Constants.baking_reward_per_endorsement ctxt)
      block_priority
  in
  Tez.(reward_per_endorsement *? Int64.of_int included_endorsements)

let endorsing_reward ctxt ~block_priority num_slots =
  error_unless Compare.Int.(block_priority >= 0) Incorrect_priority
  >>? fun () ->
  let reward_per_endorsement =
    reward_for_priority (Constants.endorsement_reward ctxt) block_priority
  in
  Tez.(reward_per_endorsement *? Int64.of_int num_slots)

let baking_priorities c level =
  let rec f priority =
    Roll.baking_rights_owner c level ~priority >|=? fun delegate ->
    LCons (delegate, fun () -> f (succ priority))
  in
  f 0

let endorsement_rights ctxt level =
  List.fold_right_es
    (fun slot acc ->
      Roll.endorsement_rights_owner ctxt level ~slot >|=? fun pk ->
      let pkh = Signature.Public_key.hash pk in
      let right =
        match Signature.Public_key_hash.Map.find pkh acc with
        | None -> (pk, [slot], false)
        | Some (pk, slots, used) -> (pk, slot :: slots, used)
      in
      Signature.Public_key_hash.Map.add pkh right acc)
    (0 --> (Constants.endorsers_per_block ctxt - 1))
    Signature.Public_key_hash.Map.empty

let[@coq_axiom_with_reason "gadt"] check_endorsement_rights ctxt chain_id ~slot
    (op : Kind.endorsement Operation.t) =
  if
    Compare.Int.(slot < 0 (* should not happen because of binary format *))
    || Compare.Int.(slot >= Constants.endorsers_per_block ctxt)
  then fail (Invalid_endorsement_slot slot)
  else
    let current_level = Level.current ctxt in
    let (Single (Endorsement {level; _})) = op.protocol_data.contents in
    Roll.endorsement_rights_owner ctxt (Level.from_raw ctxt level) ~slot
    >>=? fun pk ->
    let pkh = Signature.Public_key.hash pk in
    match Operation.check_signature pk chain_id op with
    | Error _ -> fail Unexpected_endorsement
    | Ok () -> (
        (if Raw_level.(succ level = current_level.level) then
         return (Alpha_context.allowed_endorsements ctxt)
        else endorsement_rights ctxt (Level.from_raw ctxt level))
        >>=? fun endorsements ->
        match Signature.Public_key_hash.Map.find pkh endorsements with
        | None -> fail Unexpected_endorsement (* unexpected *)
        | Some (_pk, (top_slot :: _ as slots), v) ->
            error_unless
              Compare.Int.(slot = top_slot)
              (Unexpected_endorsement_slot slot)
            >>?= fun () -> return (pkh, slots, v)
        | Some (_pk, [], _) -> fail (Unexpected_endorsement_slot slot))

let select_delegate delegate delegate_list max_priority =
  let rec loop acc l n =
    if Compare.Int.(n >= max_priority) then return (List.rev acc)
    else
      let (LCons (pk, t)) = l in
      let acc =
        if
          Signature.Public_key_hash.equal
            delegate
            (Signature.Public_key.hash pk)
        then n :: acc
        else acc
      in
      t () >>=? fun t -> loop acc t (succ n)
  in
  loop [] delegate_list 0

let first_baking_priorities ctxt ?(max_priority = 32) delegate level =
  baking_priorities ctxt level >>=? fun delegate_list ->
  select_delegate delegate delegate_list max_priority

let check_hash hash stamp_threshold =
  let bytes = Block_hash.to_bytes hash in
  let word = TzEndian.get_int64 bytes 0 in
  Compare.Uint64.(word <= stamp_threshold)

let check_header_proof_of_work_stamp shell contents stamp_threshold =
  let hash =
    Block_header.hash
      {shell; protocol_data = {contents; signature = Signature.zero}}
  in
  check_hash hash stamp_threshold

let check_proof_of_work_stamp ctxt block =
  let proof_of_work_threshold = Constants.proof_of_work_threshold ctxt in
  if
    check_header_proof_of_work_stamp
      block.Block_header.shell
      block.protocol_data.contents
      proof_of_work_threshold
  then ok_unit
  else error Invalid_stamp

let check_signature block chain_id key =
  let check_signature key
      {Block_header.shell; protocol_data = {contents; signature}} =
    let unsigned_header =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    Signature.check
      ~watermark:(Block_header chain_id)
      key
      signature
      unsigned_header
  in
  if check_signature key block then return_unit
  else
    fail
      (Invalid_block_signature
         (Block_header.hash block, Signature.Public_key.hash key))

let max_fitness_gap _ctxt = 1L

let check_fitness_gap ctxt (block : Block_header.t) =
  let current_fitness = Fitness.current ctxt in
  Fitness.to_int64 block.shell.fitness >>? fun announced_fitness ->
  let gap = Int64.sub announced_fitness current_fitness in
  if Compare.Int64.(gap <= 0L || max_fitness_gap ctxt < gap) then
    error (Invalid_fitness_gap (max_fitness_gap ctxt, gap))
  else ok_unit

(* The minimal threshold on the endorsing power for the fast-path case
   is 60% of the maximal endorsing power. *)
let fastpath_endorsing_power_threshold maximal_endorsing_power =
  3 * maximal_endorsing_power / 5

(* This function computes the minimal time at which a block is
   valid. It distinguishes between the "fast-path" case, when the
   priority is 0 and the endorsing power is at least 60% of the
   maximal endorsing power, and the "slow-path" case, when this
   condition is not satisfied. *)
let minimal_valid_time constants ~priority ~endorsing_power
    ~predecessor_timestamp =
  if
    Compare.Int.(priority = 0)
    && Compare.Int.(
         endorsing_power
         >= fastpath_endorsing_power_threshold
              constants.Constants.endorsers_per_block)
  then
    minimal_time_fastpath_case
      constants.minimal_block_delay
      predecessor_timestamp
  else
    minimal_time_slowpath_case
      constants.time_between_blocks
      (Int32.of_int priority)
      predecessor_timestamp
    >>? fun minimal_time ->
    let delay_per_missing_endorsement =
      constants.Constants.delay_per_missing_endorsement
    in
    let missing_endorsements =
      let minimal_required_endorsements =
        constants.Constants.initial_endorsers
      in
      Compare.Int.max 0 (minimal_required_endorsements - endorsing_power)
    in
    Period.mult
      (Int32.of_int missing_endorsements)
      delay_per_missing_endorsement
    >|? fun delay -> Time.add minimal_time (Period.to_seconds delay)
