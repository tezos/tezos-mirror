(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context

type nanotez = Q.t

let nanotez_enc : nanotez Data_encoding.t =
  let open Data_encoding in
  def
    "nanotez"
    ~title:"A thousandth of a mutez"
    ~description:"One thousand nanotez make a mutez (1 tez = 1e9 nanotez)"
    (conv
       (fun q -> (q.Q.num, q.Q.den))
       (fun (num, den) -> {Q.num; den})
       (tup2 z z))

let manager_op_replacement_factor_enc : Q.t Data_encoding.t =
  let open Data_encoding in
  def
    "manager operation replacement factor"
    ~title:"A manager operation's replacement factor"
    ~description:"The fee and fee/gas ratio of an operation to replace another"
    (conv
       (fun q -> (q.Q.num, q.Q.den))
       (fun (num, den) -> {Q.num; den})
       (tup2 z z))

type config = {
  minimal_fees : Tez.t;
  minimal_nanotez_per_gas_unit : nanotez;
  minimal_nanotez_per_byte : nanotez;
  clock_drift : Period.t option;
  replace_by_fee_factor : Q.t;
      (** Factor by which the fee and fee/gas ratio of an old operation in
          the mempool are both multiplied to determine the values that a new
          operation must exceed in order to replace the old operation. See
          the [better_fees_and_ratio] function further below. *)
}

let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let managers_quota =
  Stdlib.List.nth Main.validation_passes Operation_repr.manager_pass

(* If the drift is not specified, it will be the duration of round zero.
   It allows only to spam with one future round.

   /!\ Warning /!\ : current plugin implementation implies that this drift
   cumulates with the accepted  drift regarding the current head's timestamp.
*)
let default_config =
  {
    minimal_fees = default_minimal_fees;
    minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
    clock_drift = None;
    replace_by_fee_factor =
      Q.make (Z.of_int 105) (Z.of_int 100)
      (* Default value of [replace_by_fee_factor] is set to 5% *);
  }

let config_encoding : config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           minimal_fees;
           minimal_nanotez_per_gas_unit;
           minimal_nanotez_per_byte;
           clock_drift;
           replace_by_fee_factor;
         } ->
      ( minimal_fees,
        minimal_nanotez_per_gas_unit,
        minimal_nanotez_per_byte,
        clock_drift,
        replace_by_fee_factor ))
    (fun ( minimal_fees,
           minimal_nanotez_per_gas_unit,
           minimal_nanotez_per_byte,
           clock_drift,
           replace_by_fee_factor ) ->
      {
        minimal_fees;
        minimal_nanotez_per_gas_unit;
        minimal_nanotez_per_byte;
        clock_drift;
        replace_by_fee_factor;
      })
    (obj5
       (dft "minimal_fees" Tez.encoding default_config.minimal_fees)
       (dft
          "minimal_nanotez_per_gas_unit"
          nanotez_enc
          default_config.minimal_nanotez_per_gas_unit)
       (dft
          "minimal_nanotez_per_byte"
          nanotez_enc
          default_config.minimal_nanotez_per_byte)
       (opt "clock_drift" Period.encoding)
       (dft
          "replace_by_fee_factor"
          manager_op_replacement_factor_enc
          default_config.replace_by_fee_factor))

(** Static information to store in the filter state. *)
type info = {
  head : Block_header.shell_header;
  round_durations : Round.round_durations;
  hard_gas_limit_per_block : Gas.Arith.integral;
  head_round : Round.t;
  round_zero_duration : Period.t;
  grandparent_level_start : Timestamp.t;
}

let init_state_prototzresult ~head round_durations hard_gas_limit_per_block =
  let open Lwt_result_syntax in
  let*? head_round =
    Alpha_context.Fitness.round_from_raw head.Tezos_base.Block_header.fitness
  in
  let round_zero_duration = Round.round_duration round_durations Round.zero in
  let*? grandparent_round =
    Alpha_context.Fitness.predecessor_round_from_raw head.fitness
  in
  let*? proposal_level_offset =
    Round.level_offset_of_round
      round_durations
      ~round:Round.(succ grandparent_round)
  in
  let*? proposal_round_offset =
    Round.level_offset_of_round round_durations ~round:head_round
  in
  let*? proposal_offset =
    Period.(add proposal_level_offset proposal_round_offset)
  in
  let grandparent_level_start = Timestamp.(head.timestamp - proposal_offset) in
  return
    {
      head;
      round_durations;
      hard_gas_limit_per_block;
      head_round;
      round_zero_duration;
      grandparent_level_start;
    }

let init_state ~head round_durations hard_gas_limit_per_block =
  Lwt.map
    Environment.wrap_tzresult
    (init_state_prototzresult ~head round_durations hard_gas_limit_per_block)

let init context ~(head : Tezos_base.Block_header.shell_header) =
  let open Lwt_result_syntax in
  let* ( ctxt,
         (_ : Receipt.balance_updates),
         (_ : Migration.origination_result list) ) =
    prepare
      context
      ~level:(Int32.succ head.level)
      ~predecessor_timestamp:head.timestamp
      ~timestamp:head.timestamp
    |> Lwt.map Environment.wrap_tzresult
  in
  let round_durations = Constants.round_durations ctxt in
  let hard_gas_limit_per_block = Constants.hard_gas_limit_per_block ctxt in
  init_state ~head round_durations hard_gas_limit_per_block

let flush old_info ~head =
  (* To avoid the need to prepare a context as in [init], we retrieve
     the [round_durations] from the previous state. Indeed, they are
     only determined by the [minimal_block_delay] and
     [delay_increment_per_round] constants (see
     {!Raw_context.prepare}), and all the constants remain unchanged
     during the lifetime of a protocol. As to
     [hard_gas_limit_per_block], it is directly a protocol
     constant. *)
  init_state ~head old_info.round_durations old_info.hard_gas_limit_per_block

let manager_prio p = `Low p

let consensus_prio = `High

let other_prio = `Medium

let compute_manager_contents_fee_and_gas_limit contents =
  let open Operation in
  let l = to_list (Contents_list contents) in
  List.fold_left
    (fun acc -> function
      | Contents (Manager_operation {fee; gas_limit; _}) -> (
          match acc with
          | Error _ as e -> e
          | Ok (total_fee, total_gas) -> (
              match Tez.(total_fee +? fee) with
              | Ok total_fee -> Ok (total_fee, Gas.Arith.add total_gas gas_limit)
              | Error _ as e -> e))
      | _ -> acc)
    (Ok (Tez.zero, Gas.Arith.zero))
    l

type Environment.Error_monad.error += Fees_too_low

let () =
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"prefilter.fees_too_low"
    ~title:"Operation fees are too low"
    ~description:"Operation fees are too low"
    ~pp:(fun ppf () -> Format.fprintf ppf "Operation fees are too low")
    Data_encoding.unit
    (function Fees_too_low -> Some () | _ -> None)
    (fun () -> Fees_too_low)

let size_of_operation op =
  (WithExceptions.Option.get ~loc:__LOC__
  @@ Data_encoding.Binary.fixed_length
       Tezos_base.Operation.shell_header_encoding)
  + Data_encoding.Binary.length Operation.protocol_data_encoding op

(** Returns the weight and resources consumption of an operation. The weight
      corresponds to the one implemented by the baker, to decide which operations
      to put in a block first (the code is largely duplicated).
      See {!Tezos_baking_alpha.Operation_selection.weight_manager} *)
let weight_and_resources_manager_operation ~hard_gas_limit_per_block ?size ~fee
    ~gas op =
  let max_size = managers_quota.max_size in
  let size = match size with None -> size_of_operation op | Some s -> s in
  let size_f = Q.of_int size in
  let gas_f = Q.of_bigint (Gas.Arith.integral_to_z gas) in
  let fee_f = Q.of_int64 (Tez.to_mutez fee) in
  let size_ratio = Q.(size_f / Q.of_int max_size) in
  let gas_ratio =
    Q.(gas_f / Q.of_bigint (Gas.Arith.integral_to_z hard_gas_limit_per_block))
  in
  let resources = Q.max size_ratio gas_ratio in
  (Q.(fee_f / resources), resources)

let pre_filter_manager :
    type t.
    info ->
    config ->
    Operation.packed_protocol_data ->
    t Kind.manager contents_list ->
    [ `Passed_prefilter of Q.t list
    | `Branch_refused of tztrace
    | `Branch_delayed of tztrace
    | `Refused of tztrace
    | `Outdated of tztrace ] =
 fun info config packed_op op ->
  let size = size_of_operation packed_op in
  let check_gas_and_fee fee gas_limit =
    let fees_in_nanotez =
      Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
    in
    let minimal_fees_in_nanotez =
      Q.mul (Q.of_int64 (Tez.to_mutez config.minimal_fees)) (Q.of_int 1000)
    in
    let minimal_fees_for_gas_in_nanotez =
      Q.mul
        config.minimal_nanotez_per_gas_unit
        (Q.of_bigint @@ Gas.Arith.integral_to_z gas_limit)
    in
    let minimal_fees_for_size_in_nanotez =
      Q.mul config.minimal_nanotez_per_byte (Q.of_int size)
    in
    if
      Q.compare
        fees_in_nanotez
        (Q.add
           minimal_fees_in_nanotez
           (Q.add
              minimal_fees_for_gas_in_nanotez
              minimal_fees_for_size_in_nanotez))
      >= 0
    then `Fees_ok
    else `Refused [Environment.wrap_tzerror Fees_too_low]
  in
  match compute_manager_contents_fee_and_gas_limit op with
  | Error err -> `Refused (Environment.wrap_tztrace err)
  | Ok (fee, gas_limit) -> (
      match check_gas_and_fee fee gas_limit with
      | `Refused _ as err -> err
      | `Fees_ok ->
          let weight, _op_resources =
            weight_and_resources_manager_operation
              ~hard_gas_limit_per_block:info.hard_gas_limit_per_block
              ~fee
              ~gas:gas_limit
              packed_op
          in
          `Passed_prefilter [weight])

type Environment.Error_monad.error += Wrong_operation

let () =
  Environment.Error_monad.register_error_kind
    `Temporary
    ~id:"prefilter.wrong_operation"
    ~title:"Wrong operation"
    ~description:"Failing_noop operations are not accepted in the mempool."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failing_noop operations are not accepted in the mempool")
    Data_encoding.unit
    (function Wrong_operation -> Some () | _ -> None)
    (fun () -> Wrong_operation)

type Environment.Error_monad.error += Consensus_operation_in_far_future

let () =
  Environment.Error_monad.register_error_kind
    `Branch
    ~id:"prefilter.Consensus_operation_in_far_future"
    ~title:"Consensus operation in far future"
    ~description:"Consensus operation too far in the future are not accepted."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Consensus operation too far in the future are not accepted.")
    Data_encoding.unit
    (function Consensus_operation_in_far_future -> Some () | _ -> None)
    (fun () -> Consensus_operation_in_far_future)

(** {2 consensus operation filtering}

     In Tenderbake, we increased a lot the number of consensus
      operations, therefore it seems necessary to be able to filter consensus
     operations that could be produced by a Byzantine baker mis-using
     its right to produce operations in future rounds or levels.

      We consider the situation where the head is at level [h_l],
     round [h_r], and with timestamp [h_ts], with the predecessor of the head
     being at round [hp_r].
      We receive at a time [now] a consensus operation for level [op_l] and
     round [op_r].

       A consensus operation is considered too far in the future, and therefore filtered,
      if the earliest possible starting time of its round is greater than the
      current time plus a safety margin of [config.clock_drift].

      To consider potential level 2 reorgs, we first compute the expected
      timestamp of round zero at previous level [hp0_ts],

      All ops at level p_l and round r' such that time(r') is greater than (now + drift) are
     deemed too far in the future:

                  h_r                          op_ts    now+drift     (h_l,r')
     hp0_ts h_0   h_l                            |        |              |
        +----+-----+---------+-------------------+--+-----+--------------+-----------
             |     |         |                   |  |     |              |
             |    h_ts     h_r end time          | now    |        earliest expected
             |     |                             |        |        time of round r'
             |<----op_r rounds duration -------->|        |
                   |
                   |<--------------- operations kept ---->|<-rejected----------...
                   |
                   |<-----------operations considered by the filter -----------...

    For an operation on a proposal at the next level, we consider the minimum
    starting time of the operation's round, obtained by assuming that the proposal
    at the next level was built on top of a proposal at round 0 for the current
    level, itself based on a proposal at round 0 of previous level.
    Operations on proposal with higher levels are treated similarly.

    All ops at the next level and round r' such that timestamp(r') > now+drift
    are deemed too far in the future.

                r=0     r=1   h_r      now     now+drift   (h_l+1,r')
   hp0_ts h_0   h_l           h_l       |          |          |
      +----+---- |-------+----+---------+----------+----------+----------
           |     |       |    |                               |
           |     t0      |   h_ts                      earliest expected
           |     |       |    |                         time of round r'
           |<--- |    earliest|                               |
                 |  next level|                               |
                 |       |<---------------------------------->|
                                  round_offset(r')

  *)

(** At a given level a consensus operation is acceptable if its earliest
      expected timestamp, [op_earliest_ts] is below the current clock with an
      accepted drift for the clock given by a configuration.  *)
let acceptable ~drift ~op_earliest_ts ~now_timestamp =
  let open Result_syntax in
  Timestamp.(
    let+ now_drifted = now_timestamp +? drift in
    op_earliest_ts <= now_drifted)

(** Check that an operation with the given [op_round], at level [op_level]
      is likely to be correct, meaning it could have been produced before
      now (+ the safety margin from configuration).

      Given an operation at level greater or equal than/to the current level, we
      compute the expected timestamp of the operation's round. If the operation
      is at a greater level, we assume that it is based on the proposal at round
      zero of the current level.

      All operations whose (level, round) is lower than or equal to the current
      head are deemed valid.
      Note that in case where their is a high drift in the computer clock, they
      might not have been considered valid by comparing their expected timestamp
      to the clock.

      This is a stricter than necessary filter as it will reject operations that
      could be valid in the current timeframe if the proposal they attest is
      built over a predecessor of the current proposal that would be of lower
      round than the current one.

      What can we do that would be smarter: get current head's predecessor round
      and timestamp to compute the timestamp t0 of a predecessor that would have
      been proposed at round 0.

      Timestamp of round at current level for an alternative head that would be
      based on such proposal would be computed based on t0.
      For level higher than current head, compute the round's earliest timestamp
      if all proposal passed at round 0 starting from t0.
  *)
let acceptable_op ~config ~round_durations ~round_zero_duration ~proposal_level
    ~proposal_round ~proposal_timestamp
    ~(proposal_predecessor_level_start : Timestamp.t) ~op_level ~op_round
    ~now_timestamp =
  let open Result_syntax in
  if
    Raw_level.(succ op_level < proposal_level)
    || (op_level = proposal_level && op_round <= proposal_round)
  then
    (* Past and current round operations are not in the future *)
    (* This case could be handled directly in `pre_filter_far_future_consensus_ops`
       for a (slightly) better performance. *)
    Ok true
  else
    (* If, by some tolerance on local clock drift, the timestamp of the
       current head is itself in the future, we use this time instead of
       now_timestamp *)
    let now_timestamp = Timestamp.(max now_timestamp proposal_timestamp) in
    (* Computing when the current level started. *)
    let drift = Option.value ~default:round_zero_duration config.clock_drift in
    (* We compute the earliest timestamp possible [op_earliest_ts] for the
       operation's (level,round), as if all proposals were accepted at round 0
       since the previous level. *)
    (* Invariant: [op_level + 1 >= proposal_level] *)
    let level_offset = Raw_level.(diff (succ op_level) proposal_level) in
    let* time_shift = Period.mult level_offset round_zero_duration in
    let* earliest_op_level_start =
      Timestamp.(proposal_predecessor_level_start +? time_shift)
    in
    (* computing the operations's round start from it's earliest
       possible level start *)
    let* op_earliest_ts =
      Round.timestamp_of_another_round_same_level
        round_durations
        ~current_round:Round.zero
        ~current_timestamp:earliest_op_level_start
        ~considered_round:op_round
    in
    (* We finally check that the expected time of the operation is
       acceptable *)
    acceptable ~drift ~op_earliest_ts ~now_timestamp

type level_and_round = {level : Raw_level.t; round : Round.t}

let pre_filter_far_future_consensus_ops info config
    ({level = op_level; round = op_round} : level_and_round) : bool Lwt.t =
  let open Result_syntax in
  let res =
    let now_timestamp = Time.System.now () |> Time.System.to_protocol in
    let* proposal_level = Raw_level.of_int32 info.head.level in
    acceptable_op
      ~config
      ~round_durations:info.round_durations
      ~round_zero_duration:info.round_zero_duration
      ~proposal_level
      ~proposal_round:info.head_round
      ~proposal_timestamp:info.head.timestamp
      ~proposal_predecessor_level_start:info.grandparent_level_start
      ~op_level
      ~op_round
      ~now_timestamp
  in
  match res with Ok b -> Lwt.return b | Error _ -> Lwt.return_false

let prefilter_consensus_operation info config level_and_round =
  let open Lwt_syntax in
  let* keep = pre_filter_far_future_consensus_ops info config level_and_round in
  if keep then return (`Passed_prefilter consensus_prio)
  else
    return
      (`Branch_refused
        [Environment.wrap_tzerror Consensus_operation_in_far_future])

(** A quasi infinite amount of "valid" (pre)attestations could be
      sent by a committee member, one for each possible round number.

      This filter rejects (pre)attestations that refer to a round
      that could not have been reached within the time span between
      the last head's timestamp and the current local clock.

      We add [config.clock_drift] time as a safety margin.
  *)
let pre_filter info config
    ({shell = _; protocol_data = Operation_data {contents; _} as op} :
      Main.operation) =
  let open Lwt_syntax in
  let prefilter_manager_op manager_op =
    return
    @@
    match pre_filter_manager info config op manager_op with
    | `Passed_prefilter prio -> `Passed_prefilter (manager_prio prio)
    | (`Branch_refused _ | `Branch_delayed _ | `Refused _ | `Outdated _) as err
      ->
        err
  in
  match contents with
  | Single (Failing_noop _) ->
      return (`Refused [Environment.wrap_tzerror Wrong_operation])
  | Single (Preattestation consensus_content)
  | Single (Attestation {consensus_content; dal_content = _}) ->
      let level_and_round : level_and_round =
        {level = consensus_content.level; round = consensus_content.round}
      in
      prefilter_consensus_operation info config level_and_round
  | Single (Seed_nonce_revelation _)
  | Single (Double_preattestation_evidence _)
  | Single (Double_attestation_evidence _)
  | Single (Double_baking_evidence _)
  | Single (Activate_account _)
  | Single (Proposals _)
  | Single (Vdf_revelation _)
  | Single (Drain_delegate _)
  | Single (Ballot _) ->
      return (`Passed_prefilter other_prio)
  | Single (Manager_operation _) as op -> prefilter_manager_op op
  | Cons (Manager_operation _, _) as op -> prefilter_manager_op op

let syntactic_check _ = Lwt.return `Well_formed

let is_manager_operation op =
  match Operation.acceptable_pass op with
  | Some pass -> Compare.Int.equal pass Operation_repr.manager_pass
  | None -> false

(* Should not fail on a valid manager operation. *)
let compute_fee_and_gas_limit {protocol_data = Operation_data data; _} =
  compute_manager_contents_fee_and_gas_limit data.contents

let gas_as_q gas = Gas.Arith.integral_to_z gas |> Q.of_bigint

let fee_and_ratio_as_q fee gas =
  let fee = Tez.to_mutez fee |> Z.of_int64 |> Q.of_bigint in
  let gas = gas_as_q gas in
  let ratio = Q.div fee gas in
  (fee, ratio)

let bumped_fee_and_ratio_as_q config fee gas =
  let bump = Q.mul config.replace_by_fee_factor in
  let fee, ratio = fee_and_ratio_as_q fee gas in
  (bump fee, bump ratio)

(** Determine whether the new manager operation is sufficiently better
    than the old manager operation to replace it. Sufficiently better
    means that the new operation's fee and fee/gas ratio are both
    greater than or equal to the old operation's same metrics bumped by
    the factor [config.replace_by_fee_factor]. *)
let better_fees_and_ratio config old_gas old_fee new_gas new_fee =
  let bumped_old_fee, bumped_old_ratio =
    bumped_fee_and_ratio_as_q config old_fee old_gas
  in
  let new_fee, new_ratio = fee_and_ratio_as_q new_fee new_gas in
  Q.compare new_fee bumped_old_fee >= 0
  && Q.compare new_ratio bumped_old_ratio >= 0

(** [conflict_handler config] returns a conflict handler for
    {!Mempool.add_operation} (see {!Mempool.conflict_handler}).

    - For non-manager operations, we select the greater operation
      according to {!Operation.compare}.

    - A manager operation is replaced only when the new operation's
      fee and fee/gas ratio both exceed the old operation's by at least a
      factor of [config.replace_by_fee_factor] (see {!better_fees_and_ratio}).

    Precondition: both operations must be individually valid (because
    of the call to {!Operation.compare}). *)
let conflict_handler config : Mempool.conflict_handler =
  let open Result_syntax in
  fun ~existing_operation ~new_operation ->
    let (_ : Operation_hash.t), old_op = existing_operation in
    let (_ : Operation_hash.t), new_op = new_operation in
    if is_manager_operation old_op && is_manager_operation new_op then
      let new_op_is_better =
        let* old_fee, old_gas_limit = compute_fee_and_gas_limit old_op in
        let* new_fee, new_gas_limit = compute_fee_and_gas_limit new_op in
        return
          (better_fees_and_ratio
             config
             old_gas_limit
             old_fee
             new_gas_limit
             new_fee)
      in
      match new_op_is_better with
      | Ok b when b -> `Replace
      | Ok _ | Error _ -> `Keep
    else if Operation.compare existing_operation new_operation < 0 then `Replace
    else `Keep

let int64_ceil_of_q q =
  let n = Q.to_int64 q in
  if Q.(equal q (of_int64 n)) then n else Int64.succ n

(* Compute the minimal fee (expressed in mutez) that [candidate_op]
   would need to have in order for the {!conflict_handler} to let it
   replace [op_to_replace], when both operations are manager
   operations.

   As specified in {!conflict_handler}, this means that [candidate_op]
   with the returned fee needs to have both its fee and its fee/gas
   ratio exceed (or match) [op_to_replace]'s same metrics bumped by
   the {!config}'s [replace_by_fee_factor].

   Return [None] when at least one operation is not a manager
   operation.

   Also return [None] if both operations are manager operations but
   there was an error while computing the needed fee. However, note
   that this cannot happen when both manager operations have been
   successfully validated by the protocol. *)
let fee_needed_to_replace_by_fee config ~op_to_replace ~candidate_op =
  let open Result_syntax in
  if is_manager_operation candidate_op && is_manager_operation op_to_replace
  then
    (let* _fee, candidate_gas = compute_fee_and_gas_limit candidate_op in
     let* old_fee, old_gas = compute_fee_and_gas_limit op_to_replace in
     if Gas.Arith.(old_gas = zero || candidate_gas = zero) then
       (* This should not happen when both operations are valid. *)
       return_none
     else
       let candidate_gas = gas_as_q candidate_gas in
       let bumped_old_fee, bumped_old_ratio =
         bumped_fee_and_ratio_as_q config old_fee old_gas
       in
       (* The new operation needs to exceed both the bumped fee and the
          bumped ratio to make {!better_fees_and_ratio} return [true].
          (Having fee or ratio equal to its bumped counterpart is ok too,
          hence the [ceil] in [int64_ceil_of_q].) *)
       let fee_needed_for_fee = int64_ceil_of_q bumped_old_fee in
       let fee_needed_for_ratio =
         int64_ceil_of_q Q.(bumped_old_ratio * candidate_gas)
       in
       return_some (max fee_needed_for_fee fee_needed_for_ratio))
    |> Option.of_result |> Option.join
  else None

let find_manager {shell = _; protocol_data = Operation_data {contents; _}} =
  match contents with
  | Single (Manager_operation {source; _}) -> Some source
  | Cons (Manager_operation {source; _}, _) -> Some source
  | Single
      ( Preattestation _ | Attestation _ | Proposals _ | Ballot _
      | Seed_nonce_revelation _ | Vdf_revelation _ | Double_baking_evidence _
      | Double_preattestation_evidence _ | Double_attestation_evidence _
      | Activate_account _ | Drain_delegate _ | Failing_noop _ ) ->
      None

(* The purpose of this module is to offer a version of
   [fee_needed_to_replace_by_fee] where the caller doesn't need to
   provide the [op_to_replace]. Instead, it needs to call
   [Conflict_map.update] every time a new operation is added to the
   mempool. This setup prevents the mempool plugin from exposing the
   notion of manager operations and their source. *)
module Conflict_map = struct
  (* The state consists in a map of all validated manager operations,
     indexed by their source.

     Note that the protocol already enforces that there is at most one
     operation per source.

     The state only tracks manager operations because other kinds of
     operations don't have fees that we might adjust to change the
     outcome of the {!conflict_handler}, so
     [fee_needed_to_replace_by_fee] will always return [None] when
     they are involved anyway. *)
  type t = packed_operation Signature.Public_key_hash.Map.t

  let empty = Signature.Public_key_hash.Map.empty

  (* Remove all the [replacements] from the state, then add
     [new_operation]. Non-manager operations are ignored.

     It is important to remove before adding: otherwise, we would
     remove the newly added operation when it has the same manager as
     one of the replacements. *)
  let update conflict_map ~new_operation ~replacements =
    let conflict_map =
      List.fold_left
        (fun conflict_map op ->
          match find_manager op with
          | Some manager ->
              Signature.Public_key_hash.Map.remove manager conflict_map
          | None -> (* Non-manager operation: ignore it. *) conflict_map)
        conflict_map
        replacements
    in
    match find_manager new_operation with
    | Some manager ->
        Signature.Public_key_hash.Map.add manager new_operation conflict_map
    | None -> (* Non-manager operation: ignore it. *) conflict_map

  let fee_needed_to_replace_by_fee config ~candidate_op ~conflict_map =
    match find_manager candidate_op with
    | None -> (* Non-manager operation. *) None
    | Some manager -> (
        match Signature.Public_key_hash.Map.find manager conflict_map with
        | None ->
            (* This can only happen when the pre-existing conflicting
               operation is a [Drain_delegate], which cannot be replaced by a
               manager operation regardless of the latter's fee. *)
            None
        | Some op_to_replace ->
            fee_needed_to_replace_by_fee config ~candidate_op ~op_to_replace)
end

let fee_needed_to_overtake ~op_to_overtake ~candidate_op =
  let open Result_syntax in
  if is_manager_operation candidate_op && is_manager_operation op_to_overtake
  then
    (let* _fee, candidate_gas = compute_fee_and_gas_limit candidate_op in
     let* target_fee, target_gas = compute_fee_and_gas_limit op_to_overtake in
     if Gas.Arith.(target_gas = zero || candidate_gas = zero) then
       (* This should not happen when both operations are valid. *)
       return_none
     else
       (* Compute the target ratio as in {!Operation_repr.weight_manager}.
          We purposefully don't use {!fee_and_ratio_as_q} because the code
          here needs to stay in sync with {!Operation_repr.weight_manager}
          rather than {!better_fees_and_ratio}. *)
       let target_fee = Q.of_int64 (Tez.to_mutez target_fee) in
       let target_gas = Q.of_bigint (Gas.Arith.integral_to_z target_gas) in
       let target_ratio = Q.(target_fee / target_gas) in
       (* Compute the minimal fee needed to have a strictly greater ratio. *)
       let candidate_gas =
         Q.of_bigint (Gas.Arith.integral_to_z candidate_gas)
       in
       return_some (Int64.succ Q.(to_int64 (target_ratio * candidate_gas))))
    |> Option.of_result |> Option.join
  else None

type ctxt = Protocol.Alpha_context.t

let get_context context ~(head : Tezos_base.Block_header.shell_header) =
  let open Lwt_result_syntax in
  let* ( ctxt,
         (_ : Receipt.balance_updates),
         (_ : Migration.origination_result list) ) =
    prepare
      context
      ~level:(Int32.succ head.level)
      ~predecessor_timestamp:head.timestamp
      ~timestamp:head.timestamp
    |> Lwt.map Environment.wrap_tzresult
  in
  return ctxt

let sources_from_operation ctxt
    ({shell = _; protocol_data = Operation_data {contents; _}} : Main.operation)
    =
  let open Lwt_syntax in
  match contents with
  | Single (Failing_noop _) -> return_nil
  | Single (Preattestation consensus_content)
  | Single (Attestation {consensus_content; dal_content = _}) -> (
      let level = Level.from_raw ctxt consensus_content.level in
      let* slot_owner =
        Stake_distribution.slot_owner ctxt level consensus_content.slot
      in
      match slot_owner with
      | Ok (_ctxt, {delegate; consensus_pkh; consensus_pk = _}) ->
          return [delegate; consensus_pkh]
      | Error _ -> return_nil)
  | Single (Seed_nonce_revelation _)
  | Single (Double_preattestation_evidence _)
  | Single (Double_attestation_evidence _)
  | Single (Double_baking_evidence _)
  | Single (Activate_account _)
  | Single (Vdf_revelation _) ->
      return_nil
  | Single (Proposals {source; _}) | Single (Ballot {source; _}) ->
      return [source]
  | Single (Drain_delegate {delegate; _}) -> return [delegate]
  | Single (Manager_operation {source; _}) -> return [source]
  | Cons (Manager_operation {source; _}, _) -> return [source]

module Internal_for_tests = struct
  let default_config_with_clock_drift clock_drift =
    {default_config with clock_drift}

  let default_config_with_replace_factor replace_by_fee_factor =
    {default_config with replace_by_fee_factor}

  let get_clock_drift {clock_drift; _} = clock_drift

  let acceptable_op = acceptable_op

  let fee_needed_to_replace_by_fee = fee_needed_to_replace_by_fee
end
