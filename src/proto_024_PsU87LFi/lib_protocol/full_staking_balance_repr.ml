(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module is responsible for the construction, observation and encoding of
    full staking balances that are maintained to be used at cycle end to compute
    staking rights.

    The module supports lazy migrations both from Oxford to Paris and
    from Paris to Q. See comments on [encoding] below.
*)

(** This type gathers the information we need to store so that we can
    provide the [min_delegated_in_cycle] public function (used for
    baking right computation at cycle end) and
    [Internal_for_tests_and_RPCs.min_delegated_and_level] (for
    the [<block>/context/delegates/min_delegated_in_current_cycle]
    RPC).

    [last_modified_level] is always the level of the last modification
    to [t.delegated], regardless of whether it was an increase or
    decrease. (If [t.delegated] has never been modified since its
    creation, then [last_modified_level] is the level at which it has
    been created, that is, when the delegate has set its delegation to
    itself.)

    Special case: when decoding a value that has been encoded during
    an older protocol than Q, [last_modified_level] is set to [0]; see
    [encoding] below for more details.

    [previous_min] contains the value and level of the min of
    [t.delegated] from the start of [last_modified_level.cycle], up
    until BEFORE any modifications that have happened during
    [last_modified_level].

    [previous_min] is [None] in three cases:

    - when [t.delegated] hasn't changed since the level of the
    delegate's creation, that is, the level at which the delegate has
    started self-delegating.

    - when [last_modified_level] is the first level of
    [last_modified_level.cycle]. Indeed, in this case, there are no
    values of [t.delegated] that are both before [last_modified_level]
    and in the same cycle as it.

    - when decoding a value that has been encoded during a protocol
    before Q; see [encoding] below for more details.

    Invariant:
    If [previous_min = Some (previous_min_value, previous_min_level)] then
    - [previous_min_level.cycle = last_modified_level.cycle] and
    - [previous_min_level < last_modified_level]

    This invariant is enforced thanks to the following
    post-conditions, that are satisfied by all functions that create
    or modify a [min_delegated_t], where [current_level] is the level
    during which the creation/modification happens.

    - [last_modified_level = current_level]

    - If [previous_min = Some (previous_min_value, previous_min_level)] then:
      - [previous_min_level.cycle = current_level.cycle] and
      - [previous_min_level < current_level]

    (Note: decoding a value that has been encoded during an old
    protocol does not satisfy [last_modified_level = current_level],
    but it always sets [previous_min] to [None] anyway, so the
    invariant still holds.)
*)
type min_delegated_t = {
  last_modified_level : Level_repr.t;
  previous_min : (Tez_repr.t * Level_repr.t) option;
}

type t = {
  own_frozen : Tez_repr.t;
  staked_frozen : Tez_repr.t;
  delegated : Tez_repr.t;
  min_delegated_in_cycle : min_delegated_t;
}

(* This ensures the {!min_delegated_t} creation post-conditions. *)
let init ~own_frozen ~staked_frozen ~delegated ~current_level =
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle =
      {last_modified_level = current_level; previous_min = None};
  }

(* This encoding is backward-compatible with both the encodings used
   in Oxford and in Paris, to allow for lazy migrations.

   In Oxford, {!field-min_delegated_in_cycle} didn't exist at all See
   [Full_staking_balance_repr_oxford] in
   [test/unit/test_full_staking_balance_repr.ml]. A decoded value was
   encoded during Oxford (or before) IFF the "min_delegated_in_cycle"
   field is [None].

   In Paris, {!field-min_delegated_in_cycle} was of type [Tez_repr.t],
   and there was another field [level_of_min_delegated : Level_repr.t
   option]. Both were stored inside the "min_delegated_in_cycle" field
   in the Paris encoding, with the following encoding:

   {[
    obj2
      (req "min_delegated_in_cycle" Tez_repr.encoding)
      (req "level_of_min_delegated" (option Level_repr.encoding))
   ]}

   A decoded value was encoded during Paris IFF the
   "min_delegated_in_cycle" field is present, but its
   "last_modified_level" field is [None].

   When reading any value that was encoded during a pre-Q protocol, we
   don't actually care about its min-delegated-related field. So we
   set [previous_min] to [None] and [last_modified_level] to zero, and
   we will treat them as coming from an old cycle.

   If this lazy migration happens in the middle of a cycle (which
   shouldn't be the case because the new min-delegated semantics is a
   new feature, not a bug fix, so it should be activated as a result
   of the voting process), this means the cycle that contained the
   migration, the min will be computed only during the Q portion of
   the cycle, not the whole cycle.

   Note that we can't use [previous_min_level = None] to signal a
   value that was read from an older protocol, because it already
   signifies that [previous_min = None] which has a semantic of its
   own.
*)
let encoding =
  let open Data_encoding in
  let min_delegated_in_cycle_encoding =
    (* This encoding must be backward-compatible with the [added_in_p]
       encoding of [Full_staking_balance_repr_paris.encoding] in
       [test/unit/test_full_staking_balance_repr.ml]. That's why
       "previous_min_level" has to be an option, whereas
       "previous_min_value" cannot be an option.

       - "previous_min_level" contains the level part of
       {!field-previous_min} when it's [Some _], otherwise [None].

       - "previous_min_value" contains the tez part of
       {!field-previous_min} when it's [Some _], otherwise
       [Tez_repr.zero].

       - "last_modified_level" contains
       {!field-last_modified_level}. So this field is always provided
       when using this encoding to serialize a
       {!min_delegated_t}. When decoding, if this field is missing,
       this means that the value comes from the Paris encoding.

       Since only the binary encoding is used in the context and not
       the json one, we use different names for the fields. Thus, this
       encoding is only *binary* backward-compatible with both the
       encodings used in Oxford and in Paris. *)
    obj3
      (req "previous_min_value" Tez_repr.encoding)
      (req "previous_min_level" (option Level_repr.encoding))
      (varopt "last_modified_level" Level_repr.encoding)
  in
  conv
    (fun {
           own_frozen;
           staked_frozen;
           delegated;
           min_delegated_in_cycle = {last_modified_level; previous_min};
         }
       ->
      let previous_min_value, previous_min_level =
        match previous_min with
        | Some (previous_min_value, previous_min_level) ->
            (previous_min_value, Some previous_min_level)
        | None -> (Tez_repr.zero, None)
      in
      ( own_frozen,
        staked_frozen,
        delegated,
        Some (previous_min_value, previous_min_level, Some last_modified_level)
      ))
    (fun (own_frozen, staked_frozen, delegated, min_delegated_in_cycle_opt) ->
      let min_delegated_in_cycle =
        match min_delegated_in_cycle_opt with
        | None (* Oxford encoding *) | Some (_, _, None) (* Paris encoding *) ->
            {
              last_modified_level = Level_repr.level_zero_use_with_care;
              (* In [min_delegated_and_level] and
                 [compute_new_previous_min], this will ensure that
                 [Cycle_repr.(last_modified_level.cycle <
                 current_level.cycle) && previous_min = None], in
                 which case [last_modified_level] will not be used any
                 further. *)
              previous_min = None;
            }
        | Some
            ( previous_min_value,
              previous_min_level_opt,
              Some last_modified_level ) -> (
            (* Protocol Q encoding *)
            match previous_min_level_opt with
            | None ->
                (* This means that [previous_min] is [None]; ignore
                   [previous_min_value] (which should be zero
                   anyway). *)
                {last_modified_level; previous_min = None}
            | Some previous_min_level ->
                {
                  last_modified_level;
                  previous_min = Some (previous_min_value, previous_min_level);
                })
      in
      {own_frozen; staked_frozen; delegated; min_delegated_in_cycle})
    (obj4
       (req "own_frozen" Tez_repr.encoding)
       (req "staked_frozen" Tez_repr.encoding)
       (req "delegated" Tez_repr.encoding)
       (varopt "min_delegated_in_cycle" min_delegated_in_cycle_encoding))

let voting_weight
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle = _} =
  let open Result_syntax in
  let* frozen = Tez_repr.(own_frozen +? staked_frozen) in
  let+ all = Tez_repr.(frozen +? delegated) in
  Tez_repr.to_mutez all

let apply_slashing ~percentage
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle} =
  let remaining_percentage = Percentage.neg percentage in
  let own_frozen =
    Tez_repr.mul_percentage ~rounding:`Down own_frozen remaining_percentage
  in
  let staked_frozen =
    Tez_repr.mul_percentage ~rounding:`Down staked_frozen remaining_percentage
  in
  {own_frozen; staked_frozen; delegated; min_delegated_in_cycle}

let own_frozen
    {own_frozen; staked_frozen = _; delegated = _; min_delegated_in_cycle = _} =
  own_frozen

let staked_frozen
    {own_frozen = _; staked_frozen; delegated = _; min_delegated_in_cycle = _} =
  staked_frozen

let total_frozen
    {own_frozen; staked_frozen; delegated = _; min_delegated_in_cycle = _} =
  Tez_repr.(own_frozen +? staked_frozen)

let current_delegated
    {own_frozen = _; staked_frozen = _; delegated; min_delegated_in_cycle = _} =
  delegated

(** Computes [min_delegated_in_cycle] (see mli), but also the level at
    the end of which the minimum of {!field-delegated} was reached.

    If the minimum was reached multiple times, returns the earliest
    level that reached it. For instance, if {!field-delegated} hasn't
    changed at all since the beginning of the cycle, returns the first
    level of the current cycle.

    This function assumes the current level is finished, that is,
    there are no more incoming balance updates for it. Indeed, this is
    true for both intended uses of this function, which are:

    - computing the baking rights at the end of a cycle

    - the min_delegated_in_current_cycle RPC

    Post-conditions:

    - [first_level_of_current_cycle <= returned_level <= current_level]

    - [delegated_at_end_of_block(returned_level) = returned_min]

    - for all [first_level_of_current_cycle <= level <= current_level],
      [delegated_at_end_of_block(level) >= returned_min]

    - for all [first_level_of_current_cycle <= level < returned_level],
      [delegated_at_end_of_block(level) > returned_min]
 *)
let min_delegated_and_level ~cycle_eras ~(current_level : Level_repr.t)
    {
      own_frozen = _;
      staked_frozen = _;
      delegated;
      min_delegated_in_cycle = {last_modified_level; previous_min};
    } =
  match previous_min with
  | Some (previous_min_value, previous_min_level)
    when Cycle_repr.(previous_min_level.cycle = current_level.cycle) ->
      (*       assert ( *)
      (*         Raw_level_repr.(previous_min_level.level < last_modified_level.level)) ; *)
      (* [previous_min_level] is from the current cycle, so it should
         be taken into consideration. The actual [min_delegated_value]
         is [min(previous_min_value, delegated)], reached in
         [previous_min_level] or [last_modified_level]
         respectively. In case both values are equal, we want to
         return the ealiest level, and we know from invariants that
         [previous_min_level < last_modified_level]. *)
      if Tez_repr.(previous_min_value <= delegated) then
        (previous_min_value, previous_min_level)
      else (delegated, last_modified_level)
  | Some (_previous_min_value, _previous_min_level) ->
      (*       assert ( *)
      (*         Raw_level_repr.(previous_min_level.level < last_modified_level.level)) ; *)
      (* Case: [previous_min_level.cycle < current_cycle].

         From {!min_delegated_t} invariant:
         [last_modified_level.cycle = previous_min_level.cycle < current_cycle].
         This means that [delegated] has not changed since the
         beginning of the current cycle, so the minimum is [delegated],
         reached in the first level of the current cycle. *)
      ( delegated,
        Level_repr.first_level_in_cycle_from_eras
          ~cycle_eras
          current_level.cycle )
  | None ->
      (* As described in {!min_delegated_t}, there are three possibilities:

         a. [delegated] has been unmodified since the level of the
            delegate's creation, which was [last_modified_level].
            This means that [min_delegated_value = delegated], and
            [min_delegated_level] is either the level of the
            delegate's creation if it happened during the current
            cycle, or the first level of the cycle. That is:
            [min_delegated_and_level =
               (delegated,
                if last_modified_level.cycle = current_cycle
                then last_modified_level
                else first_level_of_current_cycle)]

         b. [last_modified_level] is the first level of the current
            cycle, and [delegated] has not been modified since, so:
            [min_delegated_and_level = (delegated, last_modified_level)]

         c. The value has been decoded from an older protocol. This
            means that [delegated] has not changed since the activation of
            the current protocol, so:
            [min_delegated_and_level = (delegated, first_level_of_current_cycle)]

         Notice that in case b, we know that
         [last_modified_level.cycle = current_cycle] is true.
         Moreover, in case c, {!encoding} sets [last_modified_level]
         to zero; and it's the only way for [last_modified_level] to
         be zero, since level zero always belongs to the starting
         protocol (Genesis or 000). So all three cases a, b, and c
         boil down to:

         [min_delegated_and_level =
            ( delegated,
              if last_modified_level.cycle = current_cycle
                 && last_modified_level > 0
              then last_modified_level
              else first_level_of_current_cycle )]
      *)
      ( delegated,
        if
          Cycle_repr.(last_modified_level.cycle = current_level.cycle)
          && Level_repr.(last_modified_level > level_zero_use_with_care)
        then last_modified_level
        else
          Level_repr.first_level_in_cycle_from_eras
            ~cycle_eras
            current_level.cycle )

(** This is a public function; see mli.

    Note: This function could have been implemented as simply:
{[
let min_delegated_in_cycle
    {delegated; min_delegated_in_cycle = {previous_min; _}; _} =
  match previous_min with
  | Some (previous_min_value, _previous_min_level) ->
      Tez_repr.min previous_min_value delegated
  | None -> delegated
]}
    and the [~current_level] and [~cycle_eras] arguments would not
    have been needed.

    But [min_delegated_and_level] is not expensive to compute,
    and this implementation ensures that this function and the
    [<block>/context/delegates/min_delegated_in_current_cycle] RPC
    will stay in sync.
*)
let min_delegated_in_cycle ~cycle_eras ~current_level staking_balance =
  fst (min_delegated_and_level ~cycle_eras ~current_level staking_balance)

let current_total
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle = _} =
  let open Result_syntax in
  let* total_frozen = Tez_repr.(own_frozen +? staked_frozen) in
  Tez_repr.(total_frozen +? delegated)

let allowed_staked_frozen ~adaptive_issuance_global_limit_of_staking_over_baking
    ~delegate_limit_of_staking_over_baking_millionth
    {own_frozen; staked_frozen; delegated = _; min_delegated_in_cycle = _} =
  let global_limit_of_staking_over_baking_millionth =
    Int64.(
      mul
        1_000_000L
        (of_int adaptive_issuance_global_limit_of_staking_over_baking))
  in
  let limit_of_staking_over_baking_millionth =
    Compare.Int64.min
      global_limit_of_staking_over_baking_millionth
      (Int64.of_int32 delegate_limit_of_staking_over_baking_millionth)
  in
  match
    Tez_repr.mul_ratio
      ~rounding:`Down
      own_frozen
      ~num:limit_of_staking_over_baking_millionth
      ~den:1_000_000L
  with
  | Ok max_allowed_staked_frozen ->
      Tez_repr.min staked_frozen max_allowed_staked_frozen
  | Error _max_allowed_staked_frozen_overflows -> staked_frozen

let own_ratio ~adaptive_issuance_global_limit_of_staking_over_baking
    ~delegate_limit_of_staking_over_baking_millionth
    ({own_frozen; staked_frozen; delegated = _; min_delegated_in_cycle = _} as t)
    =
  if Tez_repr.(own_frozen = zero && staked_frozen = zero) then (1L, 1L)
  else if Tez_repr.(own_frozen = zero) then (0L, 1L)
  else
    let allowed_staked_frozen =
      allowed_staked_frozen
        ~adaptive_issuance_global_limit_of_staking_over_baking
        ~delegate_limit_of_staking_over_baking_millionth
        t
    in
    if Tez_repr.(allowed_staked_frozen = zero) then (1L, 1L)
    else
      let own_frozen = Tez_repr.to_mutez own_frozen in
      let allowed_staked_frozen = Tez_repr.to_mutez allowed_staked_frozen in
      (own_frozen, Int64.add own_frozen allowed_staked_frozen)

let has_minimal_frozen_stake ~minimal_frozen_stake full_staking_balance =
  let own_frozen = own_frozen full_staking_balance in
  Tez_repr.(own_frozen >= minimal_frozen_stake)

(* The set of delegates to consider [Active_delegates_with_minimal_stake] is an
   over-approximation of participating delegates. It is maintained by
   {!Stake_storage}.
   To avoid having to do any maintenance at cycle end, we have to rely on values
   that do not change when crossing cycle boundaries: the current amount works,
   the minimal in a given cycle wouldn't. *)
let has_minimal_stake_to_be_considered ~minimal_stake full_staking_balance =
  match current_total full_staking_balance with
  | Error _total_overflows ->
      true
      (* If the total overflows, we are definitely over the minimal stake. *)
  | Ok staking_balance -> Tez_repr.(staking_balance >= minimal_stake)

(* This function must ensure {!min_delegated_t} modification
   post-conditions on {!field-previous_min}. *)
let compute_new_previous_min ~cycle_eras ~(current_level : Level_repr.t)
    ~old_delegated {last_modified_level; previous_min} =
  if Level_repr.(last_modified_level = current_level) then
    (* Nothing to do; [previous_min] has already been correctly set
       the first time {!field-delegated} has been modified in the
       current level. *)
    previous_min
  else if
    Cycle_repr.(last_modified_level.cycle < current_level.cycle)
    || Level_repr.(last_modified_level = level_zero_use_with_care)
    (* (The later means that {!encoding} has read a value from an
       older protocol; we treat it as coming from a old cycle.) *)
  then
    (* {!field-delegated} has not been modified yet in the current
       cycle. In other words, it has been equal to [old_delegated]
       from the first level of the current cycle up until now. *)
    let first_level_of_current_cycle =
      Level_repr.first_level_in_cycle_from_eras ~cycle_eras current_level.cycle
    in
    if Level_repr.(current_level = first_level_of_current_cycle) then
      (* From {!type-min_delegated_t} spec, when
         [new_last_modified_level] is the first level of
         [current_level.cycle], [new_previous_min] is set to [None] *)
      None
    else
      (* As required by the post-conditions:
         - [new_previous_min_level] is in the current cycle
         - [new_previous_min_level < current_level] *)
      Some (old_delegated, first_level_of_current_cycle)
  else
    (*     assert (Cycle_repr.(last_modified_level.cycle = current_level.cycle)) ; *)
    match previous_min with
    | None ->
        (* From {!type-min_delegated_t} spec, either:
           - [last_modified_level] is the first level of its cycle, or
           - the delegate has been created during [last_modified_level].

           (The from-an-old-protocol-encoding case has been handled
            above, when we checked for [last_modified_level.cycle = 0].) *)
        Some (old_delegated, last_modified_level)
    | Some (previous_min_value, previous_min_level) ->
        (* From {!type-min_delegated_t} invariants,
           [previous_min_level.cycle = last_modified_level.cycle =
           current_level.cycle]. *)
        if Tez_repr.(old_delegated < previous_min_value) then
          Some (old_delegated, last_modified_level)
        else Some (previous_min_value, previous_min_level)

(* This function must ensure {!min_delegated_t} modification
   post-conditions. *)
let remove_delegated ~cycle_eras ~(current_level : Level_repr.t) ~amount
    {
      own_frozen;
      staked_frozen;
      delegated = old_delegated;
      min_delegated_in_cycle;
    } =
  let open Result_syntax in
  let+ new_delegated = Tez_repr.(old_delegated -? amount) in
  let new_previous_min =
    compute_new_previous_min
      ~cycle_eras
      ~current_level
      ~old_delegated
      min_delegated_in_cycle
  in
  {
    own_frozen;
    staked_frozen;
    delegated = new_delegated;
    min_delegated_in_cycle =
      {last_modified_level = current_level; previous_min = new_previous_min};
  }

let remove_own_frozen ~amount
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle} =
  let open Result_syntax in
  let+ own_frozen = Tez_repr.(own_frozen -? amount) in
  {own_frozen; staked_frozen; delegated; min_delegated_in_cycle}

let remove_staked_frozen ~amount
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle} =
  let open Result_syntax in
  let+ staked_frozen = Tez_repr.(staked_frozen -? amount) in
  {own_frozen; staked_frozen; delegated; min_delegated_in_cycle}

(* This function must ensure {!min_delegated_t} modification
   post-conditions. *)
let add_delegated ~cycle_eras ~(current_level : Level_repr.t) ~amount
    {
      own_frozen;
      staked_frozen;
      delegated = old_delegated;
      min_delegated_in_cycle;
    } =
  let open Result_syntax in
  let+ new_delegated = Tez_repr.(old_delegated +? amount) in
  let new_previous_min =
    compute_new_previous_min
      ~cycle_eras
      ~current_level
      ~old_delegated
      min_delegated_in_cycle
  in
  {
    own_frozen;
    staked_frozen;
    delegated = new_delegated;
    min_delegated_in_cycle =
      {last_modified_level = current_level; previous_min = new_previous_min};
  }

let add_own_frozen ~amount
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle} =
  let open Result_syntax in
  let+ own_frozen = Tez_repr.(own_frozen +? amount) in
  {own_frozen; staked_frozen; delegated; min_delegated_in_cycle}

let add_staked_frozen ~amount
    {own_frozen; staked_frozen; delegated; min_delegated_in_cycle} =
  let open Result_syntax in
  let+ staked_frozen = Tez_repr.(staked_frozen +? amount) in
  {own_frozen; staked_frozen; delegated; min_delegated_in_cycle}

module Internal_for_tests_and_RPCs = struct
  let min_delegated_and_level = min_delegated_and_level

  let last_modified_level
      {
        own_frozen = _;
        staked_frozen = _;
        delegated = _;
        min_delegated_in_cycle = {last_modified_level; previous_min = _};
      } =
    last_modified_level

  let previous_min
      {
        own_frozen = _;
        staked_frozen = _;
        delegated = _;
        min_delegated_in_cycle = {last_modified_level = _; previous_min};
      } =
    previous_min

  let init_raw ~own_frozen ~staked_frozen ~delegated ~last_modified_level
      ~previous_min =
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = {last_modified_level; previous_min};
    }
end
