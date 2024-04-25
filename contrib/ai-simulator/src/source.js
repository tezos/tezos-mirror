import bigRat from "big-rational";
import bigInt from "big-integer";

bigRat.min = (q1, q2) => (q1.leq(q2) ? q1 : q2);
bigRat.max = (q1, q2) => (q1.geq(q2) ? q1 : q2);
bigRat.clip = (q, qmin, qmax) => bigRat.max(qmin, bigRat.min(q, qmax));

/**
 * Constants used by Adaptive issuance.
 */
const seconds_per_day = 86_400;
const min_per_year = 525_600;

/**
 * Initial period in cycles during which issuance is bounded by the initial minimum and maximum values.
 */
const initial_period = 10;

/**
 * Transitional period in cycles during which issuance bounds are progressive, and after which
 * issuance is bounded by the final minimum and maximum values.
 */
const transition_period = 50;

/**
 * The initial minimum value (4.5%). At the time of Adaptive Issuance's activation,
 * the issuance rate is kept over this bound during the initial period.
 */
const issuance_ratio_initial_min = bigRat(45, 1000);

/**
 * The initial maximum value (5.5%). At the time of Adaptive Issuance's activation,
 * the issuance rate is kept below this bound during the initial period.
 */
const issuance_ratio_initial_max = bigRat(55, 1000);

/**
 * The final value (0.25%) for the lower bound, reached at the end of the transition period.
 */
const issuance_ratio_global_min = bigRat(25, 10000);

/**
 * The final value (10%) for the upper bound, reached at the end of the transition period.
 */
const issuance_ratio_global_max = bigRat(10, 100);

/**
 * The speed (0.01%) at which the dynamic rate adjusts. The value is set so that a one percentage
 * point deviation of the staked funds ratio changes the dynamic rate by 0.01 percentage
 * points per day.
 */
const growth_rate = bigRat(1, 100);

/**
 * The target staked funds ratio (50%).
 */
const ratio_target = bigRat(50, 100);

/**
 * The radius of the interval centered on the target ratio (2%).
 */
const ratio_radius = bigRat(2, 100);

/**
 * The Adaptive Issuance simulator.
 * @typedef {Object} Simulator
 * @property {function(number):boolean} is_ai_activated - Takes a cycle as argument
 * and returns true if adaptive issuance is activated, false otherwise.
 * @property {number} initial_period_start_cycle - Returns the cycle at which the launch of
 * the initial period  is set to happen.
 * @property {number} transition_period_start_cycle - Returns the cycle at which the launch of
 * the transition period  is set to happen.
 * @property {number} final_period_start_cycle - Returns the cycle at which the launch of
 * the final period  is set to happen.
 * @property {function(number):boolean} is_in_initial_period - Takes a cycle as argument
 * and returns true if belongs to the initial period, false otherwise.
 * @property {function(number):boolean} is_in_transition_period - Takes a cycle as argument
 * and returns true if belongs to the transition period, false otherwise.
 * @property {function(number):boolean} is_in_final_period - Takes a cycle as argument
 * and returns true if it is after the transition period, false otherwise.
 * @property {function(number):bigRat} staked_ratio_for_next_cycle - Returns the
 * staked ratio for the cycle following the specified cycle.
 * @property {function(number):bigRat} static_rate_for_next_cycle - Returns the
 * static rate for the cycle following the specified cycle.
 * @property {function(number):bigRat} dynamic_rate_for_next_cycle - Returns the
 * dynamic rate for the cycle following the specified cycle.
 * @property {function(number):bigRat} minimum_ratio - Returns the
 * minimum ratio corresponding to the given cycle.
 * @property {function(number):bigRat} maximum_ratio - Returns the
 * maximum ratio corresponding to the given cycle.
 * @property {function(number):bigRat} reward_coeff - Returns the
 * reward coefficient corresponding to the given cycle.
 * @property {function(number):bigRat} baking_reward_fixed_portion - Returns the
 * baking reward fixed portion corresponding to the given cycle, in mutez.
 * @property {function(number):bigRat} baking_reward_bonus_per_slot - Returns the
 * baking reward bonus per slot corresponding to the given cycle, in mutez.
 * @property {function(number):bigRat} attestation_reward_per_slot - Returns the
 * attestation reward per slot corresponding to the given cycle, in mutez.
 * @property {function(number):bigRat} seed_nonce_revelation_tip - Returns the
 * seed nonce revelation tip corresponding to the given cycle, in mutez.
 * @property {function(number):bigRat} vdf_revelation_tip - Returns the
 * vdf revelation tip corresponding to the given cycle, in mutez.
 * @property {function(number):bigRat} current_yearly_rate_value - Returns the
 * current yearly rate corresponding to the given cycle.
 * @property {function(number, number):null} set_staked_ratio_at - Takes a cycle and
 * a ratio as arguments. Sets the frozen balance by applying the ratio to the total supply
 * of the given cycle. The ratio is clipped between 0 and 1.
 * @property {function(number):bigInt} total_supply - Takes a cycle and
 * returns the total supply (in mutez) available on the chain
 * @property {function(number):bigInt} total_delegated_balance - Takes a cycle and
 * returns returns the total delegated amount (in mutez) on the chain
 * @property {function(number):bigInt} total_staked_balance - Takes a cycle and
 * returns the total stake (in mutez) frozen on the chain.
 * @property {function(number):number} issuance_per_block - Takes a cycle and
 * returns the issuance per block (in mutez) for this cycle.
 * @property {function(number):number} issuance_per_cycle - Takes a cycle and
 * returns the issuance (in mutez) for this cycle.
 */

/**
 * Simulator building.
 *
 * @constructor
 * @param {Object} config. - Configuration.
 * @param {Object} config.proto - Protocol constants.
 * @param {number} config.proto.minimal_block_delay.
 * @param {number} config.proto.blocks_per_cycle.
 * @param {number} config.proto.base_total_issued_per_minute.
 * @param {number} config.proto.attestation_rewards.
 * @param {number} config.proto.blocks_per_commitment.
 * @param {number} config.proto.fixed_baking_rewards.
 * @param {number} config.proto.bonus_baking_rewards.
 * @param {number} config.proto.nonce_revelation.
 * @param {number} config.proto.vdf_tip.
 * @param {number} config.proto.consensus_committee_size.
 * @param {number} config.proto.consensus_rights_delay.
 * @param {number} config.proto.consensus_threshold.
 * @param {number} config.proto.max_bonus.
 * @param {number} config.proto.max_limit_of_staking_over_baking.
 * @param {Object} config.chain - Data concerning the chain state.
 * @param {number} config.chain.ai_activation_cycle - Adaptive Issuance activation cycle.
 * @param {array(number)} config.chain.total_supply.
 * @param {array(number)} config.chain.total_frozen_stake.
 * @param {array(number)} config.chain.total_delegated.
 *
 * @return {Simulator}. The Adaptive Issuance simulator.
 */

export class Simulator {
  #storage_issuance_bonus = [];

  #storage_total_supply = [];
  #storage_total_supply_mask = [];

  #storage_total_delegated = [];
  #storage_total_delegated_mask = [];

  #storage_total_staked = [];
  #storage_total_staked_mask = [];

  #storage_cache_index = 0;

  set_total_supply(cycle, value) {
    this.#storage_cache_index = Math.min(cycle, this.#storage_cache_index);
    this.#storage_total_supply_mask[cycle] = value;
  }

  set_total_delegated(cycle, value) {
    this.#storage_cache_index = Math.min(cycle, this.#storage_cache_index);
    this.#storage_total_delegated_mask[cycle] = value;
  }

  set_total_staked(cycle, value) {
    this.#storage_cache_index = Math.min(cycle, this.#storage_cache_index);
    this.#storage_total_staked_mask[cycle] = value;
  }

  constructor(config) {
    this.config = config;
  }

  total_supply(cycle) {
    this.#prepare_for(cycle);
    return bigInt(this.#storage_total_supply[cycle]);
  }

  total_staked_balance(cycle) {
    if (cycle <= this.config.proto.consensus_rights_delay + 1) {
      return bigInt(
        this.#storage_total_staked_mask[cycle] ??
          this.#storage_total_staked[
            this.config.proto.consensus_rights_delay + 1
          ],
      );
    }
    // staked balances are set with adjusted cycles
    this.#prepare_for(cycle - this.config.proto.consensus_rights_delay - 1);
    return bigInt(this.#storage_total_staked[cycle]);
  }

  total_delegated_balance(cycle) {
    this.#prepare_for(cycle);
    return bigInt(this.#storage_total_delegated[cycle]);
  }

  #compute_extremum(cycle, initial_value, final_value) {
    const trans = transition_period + 1;
    const t1 = this.config.chain.ai_activation_cycle + initial_period;
    const t2 = t1 + trans;
    if (cycle <= t1) {
      return initial_value;
    } else if (cycle >= t2) {
      return final_value;
    } else {
      const t = cycle - t1;
      const res = bigRat(t)
        .multiply(final_value - initial_value)
        .divide(trans)
        .add(initial_value);
      return res;
    }
  }

  minimum_ratio(cycle) {
    return this.#compute_extremum(
      cycle,
      issuance_ratio_initial_min,
      issuance_ratio_global_min,
    );
  }

  maximum_ratio(cycle) {
    return this.#compute_extremum(
      cycle,
      issuance_ratio_initial_max,
      issuance_ratio_global_max,
    );
  }

  is_ai_activated(cycle) {
    return this.config.chain.ai_activation_cycle <= cycle;
  }

  get initial_period_start_cycle() {
    return this.config.chain.ai_activation_cycle;
  }

  get transition_period_start_cycle() {
    return this.config.chain.ai_activation_cycle + initial_period;
  }

  get final_period_start_cycle() {
    return (
      this.config.chain.ai_activation_cycle + initial_period + transition_period
    );
  }

  is_in_initial_period(cycle) {
    const l = this.config.chain.ai_activation_cycle <= cycle;
    const r = cycle <= initial_period + this.config.chain.ai_activation_cycle;
    return l && r;
  }

  is_in_transition_period(cycle) {
    const l = initial_period + this.config.chain.ai_activation_cycle < cycle;
    const r =
      cycle <=
      initial_period +
        transition_period +
        this.config.chain.ai_activation_cycle;
    return l && r;
  }

  is_in_final_period(cycle) {
    return (
      cycle >
      initial_period + transition_period + this.config.chain.ai_activation_cycle
    );
  }

  set_staked_ratio_at(cycle, value) {
    this.#prepare_for(cycle);
    this.#storage_issuance_bonus = [];
    this.#storage_cache_index = Math.min(cycle, this.#storage_cache_index);

    const ratio = bigRat.clip(bigRat(value), bigRat.zero, bigRat.one);
    const i = cycle + this.config.proto.consensus_rights_delay + 1;

    const total_supply = this.#storage_total_supply[i];
    const total_staked = this.#storage_total_staked[i];
    const total_delegated = this.#storage_total_delegated[i];

    const new_value = ratio.multiply(total_supply).ceil();

    const diff_staked = new_value - total_staked;
    const diff_delegated = this.#storage_total_delegated[i] - diff_staked;

    const new_delegated = Math.max(diff_delegated, 0);
    const new_staked =
      diff_delegated < 0 ? new_value + diff_delegated : new_value;

    this.#storage_total_staked_mask[i] = new_staked;
    this.#storage_total_delegated_mask[i] = new_delegated;
  }

  staked_ratio_for_next_cycle(cycle) {
    const total_supply = this.total_supply(cycle);
    const total_frozen_stake = this.total_staked_balance(
      cycle + this.config.proto.consensus_rights_delay + 1,
    );
    return bigRat(total_frozen_stake).divide(total_supply);
  }

  static_rate_for_next_cycle(cycle) {
    const next_cycle = cycle + 1;
    const staked_ratio = this.staked_ratio_for_next_cycle(cycle);
    const ratio_min = this.minimum_ratio(next_cycle);
    const ratio_max = this.maximum_ratio(next_cycle);
    const static_rate = staked_ratio.eq(0)
      ? ratio_max
      : bigRat(1, 1600).multiply(bigRat.one.divide(staked_ratio.pow(2)));
    return bigRat.clip(static_rate, ratio_min, ratio_max);
  }

  dynamic_rate_for_next_cycle(cycle) {
    if (cycle < this.config.chain.ai_activation_cycle) {
      return bigRat.zero;
    }
    if (this.#storage_issuance_bonus[cycle]) {
      return bigRat(this.#storage_issuance_bonus[cycle]);
    }
    const previous_bonus = this.dynamic_rate_for_next_cycle(cycle - 1);
    const staked_ratio = this.staked_ratio_for_next_cycle(cycle);
    const new_cycle = cycle + 1;
    const ratio_max = this.maximum_ratio(new_cycle);
    const static_rate = this.static_rate_for_next_cycle(cycle);
    const static_rate_dist_to_max = ratio_max.minus(static_rate);
    const udist = bigRat.max(
      bigRat.zero,
      staked_ratio.minus(ratio_target).abs().minus(ratio_radius),
    );
    const dist = staked_ratio.geq(ratio_target) ? udist.negate() : udist;
    const seconds_per_cycle =
      this.config.proto.blocks_per_cycle *
      this.config.proto.minimal_block_delay;
    const days_per_cycle = bigRat(seconds_per_cycle).divide(seconds_per_day);
    const max_new_bonus = bigRat.min(
      static_rate_dist_to_max,
      this.config.proto.max_bonus,
    );
    let new_bonus = previous_bonus.add(
      dist.multiply(growth_rate).multiply(days_per_cycle),
    );
    new_bonus = bigRat.clip(new_bonus, bigRat.zero, max_new_bonus);
    console.assert(0 <= new_bonus && new_bonus <= this.config.proto.max_bonus);
    this.#storage_issuance_bonus[cycle] = new_bonus;
    return bigRat(new_bonus);
  }

  issuance_rate_for_next_cycle(cycle) {
    const next_cycle = cycle + 1;
    const ratio_min = this.minimum_ratio(next_cycle);
    const ratio_max = this.maximum_ratio(next_cycle);
    const static_rate = this.static_rate_for_next_cycle(cycle);
    const bonus = this.dynamic_rate_for_next_cycle(cycle);
    return bigRat.clip(static_rate.add(bonus), ratio_min, ratio_max);
  }

  reward_coeff(cycle) {
    if (
      cycle <=
      this.config.chain.ai_activation_cycle +
        this.config.proto.consensus_rights_delay
    ) {
      return bigRat.one;
    }
    const adjusted_cycle = cycle - this.config.proto.consensus_rights_delay - 1;
    const issuance_rate = this.issuance_rate_for_next_cycle(adjusted_cycle);
    const total_supply = this.total_supply(adjusted_cycle);
    return issuance_rate.multiply(
      bigRat(total_supply).divide(
        bigRat(this.config.proto.base_total_issued_per_minute).multiply(
          min_per_year,
        ),
      ),
    );
  }

  #sum_rewards_weight() {
    return (
      this.config.proto.attestation_rewards +
      this.config.proto.fixed_baking_rewards +
      this.config.proto.bonus_baking_rewards +
      this.config.proto.nonce_revelation_tip +
      this.config.proto.vdf_tip
    );
  }

  #tez_from_weights(weight) {
    const num = bigInt(weight).multiply(this.config.proto.minimal_block_delay);
    const den = bigInt(this.#sum_rewards_weight()).multiply(60);
    const res = bigInt(this.config.proto.base_total_issued_per_minute)
      .multiply(num)
      .divide(den);
    return res;
  }

  #reward_from_constants(cycle, weight, d = 1) {
    const coeff = bigRat(this.reward_coeff(cycle));
    const rewards = this.#tez_from_weights(weight);
    const base_rewards = rewards.divide(d);
    return base_rewards.multiply(coeff.numerator).divide(coeff.denominator);
  }

  baking_reward_fixed_portion(cycle) {
    return this.#reward_from_constants(
      cycle,
      this.config.proto.fixed_baking_rewards,
    );
  }

  baking_reward_bonus_per_slot(cycle) {
    const bonus_committee_size =
      this.config.proto.consensus_committee_size -
      this.config.proto.consensus_threshold;
    if (bonus_committee_size == 0) {
      return 0;
    }
    return this.#reward_from_constants(
      cycle,
      this.config.proto.bonus_baking_rewards,
      bonus_committee_size,
    );
  }

  attestation_reward_per_slot(cycle) {
    return this.#reward_from_constants(
      cycle,
      this.config.proto.attestation_rewards,
      this.config.proto.consensus_committee_size,
    );
  }

  seed_nonce_revelation_tip(cycle) {
    return this.#reward_from_constants(
      cycle,
      this.config.proto.nonce_revelation_tip *
        this.config.proto.blocks_per_commitment,
    );
  }

  vdf_revelation_tip(cycle) {
    return this.#reward_from_constants(
      cycle,
      this.config.proto.vdf_tip * this.config.proto.blocks_per_commitment,
    );
  }

  #current_rewards_per_minute(cycle) {
    return this.reward_coeff(cycle).times(
      this.config.proto.base_total_issued_per_minute,
    );
  }

  current_yearly_rate_value(cycle) {
    return this.#current_rewards_per_minute(cycle)
      .divide(this.total_supply(cycle))
      .times(min_per_year)
      .times(100);
  }

  issuance_per_block(cycle) {
    const baking_reward_fixed_portion = this.baking_reward_fixed_portion(cycle);

    const baking_reward_bonus_per_block = this.#reward_from_constants(
      cycle,
      this.config.proto.bonus_baking_rewards,
    );

    const attestation_rewards_per_block = this.#reward_from_constants(
      cycle,
      this.config.proto.attestation_rewards,
    );

    const vdf_revelation_tip = this.#reward_from_constants(
      cycle,
      this.config.proto.vdf_tip,
    );

    const seed_nonce_revelation_tip = this.#reward_from_constants(
      cycle,
      this.config.proto.nonce_revelation_tip,
    );

    return (
      baking_reward_fixed_portion +
      baking_reward_bonus_per_block +
      attestation_rewards_per_block +
      vdf_revelation_tip +
      seed_nonce_revelation_tip
    );
  }

  issuance_per_cycle(cycle) {
    return this.issuance_per_block(cycle) * this.config.proto.blocks_per_cycle;
  }

  #compute_new_balances(cycle) {
    if (cycle < 0) {
      return; // should not happen
    }

    if (cycle == 0) {
      this.#storage_total_supply[0] = this.#storage_total_supply_mask[0];
      this.#storage_total_delegated[0] = this.#storage_total_delegated_mask[0];
      this.#storage_total_staked[this.config.proto.consensus_rights_delay + 1] =
        this.#storage_total_staked_mask[
          this.config.proto.consensus_rights_delay + 1
        ];
      return;
    }

    const issuance = this.issuance_per_cycle(cycle - 1);

    const new_supply =
      this.#storage_total_supply_mask[cycle] ??
      this.total_supply(cycle - 1).add(issuance);

    this.#storage_total_supply[cycle] = new_supply;

    const delegated = this.total_delegated_balance(cycle - 1);

    const staked = this.total_staked_balance(
      cycle + this.config.proto.consensus_rights_delay,
    );

    if (this.is_ai_activated(cycle)) {
      const ratio_for_delegated = bigRat(delegated).divide(
        staked.times(2).add(delegated),
      );

      const ratio_for_staked = bigRat(staked)
        .times(2)
        .divide(staked.times(2).add(delegated));

      const new_delegated =
        this.#storage_total_delegated_mask[cycle] ??
        ratio_for_delegated.multiply(issuance).add(delegated).round().valueOf();

      this.#storage_total_delegated[cycle] = new_delegated;

      const new_staked =
        this.#storage_total_staked_mask[
          cycle + this.config.proto.consensus_rights_delay + 1
        ] ?? ratio_for_staked.multiply(issuance).add(staked).round().valueOf();

      this.#storage_total_staked[
        cycle + this.config.proto.consensus_rights_delay + 1
      ] = new_staked;
    } else {
      const new_delegated =
        this.#storage_total_delegated_mask[cycle] ?? delegated.add(issuance);

      this.#storage_total_delegated[cycle] = new_delegated;

      const new_staked =
        this.#storage_total_staked_mask[
          cycle + this.config.proto.consensus_rights_delay + 1
        ] ??
        this.#storage_total_staked[
          cycle + this.config.proto.consensus_rights_delay
        ];

      this.#storage_total_staked[
        cycle + this.config.proto.consensus_rights_delay + 1
      ] = new_staked;
    }
  }

  #prepare_for(cycle) {
    for (let c = this.#storage_cache_index; c <= cycle; c++) {
      this.#compute_new_balances(c);
      this.#storage_cache_index++;
    }
  }
}

/**
 * Estimated rewards
 * @typedef {Object} EstimatedRewards
 * @property {number} estimated_number_of_blocks_baked - Estimated number of blocks baked by the delegate.
 * @property {number} estimated_number_of_attestations - Estimated number of attestations by the delegate.
 * @property {number} estimated_rewards_for_fixed_portion_baking - Estimated rewards from the
 * fixed portion rewards of baked blocks.
 * @property {number} estimated_rewards_for_baking_bonus - Estimated rewards from the
 * bonus portion rewards of baked blocks.
 * @property {number} estimated_rewards_for_attestations - Estimated rewards for attesting.
 * @property {number} estimated_rewards_for_nonce_revelation - Estimated rewards from the nonce
 * revelations.
 * @property {number} estimated_rewards_for_vdf_revelation - Estimated rewards from the vdf
 * revelations.
 * @property {number} estimated_rewards_from_own_staking - Delegate's estimated rewards from staking,
 * calculated by multiplying the overall staking rewards by the ratio of the delegate's own stake
 * to its total stake.
 * @property {number} estimated_rewards_from_third_party_staking - Third party stakers estimated rewards
 * from staking, calculated by multiplying the overall staking rewards by the ratio of the third party's
 * to the delegate's total stake.
 * @property {number} estimated_rewards_from_delegating - Estimated rewards produced by the delegate
 * baking power coming from its delegations.
 * @property {number} estimated_rewards_from_edge_of_baking_over_staking - Estimated rewards
 * given by the value of edge_of_baking_over_staking defined in the delegate's policy configuration.
 * @property {number} estimated_total_rewards - Estimated total rewards.
 */

/**
 * Delegate info
 * @typedef {Object} DelegateInfo
 * @property {bigInt} considered_staked - The amount of stake considered when computing baking rights.
 * @property {bigInt} over_staked - The portion of the stake that exceeds the delegate capacity.
 * @property {bigInt} available_staked - The portion of stake that the delegate can welcome without
 * being overstaked.
 * @property {bigInt} considered_delegated - The delegated amount considered when computing baking rights.
 * @property {bigInt} over_delegated - The portion of the delegated amount that exceeds the
 * delegate capacity.
 * @property {bigInt} available_delegated - The portion of stake that the delegate can welcome without
 * being overdelegated.
 */

/**
 * Baking Power
 * @typedef {Object} BakingPower
 * @property {bigRat} baking_power - The baking power of a delegate for a given cycle.
 * @property {number} ratio_for_staking - The ratio of the baking power given by staking.
 * @property {number} ratio_for_delegating - The ratio of the baking power given by delegating.
 */

/**
 * Delegate.
 * @typedef {Object} Delegate
 * @property {function():null} clear - clear delegate cache, useful when the simulator values change
 * and the delegate's estimated rewards need to be computed again.
 * @property {function(number):bigInt} estimated_own_staked_balance - Takes a cycle as argument
 * and returns the delegate's estimated own staked balance for the given cycle.
 * @property {function(number):bigInt} estimated_third_party_staked_balance - Takes a cycle as argument
 * and returns the delegate's estimated third party staked balance for the given cycle.
 * @property {function(number):bigInt} estimated_own_spendable_balance - Takes a cycle as argument
 * and returns the delegate's estimated own spendable balance for the given cycle.
 * @property {function(number):bigInt} third_party_delegated_balance - Takes a cycle as argument
 * and returns the delegate's third party delegated balance for the given cycle.
 * @property {function(number):EstimatedRewards} estimated_rewards - Takes a cycle as argument
 * and returns the delegate's estimated rewards for this cycle.
 * @property {function(number):BakingPower} baking_power -
 * @property {function(number):DelegateInfo} delegate_info -
 * @property {function(number, number):null} set_own_spendable_balance - Takes a cycle and an amount
 * (in mutez) as arguments, and sets the delegate's own spendable delegated balance to this value
 * for the given cycle.
 * @property {function(number, number):null} set_third_party_delegated_balance - Takes a cycle and an amount
 * (in mutez) as arguments, and sets the delegate's third party delegated balance to this value for the given
 * cycle.
 * @property {function(number, number):null} set_own_staked_balance - Takes a cycle and an amount
 * (in mutez) as arguments, and sets the delegate's own staked balance to this value for the given
 * cycle.
 * @property {function(number, number):null} set_third_party_staked_balance - Takes a cycle and an amount
 * (in mutez) as arguments, and sets the delegate's third party staked balance to this value for the given
 * cycle.
 * @property {function(number):boolean} is_activated - Takes a cycle as argument
 * and returns true if the delegate is activated for this cycle, false otherwise.
 */

/**
 * Delegate creation.
 *
 * @constructor
 * @param {Object} simulator - Simulator.
 * @param {Object} config - Delegate configuration file.
 * @param {number} config.delegate_registration_cycle - Cycle at which the delegate registers.
 * @param {Object} config.delegate_policy.
 * @param {number} config.delegate_policy.limit_of_staking_over_baking -
 * Limits the total amount of stake for the source's delegators as a
 * proportion of the source's own stake. Any amount exceeding this limit
 * is considered as delegation in the stake of the delegate. The value
 * should be between 0 and 5 (default 0 if not set).
 * @param {number} config.delegate_policy.edge_of_baking_over_staking -
 * The portion of the delegate's stakers rewards that goes to the delegate.
 * This edge, taken from the staker's rewards, is accrued to the delegate's own
 * frozen balance. This value must be between 0 and 1 (e.g. 0.05 for the delegate to receive 5%
 * of the staker's rewards). Default value is 1 (delegate receives all staker's rewards).
 *
 * @return {Delegate}. A delegate.
 */
export class Delegate {
  #storage_cache_index = 0;
  #registration_cycle = null;

  #storage_own_staked_balance = [0];
  #storage_third_party_staked_balance = [0];
  #storage_own_spendable_balance = [0];
  #storage_third_party_delegated_balance = [0];

  #storage_own_staked_balance_mask = [];
  #storage_third_party_staked_balance_mask = [];
  #storage_own_spendable_balance_mask = [];
  #storage_third_party_delegated_balance_mask = [];

  constructor(simulator, config) {
    this.simulator = simulator;
    this.config = config;
  }

  clear() {
    this.#storage_cache_index = 0;
    this.#storage_own_staked_balance = [0];
    this.#storage_third_party_staked_balance = [0];
    this.#storage_own_spendable_balance = [0];
    this.#storage_third_party_delegated_balance = [0];
  }

  #own_staked_balance(cycle) {
    return (
      this.#storage_own_staked_balance_mask[cycle] ??
      this.#storage_own_staked_balance[cycle]
    );
  }

  #third_party_staked_balance(cycle) {
    return (
      this.#storage_third_party_staked_balance_mask[cycle] ??
      this.#storage_third_party_staked_balance[cycle]
    );
  }

  #own_spendable_balance(cycle) {
    return (
      this.#storage_own_spendable_balance_mask[cycle] ??
      this.#storage_own_spendable_balance[cycle]
    );
  }

  #third_party_delegated_balance(cycle) {
    return (
      this.#storage_third_party_delegated_balance_mask[cycle] ??
      this.#storage_third_party_delegated_balance[cycle]
    );
  }

  // The storage_cache_index is used to register what are the values
  // still valid or the ones that need to be recomputed. Hence, for any change
  // that occurs at a certain level we need to recompute each data above this level.
  // This is why we we take the minimal value here

  set_own_spendable_balance(cycle, value) {
    this.#storage_own_spendable_balance_mask[cycle] = value;
    this.#storage_cache_index = Math.min(this.#storage_cache_index, cycle);
  }

  set_third_party_delegated_balance(cycle, value) {
    this.#storage_third_party_delegated_balance_mask[cycle] = value;
    this.#storage_cache_index = Math.min(this.#storage_cache_index, cycle);
  }

  set_own_staked_balance(cycle, value) {
    this.#storage_own_staked_balance_mask[cycle] = value;
    this.#storage_cache_index = Math.min(this.#storage_cache_index, cycle);
  }

  set_third_party_staked_balance(cycle, value) {
    if (!this.simulator.is_ai_activated(cycle)) {
      return;
    }
    this.#storage_third_party_staked_balance_mask[cycle] = value;
    this.#storage_cache_index = Math.min(this.#storage_cache_index, cycle);
  }

  is_activated(cycle) {
    /* Once registered a new delegate must wait CONSENSUS_RIGHTS_DELAY + 1 cycles
       for its rights to be considered. */
    const activation_cycle =
      this.config.delegate_registration_cycle +
      this.simulator.config.proto.consensus_rights_delay +
      1;

    return activation_cycle <= cycle;
  }

  delegate_info(cycle) {
    if (cycle < 0) {
      return {
        own_staked: bigInt.zero,
        considered_staked: bigInt.zero,
        over_staked: bigInt.zero,
        available_staked: bigInt.zero,
        considered_delegated: bigInt.zero,
        over_delegated: bigInt.zero,
        available_delegated: bigInt.zero,
      };
    }

    this.#prepare_for(cycle);

    const own_staked = bigInt(this.#own_staked_balance(cycle));
    const third_party_staked = bigInt(this.#third_party_staked_balance(cycle));

    const considered_limit_of_staking = Math.min(
      this.config.delegate_policy.limit_of_staking_over_baking,
      this.simulator.config.proto.max_limit_of_staking_over_baking,
    );

    const max_third_party_staked = own_staked.times(
      considered_limit_of_staking,
    );

    const considered_staked = bigInt.min(
      own_staked.add(third_party_staked),
      own_staked.add(max_third_party_staked),
    );

    const stake_diff = max_third_party_staked.minus(third_party_staked);

    const over_staked = stake_diff.isPositive()
      ? bigInt.zero
      : stake_diff.negate();

    const available_staked =
      this.simulator.is_ai_activated(cycle) && stake_diff.isPositive()
        ? stake_diff
        : bigInt.zero;

    const own_delegated = bigInt(this.#own_spendable_balance(cycle));
    const third_party_delegated = bigInt(
      this.#third_party_delegated_balance(cycle),
    );

    const extended_delegated = own_delegated
      .add(third_party_delegated)
      .add(over_staked);

    const max_delegated = own_staked.times(9);

    const delegated_diff = max_delegated.minus(extended_delegated);

    const over_delegated = delegated_diff.isPositive()
      ? bigInt.zero
      : delegated_diff.negate();

    const available_delegated = delegated_diff.isPositive()
      ? delegated_diff
      : bigInt.zero;

    const considered_delegated = bigInt.min(max_delegated, extended_delegated);

    return {
      own_staked,
      considered_staked,
      over_staked,
      available_staked,
      considered_delegated,
      over_delegated,
      available_delegated,
    };
  }

  baking_power(cycle) {
    /* The rights and rewards are set for cycle c + CONSENSUS_RIGHTS_DELAY + 1
       based on the aforementioned random seed and the current active stake. */
    const adjusted_cycle =
      cycle - this.simulator.config.proto.consensus_rights_delay - 1;

    if (adjusted_cycle < 0 || !this.is_activated(cycle)) {
      return {
        baking_power: bigRat.zero,
        ratio_for_staking: bigRat.zero,
        ratio_for_delegating: bigRat.zero,
      };
    }

    const { considered_staked, considered_delegated } =
      this.delegate_info(adjusted_cycle);

    const total_staked = this.simulator.total_staked_balance(adjusted_cycle);

    const total_delegated =
      this.simulator.total_delegated_balance(adjusted_cycle);

    const num = this.simulator.is_ai_activated(adjusted_cycle)
      ? considered_staked.times(2).add(considered_delegated)
      : considered_staked.add(considered_delegated);

    const den = this.simulator.is_ai_activated(adjusted_cycle)
      ? total_staked.times(2).add(total_delegated)
      : total_staked.add(total_delegated);

    const baking_power = bigRat(num).divide(den);

    const ratio_denum = considered_staked.times(2).add(considered_delegated);

    const ratio_for_staking =
      ratio_denum.isZero() || !this.simulator.is_ai_activated(adjusted_cycle)
        ? bigRat.zero
        : bigRat(considered_staked.times(2)).divide(ratio_denum);

    const ratio_for_delegating = ratio_denum.isZero()
      ? bigRat.zero
      : !this.simulator.is_ai_activated(adjusted_cycle)
        ? bigRat.one
        : bigRat(considered_delegated).divide(ratio_denum);

    return { baking_power, ratio_for_staking, ratio_for_delegating };
  }

  estimated_rewards(cycle) {
    const { baking_power, ratio_for_staking, ratio_for_delegating } =
      this.baking_power(cycle);

    const estimated_number_of_blocks_baked = baking_power
      .times(this.simulator.config.proto.blocks_per_cycle)
      .ceil()
      .valueOf();

    const estimated_rewards_for_fixed_portion_baking =
      estimated_number_of_blocks_baked *
      this.simulator.baking_reward_fixed_portion(cycle);

    const bonus_committee_size =
      this.simulator.config.proto.consensus_committee_size -
      this.simulator.config.proto.consensus_threshold;

    const estimated_rewards_for_baking_bonus =
      estimated_number_of_blocks_baked *
      this.simulator
        .baking_reward_bonus_per_slot(cycle)
        .times(bonus_committee_size);

    const estimated_number_of_attestations = baking_power
      .times(this.simulator.config.proto.consensus_committee_size)
      .times(this.simulator.config.proto.blocks_per_cycle)
      .ceil()
      .valueOf();

    const estimated_rewards_for_attestations =
      estimated_number_of_attestations *
      this.simulator.attestation_reward_per_slot(cycle);

    const estimated_rewards_for_nonce_revelation =
      (estimated_number_of_blocks_baked /
        this.simulator.config.proto.blocks_per_commitment) *
      this.simulator.seed_nonce_revelation_tip(cycle);

    const estimated_rewards_for_vdf_revelation =
      (estimated_number_of_blocks_baked /
        this.simulator.config.proto.blocks_per_commitment) *
      this.simulator.vdf_revelation_tip(cycle);

    const estimated_total_rewards =
      estimated_rewards_for_fixed_portion_baking +
      estimated_rewards_for_baking_bonus +
      estimated_rewards_for_attestations +
      estimated_rewards_for_nonce_revelation +
      estimated_rewards_for_vdf_revelation;

    const estimated_rewards_from_delegating = Math.ceil(
      ratio_for_delegating * estimated_total_rewards,
    );

    const estimated_rewards_from_staking = ratio_for_staking.times(
      estimated_total_rewards,
    );

    const { own_staked, considered_staked } = this.delegate_info(cycle);

    const ratio_own_stake = considered_staked.isZero()
      ? bigRat.zero
      : bigRat(own_staked, considered_staked);

    const ratio_third_party_stake = considered_staked.isZero()
      ? bigRat.zero
      : bigRat.one.minus(ratio_own_stake);

    const estimated_rewards_from_own_staking = ratio_own_stake
      .times(estimated_rewards_from_staking)
      .ceil()
      .valueOf();

    const estimated_rewards_from_third_party_staking_raw =
      ratio_third_party_stake.times(estimated_rewards_from_staking);

    const estimated_rewards_from_edge_of_baking_over_staking =
      estimated_rewards_from_third_party_staking_raw
        .times(this.config.delegate_policy.edge_of_baking_over_staking)
        .ceil()
        .valueOf();

    const estimated_rewards_from_third_party_staking =
      estimated_rewards_from_third_party_staking_raw
        .minus(estimated_rewards_from_edge_of_baking_over_staking)
        .ceil()
        .valueOf();

    return {
      estimated_number_of_blocks_baked,
      estimated_number_of_attestations,
      estimated_rewards_for_fixed_portion_baking,
      estimated_rewards_for_baking_bonus,
      estimated_rewards_for_attestations,
      estimated_rewards_for_nonce_revelation,
      estimated_rewards_for_vdf_revelation,
      estimated_rewards_from_own_staking,
      estimated_rewards_from_third_party_staking,
      estimated_rewards_from_delegating,
      estimated_rewards_from_edge_of_baking_over_staking,
      estimated_total_rewards,
    };
  }

  #compute_new_balances(cycle) {
    if (cycle < 0) {
      return; // should not happen
    }

    if (cycle == 0) {
      this.#storage_own_staked_balance[0] =
        this.#storage_own_staked_balance_mask[0] ?? 0;

      this.#storage_third_party_staked_balance[0] =
        this.#storage_third_party_staked_balance_mask[0] ?? 0;

      this.#storage_own_spendable_balance[0] =
        this.#storage_own_spendable_balance_mask[0] ?? 0;

      this.#storage_third_party_delegated_balance[0] =
        this.#storage_third_party_delegated_balance_mask[0] ?? 0;

      return;
    }

    const rewards = this.estimated_rewards(cycle - 1);

    if (!this.simulator.is_ai_activated(cycle)) {
      const previous_own_staked_balance =
        this.#storage_own_staked_balance[cycle - 1];

      const previous_third_party_delegated =
        this.#storage_third_party_delegated_balance[cycle - 1];

      const previous_own_spendable_balance =
        this.#storage_own_spendable_balance[cycle - 1];

      const previous_total_delegated =
        previous_own_spendable_balance + previous_third_party_delegated;

      const new_own_staked_balance = Math.round(
        (1 / 10) *
          (previous_own_staked_balance +
            previous_total_delegated +
            rewards.estimated_total_rewards),
      );

      const new_own_spendable_balance =
        previous_own_spendable_balance -
        (previous_own_staked_balance - new_own_staked_balance);

      this.#storage_own_staked_balance[cycle] =
        this.#storage_own_staked_balance_mask[cycle] ?? new_own_staked_balance;

      this.#storage_own_spendable_balance[cycle] =
        this.#storage_own_spendable_balance_mask[cycle] ??
        new_own_spendable_balance;

      this.#storage_third_party_delegated_balance[cycle] =
        this.#storage_third_party_delegated_balance_mask[cycle] ??
        this.#storage_third_party_delegated_balance[cycle - 1];

      this.#storage_third_party_staked_balance[cycle] =
        this.#storage_third_party_staked_balance_mask[cycle] ??
        this.#storage_third_party_staked_balance[cycle - 1];
    } else {
      this.#storage_own_staked_balance[cycle] =
        this.#storage_own_staked_balance_mask[cycle] ??
        this.#storage_own_staked_balance[cycle - 1] +
          rewards.estimated_rewards_from_own_staking +
          rewards.estimated_rewards_from_edge_of_baking_over_staking;

      this.#storage_third_party_staked_balance[cycle] =
        this.#storage_third_party_staked_balance_mask[cycle] ??
        this.#storage_third_party_staked_balance[cycle - 1] +
          rewards.estimated_rewards_from_third_party_staking;

      this.#storage_own_spendable_balance[cycle] =
        this.#storage_own_spendable_balance_mask[cycle] ??
        this.#storage_own_spendable_balance[cycle - 1] +
          rewards.estimated_rewards_from_delegating;

      this.#storage_third_party_delegated_balance[cycle] =
        this.#storage_third_party_delegated_balance_mask[cycle] ??
        this.#storage_third_party_delegated_balance[cycle - 1];
    }
  }

  #prepare_for(cycle) {
    for (let i = this.#storage_cache_index; i <= cycle; i++) {
      this.#compute_new_balances(i);
      this.#storage_cache_index++;
    }
  }

  estimated_own_staked_balance(cycle) {
    this.#prepare_for(cycle);
    return this.#own_staked_balance(cycle);
  }

  estimated_third_party_staked_balance(cycle) {
    this.#prepare_for(cycle);
    return this.#third_party_staked_balance(cycle);
  }

  estimated_own_spendable_balance(cycle) {
    this.#prepare_for(cycle);
    return this.#own_spendable_balance(cycle);
  }

  third_party_delegated_balance(cycle) {
    this.#prepare_for(cycle);
    return this.#third_party_delegated_balance(cycle);
  }

  ratio_own_staked_spendable_balance(cycle) {
    this.#prepare_for(cycle);
    return (
      100 *
      (this.#own_staked_balance(cycle) /
        (this.#own_staked_balance(cycle) + this.#own_spendable_balance(cycle)))
    );
  }

  ratio_third_party_staked_delegated_balance(cycle) {
    this.#prepare_for(cycle);
    return (
      100 *
      (this.#third_party_staked_balance(cycle) /
        (this.#third_party_delegated_balance(cycle) +
          this.#third_party_staked_balance(cycle)))
    );
  }

  set_ratio_own_staked_spendable_balance(cycle, value) {
    if (!this.simulator.is_ai_activated(cycle)) {
      return;
    }
    const staked = this.#own_staked_balance(cycle);
    const spendable = this.#own_spendable_balance(cycle);
    const den = staked + spendable;
    const new_staked = Math.round((value / 100) * den);
    const new_spendable = den - new_staked;
    this.set_own_staked_balance(cycle, new_staked);
    this.set_own_spendable_balance(cycle, new_spendable);
  }

  set_ratio_third_party_staked_delegated_balance(cycle, value) {
    if (!this.simulator.is_ai_activated(cycle)) {
      return;
    }
    if (this.config.delegate_policy.limit_of_staking_over_baking == 0) {
      return;
    }
    const staked = this.#third_party_staked_balance(cycle);
    const delegated = this.#third_party_delegated_balance(cycle);
    const den = staked + delegated;
    const new_staked = Math.round((value / 100) * den);
    const new_delegated = den - new_staked;
    this.set_third_party_staked_balance(cycle, new_staked);
    this.set_third_party_delegated_balance(cycle, new_delegated);
  }
}
