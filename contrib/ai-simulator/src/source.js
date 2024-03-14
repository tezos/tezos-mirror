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

/* Helpers */

const safe_get = (array, cycle) => {
  if (cycle < 0) {
    throw new Error(
      `Querying data for a negative cycle (${cycle}) is not allowed.`,
    );
  }
  if (cycle >= array.length) {
    throw new Error(
      `Querying data for a cycle (${cycle}) that is beyond the maximum cycle, defined as ${array.length}.`,
    );
  }
  return array[cycle];
};

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
 * @param {Object} config.chain - Data concerning the chain state.
 * @param {number} config.chain.ai_activation_cycle - Adaptive Issuance activation cycle.
 * @param {array(number)} config.chain.total_supply.
 * @param {array(number)} config.chain.total_frozen_stake.
 *
 * @return {Simulator}. The Adaptive Issuance simulator.
 */

export class Simulator {

  constructor(config) {
    this.config = config;
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
}
