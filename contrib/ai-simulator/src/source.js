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
