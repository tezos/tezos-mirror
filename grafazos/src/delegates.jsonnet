// Copyright (c) 2022-2024 Nomadic Labs <contact@nomadic-labs.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local query = grafonnet.query;
local panel = grafonnet.panel;
local stat = panel.stat;
local gauge = panel.gauge;
local timeSeries = panel.timeSeries;

// Base
local base = import './base.jsonnet';
local graph = base.graph;

//##
// Octez delegate related stats
//
// Every metric queried here is a 1:1 extraction of a field of the
// `/chains/main/blocks/head/context/delegates/<pkh>` RPC response: the metric
// name is `octez_delegate_` followed by the flattened (dot -> underscore)
// JSON path of the field. Balances are in mutez, hence the `/ 1e6` in the
// queries that display XTZ.
//
// Series are expected to carry a `delegate` label (the address) and a
// `delegate_name` label (a friendly alias), as produced by the
// json-exporter-based delegate monitoring of the `octez-node` Helm chart.
//##

// Shorthands over the selected node instance + delegates.
local m(metric) = metric + base.delegate_instance_query;
local prom(expr, legend='{{ delegate_name }}') =
  query.prometheus.new(base.datasource, expr)
  + query.prometheus.withLegendFormat(legend);

// Thresholds helper for gauge panels; [thresholds] is a [value, color] list.
local gaugeThresholds(thresholds) =
  local f(t) =
    gauge.standardOptions.threshold.step.withValue(t[0])
    + gauge.standardOptions.threshold.step.withColor(t[1]);
  gauge.standardOptions.thresholds.withMode('absolute')
  + gauge.standardOptions.thresholds.withSteps(std.map(f, thresholds));

{

  // ============================================================================
  // Overview - Activity & Status
  // ============================================================================

  deactivatedStatus(h, w, x, y):
    base.info.new('Delegate Status', prom(m('octez_delegate_deactivated')), h, w, x, y)
    + base.info.withName('value_and_name')
    + base.info.withMapping([['0', 'Active', 'green'], ['1', 'Deactivated', 'red']])
    + stat.panelOptions.withDescription('Whether the protocol considers the delegate active. Deactivated (red) means it stopped attesting for a whole grace period: it no longer receives rights and must re-register to bake again.'),

  forbiddenStatus(h, w, x, y):
    base.info.new('Forbidden Status', prom(m('octez_delegate_is_forbidden')), h, w, x, y)
    + base.info.withName('value_and_name')
    + base.info.withMapping([['0', 'Allowed', 'green'], ['1', 'Forbidden', 'red']])
    + stat.panelOptions.withDescription('Whether the protocol forbids this delegate from baking/attesting, the consequence of a double-signing denunciation. Forbidden (red) requires immediate investigation of the baker setup (duplicate baker instances, key reuse).'),

  dalSufficientStatus(h, w, x, y):
    base.info.new('DAL Participation', prom(m('octez_delegate_dal_participation_sufficient_dal_participation')), h, w, x, y)
    + base.info.withName('value_and_name')
    + base.info.withMapping([['0', 'Insufficient', 'red'], ['1', 'Sufficient', 'green']])
    + stat.panelOptions.withDescription('Whether DAL attestation activity in the current cycle is above the protocol sufficiency threshold. Insufficient (red) means the cycle DAL rewards are at risk — check the DAL node health and connectivity to the baker.'),

  dalDenouncedStatus(h, w, x, y):
    base.info.new('DAL Denounced', prom(m('octez_delegate_dal_participation_denounced')), h, w, x, y)
    + base.info.withName('value_and_name')
    + base.info.withMapping([['0', 'Clear', 'green'], ['1', 'Denounced', 'red']])
    + stat.panelOptions.withDescription('Whether the delegate was denounced for DAL misbehavior in the current cycle (attesting DAL slots without the data). Clear (green) is nominal; Denounced (red) costs rewards and points at a broken DAL setup.'),

  gracePeriod(h, w, x, y):
    base.info.new('Grace Period (cycle)', prom(m('octez_delegate_grace_period')), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('none')
    + stat.panelOptions.withDescription('Cycle at which the delegate gets deactivated unless it participates again. Compare with the current cycle: the closer this value, the closer the deactivation; it moves forward every cycle the delegate is active.'),

  bakingPower(h, w, x, y):
    base.info.new('Baking Power (XTZ)', prom(m('octez_delegate_baking_power') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Stake-weighted power used to compute baking and attestation rights (own + external stake, plus delegated funds at reduced weight, within protocol limits). A drop here translates into fewer rights a few cycles later.'),

  // ============================================================================
  // Staking & Balances
  // ============================================================================

  totalStaked(h, w, x, y):
    base.info.new('Total Staked (XTZ)', prom(m('octez_delegate_total_staked') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Total frozen stake backing the delegate (own + external stakers). Staked funds count fully toward baking power and are slashable on misbehavior.'),

  totalDelegated(h, w, x, y):
    base.info.new('Total Delegated (XTZ)', prom(m('octez_delegate_total_delegated') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Total non-staked funds delegated to this delegate. Delegated funds count toward baking power at a reduced weight and are not slashable.'),

  ownFullBalance(h, w, x, y):
    base.info.new('Own Full Balance (XTZ)', prom(m('octez_delegate_own_full_balance') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('The delegate own funds, spendable + frozen. A sudden unexplained drop deserves investigation (unexpected transfer, slashing).'),

  minDelegatedInCurrentCycle(h, w, x, y):
    base.info.new('Min Delegated in Current Cycle (XTZ)', prom(m('octez_delegate_min_delegated_in_current_cycle_amount') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Lowest delegated amount observed during the current cycle — the value used for rights computation, so a temporary dip lowers future rights even if the funds come back.'),

  stakingComposition(h, w, x, y):
    local targets = [
      prom(m('octez_delegate_own_staked') + ' / 1e6', '{{ delegate_name }} - own staked'),
      prom(m('octez_delegate_external_staked') + ' / 1e6', '{{ delegate_name }} - external staked'),
    ];
    graph.new('Staking Composition (XTZ)', targets, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull'])
    + timeSeries.panelOptions.withDescription('Own vs external staked funds over time. External stake is capped at the staking-over-baking limit times the own stake: if own stake shrinks, external stake above the cap stops earning full rights. Watch for large external departures.'),

  delegationComposition(h, w, x, y):
    local targets = [
      prom(m('octez_delegate_own_delegated') + ' / 1e6', '{{ delegate_name }} - own delegated'),
      prom(m('octez_delegate_external_delegated') + ' / 1e6', '{{ delegate_name }} - external delegated'),
    ];
    graph.new('Delegation Composition (XTZ)', targets, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull'])
    + timeSeries.panelOptions.withDescription('Own vs external delegated funds over time. Large external departures reduce baking power in the following cycles; the minimum over the cycle (not the instant value) is what rights are computed from.'),

  // ============================================================================
  // Participation (current cycle)
  // ============================================================================

  slotSuccessRate(h, w, x, y):
    local q = prom(
      '(1 - (' + m('octez_delegate_participation_missed_slots')
      + ' / ' + m('octez_delegate_participation_expected_cycle_activity')
      + ')) * 100'
    );
    gauge.new('Attested Slots (%)')
    + gauge.panelOptions.withGridPos(h, w, x, y)
    + gauge.panelOptions.withDescription('Share of assigned attestation slots not missed in the current cycle. Falling below the minimal cycle activity (~2/3) forfeits attesting rewards.')
    + gauge.queryOptions.withTargets(q)
    + gauge.standardOptions.withUnit('percent')
    + gauge.standardOptions.withMin(0)
    + gauge.standardOptions.withMax(100)
    + gaugeThresholds([[0, 'red'], [66.7, 'yellow'], [90, 'green']]),

  remainingAllowedMissedSlots(h, w, x, y):
    base.info.new('Remaining Allowed Missed Slots', prom(m('octez_delegate_participation_remaining_allowed_missed_slots')), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Slots the delegate can still miss this cycle before losing attesting rewards. Reaching 0 means the cycle rewards are already forfeited; the counter resets at the next cycle.'),

  missedLevels(h, w, x, y):
    base.info.new('Missed Levels', prom(m('octez_delegate_participation_missed_levels')), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Levels of the current cycle where the delegate missed all of its attestation slots. Sustained growth points at a baker liveness or connectivity problem rather than isolated packet loss.'),

  expectedAttestingRewards(h, w, x, y):
    base.info.new('Expected Attesting Rewards (XTZ)', prom(m('octez_delegate_participation_expected_attesting_rewards') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Attesting rewards the delegate receives at the end of the current cycle, provided its activity stays above the minimal threshold — they drop to zero if too many slots are missed.'),

  participationActivity(h, w, x, y):
    local targets = [
      prom(m('octez_delegate_participation_expected_cycle_activity'), '{{ delegate_name }} - expected'),
      prom(m('octez_delegate_participation_minimal_cycle_activity'), '{{ delegate_name }} - minimal'),
      prom(m('octez_delegate_participation_missed_slots'), '{{ delegate_name }} - missed'),
    ];
    graph.new('Participation Activity (slots)', targets, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull'])
    + timeSeries.panelOptions.withDescription('Attestation slots in the current cycle: expected (assigned), minimal (threshold to keep rewards) and missed. Counters reset at each cycle. Rewards are lost once missed exceeds expected minus minimal — i.e. keep the missed line near zero.'),

  participationRate(h, w, x, y):
    local q = prom(
      '(1 - (' + m('octez_delegate_participation_missed_slots')
      + ' / ' + m('octez_delegate_participation_expected_cycle_activity')
      + ')) * 100'
    );
    graph.new('Participation Rate Over Time (%)', q, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull', 'min'])
    + timeSeries.panelOptions.withDescription('Share of assigned attestation slots successfully attested, over time. A healthy baker stays near 100%; dips align with downtime windows. Below the minimal cycle activity (~2/3) the cycle attesting rewards are forfeited.'),

  // ============================================================================
  // DAL Participation (current cycle)
  // ============================================================================

  dalSuccessRate(h, w, x, y):
    local q = prom(
      '(' + m('octez_delegate_dal_participation_delegate_attested_dal_slots')
      + ' / ' + m('octez_delegate_dal_participation_delegate_attestable_dal_slots')
      + ') * 100'
    );
    gauge.new('DAL Attested Slots (%)')
    + gauge.panelOptions.withGridPos(h, w, x, y)
    + gauge.panelOptions.withDescription('Share of attestable DAL slots attested in the current cycle. The sufficiency threshold is a protocol parameter (~2/3).')
    + gauge.queryOptions.withTargets(q)
    + gauge.standardOptions.withUnit('percent')
    + gauge.standardOptions.withMin(0)
    + gauge.standardOptions.withMax(100)
    + gaugeThresholds([[0, 'red'], [66.7, 'yellow'], [90, 'green']]),

  dalExpectedShardsPerSlot(h, w, x, y):
    base.info.new('Expected Assigned Shards per Slot', prom(m('octez_delegate_dal_participation_expected_assigned_shards_per_slot')), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('DAL shards the delegate is expected to attest per slot, proportional to its stake. Context for the attested/attestable counters — a larger delegate carries more shards and its misses weigh more.'),

  expectedDalRewards(h, w, x, y):
    base.info.new('Expected DAL Rewards (XTZ)', prom(m('octez_delegate_dal_participation_expected_dal_rewards') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('DAL rewards the delegate receives at the end of the current cycle, provided DAL participation stays sufficient — insufficient participation or a DAL denunciation forfeits them.'),

  dalSlots(h, w, x, y):
    local targets = [
      prom(m('octez_delegate_dal_participation_delegate_attestable_dal_slots'), '{{ delegate_name }} - attestable'),
      prom(m('octez_delegate_dal_participation_delegate_attested_dal_slots'), '{{ delegate_name }} - attested'),
    ];
    graph.new('DAL Slots (current cycle)', targets, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull'])
    + timeSeries.panelOptions.withDescription('Attestable vs attested DAL slots in the current cycle (counters reset each cycle). The two lines should climb together; a widening gap means the baker attests L1 without seeing the DAL data — check the DAL node and its connection to the baker.'),

  dalStatusHistory(h, w, x, y):
    local targets = [
      prom(m('octez_delegate_dal_participation_sufficient_dal_participation'), '{{ delegate_name }} - sufficient'),
      prom(m('octez_delegate_dal_participation_denounced'), '{{ delegate_name }} - denounced'),
    ];
    graph.new('DAL Participation Status (1 = true)', targets, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull'])
    + timeSeries.panelOptions.withDescription('History of the DAL flags: sufficient should sit at 1 and denounced at 0. sufficient flapping to 0 reveals intermittent DAL trouble even when the current-cycle gauge looks fine; any denounced=1 costs the cycle rewards.'),

  // ============================================================================
  // Voting & Governance
  // ============================================================================

  votingPower(h, w, x, y):
    local targets = [
      prom(m('octez_delegate_voting_power') + ' / 1e6', '{{ delegate_name }} - voting power'),
      prom(m('octez_delegate_current_voting_power') + ' / 1e6', '{{ delegate_name }} - current voting power'),
    ];
    graph.new('Voting Power (XTZ)', targets, h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull'])
    + timeSeries.panelOptions.withDescription('voting power is the snapshot used in the current governance period; current voting power is the live value that will apply from the next period. A gap between the two simply reflects stake movements since the snapshot.'),

  remainingProposals(h, w, x, y):
    base.info.new('Remaining Proposals', prom(m('octez_delegate_voting_info_remaining_proposals')), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Protocol proposals the delegate can still submit in the current governance period — only meaningful during a Proposal period; it resets when a new period starts.'),

  // ============================================================================
  // Staking Parameters & Risk
  // ============================================================================

  stakingLimitOverBaking(h, w, x, y):
    base.info.new('Limit of Staking over Baking', prom(m('octez_delegate_active_staking_parameters_limit_of_staking_over_baking_millionth') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Maximum external stake accepted, as a multiple of the delegate own stake'),

  edgeOfBakingOverStaking(h, w, x, y):
    base.info.new('Edge of Baking over Staking', prom(m('octez_delegate_active_staking_parameters_edge_of_baking_over_staking_billionth') + ' / 1e9'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('percentunit')
    + stat.panelOptions.withDescription('Share of the stakers rewards kept by the delegate'),

  stakingDenominator(h, w, x, y):
    base.info.new('Staking Denominator (XTZ)', prom(m('octez_delegate_staking_denominator') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.panelOptions.withDescription('Total staking pseudotokens issued by this delegate, the denominator converting a staker share into tez. Moves with stake/unstake events; mostly useful when auditing individual staker balances.'),

  pendingSlashedAmount(h, w, x, y):
    base.info.new('Pending Slashed Amount (XTZ)', prom(m('octez_delegate_estimated_shared_pending_slashed_amount') + ' / 1e6'), h, w, x, y, instant=false)
    + stat.standardOptions.withUnit('short')
    + stat.options.withColorMode('value')
    + base.info.withThreshold([[0, 'green'], [0.000001, 'red']])
    + stat.panelOptions.withDescription('Estimated amount to be slashed following pending denunciations; anything above 0 needs attention'),

}
