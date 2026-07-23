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
local dashboard = grafonnet.dashboard;
local panel = grafonnet.panel;

// Base
local base = import './base.jsonnet';
local delegates = import './delegates.jsonnet';

// External variables
local uid_ext = std.extVar('uid_ext');
local uid = uid_ext == 'default';

// Row positions
local overview_y = 0;
local staking_y = 5;
local participation_y = 18;
local dal_y = 31;
local voting_y = 44;
local params_y = 54;

//###
// Octez Delegates Dashboard
//###
dashboard.new('Octez delegates dashboard' + if !uid && uid_ext != '' then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ (if !uid then dashboard.withUid('octez-delegates' + uid_ext) else {})
+ dashboard.withDescription('Delegate/baker health, participation and staking, from the octez_delegate_* metrics (delegates RPC endpoint)' + base.build_options)
+ dashboard.withTags(['tezos', 'octez', 'grafazos', 'delegates'])
+ dashboard.time.withFrom('now-24h')
+ dashboard.withRefresh('1m')
+ dashboard.withVariables(base.standardVariablesDelegates)

+ dashboard.withPanels(

  //#######
  grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Overview')], panelWidth=24, panelHeight=4, startY=overview_y)
  + [
    delegates.deactivatedStatus(h=4, w=4, x=0, y=overview_y + 1),
    delegates.forbiddenStatus(h=4, w=4, x=4, y=overview_y + 1),
    delegates.dalSufficientStatus(h=4, w=4, x=8, y=overview_y + 1),
    delegates.dalDenouncedStatus(h=4, w=4, x=12, y=overview_y + 1),
    delegates.gracePeriod(h=4, w=4, x=16, y=overview_y + 1),
    delegates.bakingPower(h=4, w=4, x=20, y=overview_y + 1),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Staking & Balances')], panelWidth=24, panelHeight=12, startY=staking_y)
  + [
    delegates.totalStaked(h=4, w=6, x=0, y=staking_y + 1),
    delegates.totalDelegated(h=4, w=6, x=6, y=staking_y + 1),
    delegates.ownFullBalance(h=4, w=6, x=12, y=staking_y + 1),
    delegates.minDelegatedInCurrentCycle(h=4, w=6, x=18, y=staking_y + 1),
    delegates.stakingComposition(h=8, w=12, x=0, y=staking_y + 5),
    delegates.delegationComposition(h=8, w=12, x=12, y=staking_y + 5),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Participation (current cycle)')], panelWidth=24, panelHeight=12, startY=participation_y)
  + [
    delegates.slotSuccessRate(h=8, w=6, x=0, y=participation_y + 1),
    delegates.remainingAllowedMissedSlots(h=4, w=6, x=6, y=participation_y + 1),
    delegates.missedLevels(h=4, w=6, x=12, y=participation_y + 1),
    delegates.expectedAttestingRewards(h=4, w=6, x=18, y=participation_y + 1),
    delegates.participationRate(h=8, w=12, x=6, y=participation_y + 5),
    delegates.participationActivity(h=8, w=6, x=18, y=participation_y + 5),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('DAL Participation (current cycle)')], panelWidth=24, panelHeight=12, startY=dal_y)
  + [
    delegates.dalSuccessRate(h=8, w=6, x=0, y=dal_y + 1),
    delegates.dalExpectedShardsPerSlot(h=4, w=6, x=6, y=dal_y + 1),
    delegates.expectedDalRewards(h=4, w=6, x=12, y=dal_y + 1),
    delegates.dalSlots(h=8, w=12, x=6, y=dal_y + 5),
    delegates.dalStatusHistory(h=8, w=6, x=18, y=dal_y + 5),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Voting & Governance')], panelWidth=24, panelHeight=9, startY=voting_y)
  + [
    delegates.votingPower(h=8, w=18, x=0, y=voting_y + 1),
    delegates.remainingProposals(h=4, w=6, x=18, y=voting_y + 1),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Staking Parameters & Risk')], panelWidth=24, panelHeight=5, startY=params_y)
  + [
    delegates.stakingLimitOverBaking(h=4, w=6, x=0, y=params_y + 1),
    delegates.edgeOfBakingOverStaking(h=4, w=6, x=6, y=params_y + 1),
    delegates.stakingDenominator(h=4, w=6, x=12, y=params_y + 1),
    delegates.pendingSlashedAmount(h=4, w=6, x=18, y=params_y + 1),
  ]

)
