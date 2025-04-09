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

local base = import './base.jsonnet';
local node = import './node.jsonnet';
local p2p = import './p2p.jsonnet';
local workers = import './workers.jsonnet';
local rpc = import './rpc.jsonnet';

// External variables
local uid_ext = std.extVar('uid_ext');
local uid = uid_ext == 'default';

//Position variables
local p2p_y = 39;
local worker_y = 72;
local rpc_y = 97;
local misc_y = 114;

//###
// Grafana main stuffs
//##
dashboard.new('Octez basic dashboard' + if !uid then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ (if !uid then dashboard.withUid('octez-basic' + uid_ext) else {})
+ dashboard.withDescription('A basic dashboard for Octez')
+ dashboard.withTags(['tezos', 'octez'])
+ dashboard.time.withFrom('now-3h')
+ dashboard.withRefresh('20s')
+ dashboard.withVariables([base.nodeInstance])

+ dashboard.withPanels(

  //#######
  grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Node stats')], panelWidth=26, panelHeight=30, startY=1)
  + [
    node.bootstrapStatus(h=3, w=2, x=0, y=1),
    node.syncStatus(h=3, w=2, x=2, y=1),
    node.chainNameInfo(h=3, w=4, x=4, y=1),
    node.releaseVersionInfo(h=3, w=3, x=8, y=1),
    node.releaseCommitInfo(h=3, w=3, x=11, y=1),
    node.uptime(h=3, w=4, x=0, y=4),
    node.headLevel(h=3, w=4, x=0, y=7),
    node.p2pVersion(h=3, w=2, x=0, y=10),
    node.distributedDbVersion(h=3, w=2, x=2, y=10),
    node.savepointLevel(h=3, w=2, x=0, y=13),
    node.checkpointLevel(h=3, w=2, x=2, y=13),
    node.cabooseLevel(h=3, w=2, x=0, y=16),
    node.headCycleLevel(h=3, w=2, x=2, y=16),
    p2p.trustedPoints(h=3, w=2, x=0, y=19),
    p2p.privateConnections(h=3, w=2, x=2, y=19),
    node.headHistory(h=10, w=10, x=4, y=4),
    node.blocksValidationTime(h=8, w=10, x=4, y=14),
    node.headOperations(h=8, w=14, x=0, y=22),
    node.gasConsumedHistory(h=8, w=14, x=0, y=30),
    node.invalidBlocksHistory(h=9, w=10, x=14, y=1),
    node.roundHistory(h=9, w=10, x=14, y=10),
    node.writtenBlockSize(h=10, w=10, x=14, y=19),
    node.storeMergeTimeGraph(h=9, w=10, x=14, y=29),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[
    panel.row.new('P2P stats'),
  ], panelWidth=26, panelHeight=32, startY=p2p_y)
  + [
    p2p.mempoolPending(h=8, w=12, x=0, y=p2p_y),
    p2p.totalConnections(h=8, w=12, x=12, y=p2p_y),
    p2p.peers(h=8, w=12, x=0, y=p2p_y + 8),
    p2p.points(h=8, w=12, x=12, y=p2p_y + 8),
    p2p.DDB.currentHead(h=8, w=6, x=0, y=p2p_y + 16),
    p2p.DDB.currentBranch(h=8, w=6, x=6, y=p2p_y + 16),
    p2p.DDB.blockHeaders(h=8, w=6, x=12, y=p2p_y + 16),
    p2p.DDB.predHeader(h=8, w=6, x=18, y=p2p_y + 16),
    p2p.DDB.operations(h=8, w=6, x=0, y=p2p_y + 24),
    p2p.DDB.ops4Blocks(h=8, w=6, x=6, y=p2p_y + 24),
    p2p.DDB.protocols(h=8, w=3, x=12, y=p2p_y + 24),
    p2p.DDB.protoBranch(h=8, w=3, x=15, y=p2p_y + 24),
    p2p.DDB.checkpoint(h=8, w=3, x=18, y=p2p_y + 24),
    p2p.DDB.deactivate(h=8, w=3, x=21, y=p2p_y + 24),
  ]

  //######
  + grafonnet.util.grid.wrapPanels(panels=[
    panel.row.new('Workers stats'),
  ], panelWidth=26, panelHeight=32, startY=worker_y)
  + [
    workers.requests(h=8, w=12, x=0, y=worker_y),
    workers.distributedDB(h=8, w=12, x=12, y=worker_y),
    workers.validatorTreatmentRequests(h=8, w=12, x=0, y=worker_y + 16),
    workers.validatorCompletionRequests(h=8, w=12, x=12, y=worker_y + 16),
    workers.peerValidators(h=8, w=12, x=0, y=worker_y + 24),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[
    panel.row.new('RPC'),
  ], panelWidth=26, panelHeight=32, startY=rpc_y)
  + [
    rpc.calls(h=8, w=12, x=0, y=rpc_y),
    rpc.durations(h=8, w=12, x=0, y=rpc_y + 8),
    rpc.totalCalls(h=8, w=6, x=12, y=rpc_y),
    rpc.callsRate(h=8, w=6, x=18, y=rpc_y),
    rpc.averageDuration(h=8, w=3, x=12, y=rpc_y + 8),
    rpc.maxAverageDuration(h=8, w=3, x=15, y=rpc_y + 8),
    rpc.maxTotalDuration(h=8, w=3, x=18, y=rpc_y + 8),
    rpc.totalDuration(h=8, w=3, x=21, y=rpc_y + 8),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[
    panel.row.new('Miscellaneous'),
  ], panelWidth=24, panelHeight=8, startY=misc_y)
  + [
    node.gcOperations(h=8, w=12, x=0, y=misc_y),
    node.gcMajorHeap(h=8, w=12, x=12, y=misc_y),
  ]
)
