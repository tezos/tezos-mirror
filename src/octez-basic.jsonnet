// Copyright (c) 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
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

local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;
local singlestat = grafana.singlestat;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;
local row = grafana.row;

local node = import './node.jsonnet';
local p2p = import './p2p.jsonnet';
local workers = import './workers.jsonnet';
local rpc = import './rpc.jsonnet';

local boardtitle = 'Octez basic dashboard';


//Position variables
local p2p_y = 39;
local worker_y = 72;
local rpc_y = 97;
local misc_y = 114;

//###
// Grafana main stuffs
//##
dashboard.new(
  title=boardtitle,
  tags=['tezos', 'octez'],
  schemaVersion=18,
  editable=true,
  time_from='now-3h',
  refresh='',
)

.addTemplate(
  template.new(
    name='node_instance',
    datasource='Prometheus',
    query='label_values(octez_version,' + std.extVar('node_instance_label') + ')',
    refresh='load',
    label='Node instance'
  )
)

//Node a grid is 24 slots wide
.addPanels(
  [
    //#######
    row.new(
      title='Node stats',
      repeat='',
      showTitle=true,
    ),
    node.bootstrapStatus { gridPos: { h: 3, w: 2, x: 0, y: 1 } },
    node.syncStatus { gridPos: { h: 3, w: 2, x: 2, y: 1 } },
    node.chainNameInfo { gridPos: { h: 3, w: 4, x: 4, y: 1 } },
    node.releaseVersionInfo { gridPos: { h: 3, w: 3, x: 8, y: 1 } },
    node.releaseCommitInfo { gridPos: { h: 3, w: 3, x: 11, y: 1 } },
    node.uptime { gridPos: { h: 3, w: 4, x: 0, y: 4 } },
    node.headLevel { gridPos: { h: 3, w: 4, x: 0, y: 7 } },
    node.p2pVersion { gridPos: { h: 3, w: 2, x: 0, y: 10 } },
    node.distributedDbVersion { gridPos: { h: 3, w: 2, x: 2, y: 10 } },
    node.savepointLevel { gridPos: { h: 3, w: 2, x: 0, y: 13 } },
    node.checkpointLevel { gridPos: { h: 3, w: 2, x: 2, y: 13 } },
    node.cabooseLevel { gridPos: { h: 3, w: 2, x: 0, y: 16 } },
    node.headCycleLevel { gridPos: { h: 3, w: 2, x: 2, y: 16 } },
    p2p.trustedPoints { gridPos: { h: 3, w: 2, x: 0, y: 19 } },
    p2p.privateConnections { gridPos: { h: 3, w: 2, x: 2, y: 19 } },
    node.headHistory { gridPos: { h: 10, w: 10, x: 4, y: 4 } },
    node.blocksValidationTime { gridPos: { h: 8, w: 10, x: 4, y: 14 } },
    node.headOperations { gridPos: { h: 8, w: 14, x: 0, y: 22 } },
    node.gasConsumedHistory { gridPos: { h: 8, w: 14, x: 0, y: 30 } },
    node.invalidBlocksHistory { gridPos: { h: 9, w: 10, x: 14, y: 1 } },
    node.roundHistory { gridPos: { h: 9, w: 10, x: 14, y: 10 } },
    node.writtenBlockSize { gridPos: { h: 10, w: 10, x: 14, y: 19 } },
    node.storeMergeTimeGraph { gridPos: { h: 9, w: 10, x: 14, y: 29 } },

    //#######
    row.new(
      title='P2P stats',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 8, x: 0, y: p2p_y } },
    p2p.mempoolPending { gridPos: { h: 8, w: 12, x: 0, y: p2p_y } },
    p2p.totalConnections { gridPos: { h: 8, w: 12, x: 12, y: p2p_y } },

    p2p.peers { gridPos: { h: 8, w: 12, x: 0, y: p2p_y + 8 } },
    p2p.points { gridPos: { h: 8, w: 12, x: 12, y: p2p_y + 8 } },

    p2p.DDBCurrentHead { gridPos: { h: 8, w: 6, x: 0, y: p2p_y + 16 } },
    p2p.DDBCurrentBranch { gridPos: { h: 8, w: 6, x: 6, y: p2p_y + 16 } },
    p2p.DDBBlockHeaders { gridPos: { h: 8, w: 6, x: 12, y: p2p_y + 16 } },
    p2p.DDBPredHeader { gridPos: { h: 8, w: 6, x: 18, y: p2p_y + 16 } },
    p2p.DDBOperations { gridPos: { h: 8, w: 6, x: 0, y: p2p_y + 24 } },
    p2p.DDBOps4Blocks { gridPos: { h: 8, w: 6, x: 6, y: p2p_y + 24 } },
    p2p.DDBProtocols { gridPos: { h: 8, w: 3, x: 12, y: p2p_y + 24 } },
    p2p.DDBProtoBranch { gridPos: { h: 8, w: 3, x: 15, y: p2p_y + 24 } },
    p2p.DDBCheckpoint { gridPos: { h: 8, w: 3, x: 18, y: p2p_y + 24 } },
    p2p.DDBDeactivate { gridPos: { h: 8, w: 3, x: 21, y: p2p_y + 24 } },

    //#######
    row.new(
      title='Workers stats',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 8, x: 0, y: worker_y } },
    workers.requests { gridPos: { h: 8, w: 12, x: 0, y: worker_y } },
    workers.distributedDB { gridPos: { h: 8, w: 12, x: 12, y: worker_y } },
    workers.validatorTreatmentRequests { gridPos: { h: 8, w: 12, x: 0, y: worker_y + 16 } },
    workers.validatorCompletionRequests { gridPos: { h: 8, w: 12, x: 12, y: worker_y + 16 } },
    workers.peerValidators { gridPos: { h: 8, w: 12, x: 0, y: worker_y + 24 } },

    //#######
    row.new(
      title='RPC',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 16, w: 8, x: 0, y: rpc_y } },
    rpc.calls { gridPos: { h: 8, w: 12, x: 0, y: rpc_y } },
    rpc.durations { gridPos: { h: 8, w: 12, x: 0, y: rpc_y + 8 } },
    rpc.totalCalls { gridPos: { h: 8, w: 6, x: 12, y: rpc_y } },
    rpc.callsRate { gridPos: { h: 8, w: 6, x: 18, y: rpc_y } },
    rpc.averageDuration { gridPos: { h: 8, w: 3, x: 12, y: rpc_y + 8 } },
    rpc.maxAverageDuration { gridPos: { h: 8, w: 3, x: 15, y: rpc_y + 8 } },
    rpc.maxTotalDuration { gridPos: { h: 8, w: 3, x: 18, y: rpc_y + 8 } },
    rpc.totalDuration { gridPos: { h: 8, w: 3, x: 21, y: rpc_y + 8 } },

    //#######
    row.new(
      title='Miscellaneous',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 8, w: 8, x: 0, y: misc_y } },
    node.gcOperations { gridPos: { h: 8, w: 12, x: 0, y: misc_y } },
    node.gcMajorHeap { gridPos: { h: 8, w: 12, x: 12, y: misc_y } },

  ]
)
