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
local base = import '../base.jsonnet';

// We reuse Octez-p2p for p2p pannels
local p2p = import '../p2p.jsonnet';
local gossipsub = import './gossipsub.jsonnet';
local dalNode = import './dal-node.jsonnet';

// External variables
local uid_ext = std.extVar('uid_ext');
local uid = uid_ext == 'default';

//Position variables
local node_y = 1;
local gossipsub_y = node_y + 16;
local p2p_y = gossipsub_y + 24;

dashboard.new('Octez DAL Node Dashboard' + if !uid && uid_ext != '' then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ (if !uid then dashboard.withUid('dal-basic' + uid_ext) else {})
+ dashboard.withDescription('A dashboard for Octez DAL node')
+ dashboard.withTags(['tezos', 'octez', 'dal'])
+ dashboard.time.withFrom('now-3h')
+ dashboard.withRefresh('20s')
+ dashboard.withVariables([base.nodeInstanceDal, base.slotIndex, base.pkh, base.peer])

+ dashboard.withPanels(

  //#######
  grafonnet.util.grid.wrapPanels(panels=[panel.row.new("DAL node' core stats")], panelWidth=26, panelHeight=30, startY=node_y)
  + [
    // ## First line of pannels
    dalNode.layer1MonitorLevels(h=8, w=8, x=0, y=node_y),
    dalNode.layer1MonitorRounds(h=8, w=8, x=8, y=node_y),
    dalNode.storedShards(h=8, w=8, x=16, y=node_y),

    // ## Second line of pannels
    dalNode.slotsAttesatationSummary(h=8, w=8, x=0, y=node_y + 8),
    dalNode.slotsWaitingAttestations(h=8, w=8, x=8, y=node_y + 8),
    dalNode.slotsAttested(h=8, w=8, x=16, y=node_y + 8),

    // ## Third line of pannels
    dalNode.L1BlockProcessingTime(h=8, w=8, x=0, y=node_y + 16),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[
    panel.row.new("Gossipsub worker's stats"),
  ], panelWidth=26, panelHeight=32, startY=gossipsub_y)
  + [
    // ## First line of pannels
    gossipsub.scoresOfPeers(h=8, w=12, x=0, y=gossipsub_y),
    gossipsub.peersOfTopics(h=8, w=12, x=12, y=gossipsub_y),

    // ## Second line of pannels
    gossipsub.countTopics(h=8, w=8, x=0, y=gossipsub_y + 8),
    gossipsub.countConnections(h=8, w=8, x=8, y=gossipsub_y + 8),
    gossipsub.workerStreams(h=8, w=8, x=16, y=gossipsub_y + 8),

    // ## Third line of pannels
    gossipsub.appMessagesDeriv(h=8, w=8, x=0, y=gossipsub_y + 16),
    gossipsub.sentOtherMessagesDeriv(h=8, w=8, x=8, y=gossipsub_y + 16),
    gossipsub.receivedOtherMessagesDeriv(h=8, w=8, x=16, y=gossipsub_y + 16),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[
    panel.row.new('P2P stats'),
  ], panelWidth=26, panelHeight=32, startY=p2p_y)
  + [
    // ## First line of pannels
    p2p.exchangedData(h=8, w=12, x=0, y=p2p_y),
    p2p.totalConnections(h=8, w=12, x=12, y=p2p_y),

    // ## Second line of pannels
    p2p.peersLegendBottom(h=10, w=12, x=0, y=p2p_y + 8),
    p2p.points(h=10, w=12, x=12, y=p2p_y + 8),
  ]

)
