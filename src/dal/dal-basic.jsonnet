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


local grafana = import '../../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;
local prometheus = grafana.prometheus;
local row = grafana.row;

// We reuse Octez-p2p for p2p pannels
local p2p = import '../p2p.jsonnet';
local gossipsub = import './gossipsub.jsonnet';

local boardtitle = 'Octez DAL Node Dashboard';

local gossipsub_y = 1;
local p2p_y = gossipsub_y + 24;


//###
// Grafana main stuffs
//##
dashboard.new(
  title=boardtitle,
  tags=['tezos', 'octez', 'dal'],
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

# The grid is 24 slots wide, where a slot is the unit used to position Grafana panels
.addPanels(
  [

    //# Gossipsub row
    row.new(
      title="Gossipsub worker's stats",
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 24, x: 0, y: gossipsub_y } },

    // ## First line of pannels
    gossipsub.scoresOfPeers { gridPos: { h: 8, w: 12, x: 0, y: gossipsub_y } },
    gossipsub.peersOfTopics { gridPos: { h: 8, w: 12, x: 12, y: gossipsub_y } },

    // ## Second line of pannels
    gossipsub.countTopics { gridPos: { h: 8, w: 8, x: 0, y: gossipsub_y + 8 } },
    gossipsub.countConnections { gridPos: { h: 8, w: 8, x: 8, y: gossipsub_y + 8 } },
    gossipsub.workerStreams { gridPos: { h: 8, w: 8, x: 16, y: gossipsub_y + 8 } },

    // ## Third line of pannels
    gossipsub.appMessagesDeriv { gridPos: { h: 8, w: 8, x: 0, y: gossipsub_y + 16 } },
    gossipsub.sentOtherMessagesDeriv { gridPos: { h: 8, w: 8, x: 8, y: gossipsub_y + 16 } },
    gossipsub.receivedOtherMessagesDeriv { gridPos: { h: 8, w: 8, x: 16, y: gossipsub_y + 16 } },

    //# P2P row
    row.new(
      title='P2P stats',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 24, x: 0, y: p2p_y } },

    // ## First line of pannels
    p2p.exchangedData { gridPos: { h: 8, w: 12, x: 0, y: p2p_y } },
    p2p.totalConnections { gridPos: { h: 8, w: 12, x: 12, y: p2p_y } },

    // ## Second line of pannels
    p2p.peersLegendBottom { gridPos: { h: 10, w: 12, x: 0, y: p2p_y + 8 } },
    p2p.points { gridPos: { h: 10, w: 12, x: 12, y: p2p_y + 8 } },

  ]
)
