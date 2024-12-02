// Copyright (c) 2024 TriliTech <contact@trili.tech>

// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local dashboard = grafonnet.dashboard;
local panel = grafonnet.panel;

// Base imports
local base = import './base.jsonnet';
local profiling = import './profiling.jsonnet';

// Constants
local panelWidth = 10;
local panelHeight = 10;
local store_y = 1;
local mempool_y = 41;

// Create the dashboard
dashboard.new('Octez Profiling Dashboard')
+ dashboard.withDescription('In-depth profiling for Octez function performance, showing average execution times.')
+ dashboard.withTags(['tezos', 'octez', 'profiling'])
+ dashboard.time.withFrom('now-8h')
+ dashboard.withRefresh('10s')
+ dashboard.withVariables([base.nodeInstance])

+ dashboard.withPanels(
  //#######
  grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Store Profiling')], panelWidth=20, panelHeight=20, startY=store_y)
  + [
    profiling.setHead(h=panelHeight, w=panelWidth, x=0, y=store_y),
    profiling.storeBlock(h=panelHeight, w=panelWidth, x=panelWidth, y=store_y),
    profiling.computeLiveBlocks(h=panelHeight, w=panelWidth, x=0, y=store_y + panelHeight),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Mempool Profiling')], panelWidth=20, panelHeight=20, startY=mempool_y)
  + [
    profiling.onRequest(h=panelHeight, w=2 * panelWidth, x=0, y=mempool_y),
    profiling.handleUnprocessed(h=panelHeight, w=panelWidth, x=0, y=mempool_y + panelHeight),
  ]
)
