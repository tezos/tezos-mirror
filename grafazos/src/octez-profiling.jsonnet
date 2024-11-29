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
local startY = 1;

// Create the dashboard
dashboard.new('Octez Profiling Dashboard')
+ dashboard.withDescription('In-depth profiling for Octez function performance, showing average execution times.')
+ dashboard.withTags(['tezos', 'octez', 'profiling'])
+ dashboard.time.withFrom('now-8h')
+ dashboard.withRefresh('10s')
+ dashboard.withVariables([base.nodeInstance])

+ dashboard.withPanels(
  [
    panel.row.new('Store Profiling'),
    profiling.setHead(h=panelHeight, w=panelWidth, x=0, y=startY),
    profiling.storeBlock(h=panelHeight, w=panelWidth, x=panelWidth, y=startY),
    profiling.computeLiveBlocks(h=panelHeight, w=panelWidth, x=0, y=startY + panelHeight),
  ]
)
