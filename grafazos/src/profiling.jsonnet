// Copyright (c) 2024 TriliTech <contact@trili.tech>

// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local query = grafonnet.query;
local panel = grafonnet.panel;
local timeSeries = panel.timeSeries;

// Base
local base = import './base.jsonnet';
local graph = base.graph;

// Constants
local rate = '30s';
local msConversion = '1000';  // From seconds to milliseconds

// Profiling constants
local store = 'profiling_store_time';

// Helper function to calculate rates for a profiling metric
local profilingRate(profiling, metricId, metricType) =
  'rate(' + profiling + '_' + metricType + '{id="' + metricId + '",' + base.node_instance + '="$node_instance"}[' + rate + '])';

// Helper function to create PromQL query for average execution time
local profilingQuery(profiling, metricId, legend) =
  local sum = profilingRate(profiling, metricId, 'sum');
  local count = profilingRate(profiling, metricId, 'count');
  query.prometheus.new('Prometheus', '(' + sum + ' / ' + count + ') * ' + msConversion)
  + query.prometheus.withLegendFormat(legend);

// Helper function to create a Grafana panel
local profilingPanel(profiling, metricId, legend, color, h, w, x, y) =
  local q = profilingQuery(profiling, metricId, legend);
  graph.new('Average Execution Time: ' + metricId, [q], h, w, x, y)
  + graph.withLegendBottom(calcs=['lastNotNull', 'mean', 'min', 'max'])
  + graph.withFixedColor(color)
  + timeSeries.standardOptions.withUnit('ms');

{
  setHead(h, w, x, y):
    profilingPanel(store, 'set_head', 'Set Head', 'light-blue', h, w, x, y),

  storeBlock(h, w, x, y):
    profilingPanel(store, 'store_block', 'Store Block', 'light-green', h, w, x, y),

  computeLiveBlocks(h, w, x, y):
    profilingPanel(store, 'compute_live_blocks', 'Compute Live Blocks', 'light-red', h, w, x, y),
}
