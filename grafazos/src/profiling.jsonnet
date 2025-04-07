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
local mempool = 'profiling_mempool_time';
local chain_validator = 'profiling_chain_validator_time';
local peer_validator = 'profiling_peer_validator_time';
local block_validator = 'profiling_block_validator_time';

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
local profilingSinglePanel(profiling, metricId, legend, color, h, w, x, y) =
  local q = profilingQuery(profiling, metricId, legend);
  graph.new('Average Execution Time: ' + metricId, [q], h, w, x, y)
  + graph.withLegendBottom(calcs=['lastNotNull', 'mean', 'min', 'max'])
  + graph.withFixedColor(color)
  + timeSeries.standardOptions.withUnit('ms');

// Helper function to create a Grafana panel with multile PromQL queries
local profilingMultiplePanels(profiling, legend, metrics, h, w, x, y) =
  local queries = std.map(function(metric) profilingQuery(profiling, metric.id, metric.legend), metrics);
  graph.new('Average Execution Time: ' + legend, queries, h, w, x, y)
  + graph.withLegendRight(calcs=['mean', 'lastNotNull', 'max', 'min'])
  + timeSeries.standardOptions.withUnit('ms');

{
  // Store Profiling
  setHead(h, w, x, y):
    profilingSinglePanel(store, 'set_head', 'Set Head', 'light-blue', h, w, x, y),

  storeBlock(h, w, x, y):
    profilingSinglePanel(store, 'store_block', 'Store Block', 'light-green', h, w, x, y),

  computeLiveBlocks(h, w, x, y):
    profilingSinglePanel(store, 'compute_live_blocks', 'Compute Live Blocks', 'light-red', h, w, x, y),

  // Mempool Profiling

  handleUnprocessed(h, w, x, y):
    profilingSinglePanel(mempool, 'handle_unprocessed', 'Handle Unprocessed', 'light-blue', h, w, x, y),

  onMempoolRequest(h, w, x, y):
    profilingMultiplePanels(mempool, 'on_request', [
      { id: 'on_advertise', legend: 'On Advertise' },
      { id: 'on_arrived', legend: 'On Arrived' },
      { id: 'on_flush', legend: 'On Flush' },
      { id: 'on_inject', legend: 'On Inject' },
      { id: 'on_notify', legend: 'On Notify' },
    ], h, w, x, y),

  // Chain Validator Profiling
  onChainValidatorRequest(h, w, x, y):
    profilingMultiplePanels(chain_validator, 'on_request', [
      { id: 'on_validation_request', legend: 'On Validation Request' },
      { id: 'on_notify_branch', legend: 'On Notify Branch' },
      { id: 'on_notify_head', legend: 'On Notify Head' },
      { id: 'on_disconnection', legend: 'On Disconnection' },
    ], h, w, x, y),

  // Peer Validator Profiling
  onPeerValidatorRequest(h, w, x, y):
    profilingSinglePanel(peer_validator, 'validate_new_head', 'Validate New Head', 'light-red', h, w, x, y),

  // Block Validator Profiling
  applyBlock(h, w, x, y):
    profilingSinglePanel(block_validator, 'apply_block', 'Apply Block', 'light-blue', h, w, x, y),

  applyOperations(h, w, x, y):
    profilingSinglePanel(block_validator, 'apply_operations', 'Apply Operations', 'light-blue', h, w, x, y),

  beginApplication(h, w, x, y):
    profilingSinglePanel(block_validator, 'begin_application', 'Begin Application', 'light-red', h, w, x, y),

  beginValidation(h, w, x, y):
    profilingSinglePanel(block_validator, 'begin_validation', 'Begin Validation', 'light-red', h, w, x, y),

  finalizeApplication(h, w, x, y):
    profilingSinglePanel(block_validator, 'finalize_application', 'Finalize Application', 'light-green', h, w, x, y),

  finalizeValidation(h, w, x, y):
    profilingSinglePanel(block_validator, 'finalize_validation', 'Finalize Validation', 'light-green', h, w, x, y),

  validateBlock(h, w, x, y):
    profilingSinglePanel(block_validator, 'validate_block', 'Validate Block', 'yellow', h, w, x, y),

  validateOperation(h, w, x, y):
    profilingSinglePanel(block_validator, 'validate_operation', 'Validate Operation', 'yellow', h, w, x, y),
}
