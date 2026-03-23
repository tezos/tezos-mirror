//
// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//

// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local variable = grafonnet.dashboard.variable;
local query = grafonnet.query;
local panel = grafonnet.panel;
local timeSeries = panel.timeSeries;

// Base
local base = import './base.jsonnet';
local graph = base.graph;

{
  datasource:
    variable.datasource.new(
      name='datasource',
      type='prometheus',
    )
    + variable.datasource.generalOptions.withLabel('Prometheus source')
    + variable.datasource.withRegex('^(?!default$).*')
    + { multi: true }  // Enable Multi-value
  ,

  query(agg, q, legendFormat):
    query.prometheus.new(base.datasource, agg + '(' + q + ')')
    + variable.query.withDatasourceFromVariable(self.datasource)
    + query.prometheus.withLegendFormat(legendFormat),

  // Node

  headHistory(agg='', h, w, x, y):
    local q = self.query(agg, 'octez_validator_chain_head_level', legendFormat='Head level');
    graph.new('Head level history', [q], h, w, x, y),

  blocksValidationTime(agg='', h, w, x, y):
    local completion = 'octez_validator_block_last_finished_request_completion_timestamp';
    local treatment = 'octez_validator_block_last_finished_request_treatment_timestamp';
    local q = self.query(agg, completion + '-' + treatment, 'Validation time');
    graph.new('Block validation time', [q], h, w, x, y)
    + timeSeries.standardOptions.withUnit('s'),

  // Hardware

  cpu(agg='', app_group, h, w, x, y):
    local userEntries = base.selectMetrics({
      netdata: 'netdata_app_cpu_utilization_percentage_average{app_group="' + app_group + '",dimension="user"}',
      'process-exporter': 'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"' + app_group + '.*",mode="user"}[5m])*100',
    }, { netdata: 'User ' + agg, 'process-exporter': 'User ' + agg + ' ({{ groupname }})' });
    local systemEntries = base.selectMetrics({
      netdata: 'netdata_app_cpu_utilization_percentage_average{app_group="' + app_group + '",dimension="system"}',
      'process-exporter': 'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"' + app_group + '.*",mode="system"}[5m])*100',
    }, { netdata: 'System ' + agg, 'process-exporter': 'System ' + agg + ' ({{ groupname }})' });
    local userQueries = std.map(function(e) self.query(agg, e.metric, e.legend), userEntries);
    local systemQueries = std.map(function(e) self.query(agg, e.metric, e.legend), systemEntries);
    graph.new('[' + app_group + '] ' + 'Cpu activity', userQueries + systemQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent'),

  memory(agg='', app_group, h, w, x, y):
    local ramEntries = base.selectMetrics({
      netdata: 'netdata_app_mem_usage_MiB_average{app_group="' + app_group + '"}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"' + app_group + '.*",memtype="resident"}/1024/1024',
    }, { netdata: 'RAM ' + agg, 'process-exporter': 'RAM ' + agg + ' ({{ groupname }})' });
    local swapEntries = base.selectMetrics({
      netdata: 'netdata_app_swap_usage_MiB_average{app_group="' + app_group + '"}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"' + app_group + '.*",memtype="swapped"}/1024/1024',
    }, { netdata: 'Swap ' + agg, 'process-exporter': 'Swap ' + agg + ' ({{ groupname }})' });
    local ramQueries = std.map(function(e) self.query(agg, e.metric, e.legend), ramEntries);
    local swapQueries = std.map(function(e) self.query(agg, e.metric, e.legend), swapEntries);
    graph.new('[' + app_group + '] ' + 'Memory usage', ramQueries + swapQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('MB'),

  ios(agg='', app_group, h, w, x, y):
    local readsEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{app_group="' + app_group + '",dimension="reads"}',
      'process-exporter': 'rate(namedprocess_namegroup_read_bytes_total{groupname=~"' + app_group + '.*"}[5m])/1024',
    }, { netdata: 'Reads ' + agg, 'process-exporter': 'Reads ' + agg + ' ({{ groupname }})' });
    local writesEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{app_group="' + app_group + '",dimension="writes"}',
      'process-exporter': 'rate(namedprocess_namegroup_write_bytes_total{groupname=~"' + app_group + '.*"}[5m])/1024',
    }, { netdata: 'Writes ' + agg, 'process-exporter': 'Writes ' + agg + ' ({{ groupname }})' });
    local readsQueries = std.map(function(e) self.query(agg, e.metric, e.legend), readsEntries);
    local writesQueries = std.map(function(e) self.query(agg, e.metric, e.legend), writesEntries);
    graph.new('[' + app_group + '] ' + 'Disk IOs', readsQueries + writesQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('kB/s'),

  networkIOS(agg='', h, w, x, y):
    local receivedEntries = base.selectMetrics({
      netdata: 'abs(netdata_net_net_kilobits_persec_average{interface_type="real",dimension="received"}) / 8',
      'node-exporter': 'rate(node_network_receive_bytes_total{device=~".+"}[5m])*8/1000',
    }, { netdata: 'Received ' + agg + ' ({{ dimension }})', 'node-exporter': 'Received ' + agg + ' ({{ device }})' });
    local transmittedEntries = base.selectMetrics({
      netdata: 'abs(netdata_net_net_kilobits_persec_average{interface_type="real",dimension="sent"}) / 8',
      'node-exporter': 'rate(node_network_transmit_bytes_total{device=~".+"}[5m])*8/1000',
    }, { netdata: 'Transmitted ' + agg + ' ({{ dimension }})', 'node-exporter': 'Transmitted ' + agg + ' ({{ device }})' });
    local receivedQueries = std.map(function(e) self.query(agg, e.metric, e.legend), receivedEntries);
    local transmittedQueries = std.map(function(e) self.query(agg, e.metric, e.legend), transmittedEntries);
    graph.new('Network traffic', receivedQueries + transmittedQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('kB/s'),

  gcOperations(agg='', h, w, x, y):
    local minor = 'Minor collections ' + agg;
    local major = 'Major collections ' + agg;
    local forced = 'Forced major collections ' + agg;
    local compact = 'Heap compactions ' + agg;
    local minorQuery = self.query(agg, 'ocaml_gc_minor_collections', minor);
    local majorQuery = self.query(agg, 'ocaml_gc_major_collections', major);
    local forcedQuery = self.query(agg, 'ocaml_gc_forced_major_collections', forced);
    local compactQuery = self.query(agg, 'ocaml_gc_compactions', compact);
    graph.new('[octez-node] GC maintenance operations', [minorQuery, majorQuery, forcedQuery, compactQuery], h, w, x, y)
    + graph.withLogScale(),

  gcMajorHeap(agg='', h, w, x, y):
    local major = 'Major heap ' + agg;
    local majorQuery = self.query(agg, 'ocaml_gc_heap_words * 8', major);
    graph.new('[octez-node] GC major word sizes', [majorQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('bytes'),
}
