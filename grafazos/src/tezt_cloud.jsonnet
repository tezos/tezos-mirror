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

  // Metric selector for hardware_src switching
  selectMetric(netdataMetric, promExporterMetric):
    if base.hardware_src == 'prom-exporters' then promExporterMetric else netdataMetric,

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
    local avgUserQuery = self.query(agg, self.selectMetric(
      'netdata_app_cpu_utilization_percentage_average{app_group="' + app_group + '",dimension="user"}',
      'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"' + app_group + '.*",mode="user"}[5m])*100'  // process-exporter
    ), 'User ' + agg + ' ({{ groupname }})');
    local avgSystemQuery = self.query(agg, self.selectMetric(
      'netdata_app_cpu_utilization_percentage_average{app_group="' + app_group + '",dimension="system"}',
      'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"' + app_group + '.*",mode="system"}[5m])*100'  // process-exporter
    ), 'System ' + agg + ' ({{ groupname }})');
    graph.new('[' + app_group + '] ' + 'Cpu activity', [avgUserQuery, avgSystemQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent'),

  memory(agg='', app_group, h, w, x, y):
    local avgRamQuery = self.query(agg, self.selectMetric(
      'netdata_app_mem_usage_MiB_average{app_group="' + app_group + '"}',
      'namedprocess_namegroup_memory_bytes{groupname=~"' + app_group + '.*",memtype="resident"}/1024/1024'  // process-exporter
    ), 'RAM ' + agg + ' ({{ groupname }})');
    local avgSwapQuery = self.query(agg, self.selectMetric(
      'netdata_app_swap_usage_MiB_average{app_group="' + app_group + '"}',
      'namedprocess_namegroup_memory_bytes{groupname=~"' + app_group + '.*",memtype="swapped"}/1024/1024'  // process-exporter
    ), 'swap ' + agg + ' ({{ groupname }})');
    graph.new('[' + app_group + '] ' + 'Memory usage', [avgRamQuery, avgSwapQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('MB'),

  ios(agg='', app_group, h, w, x, y):
    local readsQuery = self.query(agg, self.selectMetric(
      'netdata_app_disk_logical_io_KiB_persec_average{app_group="' + app_group + '"}',
      'rate(namedprocess_namegroup_read_bytes_total{groupname=~"' + app_group + '.*"}[5m])/1024'  // process-exporter
    ), 'Reads ' + agg + ' ({{ groupname }})');
    local writesQuery = self.query(agg, self.selectMetric(
      'netdata_app_disk_logical_io_KiB_persec_average{app_group="' + app_group + '"}',
      'rate(namedprocess_namegroup_write_bytes_total{groupname=~"' + app_group + '.*"}[5m])/1024'  // process-exporter
    ), 'Writes ' + agg + ' ({{ groupname }})');
    graph.new('[' + app_group + '] ' + 'Disk IOs', [readsQuery, writesQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('kB/s'),

  networkIOS(agg='', h, w, x, y):
    local avgReceivedQuery = self.query(agg, self.selectMetric(
      'abs(netdata_net_net_kilobits_persec_average{interface_type="real",dimension="received"}) / 8',
      'rate(node_network_receive_bytes_total{device=~".+"}[5m])*8/1000'  // node-exporter (system-level network)
    ), 'Received ' + agg + ' ({{ device }})');
    local avgTransmittedQuery = self.query(agg, self.selectMetric(
      'abs(netdata_net_net_kilobits_persec_average{interface_type="real",dimension="sent"}) / 8',
      'rate(node_network_transmit_bytes_total{device=~".+"}[5m])*8/1000'  // node-exporter (system-level network)
    ), 'Transmitted ' + agg + ' ({{ device }})');
    graph.new('Network traffic', [avgReceivedQuery, avgTransmittedQuery], h, w, x, y)
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
