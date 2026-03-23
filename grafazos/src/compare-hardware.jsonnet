// Copyright (c) 2022-2026 Nomadic Labs <contact@nomadic-labs.com>
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
local query = grafonnet.query;
local timeSeries = panel.timeSeries;
local variableCustom = dashboard.variable.custom;

// Base
local base = import './base.jsonnet';
local graph = base.graph;

local uid_ext = std.extVar('uid_ext');
local isDefaultUid = uid_ext == 'default';

//##
// Comparison dashboard: hardware exporters side-by-side
//##

local panelHeight = 8;
local inst = base.node_instance + '="$node_instance"';

// Helper to create queries
local datasource = if base.enable_datasource_selection then '$prometheus_datasource' else base.datasource_default;
local queryHelper(q, legendFormat) =
  query.prometheus.new(datasource, q)
  + query.prometheus.withLegendFormat(legendFormat);

// ============================================
// EXPORTER DEFINITIONS
// ============================================
// Each exporter defines its available metrics. Panels are generated
// at build time based on the configured hardware_src exporters.

local exporterOrder = ['netdata', 'process-exporter', 'cadvisor', 'node-exporter', 'local-storage-exporter'];

local allExporters = {
  netdata: {
    ios: {
      queries: [
        queryHelper('rate(netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="reads",' + inst + '}[1m])', 'reads'),
        queryHelper('rate(netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="writes",' + inst + '}[1m])', 'writes'),
      ],
      unit: 'kbytes',
      colors: [['reads', 'light-green'], ['writes', 'light-yellow']],
    },
    cpu: {
      queries: [
        queryHelper('sum(netdata_app_cpu_utilization_percentage_average{app_group="octez",' + inst + '})', 'CPU Load'),
      ],
      unit: 'percent',
      colors: [['CPU Load', 'light-yellow']],
    },
    memory: {
      queries: [
        queryHelper('netdata_app_mem_usage_MiB_average{app_group="octez",' + inst + '}', 'RAM'),
        queryHelper('netdata_app_swap_usage_MiB_average{app_group="octez",' + inst + '}', 'Swap'),
      ],
      unit: 'mbytes',
      calcs: ['mean', 'max'],
      colors: [['RAM', 'light-green'], ['Swap', 'light-orange']],
    },
    fds: {
      queries: [
        queryHelper('netdata_app_fds_open_fds_average{app_group="octez",' + inst + '}', 'Open FDs'),
      ],
      unit: 'short',
      colors: [['Open FDs', 'light-blue']],
    },
    storage: {
      queries: [
        queryHelper('netdata_disk_space_GiB_average{dimension="used",' + inst + '}', 'Used Space'),
      ],
      unit: 'decgbytes',
      colors: [['Used Space', 'light-red']],
    },
    network: {
      queries: [
        queryHelper('abs(netdata_net_net_kilobits_persec_average{interface_type="real",dimension="received",' + inst + '}) * 125', 'Bytes received'),
        queryHelper('abs(netdata_net_net_kilobits_persec_average{interface_type="real",dimension="sent",' + inst + '}) * 125', 'Bytes transmitted'),
      ],
      unit: 'Bps',
      colors: [['Bytes received', 'light-blue'], ['Bytes transmitted', 'light-orange']],
    },
  },

  'process-exporter': {
    ios: {
      queries: [
        queryHelper('rate(namedprocess_namegroup_read_bytes_total{groupname=~"octez.*",' + inst + '}[1m])/1024', 'reads ({{ groupname }})'),
        queryHelper('rate(namedprocess_namegroup_write_bytes_total{groupname=~"octez.*",' + inst + '}[1m])/1024', 'writes ({{ groupname }})'),
      ],
      unit: 'kbytes',
      colors: [['reads', 'light-green'], ['writes', 'light-yellow']],
    },
    cpu: {
      queries: [
        queryHelper('rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"octez.*",mode="user",' + inst + '}[5m])*100', 'CPU Load ({{ groupname }})'),
      ],
      unit: 'percent',
      colors: [['CPU Load', 'light-yellow']],
    },
    memory: {
      queries: [
        queryHelper('namedprocess_namegroup_memory_bytes{groupname=~"octez.*",memtype="resident",' + inst + '} / 1024 / 1024', 'RAM ({{ groupname }})'),
        queryHelper('namedprocess_namegroup_memory_bytes{groupname=~"octez.*",memtype="swapped",' + inst + '} / 1024 / 1024', 'Swap ({{ groupname }})'),
      ],
      unit: 'mbytes',
      calcs: ['mean', 'max'],
      colors: [['RAM', 'light-green'], ['Swap', 'light-orange']],
    },
    fds: {
      queries: [
        queryHelper('namedprocess_namegroup_open_filedesc{groupname=~"octez.*",' + inst + '}', 'Open FDs ({{ groupname }})'),
      ],
      unit: 'short',
      colors: [['Open FDs', 'light-blue']],
    },
  },

  cadvisor: {
    ios: {
      queries: [
        queryHelper('rate(container_fs_reads_bytes_total{pod=~"octez-.*",' + inst + '}[1m]) / 1024', 'reads ({{ pod }})'),
        queryHelper('rate(container_fs_writes_bytes_total{pod=~"octez-.*",' + inst + '}[1m]) / 1024', 'writes ({{ pod }})'),
      ],
      unit: 'kbytes',
      colors: [['reads', 'light-green'], ['writes', 'light-yellow']],
    },
    cpu: {
      queries: [
        queryHelper('rate(container_cpu_usage_seconds_total{pod=~"octez-.*",' + inst + '}[5m]) * 100', 'CPU Load ({{ pod }})'),
      ],
      unit: 'percent',
      colors: [['CPU Load', 'light-yellow']],
    },
    memory: {
      queries: [
        queryHelper('sum by (pod)(container_memory_working_set_bytes{pod=~"octez-.*",' + inst + '}) / 1024 / 1024', 'RAM ({{ pod }})'),
        queryHelper('sum by (pod)(container_memory_swap{pod=~"octez-.*",' + inst + '}) / 1024 / 1024', 'Swap ({{ pod }})'),
      ],
      unit: 'mbytes',
      calcs: ['mean', 'max'],
      colors: [['RAM', 'light-green'], ['Swap', 'light-orange']],
    },
    fds: {
      queries: [
        queryHelper('sum(process_open_fds{pod=~"octez-.*",' + inst + '})', 'Open FDs'),
      ],
      unit: 'short',
      colors: [['Open FDs', 'light-blue']],
    },
    storage: {
      queries: [
        queryHelper('sum(container_fs_usage_bytes{pod=~"octez-.*",' + inst + '}) / 1024 / 1024 / 1024', 'Used Space'),
      ],
      unit: 'decgbytes',
      colors: [['Used Space', 'light-red']],
    },
    network: {
      queries: [
        queryHelper('rate(container_network_receive_bytes_total{pod=~"octez-.*",' + inst + '}[5m])', 'Bytes received ({{ pod }})'),
        queryHelper('rate(container_network_transmit_bytes_total{pod=~"octez-.*",' + inst + '}[5m])', 'Bytes transmitted ({{ pod }})'),
      ],
      unit: 'Bps',
      colors: [['Bytes received', 'light-blue'], ['Bytes transmitted', 'light-orange']],
    },
  },

  'node-exporter': {
    storage: {
      queries: [
        queryHelper('node_filesystem_size_bytes{mountpoint=~"^/$",' + inst + '}', 'Total Size ({{ device }})'),
      ],
      unit: 'bytes',
      colors: [['Total Size', 'light-red']],
    },
    network: {
      queries: [
        queryHelper('-(irate(node_network_receive_bytes_total{' + inst + '}[5m]))', 'Bytes received'),
        queryHelper('irate(node_network_transmit_bytes_total{' + inst + '}[5m])', 'Bytes transmitted'),
      ],
      unit: 'Bps',
      colors: [['Bytes received', 'light-blue'], ['Bytes transmitted', 'light-orange']],
    },
  },

  'local-storage-exporter': {
    storage: {
      queries: [
        queryHelper('local_storage_pv_used_bytes{' + inst + '}', 'Used Space ({{ persistentvolume }})'),
      ],
      unit: 'bytes',
      colors: [['Used Space', 'light-red']],
    },
  },
};

// ============================================
// DYNAMIC LAYOUT
// ============================================

// Active exporters in display order
local activeExporters = [
  e
  for e in exporterOrder
  if std.member(base.hardware_exporters, e) && std.objectHas(allExporters, e)
];

// Metric rows in display order
local metricRows = [
  { key: 'ios', title: 'Disk IOs' },
  { key: 'cpu', title: 'CPU Activity' },
  { key: 'memory', title: 'Memory Usage' },
  { key: 'fds', title: 'Open File Descriptors' },
  { key: 'storage', title: 'Storage' },
  { key: 'network', title: 'Network Traffic' },
];

// For each metric, find which exporters support it
local exportersForMetric(metricKey) = [
  e
  for e in activeExporters
  if std.objectHas(allExporters[e], metricKey)
];

// Generate one panel; width adapts to how many exporters share this row
local makePanel(exporter, metricTitle, config, colIdx, numCols, yPos) =
  local w = std.floor(24 / numCols);
  graph.new(
    '[' + exporter + '] ' + metricTitle,
    config.queries,
    panelHeight,
    w,
    colIdx * w,
    yPos,
  )
  + timeSeries.standardOptions.withUnit(config.unit)
  + graph.withLegendBottom(calcs=std.get(config, 'calcs', ['mean', 'lastNotNull', 'max']))
  + graph.withQueryColor(config.colors);

// Build panels with row headers; each metric row gets a row panel + exporter columns
// rowHeight = 1 (row header) + panelHeight
local rowHeight = panelHeight + 1;
local panels = std.flattenArrays([
  local exporters = exportersForMetric(metric.key);
  local numCols = std.length(exporters);
  local yBase = rowIdx * rowHeight;
  // Row header
  [panel.row.new(metric.title) + panel.row.withGridPos(y=yBase)]
  // Exporter panels below the row header
  + [
    makePanel(
      exporters[colIdx],
      metric.title,
      allExporters[exporters[colIdx]][metric.key],
      colIdx,
      numCols,
      yBase + 1,
    )
    for colIdx in std.range(0, numCols - 1)
  ]
  for rowIdx in std.range(0, std.length(metricRows) - 1)
  for metric in [metricRows[rowIdx]]
]);

// ============================================
// EXPORTER VARIABLE (informational dropdown)
// ============================================

local exporterVariable =
  variableCustom.new(
    name='exporter',
    values=activeExporters,
  )
  + variableCustom.generalOptions.withLabel('Exporter')
  + variableCustom.selectionOptions.withIncludeAll(true)
  + variableCustom.selectionOptions.withMulti(true)
  + variableCustom.generalOptions.withCurrent('All', '$__all');

// ============================================
// MAIN DASHBOARD
// ============================================

dashboard.new('Hardware Metrics Comparison' + if !isDefaultUid && uid_ext != '' then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ dashboard.withDescription('Side-by-side comparison of hardware metrics across configured exporters' + base.build_options)
+ dashboard.withTags(['grafazos', 'comparison', 'hardware', 'metrics'] + activeExporters)
+ dashboard.withVariables([exporterVariable] + base.standardVariables)
+ dashboard.withRefresh('30s')
+ dashboard.withPanels(panels)
+ (if !isDefaultUid then dashboard.withUid('compare-hardware' + uid_ext) else {})
