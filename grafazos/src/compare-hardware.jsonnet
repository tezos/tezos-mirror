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

// Base
local base = import './base.jsonnet';
local graph = base.graph;

//##
// Comparison dashboard: netdata vs prom-exporters
//##

local panelHeight = 8;
local panelWidth = 12;

// Helper to create queries
local queryHelper(q, legendFormat) =
  query.prometheus.new('Prometheus', q)
  + query.prometheus.withLegendFormat(legendFormat);

// ============================================
// NETDATA METRICS (left column)
// ============================================

local netdata_ios = graph.new(
                      '[netdata] Disk IOs',
                      [
                        queryHelper('rate(netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="reads",' + base.node_instance + '="$node_instance"}[1m])', 'reads'),
                        queryHelper('rate(netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="writes",' + base.node_instance + '="$node_instance"}[1m])', 'writes'),
                      ],
                      panelHeight,
                      panelWidth,
                      0,
                      0
                    )
                    + timeSeries.standardOptions.withUnit('kbytes')
                    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                    + graph.withQueryColor([['reads', 'light-green'], ['writes', 'light-yellow']]);

local netdata_cpu = graph.new(
                      '[netdata] CPU Activity',
                      [
                        queryHelper('sum(netdata_app_cpu_utilization_percentage_average{app_group="octez",' + base.node_instance + '="$node_instance"})', 'CPU Load'),
                      ],
                      panelHeight,
                      panelWidth,
                      0,
                      panelHeight
                    )
                    + timeSeries.standardOptions.withUnit('percent')
                    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                    + graph.withQueryColor([['CPU Load', 'light-yellow']]);

local netdata_memory = graph.new(
                         '[netdata] Memory Usage',
                         [
                           queryHelper('netdata_app_mem_usage_MiB_average{app_group="octez",' + base.node_instance + '="$node_instance"}', 'RAM'),
                           queryHelper('netdata_app_swap_usage_MiB_average{app_group="octez",' + base.node_instance + '="$node_instance"}', 'Swap'),
                         ],
                         panelHeight,
                         panelWidth,
                         0,
                         panelHeight * 2
                       )
                       + timeSeries.standardOptions.withUnit('mbytes')
                       + graph.withLegendBottom(calcs=['mean', 'max'])
                       + graph.withQueryColor([['RAM', 'light-green'], ['Swap', 'light-orange']]);

local netdata_fds = graph.new(
                      '[netdata] Open File Descriptors',
                      [
                        queryHelper('netdata_app_fds_open_fds_average{app_group="octez",' + base.node_instance + '="$node_instance"}', 'Open FDs'),
                      ],
                      panelHeight,
                      panelWidth,
                      0,
                      panelHeight * 3
                    )
                    + timeSeries.standardOptions.withUnit('short')
                    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                    + graph.withQueryColor([['Open FDs', 'light-blue']]);

local netdata_storage = graph.new(
                          '[netdata] Storage',
                          [
                            queryHelper('netdata_disk_space_GiB_average{dimension="used",' + base.node_instance + '="$node_instance"}', 'Used Space'),
                          ],
                          panelHeight,
                          panelWidth,
                          0,
                          panelHeight * 4
                        )
                        + timeSeries.standardOptions.withUnit('short')
                        + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                        + graph.withQueryColor([['Used Space', 'light-red']]);

// ============================================
// PROM-EXPORTERS METRICS (right column)
// ============================================

local promexporters_ios = graph.new(
                            '[prom-exporters] Disk IOs',
                            [
                              queryHelper('rate(namedprocess_namegroup_read_bytes_total{groupname=~"octez.*",' + base.node_instance + '="$node_instance"}[1m])/1024', 'reads ({{ groupname }})'),
                              queryHelper('rate(namedprocess_namegroup_write_bytes_total{groupname=~"octez.*",' + base.node_instance + '="$node_instance"}[1m])/1024', 'writes ({{ groupname }})'),
                            ],
                            panelHeight,
                            panelWidth,
                            panelWidth,
                            0
                          )
                          + timeSeries.standardOptions.withUnit('kbytes')
                          + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                          + graph.withQueryColor([['reads', 'light-green'], ['writes', 'light-yellow']]);

local promexporters_cpu = graph.new(
                            '[prom-exporters] CPU Activity',
                            [
                              queryHelper('rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"octez.*",mode="user",' + base.node_instance + '="$node_instance"}[5m])*100', 'CPU Load ({{ groupname }})'),
                            ],
                            panelHeight,
                            panelWidth,
                            panelWidth,
                            panelHeight
                          )
                          + timeSeries.standardOptions.withUnit('percent')
                          + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                          + graph.withQueryColor([['CPU Load', 'light-yellow']]);

local promexporters_memory = graph.new(
                               '[prom-exporters] Memory Usage',
                               [
                                 queryHelper('namedprocess_namegroup_memory_bytes{groupname=~"octez.*",memtype="resident",' + base.node_instance + '="$node_instance"} / 1024 / 1024', 'RAM ({{ groupname }})'),
                                 queryHelper('namedprocess_namegroup_memory_bytes{groupname=~"octez.*",memtype="swapped",' + base.node_instance + '="$node_instance"} / 1024 / 1024', 'Swap ({{ groupname }})'),
                               ],
                               panelHeight,
                               panelWidth,
                               panelWidth,
                               panelHeight * 2
                             )
                             + timeSeries.standardOptions.withUnit('mbytes')
                             + graph.withLegendBottom(calcs=['mean', 'max'])
                             + graph.withQueryColor([['RAM', 'light-green'], ['Swap', 'light-orange']]);

local promexporters_fds = graph.new(
                            '[prom-exporters] Open File Descriptors',
                            [
                              queryHelper('namedprocess_namegroup_open_filedesc{groupname=~"octez.*",' + base.node_instance + '="$node_instance"}', 'Open FDs ({{ groupname }})'),
                            ],
                            panelHeight,
                            panelWidth,
                            panelWidth,
                            panelHeight * 3
                          )
                          + timeSeries.standardOptions.withUnit('short')
                          + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                          + graph.withQueryColor([['Open FDs', 'light-blue']]);

local promexporters_storage = graph.new(
                                '[prom-exporters] Storage',
                                [
                                  queryHelper('node_filesystem_size_bytes{mountpoint=~"^/$"}', 'Total Size ({{ device }})'),
                                ],
                                panelHeight,
                                panelWidth,
                                panelWidth,
                                panelHeight * 4
                              )
                              + timeSeries.standardOptions.withUnit('bytes')
                              + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
                              + graph.withQueryColor([['Total Size', 'light-red']]);

// ============================================
// MAIN DASHBOARD
// ============================================

dashboard.new('Hardware Metrics Comparison: netdata vs prom-exporters')
+ dashboard.withDescription('Side-by-side comparison of hardware metrics from netdata (left) vs process-exporter + node-exporter (right)')
+ dashboard.withTags(['comparison', 'hardware', 'metrics', 'netdata', 'prom-exporters'])
+ dashboard.withVariables([base.nodeInstance])
+ dashboard.withRefresh('30s')
+ dashboard.withPanels([
  netdata_ios,
  netdata_cpu,
  netdata_memory,
  netdata_fds,
  netdata_storage,
  promexporters_ios,
  promexporters_cpu,
  promexporters_memory,
  promexporters_fds,
  promexporters_storage,
])
