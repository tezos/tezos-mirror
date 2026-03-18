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
local query = grafonnet.query;
local timeSeries = grafonnet.panel.timeSeries;
local stat = grafonnet.panel.stat;

// Base
local base = import './base.jsonnet';
local graph = base.graph;

local filecheck = std.extVar('storage_mode') == 'filecheck';
local mountpoint = std.extVar('mountpoint');

//##
// Node Hardware related stats
//##

{

  // Query helper
  query(q, legendFormat):
    query.prometheus.new(base.datasource, q)
    + query.prometheus.withLegendFormat(legendFormat),

  ios(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local readsEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="reads",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_read_bytes_total{groupname=~"octez.*",' + inst + '}[1m])/1024',
    }, { netdata: 'reads', 'process-exporter': 'reads ({{ groupname }})' });
    local writesEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="writes",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_write_bytes_total{groupname=~"octez.*",' + inst + '}[1m])/1024',
    }, { netdata: 'writes', 'process-exporter': 'writes ({{ groupname }})' });
    local readsQueries = std.map(function(e) self.query(e.metric, e.legend), readsEntries);
    local writesQueries = std.map(function(e) self.query(e.metric, e.legend), writesEntries);
    local readsColors = std.map(function(e) [e.legend, 'light-green'], readsEntries);
    local writesColors = std.map(function(e) [e.legend, 'light-yellow'], writesEntries);
    graph.new('IOs', readsQueries + writesQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('kbytes')
    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
    + graph.withQueryColor(readsColors + writesColors),

  cpu(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local loadEntries = base.selectMetrics({
      netdata: 'sum(netdata_app_cpu_utilization_percentage_average{app_group="octez",' + inst + '})',
      'process-exporter': 'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"octez.*",mode="user",' + inst + '}[5m])*100',
    }, { netdata: 'Cpu load', 'process-exporter': 'Cpu load ({{ groupname }})' });
    local loadQueries = std.map(function(e) self.query(e.metric, e.legend), loadEntries);
    local loadColors = std.map(function(e) [e.legend, 'light-yellow'], loadEntries);
    graph.new('Cpu activity', loadQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent')
    + graph.withQueryColor(loadColors),

  memory(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local ramEntries = base.selectMetrics({
      netdata: 'netdata_app_mem_usage_MiB_average{app_group="octez",' + inst + '}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"octez.*",memtype="resident",' + inst + '} / 1024 / 1024',
    }, { netdata: 'RAM', 'process-exporter': 'RAM ({{ groupname }})' });
    local swapEntries = base.selectMetrics({
      netdata: 'netdata_app_swap_usage_MiB_average{app_group="octez",' + inst + '}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"octez.*",memtype="swapped",' + inst + '} / 1024 / 1024',
    }, { netdata: 'Swap', 'process-exporter': 'Swap ({{ groupname }})' });
    local ramQueries = std.map(function(e) self.query(e.metric, e.legend), ramEntries);
    local swapQueries = std.map(function(e) self.query(e.metric, e.legend), swapEntries);
    local ramColors = std.map(function(e) [e.legend, 'light-green'], ramEntries);
    local swapColors = std.map(function(e) [e.legend, 'light-orange'], swapEntries);
    graph.new('Memory usage', ramQueries + swapQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('mbytes')
    + graph.withLegendBottom(calcs=['mean', 'max'])
    + graph.withQueryColor(ramColors + swapColors),

  storage(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local queries =
      if filecheck then [self.query('netdata_filecheck_dir_size_bytes_average{' + inst + '}', '{{dimension}}')]
      else
        local entries = base.selectMetrics({
          netdata: 'netdata_disk_space_GiB_average{dimension="used", ' + inst + '}',
          'node-exporter': 'node_filesystem_size_bytes{' + inst + '} / (1024 * 1024 * 1024)',
          'local-storage-exporter': 'local_storage_pv_used_bytes{' + inst + '}',
        }, { netdata: '{{ family }}', 'node-exporter': '{{ mountpoint }}', 'local-storage-exporter': '{{ persistentvolume }}' });
        std.map(function(e) self.query(e.metric, e.legend), entries);
    graph.new('Storage', queries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('bytes'),

  diskFreeSpace(h, w, x, y):
    local qall = self.query('node_filesystem_free_bytes{' + base.node_instance + '="$node_instance"}', 'Available bytes on disk {{mountpoint}}');
    local qroot = self.query('node_filesystem_free_bytes{mountpoint="/",' + base.node_instance + '="$node_instance"}', 'Available bytes on disk {{mountpoint}}');
    local qother = self.query('node_filesystem_free_bytes{mountpoint="' + mountpoint + '",' + base.node_instance + '="$node_instance"}', 'Available bytes on disk {{mountpoint}}');
    local mountq = if mountpoint != null then [qroot, qother] else [qall];
    graph.new('Disk free space', mountq, h, w, x, y)
    + stat.standardOptions.withUnit('decbytes')
    + stat.options.withReduceOptions(stat.options.reduceOptions.withCalcs(['lastNotNull'])),

  fileDescriptors(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local hasMultiple = base.hasExporter('netdata') && base.hasExporter('process-exporter');
    local suffix(e) = if hasMultiple then ' [' + e + ']' else '';
    // Netdata: individual dimensions + computed total
    local netdataEntries = if base.hasExporter('netdata') then
      local s = suffix('netdata');
      local socketsTarget = 'netdata_app_fds_open_fds_average{dimension="sockets",app_group="octez",' + inst + '}';
      local filesTarget = 'netdata_app_fds_open_fds_average{dimension="files",app_group="octez",' + inst + '}';
      local pipesTarget = 'netdata_app_fds_open_fds_average{dimension="pipes",app_group="octez",' + inst + '}';
      local totalTarget = 'sum(' + socketsTarget + ') + sum(' + filesTarget + ') + sum(' + pipesTarget + ')';
      [
        { metric: totalTarget, legend: 'All fds' + s, color: 'light-green' },
        { metric: socketsTarget, legend: 'Sockets' + s, color: 'light-yellow' },
        { metric: filesTarget, legend: 'Files' + s, color: 'light-blue' },
        { metric: pipesTarget, legend: 'Pipes' + s, color: 'light-orange' },
      ]
    else [];
    // Process-exporter: single aggregate metric
    local processExporterEntries = if base.hasExporter('process-exporter') then
      local s = suffix('process-exporter');
      local fdMetric = 'sum(namedprocess_namegroup_open_filedesc{groupname=~"octez.*",' + inst + '})';
      [
        { metric: fdMetric, legend: 'All fds' + s, color: 'light-green' },
      ]
    else [];
    local allEntries = netdataEntries + processExporterEntries;
    local queries = std.map(function(e) self.query(e.metric, e.legend), allEntries);
    local colors = std.map(function(e) [e.legend, e.color], allEntries);
    graph.new('File descriptors', queries, h, w, x, y)
    + graph.withLegendRight(calcs=['current', 'mean', 'min', 'max'])
    + graph.withQueryColor(colors),

  networkIOS(h, w, x, y):
    local receivedQuery = self.query('-(irate(node_network_receive_bytes_total{' + base.node_instance + '="$node_instance"}[5m]))', 'Bytes received');
    local transmittedQuery = self.query('irate(node_network_transmit_bytes_total{' + base.node_instance + '="$node_instance"}[5m])', 'Bytes transmitted');
    graph.new('Network traffic', [receivedQuery, transmittedQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('Bps')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max']),
}
// test change
