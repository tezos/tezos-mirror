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
local hardwareSrc = std.extVar('hardware_src');

//##
// Node Hardware related stats
//##

{

  // Query helper
  query(q, legendFormat):
    query.prometheus.new(base.datasource, q)
    + query.prometheus.withLegendFormat(legendFormat),

  // Helper to select metric based on HARDWARE_SRC
  selectMetric(netdataMetric, nodeExporterMetric):
    if hardwareSrc == 'node-exporter' then nodeExporterMetric else netdataMetric,

  ios(h, w, x, y):
    local reads = 'reads';
    local writes = 'writes';
    local readsMetric = self.selectMetric(
      'netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="reads",' + base.node_instance + '="$node_instance"}',
      'rate(node_disk_read_bytes_total[1m])/1024'
    );
    local writesMetric = self.selectMetric(
      'netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="writes",' + base.node_instance + '="$node_instance"}',
      'rate(node_disk_written_bytes_total[1m])/1024'
    );
    local readsQuery = self.query(readsMetric, reads);
    local writesQuery = self.query(writesMetric, writes);
    graph.new('IOs', [readsQuery, writesQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('kbytes')
    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max'])
    + graph.withQueryColor([[reads, 'light-green'], [writes, 'light-yellow']]),

  cpu(h, w, x, y):
    local load = 'Cpu load';
    local cpuMetric = self.selectMetric(
      'sum(netdata_app_cpu_utilization_percentage_average{app_group="octez",' + base.node_instance + '="$node_instance"})',
      '100 * (1 - avg(irate(node_cpu_seconds_total{mode="idle"}[5m])))'
    );
    local loadQuery = self.query(cpuMetric, load);
    graph.new('Cpu activity', [loadQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent')
    + graph.withQueryColor([[load, 'light-yellow']]),

  memory(h, w, x, y):
    local ram = 'reads';
    local swap = 'writes';
    local ramMetric = self.selectMetric(
      'netdata_app_mem_usage_MiB_average{app_group="octez",' + base.node_instance + '="$node_instance"}',
      '(node_memory_MemTotal_bytes - node_memory_MemAvailable_bytes) / 1024 / 1024'
    );
    local swapMetric = self.selectMetric(
      'netdata_app_swap_usage_MiB_average{app_group="octez",' + base.node_instance + '="$node_instance"}',
      '(node_memory_SwapTotal_bytes - node_memory_SwapFree_bytes) / 1024 / 1024'
    );
    local ramQuery = self.query(ramMetric, ram);
    local swapQuery = self.query(swapMetric, swap);
    graph.new('Memory usage', [ramQuery, swapQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('mbytes')
    + graph.withLegendBottom(calcs=['mean', 'max'])
    + graph.withQueryColor([[ram, 'light-green'], [swap, 'light-orange']]),

  storage(h, w, x, y):
    local q =
      if filecheck then self.query('netdata_filecheck_dir_size_bytes_average{' + base.node_instance + '="$node_instance"}', '{{dimension}}')
      else
        local netdataMetric = 'netdata_disk_space_GiB_average{dimension="used", ' + base.node_instance + '="$node_instance"}';
        local nodeExporterMetric = 'node_filesystem_size_bytes{' + base.node_instance + '="$node_instance"} / (1024 * 1024 * 1024)';
        local storageMetric = self.selectMetric(netdataMetric, nodeExporterMetric);
        self.query(storageMetric, '{{family}}');
    graph.new('Storage', [q], h, w, x, y)
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
    local total = 'All fds';
    local sockets = 'Sockets';
    local files = 'reads';
    local pipes = 'writes';
    local socketsTarget = self.selectMetric(
      'netdata_app_fds_open_fds_average{dimension="sockets",app_group="octez",' + base.node_instance + '="$node_instance"}',
      'process_open_fds'  // Note: only available for exporter process itself, not octez
    );
    local filesTarget = self.selectMetric(
      'netdata_app_fds_open_fds_average{dimension="files",app_group="octez",' + base.node_instance + '="$node_instance"}',
      'process_open_fds'
    );
    local pipesTarget = self.selectMetric(
      'netdata_app_fds_open_fds_average{dimension="pipes",app_group="octez",' + base.node_instance + '="$node_instance"}',
      'process_open_fds'
    );
    local totalTarget = 'sum(' + socketsTarget + ') + sum(' + filesTarget + ') + sum(' + pipesTarget + ')';
    local totalQuery = self.query(totalTarget, total);
    local socketsQuery = self.query(socketsTarget, sockets);
    local filesQuery = self.query(filesTarget, files);
    local pipesQuery = self.query(pipesTarget, pipes);
    graph.new('File descriptors', [totalQuery, socketsQuery, filesQuery, pipesQuery], h, w, x, y)
    + graph.withLegendRight(calcs=['current', 'mean', 'min', 'max'])
    + graph.withQueryColor([[total, 'light-green'], [sockets, 'light-yellow'], [files, 'light-blue'], [pipes, 'light-orange']]),

  networkIOS(h, w, x, y):
    local receivedQuery = self.query('-(irate(node_network_receive_bytes_total{' + base.node_instance + '="$node_instance"}[5m]))', 'Bytes received');
    local transmittedQuery = self.query('irate(node_network_transmit_bytes_total{' + base.node_instance + '="$node_instance"}[5m])', 'Bytes transmitted');
    graph.new('Network traffic', [receivedQuery, transmittedQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('Bps')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max']),
}
