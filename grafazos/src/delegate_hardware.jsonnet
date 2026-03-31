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
local timeSeries = grafonnet.panel.timeSeries;
local query = grafonnet.query;

// Base
local base = import './base.jsonnet';
local graph = base.graph;

//##
// Delegates Hardware related stats
//##

{

  // Query helper
  query(q, legendFormat):
    query.prometheus.new(base.datasource, q)
    + query.prometheus.withLegendFormat(legendFormat),

  ios(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local readsAccuserEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{dimension="reads", app_group="octez-accuser",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_read_bytes_total{groupname=~"octez-accuser.*",' + inst + '}[5m])/1024',
    }, 'reads accuser');
    local writesAccuserEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{dimension="writes", app_group="octez-accuser",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_write_bytes_total{groupname=~"octez-accuser.*",' + inst + '}[5m])/1024',
    }, 'writes accuser');
    local readsBakerEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{dimension="reads", app_group="octez-baker",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_read_bytes_total{groupname=~"octez-baker.*",' + inst + '}[5m])/1024',
    }, 'reads baker');
    local writesBakerEntries = base.selectMetrics({
      netdata: 'netdata_app_disk_logical_io_KiB_persec_average{dimension="writes", app_group="octez-baker",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_write_bytes_total{groupname=~"octez-baker.*",' + inst + '}[5m])/1024',
    }, 'writes baker');
    local mkQueries(entries) = std.map(function(e) self.query(e.metric, e.legend), entries);
    local allQueries = mkQueries(readsAccuserEntries) + mkQueries(writesAccuserEntries) + mkQueries(readsBakerEntries) + mkQueries(writesBakerEntries);
    local mkColors(entries, color) = std.map(function(e) [e.legend, color], entries);
    local allColors = mkColors(readsAccuserEntries, 'dark-yellow') + mkColors(writesAccuserEntries, 'light-yellow') + mkColors(readsBakerEntries, 'dark-red') + mkColors(writesBakerEntries, 'light-red');
    graph.new('IOs', allQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('kbytes')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max'])
    + graph.withQueryColor(allColors),

  cpu(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local loadAccuserEntries = base.selectMetrics({
      netdata: 'netdata_app_cpu_utilization_percentage_average{app_group="octez-accuser",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"octez-accuser.*",mode="user",' + inst + '}[5m])*100',
    }, 'Cpu load accuser');
    local loadBakerEntries = base.selectMetrics({
      netdata: 'netdata_app_cpu_utilization_percentage_average{app_group="octez-baker",' + inst + '}',
      'process-exporter': 'rate(namedprocess_namegroup_cpu_seconds_total{groupname=~"octez-baker.*",mode="user",' + inst + '}[5m])*100',
    }, 'Cpu load baker');
    local mkQueries(entries) = std.map(function(e) self.query(e.metric, e.legend), entries);
    local allQueries = mkQueries(loadAccuserEntries) + mkQueries(loadBakerEntries);
    local mkColors(entries, color) = std.map(function(e) [e.legend, color], entries);
    local allColors = mkColors(loadAccuserEntries, 'light-yellow') + mkColors(loadBakerEntries, 'light-red');
    graph.new('Cpu activity', allQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent')
    + graph.withQueryColor(allColors),

  memory(h, w, x, y):
    local inst = base.node_instance + '="$node_instance"';
    local ramAccuserEntries = base.selectMetrics({
      netdata: 'netdata_app_mem_usage_MiB_average{app_group="octez-accuser",' + inst + '}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"octez-accuser.*",memtype="resident",' + inst + '}/1024/1024',
    }, 'Memory usage accuser');
    local swapAccuserEntries = base.selectMetrics({
      netdata: 'netdata_app_swap_usage_MiB_average{app_group="octez-accuser",' + inst + '}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"octez-accuser.*",memtype="swapped",' + inst + '}/1024/1024',
    }, 'Swap usage accuser');
    local ramBakerEntries = base.selectMetrics({
      netdata: 'netdata_app_mem_usage_MiB_average{app_group="octez-baker",' + inst + '}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"octez-baker.*",memtype="resident",' + inst + '}/1024/1024',
    }, 'Memory usage baker');
    local swapBakerEntries = base.selectMetrics({
      netdata: 'netdata_app_swap_usage_MiB_average{app_group="octez-baker",' + inst + '}',
      'process-exporter': 'namedprocess_namegroup_memory_bytes{groupname=~"octez-baker.*",memtype="swapped",' + inst + '}/1024/1024',
    }, 'Swap usage baker');
    local mkQueries(entries) = std.map(function(e) self.query(e.metric, e.legend), entries);
    local allQueries = mkQueries(ramAccuserEntries) + mkQueries(swapAccuserEntries) + mkQueries(ramBakerEntries) + mkQueries(swapBakerEntries);
    local mkColors(entries, color) = std.map(function(e) [e.legend, color], entries);
    local allColors = mkColors(ramAccuserEntries, 'dark-yellow') + mkColors(swapAccuserEntries, 'light-yellow') + mkColors(ramBakerEntries, 'dark-red') + mkColors(swapBakerEntries, 'light-red');
    graph.new('Memory usage', allQueries, h, w, x, y)
    + timeSeries.standardOptions.withUnit('mbytes')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max'])
    + graph.withQueryColor(allColors),
}
