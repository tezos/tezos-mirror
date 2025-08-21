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
    query.prometheus.new('Prometheus', q)
    + query.prometheus.withLegendFormat(legendFormat),

  ios(h, w, x, y):
    local readsAccuser = 'reads accuser';
    local writesAccuser = 'writes accuser';
    local readsBaker = 'reads baker';
    local writesBaker = 'writes baker';
    local readsAccuserQuery = self.query('netdata_app_disk_logical_io_KiB_persec_average{dimension="reads", app_group="octez-accuser",' + base.node_instance + '="$node_instance"}', readsAccuser);
    local writesAccuserQuery = self.query('netdata_app_disk_logical_io_KiB_persec_average{dimension="writes", app_group="octez-accuser",' + base.node_instance + '="$node_instance"}', writesAccuser);
    local readsBakerQuery = self.query('netdata_app_disk_logical_io_KiB_persec_average{dimension="reads", app_group="octez-baker",' + base.node_instance + '="$node_instance"}', readsBaker);
    local writesBakerQuery = self.query('netdata_app_disk_logical_io_KiB_persec_average{dimension="writes", app_group="octez-baker",' + base.node_instance + '="$node_instance"}', writesBaker);
    graph.new('IOs', [readsAccuserQuery, writesAccuserQuery, readsBakerQuery, writesBakerQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('kbytes')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max'])
    + graph.withQueryColor([[readsAccuser, 'dark-yellow'], [writesAccuser, 'light-yellow'], [readsBaker, 'dark-red'], [writesBaker, 'light-red']]),

  cpu(h, w, x, y):
    local loadAccuser = 'Cpu load accuser';
    local loadBaker = 'Cpu load baker';
    local loadAccuserQuery = self.query('netdata_app_cpu_utilization_percentage_average{app_group="octez-accuser",' + base.node_instance + '="$node_instance"}', loadAccuser);
    local loadBakerQuery = self.query('netdata_app_cpu_utilization_percentage_average{app_group="octez-baker",' + base.node_instance + '="$node_instance"}', loadBaker);
    graph.new('Cpu activity', [loadAccuserQuery, loadBakerQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent')
    + graph.withQueryColor([[loadAccuser, 'light-yellow'], [loadBaker, 'light-red']]),

  memory(h, w, x, y):
    local ramAccuser = 'Memory usage accuser';
    local swapAccuser = 'Swap usage accuser';
    local ramBaker = 'Memory usage baker';
    local swapBaker = 'Swap usage baker';
    local ramAccuserQuery = self.query('netdata_app_mem_usage_MiB_average{app_group="octez-accuser",' + base.node_instance + '="$node_instance"}', ramAccuser);
    local swapAccuserQuery = self.query('netdata_app_swap_usage_MiB_average{app_group="octez-accuser",' + base.node_instance + '="$node_instance"}', swapAccuser);
    local ramBakerQuery = self.query('netdata_app_mem_usage_MiB_average{app_group="octez-baker",' + base.node_instance + '="$node_instance"}', ramBaker);
    local swapBakerQuery = self.query('netdata_app_swap_usage_MiB_average{app_group="octez-baker",' + base.node_instance + '="$node_instance"}', swapBaker);
    graph.new('Memory usage', [ramAccuserQuery, swapAccuserQuery, ramBakerQuery, swapBakerQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('mbytes')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max'])
    + graph.withQueryColor([[ramAccuser, 'dark-yellow'], [swapAccuser, 'light-yellow'], [ramBaker, 'dark-red'], [swapBaker, 'light-red']]),
}
