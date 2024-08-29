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
    local readsAccuserQuery = self.query('netdata_apps_lreads_KiB_persec_average{dimension="octez-accuser"}', readsAccuser);
    local writesAccuserQuery = self.query('netdata_apps_lwrites_KiB_persec_average{dimension="octez-accuser"}', writesAccuser);
    local readsBakerQuery = self.query('netdata_apps_lreads_KiB_persec_average{dimension="octez-baker"}', readsBaker);
    local writesBakerQuery = self.query('netdata_apps_lwrites_KiB_persec_average{dimension="octez-baker"}', writesBaker);
    graph.new('IOs', [readsAccuserQuery, writesAccuserQuery, readsBakerQuery, writesBakerQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('kbytes')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max'])
    + graph.withQueryColor([[readsAccuser, 'dark-yellow'], [writesAccuser, 'light-yellow'], [readsBaker, 'dark-red'], [writesBaker, 'light-red']]),

  cpu(h, w, x, y):
    local loadAccuser = 'Cpu load accuser';
    local loadBaker = 'Cpu load baker';
    local loadAccuserQuery = self.query('netdata_apps_cpu_percentage_average{dimension="octez-accuser"}', loadAccuser);
    local loadBakerQuery = self.query('netdata_apps_cpu_percentage_average{dimension="octez-baker"}', loadBaker);
    graph.new('Cpu activity', [loadAccuserQuery, loadBakerQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('percent')
    + graph.withQueryColor([[loadAccuser, 'light-yellow'], [loadBaker, 'light-red']]),

  memory(h, w, x, y):
    local ramAccuser = 'Memory usage accuser';
    local swapAccuser = 'Swap usage accuser';
    local ramBaker = 'Memory usage baker';
    local swapBaker = 'Swap usage baker';
    local ramAccuserQuery = self.query('netdata_apps_mem_MiB_average{dimension="octez-accuser"}', ramAccuser);
    local swapAccuserQuery = self.query('netdata_apps_swap_MiB_average{dimension="octez-accuser"}', swapAccuser);
    local ramBakerQuery = self.query('netdata_apps_mem_MiB_average{dimension="octez-baker"}', ramBaker);
    local swapBakerQuery = self.query('netdata_apps_swap_MiB_average{dimension="octez-baker"}', swapBaker);
    graph.new('Memory usage', [ramAccuserQuery, swapAccuserQuery, ramBakerQuery, swapBakerQuery], h, w, x, y)
    + timeSeries.standardOptions.withUnit('mbytes')
    + graph.withLegendBottom(calcs=['current', 'mean', 'max'])
    + graph.withQueryColor([[ramAccuser, 'dark-yellow'], [swapAccuser, 'light-yellow'], [ramBaker, 'dark-red'], [swapBaker, 'light-red']]),


}
