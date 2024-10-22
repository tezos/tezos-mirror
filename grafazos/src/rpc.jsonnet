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
local panel = grafonnet.panel;
local stat = panel.stat;
local timeSeries = panel.timeSeries;

// Base
local base = import './base.jsonnet';
local info = base.info;
local graph = base.graph;


//##
// RPCs stats
//##


{


  countQuery(endpoint):
    'sum(' + base.namespace + '_rpc_calls_count{endpoint=~' + endpoint + ',' + base.node_instance + '="$node_instance"})',

  sumQuery(endpoint):
    'sum(' + base.namespace + '_rpc_calls_sum{endpoint=~' + endpoint + ',' + base.node_instance + '="$node_instance"})',

  averageQuery(endpoint):
    self.sumQuery(endpoint) + '/' + self.countQuery(endpoint),

  graph(title, q, h, w, x, y, unit='none'):
    local chains = 'Chains';
    local blocks = 'Blocks';
    local version = 'Versions';
    local config = 'Config';
    local workers = 'Workers';
    local network = 'Network';
    local protocols = 'Protocols';
    local injection = 'Injection';
    local private = 'Private';
    local misc = 'Misc';
    local chainsQuery = query.prometheus.new('Prometheus', q("'/chains/<chain_id>/.*'"))
                        + query.prometheus.withLegendFormat(chains);
    local blocksQuery = query.prometheus.new('Prometheus', q("'/chains/<chain_id>/blocks/.*'"))
                        + query.prometheus.withLegendFormat(blocks);
    local versionQuery = query.prometheus.new('Prometheus', q("'/version.*'"))
                         + query.prometheus.withLegendFormat(version);
    local configQuery = query.prometheus.new('Prometheus', q("'/config.*'"))
                        + query.prometheus.withLegendFormat(config);
    local workersQuery = query.prometheus.new('Prometheus', q("'/workers.*'"))
                         + query.prometheus.withLegendFormat(workers);
    local networkQuery = query.prometheus.new('Prometheus', q("'/network.*'"))
                         + query.prometheus.withLegendFormat(network);
    local protocolsQuery = query.prometheus.new('Prometheus', q("'/protocols.*'"))
                           + query.prometheus.withLegendFormat(protocols);
    local injectionQuery = query.prometheus.new('Prometheus', q("'/injection.*'"))
                           + query.prometheus.withLegendFormat(injection);
    local privateQuery = query.prometheus.new('Prometheus', q("'/private.*'"))
                         + query.prometheus.withLegendFormat(private);
    local miscQuery = query.prometheus.new('Prometheus', q("'/(fetch_protocol|stats|monitor)*.'"))
                      + query.prometheus.withLegendFormat(misc);
    graph.new(title, [chainsQuery, blocksQuery, versionQuery, configQuery, workersQuery, networkQuery, protocolsQuery, injectionQuery, privateQuery, miscQuery], h, w, x, y)
    + graph.withLegendRight()
    + graph.withQueryColor([[chains, 'light-red'], [blocks, 'light-blue'], [version, 'light-green'], [config, 'light-yellow'], [network, 'blue'], [workers, 'light-red'], [injection, 'green'], [protocols, 'blue'], [private, 'purple'], [misc, 'grey']])
    + timeSeries.standardOptions.withUnit(unit),

  calls(h, w, x, y): self.graph('RPC calls', self.countQuery, h, w, x, y),

  durations(h, w, x, y): self.graph('RPC durations', self.averageQuery, h, w, x, y, unit='s'),

  //The total number of calls
  totalCalls(h, w, x, y):
    local q = query.prometheus.new('Prometheus', self.countQuery("'.*'"))
              + query.prometheus.withLegendFormat('total calls');
    info.new('Total of RPC calls', q, h, w, x, y)
    + info.withThreshold([['Base', 'light-yellow']])
    + stat.options.reduceOptions.withCalcs(['last'])
    + stat.options.withColorMode('value'),

  //The rate of calls from the last hour
  callsRate(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', 'sum(' + 'rate(' + base.namespace + '_rpc_calls_count{endpoint=~".*",' + base.node_instance + '="$node_instance"}' + '[1h]))')
              + query.prometheus.withLegendFormat('calls rate');
    info.new('RPC calls rate per hour', q, h, w, x, y)
    + info.withThreshold([['Base', 'light-yellow']])
    + stat.options.withColorMode('value'),

  //The total duration of all RPCs calls
  totalDuration(h, w, x, y):
    local q = query.prometheus.new('Prometheus', self.sumQuery("'.*'"))
              + query.prometheus.withLegendFormat('total duration');
    info.new('Total of RPC calls durations', q, h, w, x, y)
    + info.withThreshold([['Base', 'blue']])
    + stat.options.reduceOptions.withCalcs(['last'])
    + stat.standardOptions.withUnit('s')
    + stat.options.withColorMode('value'),

  //The average duration of all RPC calls
  averageDuration(h, w, x, y):
    local q = query.prometheus.new('Prometheus', self.sumQuery("'.*'") + '/' + self.countQuery("'.*'"))
              + query.prometheus.withLegendFormat('total calls');
    info.new('Average of calls durations', q, h, w, x, y)
    + info.withThreshold([['Base', 'blue']])
    + stat.options.reduceOptions.withCalcs(['mean'])
    + stat.standardOptions.withUnit('s')
    + stat.options.withColorMode('value'),

  //The maximum of the total duration of each RPC calls
  maxTotalDuration(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', 'max(' + base.namespace + '_rpc_calls_sum{' + base.node_instance + '="$node_instance"})')
              + query.prometheus.withLegendFormat('max total duration');
    info.new('Max of total calls durations', q, h, w, x, y)
    + info.withThreshold([['Base', 'blue']])
    + stat.options.reduceOptions.withCalcs(['max'])
    + stat.standardOptions.withUnit('s')
    + stat.options.withColorMode('value'),

  //The maximum of the average duration of each RPC calls
  // i.e the duration of the longest call on average
  maxAverageDuration(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', 'max(' + base.namespace + '_rpc_calls_sum{' + base.node_instance + '="$node_instance"} / ' + base.namespace + '_rpc_calls_count{' + base.node_instance + '="$node_instance"})')
              + query.prometheus.withLegendFormat('max average duration');
    info.new('Max of average calls durations', q, h, w, x, y)
    + info.withThreshold([['Base', 'blue']])
    + stat.options.reduceOptions.withCalcs(['max'])
    + stat.standardOptions.withUnit('s')
    + stat.options.withColorMode('value'),
}
