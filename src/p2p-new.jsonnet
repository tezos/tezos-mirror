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
local table = panel.table;
local timeSeries = panel.timeSeries;
local stat = panel.stat;

// Base
local base = import './base.jsonnet';
local namespace = base.namespace;
local node_instance = base.node_instance;
local prometheus = base.prometheus;
local info = base.info;
local infoName = base.infoName;
local graph = base.graph;
local table = base.table;

//##
// P2p related stats
//##

{
  connectionsTable(h, w, x, y):
    local incomingQuery = prometheus('p2p_connections_incoming', legendFormat='Incoming connections');
    local trustedQuery = prometheus('p2p_points_trusted', legendFormat='Trusted points');
    local privateQuery = prometheus('p2p_connections_private', legendFormat='Private points');
    table('Connections', [incomingQuery, trustedQuery, privateQuery], h, w, x, y),

  totalConnections(h, w, x, y):
    local outgoingQuery = prometheus('p2p_connections_outgoing', legendFormat='Outgoing connections');
    local incomingQuery = prometheus('p2p_connections_incoming', legendFormat='Incoming connections');
    graph.new('P2P total connections', [outgoingQuery, incomingQuery], h, w, x, y)
    + graph.withLegendBottom(calcs=['mean'])
    + graph.withQueryColor([['Outgoing connections', 'light-red'], ['Incoming connections', 'green']]),

  incomingConnectionsMean(h, w, x, y):
    local q = prometheus('p2p_connections_incoming');
    info.new('Incoming connections mean', q, h, w, x, y, instant=false)
    + stat.options.withGraphMode('area'),
}
