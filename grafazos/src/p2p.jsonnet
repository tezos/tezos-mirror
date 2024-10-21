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

// Base
local base = import './base.jsonnet';
local namespace = base.namespace;
local prometheus = base.prometheus;
local info = base.info;
local graph = base.graph;
local table = base.table;

//##
// P2p related stats
//##

local mkPeersPanel(h, w, x, y, legendRightSide) =
  local calcs = ['mean', 'max', 'min'];
  local legend =
    if legendRightSide then
      graph.withLegendRight(calcs)
    else
      graph.withLegendBottom(calcs);
  local disconnectedQuery = prometheus('p2p_peers_disconnected', legendFormat='Disconnected');
  local runningQuery = prometheus('p2p_peers_running', legendFormat='Running peers');
  local greylistedQuery = prometheus('p2p_points_greylisted', legendFormat='Waiting to reconnect');
  graph.new('P2P peers connections', [disconnectedQuery, runningQuery, greylistedQuery], h, w, x, y)
  + graph.withQueryColor([['Disconnected', 'light-yellow'], ['Running peers', 'light-green'], ['Waiting to reconnect', 'light-red']])
  + legend;

{

  trustedPoints(h, w, x, y):
    local q = prometheus('p2p_points_trusted', legendFormat='trusted points');
    info.new('Trusted points', q, h, w, x, y, instant=false)
    + stat.options.reduceOptions.withCalcs('lastNotNull'),

  privateConnections(h, w, x, y):
    local q = prometheus('p2p_connections_private', legendFormat='Private Connections');
    info.new('Private connections', q, h, w, x, y, instant=false)
    + stat.options.reduceOptions.withCalcs('lastNotNull'),

  peers(h, w, x, y): mkPeersPanel(h, w, x, y, legendRightSide=true),

  peersLegendBottom(h, w, x, y): mkPeersPanel(h, w, x, y, legendRightSide=false),

  exchangedData(h, w, x, y):
    local received = 'Received data (KB)';
    local sent = 'Sent data (KB)';
    local receivedQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_p2p_io_scheduler_total_recv' + base.node_instance_query + '[1m]) / 1000')
                          + query.prometheus.withLegendFormat(received);
    local sentQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_p2p_io_scheduler_total_sent' + base.node_instance_query + '[1m]) / 1000')
                      + query.prometheus.withLegendFormat(sent);
    graph.new('Average data exchange (1-minute interval)', [receivedQuery, sentQuery], h, w, x, y)
    + graph.withQueryColor([[received, 'blue'], [sent, 'green']])
    + graph.withLegendBottom(),

  points(h, w, x, y):
    local disconnectedQuery = prometheus('p2p_points_disconnected', legendFormat='Disconnected points');
    local runningQuery = prometheus('p2p_points_running', legendFormat='Running points');
    local trustedQuery = prometheus('p2p_points_trusted', legendFormat='Trusted points');
    graph.new('P2P points connections', [disconnectedQuery, runningQuery, trustedQuery], h, w, x, y)
    + graph.withQueryColor([['Disconnected points', 'light-red'], ['Running points', 'light-green'], ['Trusted points', 'light-yellow']]),

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

  mempoolPending(h, w, x, y):
    local validatedQuery = prometheus('mempool_pending_validated', legendFormat='Validated');
    local refusedQuery = prometheus('mempool_pending_refused', legendFormat='Refused');
    local outdatedQuery = prometheus('mempool_pending_outdated', legendFormat='Outdated');
    local branchRefusedQuery = prometheus('mempool_pending_branch_refused', legendFormat='Branch refused');
    local branchDelayedQuery = prometheus('mempool_pending_branch_delayed', legendFormat='Branch delayed');
    local unprocessedQuery = prometheus('mempool_pending_unprocessed', legendFormat='Unprocessed');
    graph.new('Mempool status', [validatedQuery, refusedQuery, outdatedQuery, branchRefusedQuery, branchDelayedQuery, unprocessedQuery], h, w, x, y)
    + graph.withLegendRight(calcs=['mean', 'lastNotNull', 'max', 'min', 'total'])
    + graph.withLogScale()
    + graph.withQueryColor([['Validated', 'green'], ['Refused', 'red'], ['Outdated', 'blue'], ['Branch refused', 'light-orange'], ['Branch delayed', 'light-red'], ['Unprocessed', 'light-yellow']]),

  DDB:
    {
      base(name, nameGet, queryName, queryNameGet, h, w, x, y):
        local getSent = 'get ' + name + ' sent';
        local received = name + ' received';
        local getReceived = 'get ' + name + ' received';
        local sent = name + ' sent';
        local queryGet(t) = query.prometheus.new('Prometheus', base.namespace + '_distributed_db_message_get_' + queryNameGet + '_messages{action="' + t + '",' + base.node_instance + '="$node_instance"}');
        local query(t) = grafonnet.query.prometheus.new('Prometheus', base.namespace + '_distributed_db_message_' + queryName + '_messages{action="' + t + '",' + base.node_instance + '="$node_instance"}');
        local getSentQuery = queryGet('sent')
                             + grafonnet.query.prometheus.withLegendFormat(getSent);
        local receivedQuery = query('received')
                              + grafonnet.query.prometheus.withLegendFormat(received);
        local getReceivedQuery = queryGet('received')
                                 + grafonnet.query.prometheus.withLegendFormat(getReceived);
        local sentQuery = query('sent')
                          + grafonnet.query.prometheus.withLegendFormat(sent);
        graph.new('DDB ' + name + '  messages', [getSentQuery, receivedQuery, getReceivedQuery, sentQuery], h, w, x, y)
        + graph.withLogScale()
        + graph.withQueryColor([[getSent, 'green'], [received, 'light-blue'], [getReceived, 'blue'], [sent, 'light-green']]),


      currentBranch(h, w, x, y): self.base('current branch', 'current branch', 'current_branch', 'current_branch', h, w, x, y),

      blockHeaders(h, w, x, y): self.base('block header', 'block headers', 'block_header', 'block_headers', h, w, x, y),

      predHeader(h, w, x, y): self.base('predecessor header', 'protocols', 'predecessor_header', 'predecessor_header', h, w, x, y),

      operations(h, w, x, y): self.base('operation', 'operations', 'operation', 'operations', h, w, x, y),

      ops4Blocks(h, w, x, y): self.base('ops for block', 'ops for blocks', 'operations_for_block', 'operations_for_blocks', h, w, x, y),

      protocols(h, w, x, y): self.base('protocol', 'protocols', 'protocol', 'protocols', h, w, x, y),

      protoBranch(h, w, x, y): self.base('protocol branch', 'protocol branch', 'protocol_branch', 'protocol_branch', h, w, x, y),

      checkpoint(h, w, x, y): self.base('checkpoint', 'checkpoint', 'checkpoint', 'checkpoint', h, w, x, y),

      deactivate(h, w, x, y): self.base('deactivate', 'deactivate', 'deactivate', 'deactivate', h, w, x, y),

      currentHead(h, w, x, y):
        local getBroadcasted = 'get current head broadcasted';
        local getSent = 'get current head sent';
        local received = 'current head received';
        local getReceived = 'get current head received';
        local sent = 'current head sent';
        local broadcasted = 'current head broadcasted';
        local queryGet(t) = query.prometheus.new('Prometheus', base.namespace + '_distributed_db_message_get_current_head_messages{action="' + t + '",' + base.node_instance + '="$node_instance"}');
        local query(t) = grafonnet.query.prometheus.new('Prometheus', base.namespace + '_distributed_db_message_current_head_messages{action="' + t + '",' + base.node_instance + '="$node_instance"}');
        local getBroadcastedQuery = queryGet('broadcasted')
                                    + grafonnet.query.prometheus.withLegendFormat(getBroadcasted);
        local getSentQuery = queryGet('sent')
                             + grafonnet.query.prometheus.withLegendFormat(getSent);
        local receivedQuery = query('received')
                              + grafonnet.query.prometheus.withLegendFormat(received);
        local getReceivedQuery = queryGet('received')
                                 + grafonnet.query.prometheus.withLegendFormat(getReceived);
        local sentQuery = query('sent')
                          + grafonnet.query.prometheus.withLegendFormat(sent);
        local broadcastedQuery = query('broadcased')
                                 + grafonnet.query.prometheus.withLegendFormat(broadcasted);
        graph.new('DDB current head messages', [getBroadcastedQuery, getSentQuery, receivedQuery, getReceivedQuery, sentQuery, broadcastedQuery], h, w, x, y)
        + graph.withLogScale()
        + graph.withQueryColor([[getBroadcasted, 'green'], [getSent, 'green'], [received, 'light-blue'], [getReceived, 'blue'], [sent, 'light-green'], [broadcasted, 'light-green']]),

    },
}
