// Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
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

local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local singlestat = grafana.singlestat;
local statPanel = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local tablePanel = grafana.tablePanel;
local prometheus = grafana.prometheus;
local namespace = 'octez';
local node_instance = '{' + std.extVar('node_instance_label') + '="$node_instance"}';

//##
// P2P
//##

{
  trustedPoints:
    singlestat.new(
      title='Trusted points',
      datasource='Prometheus',
      format='none',
      valueName='max',
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_trusted' + node_instance,
        legendFormat='trusted points',
      )
    ),

  privateConnections:
    singlestat.new(
      title='Private Connections',
      datasource='Prometheus',
      format='none',
      valueName='max',
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_private' + node_instance,
        legendFormat='Private Connections',
      )
    ),

  peers:
    local disconnected = 'Disconnected peers';
    local running = 'Running peers';
    local greylisted = 'Waiting to reconnect';
    graphPanel.new(
      title='P2P peers connections',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_rightSide=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [disconnected]: 'light-yellow',
        [running]: 'light-green',
        [greylisted]: 'light-red',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_peers_disconnected' + node_instance,
        legendFormat=disconnected,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_peers_running' + node_instance,
        legendFormat=running,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_greylisted' + node_instance,
        legendFormat=greylisted,
      )
    ),

  points:
    local disconnected = 'Disconnected points';
    local running = 'Running points';
    graphPanel.new(
      title='P2P points connections',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [disconnected]: 'light-red',
        [running]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_disconnected' + node_instance,
        legendFormat=disconnected,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_running' + node_instance,
        legendFormat=running,
      )
    ),

  totalConnections:
    local outgoing = 'Outgoing connections';
    local incoming = 'Incoming connections';
    graphPanel.new(
      title='P2P total connections',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [outgoing]: 'light-red',
        [incoming]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_outgoing' + node_instance,
        legendFormat=outgoing,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_incoming' + node_instance,
        legendFormat=incoming,
      )
    ),

  incomingConnectionsMean:
    statPanel.new(
      title='Incoming connections mean',
      datasource='Prometheus',
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_incoming' + node_instance
      )
    ),

  mempoolPending:
    local applied = 'Applied';
    local refused = 'Refused';
    local outdated = 'Outdated';
    local branch_refused = 'Branch refused';
    local branch_delayed = 'Branch delayed';
    local unprocessed = 'Unprocessed';
    graphPanel.new(
      title='Mempool status',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      decimals=0,
      legend_alignAsTable=true,
      legend_current=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_rightSide=true,
      legend_show=true,
      legend_total=true,
      legend_values=true,
      logBase1Y=10,
      aliasColors={
        [applied]: 'green',
        [refused]: 'red',
        [outdated]: 'blue',
        [branch_refused]: 'light-orange',
        [branch_delayed]: 'light-red',
        [unprocessed]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_validated' + node_instance,
        legendFormat=applied,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_refused' + node_instance,
        legendFormat=refused,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_outdated' + node_instance,
        legendFormat=outdated,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_branch_refused' + node_instance,
        legendFormat=branch_refused,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_branch_delayed' + node_instance,
        legendFormat=branch_delayed,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_unprocessed' + node_instance,
        legendFormat=unprocessed,
      )
    ),

  connectionsTable:
    tablePanel.new(
      title='Connections',
      datasource='Prometheus',
      transform=('timeseries_to_rows'),
    ).addTargets([
      prometheus.target(
        namespace + '_p2p_connections_incoming' + node_instance,
        legendFormat='Incomming connections',
        instant=true
      ),
      prometheus.target(
        namespace + '_p2p_points_trusted' + node_instance,
        legendFormat='Trusted points',
        instant=true
      ),
      prometheus.target(
        namespace + '_p2p_connections_private' + node_instance,
        legendFormat='Private points',
        instant=true
      ),
    ]).hideColumn('Time'),

}
