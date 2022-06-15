local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local singlestat = grafana.singlestat;
local statPanel = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;
local namespace = 'octez';
local node_instance = '{instance="$node_instance"}';

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
        namespace + '_p2p_points_trusted',
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
    ).addThresholds([
      {
        color: 'green',
        value: 0,
      },
      {
        color: 'red',
        value: 1,
      },
    ]),

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
        namespace + '_mempool_pending_applied' + node_instance,
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

}
