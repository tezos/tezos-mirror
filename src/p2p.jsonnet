local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local singlestat = grafana.singlestat;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;

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
        'tezos_metrics_p2p_points_trusted',
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
        'tezos_metrics_p2p_connections_private',
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
      aliasColors={
        [disconnected]: 'light-yellow',
        [running]: 'light-green',
        [greylisted]: 'light-red',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_peers_disconnected',
        legendFormat=disconnected,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_peers_running',
        legendFormat=running,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_points_greylisted',
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
      aliasColors={
        [disconnected]: 'light-yellow',
        [running]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_points_disconnected',
        legendFormat=disconnected,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_points_running',
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
      aliasColors={
        [outgoing]: 'light-yellow',
        [incoming]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_connections_total - tezos_metrics_p2p_connections_incoming',
        legendFormat=outgoing,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_p2p_connections_incoming',
        legendFormat=incoming,
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
      aliasColors={
        [applied]: 'light-green',
        [refused]: 'light-yellow',
        [outdated]: 'light-blue',
        [branch_refused]: 'light-orange',
        [branch_delayed]: 'light-red',
        [unprocessed]: 'blue',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_mempool_pending_applied',
        legendFormat=applied,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_mempool_pending_refused',
        legendFormat=refused,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_mempool_pending_outdated',
        legendFormat=outdated,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_mempool_pending_branch_refused',
        legendFormat=branch_refused,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_mempool_pending_branch_delayed',
        legendFormat=branch_delayed,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_mempool_pending_unprocessed',
        legendFormat=unprocessed,
      )
    ),

}
