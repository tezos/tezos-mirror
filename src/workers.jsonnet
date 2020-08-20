local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local singlestat = grafana.singlestat;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;

//##
// Workers
//##
{
  requests:
    local blockvalidator = 'Block validator';
    local chainprevalidator = 'Chain prevalidator';
    local chainvalidators = 'Chain validators';
    graphPanel.new(
      title='Pending workers requests',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      aliasColors={
        [blockvalidator]: 'light-green',
        [chainprevalidator]: 'light-yellow',
        [chainvalidators]: 'light-blue',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_block_validator_pending_requests',
        legendFormat=blockvalidator,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_prevalidators_pending_requests',
        legendFormat=chainprevalidator,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_pending_requests',
        legendFormat=chainvalidators,
      )
    ),

  distributedDB:
    local headers_table = 'headers table';
    local headers_scheduler = 'headers scheduler';
    local operations_table = 'operations table';
    local operations_scheduler = 'operations scheduler';
    local operation_table = 'operation table';
    local operation_scheduler = 'operation scheduler';
    graphPanel.new(
      title='DDB Workers',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_current=false,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_rightSide=true,
      legend_show=true,
      legend_total=false,
      legend_values=true,
      aliasColors={
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_ddb_block_header_db_table_length',
        legendFormat=headers_table,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_ddb_block_header_db_scheduler_length',
        legendFormat=headers_scheduler,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_ddb_operations_db_table_length',
        legendFormat=operations_table,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_ddb_operations_db_scheduler_length',
        legendFormat=operations_scheduler,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_ddb_operation_db_table_length',
        legendFormat=operation_table,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_ddb_operation_db_scheduler_length',
        legendFormat=operation_scheduler,
      )
    ),

  prevalidators:
    local closed = 'closed';
    local closing = 'closing';
    local launching = 'launching';
    local running = 'running';
    graphPanel.new(
      title='Main chain prevalidators workers',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      aliasColors={
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_prevalidators_workers_closed',
        legendFormat=closed,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_prevalidators_workers_closing',
        legendFormat=closing,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_prevalidators_workers_launching',
        legendFormat=launching,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_prevalidators_workers_running',
        legendFormat=running,
      )
    ),

  peerValidators:
    local closed = 'closed';
    local closing = 'closing';
    local launching = 'launching';
    local running = 'running';
    graphPanel.new(
      title='Main chain peer validators workers',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      aliasColors={
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_closed',
        legendFormat=closed,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_closing',
        legendFormat=closing,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_launching',
        legendFormat=launching,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_running',
        legendFormat=running,
      )
    ),

  peerValidatorsFetchedBlockPipelines:
    local avg = 'fetched blocks avg';
    local max = 'fetched blocks max';
    local min = 'fetched blocks min';
    graphPanel.new(
      title='Peer validators fetched block pipelines',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      aliasColors={
        [avg]: 'light-green',
        [max]: 'light-red',
        [min]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_fetched_block_avg',
        legendFormat=avg,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_fetched_block_max',
        legendFormat=max,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_fetched_block_min',
        legendFormat=min,
      )
    ),

  peerValidatorsFetchedHeaderPipelines:
    local avg = 'fetched blocks avg';
    local max = 'fetched blocks max';
    local min = 'fetched blocks min';
    graphPanel.new(
      title='Peer validators fetched header pipelines',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      aliasColors={
        [avg]: 'light-green',
        [max]: 'light-red',
        [min]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_fetched_block_avg',
        legendFormat=avg,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_fetched_block_max',
        legendFormat=max,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_stats_main_chain_validators_peer_validators_fetched_block_min',
        legendFormat=min,
      )
    ),
}
