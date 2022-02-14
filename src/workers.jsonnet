local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local singlestat = grafana.singlestat;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;
local namespace = 'octez';

//##
// Workers
//##
{
  requests:
    local blockvalidator = 'Block validator';
    local chainvalidators = 'Chain validators';
    graphPanel.new(
      title='Pending workers requests',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_avg=true,
      aliasColors={
        [blockvalidator]: 'light-green',
        [chainvalidators]: 'light-blue',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_validator_block_pending_requests',
        legendFormat=blockvalidator,
      ),
      prometheus.target(
        namespace + '_validator_chain_pending_requests',
        legendFormat=chainvalidators,
      )
    ]),

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
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_requester_table_length{requester_kind="block_header"}',
        legendFormat=headers_table,
      ),
      prometheus.target(
        namespace + '_distributed_db_requester_scheduler_length{requester_kind="block_header"}',
        legendFormat=headers_scheduler,
      ),
      prometheus.target(
        namespace + '_distributed_db_requester_table_length{requester_kind="operations"}',
        legendFormat=operations_table,
      ),
      prometheus.target(
        namespace + '_distributed_db_requester_scheduler_length{requester_kind="operations"}',
        legendFormat=operations_scheduler,
      ),
      prometheus.target(
	namespace + '_distributed_db_requester_table_length{requester_kind="operation"}',
        legendFormat=operation_table,
      ),
      prometheus.target(
        namespace + '_distributed_db_requester_scheduler_length{requester_kind="operation"}',
        legendFormat=operation_scheduler,
      )
    ]),

  peerValidators:
    local connections = 'connections';
    local invalid_blocks = 'invalid blocks';
    local invalid_locator = 'invalid locator';
    local new_branch_completed = 'new branch completed';
    local new_head_completed = 'new head completed';
    local on_no_request = 'on no request count';
    local new_branch = 'fetching canceled new branch';
    local new_known_valid_head = 'fetching canceled new known valid head';
    local new_unknown_head = 'fetching canceled knew unknown head';
    local system_error = 'system error';
    local too_short_locator = 'too short locator';
    local unavailable_protocol = 'unavailable protocol';
    local unknown_ancestor = 'unknown ancestor';
    local unknown_error = 'unknown error';
    graphPanel.new(
      title='Peer validators',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      logBase1Y=10,
      aliasColors={
	[connections]: 'light-green',
	[invalid_blocks]: 'light-red',
	[invalid_locator]: 'light-orange',
	[new_branch_completed]: 'light-blue',
	[new_head_completed]: 'blue',
	[on_no_request]: 'white',
	[new_branch]: 'green',
	[new_known_valid_head]: 'light-yellow',
	[new_unknown_head]: 'light-orange',
	[system_error]: 'orange',
	[too_short_locator]: 'brown',
	[unavailable_protocol]: 'light-red',
	[unknown_ancestor]: 'yellow',
	[unknown_error]: 'red',
      },
    ).addTargets(
      [prometheus.target(
        namespace + '_validator_peer_connections',
        legendFormat=connections,
      ),
      prometheus.target(
        namespace + '_validator_peer_invalid_block',
        legendFormat=invalid_blocks,
      ),
      prometheus.target(
        namespace + '_validator_peer_invalid_locator',
        legendFormat=invalid_locator
      ),
      prometheus.target(
        namespace + '_validator_peer_new_branch_completed',
        legendFormat=new_branch_completed,
      ),
      prometheus.target(
        namespace + '_validator_peer_new_head_completed',
        legendFormat=new_head_completed,
      ),
      prometheus.target(
        namespace + '_validator_peer_on_no_request_count',
        legendFormat=on_no_request,
      ),
      prometheus.target(
        namespace + '_validator_peer_operations_fetching_canceled_new_branch',
        legendFormat=new_branch,
      ),
      prometheus.target(
        namespace + '_validator_peer_operations_fetching_canceled_new_known_valid_head',
        legendFormat=new_known_valid_head,
      ),
      prometheus.target(
        namespace + '_validator_peer_operations_fetching_canceled_new_unknown_head',
        legendFormat=new_unknown_head,
      ),
      prometheus.target(
        namespace + '_validator_peer_system_error',
        legendFormat=system_error,
      ),
      prometheus.target(
        namespace + '_validator_peer_too_short_locator',
        legendFormat=too_short_locator,
      ),
      prometheus.target(
        namespace + '_validator_peer_unavailable_protocol',
        legendFormat=unavailable_protocol,
      ),
      prometheus.target(
        namespace + '_validator_peer_unknown_ancestor',
        legendFormat=unknown_ancestor,
      ),
      prometheus.target(
        namespace + '_validator_peer_unknown_error',
        legendFormat=unknown_error,
      )]
    )
    ,


  validatorTreatmentRequests:
    local chainPush = namespace + '_validator_chain_last_finished_request_push_timestamp';
    local chainTreatment = namespace + '_validator_chain_last_finished_request_treatment_timestamp';
    local blockPush = namespace + '_validator_block_last_finished_request_push_timestamp';
    local blockTreatment = namespace + '_validator_block_last_finished_request_treatment_timestamp';
    local chainTreatmentTime = chainTreatment + ' - ' + chainPush;
    local blockTreatmentTime = blockTreatment + ' - ' + blockPush;
    local chain = 'chain validator';
    local block = 'block validator';
    graphPanel.new(
      title='Validators Requests Treatments',
      datasource='Prometheus',
      linewidth=1,
      format='s',
      legend_alignAsTable=true,
      legend_current=false,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_values=true,
      aliasColors={
        [chain]: 'light-green',
        [block]: 'light-red',
      },
    ).addTargets([
      prometheus.target(
	chainTreatmentTime,
        legendFormat=chain,
      ),
      prometheus.target(
        blockTreatmentTime,
        legendFormat=block,
      )
    ]),

  validatorCompletionRequests:
    local chainTreatment = namespace + '_validator_chain_last_finished_request_treatment_timestamp';
    local chainCompletion = namespace + '_validator_chain_last_finished_request_completion_timestamp';
    local blockTreatment = namespace + '_validator_block_last_finished_request_treatment_timestamp';
    local blockCompletion = namespace + '_validator_block_last_finished_request_completion_timestamp';
    local chainCompletionTime = chainCompletion + ' - ' + chainTreatment;
    local blockCompletionTime = blockCompletion + ' - ' + blockTreatment;
    local chain = 'chain validator';
    local block = 'block validator';
    graphPanel.new(
      title='Validators Requests Completion',
      datasource='Prometheus',
      linewidth=1,
      format='s',
      legend_alignAsTable=true,
      legend_current=false,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_values=true,
      aliasColors={
        [chain]: 'light-green',
        [block]: 'light-red',
      },
    ).addTargets([
      prometheus.target(
	chainCompletionTime,
        legendFormat=chain,
      ),
      prometheus.target(
        blockCompletionTime,
        legendFormat=block,
      )
    ])

}
