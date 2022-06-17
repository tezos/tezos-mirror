local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;
local singlestat = grafana.singlestat;
local statPanel = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local tablePanel = grafana.tablePanel;
local prometheus = grafana.prometheus;
local namespace = 'octez';
local node_instance = '{' + std.extVar('node_instance_label') + '="$node_instance"}';

//##
// Octez related stats
//##

{
  releaseVersionInfo:
    singlestat.new(
      title='Node release version',
      datasource='Prometheus',
      format='none',
      valueName='name',
    ).addTarget(
      prometheus.target(
        namespace + '_version' + node_instance,
        legendFormat='{{ version }}',
        instant=true
      )
    ),

  releaseCommitInfo:
    singlestat.new(
      title='Node release commit',
      datasource='Prometheus',
      format='none',
      valueName='name',
    ).addTarget(
      prometheus.target(
        '(label_replace(' + namespace + '_version' + node_instance + ',"commit_hash_short" ,"$1","commit_hash","^(.{8}).*$"))',
        legendFormat='{{ commit_hash_short }}',
        instant=true
      )
    ),

  chainNameInfo:
    singlestat.new(
      title='Chain name',
      datasource='Prometheus',
      format='none',
      valueName='name',
    ).addTarget(
      prometheus.target(
        namespace + '_version' + node_instance,
        legendFormat='{{ chain_name }}',
        instant=true
      )
    ),

  p2pVersion:
    singlestat.new(
      title='P2p version',
      datasource='Prometheus',
      format='none',
      valueName='name',
    ).addTarget(
      prometheus.target(
        namespace + '_version' + node_instance,
        legendFormat='{{ p2p_version }}',
        instant=true
      )
    ),

  distributedDbVersion:
    singlestat.new(
      title='Distributed db version',
      datasource='Prometheus',
      format='none',
      valueName='value',
    ).addTarget(
      prometheus.target(
        namespace + '_version' + node_instance,
        legendFormat='{{ distributed_db_version }}',
        instant=true
      )
    ),

  bootstrapStatus:
    statPanel.new(
      title='Bootstrap status',
      datasource='Prometheus',
    ).addMappings(
      [
        {
          options: {
            '0': {
              color: 'red',
              index: 1,
              text: 'Bootstrapping',
            },
            '1': {
              color: 'green',
              index: 0,
              text: 'Bootstrapped',
            },
            'null': {
              color: 'yellow',
              index: 2,
              text: 'Unknown',
            },
          },
          type: 'value',
        },
      ],
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_is_bootstrapped' + node_instance,
        instant=true
      )
    ),

  syncStatus:
    statPanel.new(
      title='Sync status',
      datasource='Prometheus',
    ).addMappings(
      [
        {
          options: {
            '0': {
              color: 'red',
              index: 0,
              text: 'Unsync',
            },
            '1': {
              color: 'green',
              index: 1,
              text: 'Sync',
            },
            '2': {
              color: 'red',
              index: 2,
              text: 'Stuck',
            },
            'null': {
              color: 'yellow',
              index: 3,
              text: 'Unknow',
            },
          },
          type: 'value',
        },
      ],
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_synchronisation_status' + node_instance,
        instant=true
      )
    ),

  // Reflects the uptime of the monitoring of the job, not the uptime
  // of the process.
  uptime:
    singlestat.new(
      title='Node uptime',
      datasource='Prometheus',
      format='dtdhms',
      valueName='max',
      description='Reflects the uptime of the monitoring of the job, not the uptime of the process.',
    ).addTarget(
      prometheus.target(
        'time()-(process_start_time_seconds' + node_instance + ')',
        legendFormat='node uptime',
        instant=true
      )
    ),

  headLevel:
    singlestat.new(
      title='Current head level',
      datasource='Prometheus',
      format='none',
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_level' + node_instance,
        legendFormat='current head level',
        instant=true
      )
    ),

  savepointLevel:
    singlestat.new(
      title='Current savepoint level',
      datasource='Prometheus',
      format='none',
    ).addTarget(
      prometheus.target(
        namespace + '_store_savepoint_level' + node_instance,
        legendFormat='current savepoint',
        instant=true
      )
    ),

  checkpointLevel:
    singlestat.new(
      title='Current checkpoint level',
      datasource='Prometheus',
      format='none'
    ).addTarget(
      prometheus.target(
        namespace + '_store_checkpoint_level' + node_instance,
        legendFormat='current checkpoint',
        instant=true
      )
    ),

  cabooseLevel:
    singlestat.new(
      title='Current caboose level',
      datasource='Prometheus',
      format='none'
    ).addTarget(
      prometheus.target(
        namespace + '_store_caboose_level' + node_instance,
        legendFormat='current caboose',
        instant=true
      )
    ),

  levelsTable:
    tablePanel.new(
      title='Chain levels',
      datasource='Prometheus',
      transform=('timeseries_to_rows'),
    ).addTargets([
      prometheus.target(
        namespace + '_validator_chain_head_cycle' + node_instance,
        legendFormat='Current cycle',
        instant=true
      ),
      prometheus.target(
        namespace + '_store_caboose_level' + node_instance,
        legendFormat='Current caboose',
        instant=true
      ),
      prometheus.target(
        namespace + '_store_checkpoint_level' + node_instance,
        legendFormat='Current checkpoint',
        instant=true
      ),
      prometheus.target(
        namespace + '_store_savepoint_level' + node_instance,
        legendFormat='Current savepoint',
        instant=true
      ),
    ]).hideColumn('Time'),


  headCycleLevel:
    singlestat.new(
      title='Current cycle',
      datasource='Prometheus',
      format='none'
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_cycle' + node_instance,
        legendFormat='Current cycle',
        instant=true
      )
    ),

  headHistory:
    local head = 'Head level';
    graphPanel.new(
      title='Head level history',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=false,
      aliasColors={
        [head]: 'green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_level' + node_instance,
        legendFormat=head
      )
    ),


  invalidBlocksHistory:
    local blocks = 'Invalid blocks';
    graphPanel.new(
      title='Invalid blocks history',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_total=true,
      legend_values=true,
      aliasColors={
        [blocks]: 'light-red',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_store_invalid_blocks' + node_instance,
        legendFormat=blocks,
      )
    ),

  invalidBlocksMean: statPanel.new(
    title='Invalid blocks mean',
    datasource='Prometheus',
  ).addTarget(
    prometheus.target(
      namespace + '_store_invalid_blocks' + node_instance
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

  alternateHeadsCount:
    local alternateHeads = 'Alternate heads count';
    graphPanel.new(
      title='Alternate heads count',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      aliasColors={
        [alternateHeads]: 'yellow',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_store_alternate_heads_count' + node_instance,
        legendFormat=alternateHeads,
      )
    ),

  gasConsumedHistory:
    local gas = 'Gas consumed';
    graphPanel.new(
      title='Gas consumed history',
      datasource='Prometheus',
      linewidth=1,
      format='sci',
      legend_alignAsTable=true,
      legend_current=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_total=true,
      legend_values=true,
      aliasColors={
        [gas]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_consumed_gas' + node_instance,
        legendFormat=gas,
      )
    ),

  roundHistory:
    local round = 'Round';
    graphPanel.new(
      title='Round history',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_max=true,
      legend_alignAsTable=true,
      legend_values=true,
      aliasColors={
        [round]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_round' + node_instance,
        legendFormat=round,
      )
    ),

  maxRound:
    statPanel.new(
      title='Max round',
      datasource='Prometheus',
      reducerFunction='max',
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_round' + node_instance,
      )
    ),

  branchSwitchCount:
    statPanel.new(
      title='Branch switch count',
      datasource='Prometheus',
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_branch_switch_count' + node_instance,
      )
    ),

  blocksValidationTime:
    local treatment = namespace + '_validator_block_last_finished_request_treatment_timestamp' + node_instance;
    local completion = namespace + '_validator_block_last_finished_request_completion_timestamp' + node_instance;
    local validation = 'Validation time';
    graphPanel.new(
      title='Block validation time',
      datasource='Prometheus',
      linewidth=1,
      format='s',
      legend_alignAsTable=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_values=true,
      aliasColors={
        [validation]: 'light-blue',
      },
    ).addTarget(
      prometheus.target(
        completion + ' - ' + treatment,
        format='time_series',
        legendFormat=validation,
      )
    ),

  blocksValidationMean:
    local treatment = namespace + '_validator_block_last_finished_request_treatment_timestamp' + node_instance;
    local completion = namespace + '_validator_block_last_finished_request_completion_timestamp' + node_instance;
    local blocksValidation = 'blocks validation mean';
    statPanel.new(
      title='Blocks validation mean',
      datasource='Prometheus',
    ).addTarget(
      prometheus.target(
        completion + ' - ' + treatment,
        legendFormat=blocksValidation,
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

  headOperations:
    local consensus = 'Consensus';
    local vote = 'Vote';
    local anonymous = 'Anonymous';
    local manager = 'Manager';
    graphPanel.new(
      title='Head operations',
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
      legend_values=true,
      aliasColors={
      },
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="0",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=consensus,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="1",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=vote,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="2",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=anonymous,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="3",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=manager,
      )
    )
  ,

  storeMergeTimeGraph:
    local mergeTime = 'Merge time';
    graphPanel.new(
      title='Store merge time',
      datasource='Prometheus',
      linewidth=1,
      format='s',
      legend_alignAsTable=true,
      legend_max=true,
      legend_current=true,
      legend_values=true,
      aliasColors={
        [mergeTime]: 'light-blue',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_store_last_merge_time' + node_instance,
        legendFormat=mergeTime,
      )
    )
  ,

  storeMergeTime:
    local mergeTime = 'Merge time';
    statPanel.new(
      title='Store merge time',
      datasource='Prometheus',
    ).addTarget(
      prometheus.target(
        namespace + '_store_last_merge_time' + node_instance,
        legendFormat=mergeTime,
      )
    )
  ,

  writtenBlockSize:
    local writtenBlockSize = 'Written block size';
    graphPanel.new(
      title='Last written block size',
      datasource='Prometheus',
      linewidth=1,
      format='bytes',
      legend_alignAsTable=true,
      legend_min=true,
      legend_avg=true,
      legend_max=true,
      legend_total=true,
      legend_current=true,
      legend_values=true,
      aliasColors={
        [writtenBlockSize]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_store_last_written_block_size' + node_instance,
        legendFormat=writtenBlockSize,
      )
    )
  ,

  //## GC

  gcOperations:
    local minor = 'Minor collections';
    local major = 'Major collections';
    local forced = 'Forced major collections';
    local compact = 'Heap compactions';
    graphPanel.new(
      title='GC maintenance operations',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      logBase1Y=10,
      aliasColors={
        [minor]: 'light-green',
        [major]: 'light-yellow',
        [forced]: 'light-blue',
        [compact]: 'light-red',
      },
    ).addTargets([
      prometheus.target(
        'ocaml_gc_minor_collections' + node_instance,
        legendFormat=minor,
      ),
      prometheus.target(
        'ocaml_gc_major_collections' + node_instance,
        legendFormat=major,
      ),
      prometheus.target(
        'ocaml_gc_forced_major_collections' + node_instance,
        legendFormat=forced,
      ),
      prometheus.target(
        'ocaml_gc_compactions' + node_instance,
        legendFormat=compact,
      ),
    ]),

  gcMajorHeap:
    local major = 'Major heap';
    graphPanel.new(
      title='GC major word sizes',
      datasource='Prometheus',
      linewidth=1,
      format='bytes',
      legend_show=false,
      aliasColors={
        [major]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'ocaml_gc_heap_words' + node_instance,
        legendFormat=major,
      )
    ),

}
