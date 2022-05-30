local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;
local singlestat = grafana.singlestat;
local statPanel = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local heatmapPanel = grafana.heatmapPanel;
local prometheus = grafana.prometheus;
local namespace = "octez";

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
	namespace + '_version',
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
	'(label_replace(' + namespace + '_version,"commit_hash_short","$1","commit_hash","^(.{8}).*$"))',
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
	namespace + '_version',
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
	namespace + '_version',
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
	namespace + '_version',
	legendFormat='{{ distributed_db_version }}',
	instant=true
      )
    ),

  bootstrapStatus:
    statPanel.new(
      title='Bootstrap status',
      datasource='Prometheus',
      ).addMappings([
	{
          "options": {
            "0": {
              "color": "red",
              "index": 1,
              "text": "Bootstrapping"
            },
            "1": {
              "color": "green",
              "index": 0,
              "text": "Bootstrapped"
            },
            "null": {
              "color": "yellow",
              "index": 2,
              "text": "Unknown"
            }
          },
          "type": "value"
        }
      ],
    ).addTarget(
      prometheus.target(
	namespace + '_validator_chain_is_bootstrapped',
	instant=true
      )
    ),

  syncStatus:
    statPanel.new(
      title='Sync status',
      datasource='Prometheus',
    ).addMappings([
	{
          "options": {
            "0": {
              "color": "red",
              "index": 0,
              "text": "Unsync"
            },
            "1": {
              "color": "green",
              "index": 1,
              "text": "Sync"
            },
	    "2": {
              "color": "green",
              "index": 2,
              "text": "Stuck"
            },
            "null": {
              "color": "yellow",
              "index": 3,
              "text": "Unknow"
            }
          },
          "type": "value"
        }
      ],
    ).addTarget(
      prometheus.target(
	namespace + '_validator_chain_synchronisation_status',
	instant=true
      )
    ),

  # Reflects the uptime of the monitoring of the job, not the uptime
  # of the process.
  uptime:
    singlestat.new(
      title='Node uptime',
      datasource='Prometheus',
      format='dtdhms',
      valueName='max',
      description= 'Reflects the uptime of the monitoring of the job, not the uptime of the process.',
    ).addTarget(
      prometheus.target(
	'time()-(process_start_time_seconds{job="node"})',
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
        namespace + '_validator_chain_head_level',
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
        namespace + '_store_savepoint_level',
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
        namespace + '_store_checkpoint_level',
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
        namespace + '_store_caboose_level',
        legendFormat='current caboose',
	instant=true
      )
    ),

  headCycleLevel:
    singlestat.new(
      title='Current cycle',
      datasource='Prometheus',
      format='none'
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_cycle',
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
        namespace + '_validator_chain_head_level',
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
        namespace + '_store_invalid_blocks',
        legendFormat=blocks,
      )
    ),

  gasConsumedHistory:
    local blocks = 'Gas consumed';
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
        [blocks]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_consumed_gas',
        legendFormat=blocks,
      )
    ),

  roundHistory:
    local blocks = 'Round';
    graphPanel.new(
      title='Round history',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_max=true,
      legend_alignAsTable=true,
      legend_values=true,
      aliasColors={
        [blocks]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_validator_chain_head_round',
        legendFormat=blocks,
      )
    ),

  blocksValidationTime:
    local treatment =  namespace + '_validator_block_last_finished_request_treatment_timestamp';
    local completion = namespace + '_validator_block_last_finished_request_completion_timestamp';
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
        namespace + '_validator_block_operations_per_pass{pass_id="0"}',
        legendFormat=consensus,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="1"}',
        legendFormat=vote,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="2"}',
        legendFormat=anonymous,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_validator_block_operations_per_pass{pass_id="3"}',
        legendFormat=manager,
      )
    )
    ,

  storeMergeTime:
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
        namespace + '_store_last_merge_time',
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
        namespace + '_store_last_written_block_size',
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
        'ocaml_gc_minor_collections',
        legendFormat=minor,
      ),
      prometheus.target(
        'ocaml_gc_major_collections',
        legendFormat=major,
      ),
      prometheus.target(
        'ocaml_gc_forced_major_collections',
        legendFormat=forced,
      ),
      prometheus.target(
        'ocaml_gc_compactions',
        legendFormat=compact,
      )
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
        'ocaml_gc_heap_words',
        legendFormat=major,
    ))

}
