local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;

local node = import './node.jsonnet';
local p2p = import './p2p.jsonnet';
local workers = import './workers.jsonnet';

local boardtitle = 'Tezos compact dashboard - branch: ' + std.extVar('branch');

dashboard.new(
  title=boardtitle,
  tags=['tezos', 'octez'],
  schemaVersion=18,
  editable=true,
  time_from='now-3h',
  refresh='',
)

.addTemplate(
  template.new(
    name='node_instance',
    datasource='Prometheus',
    query='label_values(octez_version,' + std.extVar('node_instance_label') + ')',
    refresh='load',
    label='Node instance'
  )
)

.addPanels(
  [
    node.bootstrapStatus { gridPos: { h: 3, w: 2, x: 0, y: 1 } },
    node.syncStatus { gridPos: { h: 3, w: 2, x: 2, y: 1 } },
    node.chainNameInfo { gridPos: { h: 3, w: 4, x: 4, y: 1 } },
    node.releaseVersionInfo { gridPos: { h: 3, w: 3, x: 8, y: 1 } },
    node.releaseCommitInfo { gridPos: { h: 3, w: 3, x: 11, y: 1 } },
    node.uptime { gridPos: { h: 3, w: 4, x: 0, y: 4 } },
    node.headLevel { gridPos: { h: 3, w: 4, x: 0, y: 7 } },
    node.p2pVersion { gridPos: { h: 3, w: 2, x: 0, y: 10 } },
    node.distributedDbVersion { gridPos: { h: 3, w: 2, x: 2, y: 10 } },
    node.savepointLevel { gridPos: { h: 3, w: 2, x: 0, y: 13 } },
    node.checkpointLevel { gridPos: { h: 3, w: 2, x: 2, y: 13 } },
    node.cabooseLevel { gridPos: { h: 3, w: 2, x: 0, y: 16 } },
    node.headCycleLevel { gridPos: { h: 3, w: 2, x: 2, y: 16 } },
    p2p.trustedPoints { gridPos: { h: 3, w: 2, x: 0, y: 19 } },
    p2p.privateConnections { gridPos: { h: 3, w: 2, x: 2, y: 19 } },
    p2p.totalConnections { gridPos: { h: 8, w: 4, x: 0, y: 21 } },
    node.headHistory { gridPos: { h: 9, w: 5, x: 9, y: 12 } },
    node.blocksValidationTime { gridPos: { h: 9, w: 5, x: 4, y: 12 } },
    node.headOperations { gridPos: { h: 9, w: 10, x: 4, y: 3 } },
    node.gasConsumedHistory { gridPos: { h: 8, w: 10, x: 14, y: 13 } },
    node.roundHistory { gridPos: { h: 8, w: 10, x: 14, y: 10 } },
    node.writtenBlockSize { gridPos: { h: 8, w: 5, x: 4, y: 21 } },
    node.storeMergeTime { gridPos: { h: 8, w: 10, x: 14, y: 5 } },
    node.alternateHeadsCount { gridPos: { h: 8, w: 5, x: 9, y: 21 } },
    p2p.incomingConnectionsMean { gridPos: { h: 5, w: 2, x: 14, y: 0 } },
    node.invalidBlocksMean { gridPos: { h: 5, w: 2, x: 16, y: 0 } },
    node.blocksValidationMean { gridPos: { h: 5, w: 2, x: 18, y: 0 } },
    workers.chainValidatorRequestCompletionMean { gridPos: { h: 5, w: 2, x: 20, y: 0 } },
    workers.peerValidatorErrorsMean { gridPos: { h: 5, w: 2, x: 22, y: 0 } },
  ]
)
