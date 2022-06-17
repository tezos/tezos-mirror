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
    node.bootstrapStatus { gridPos: { h: 3, w: 3, x: 0, y: 0 } },
    node.syncStatus { gridPos: { h: 3, w: 3, x: 3, y: 0 } },
    node.chainNameInfo { gridPos: { h: 3, w: 4, x: 6, y: 0 } },
    node.releaseVersionInfo { gridPos: { h: 3, w: 3, x: 10, y: 0 } },
    node.releaseCommitInfo { gridPos: { h: 3, w: 3, x: 13, y: 0 } },
    node.uptime { gridPos: { h: 3, w: 4, x: 0, y: 3 } },
    node.headLevel { gridPos: { h: 3, w: 4, x: 0, y: 6 } },
    node.levelsTable { gridPos: { h: 6, w: 4, x: 0, y: 9 } },
    p2p.connectionsTable { gridPos: { h: 5, w: 4, x: 0, y: 15 } },
    p2p.totalConnections { gridPos: { h: 8, w: 4, x: 0, y: 20 } },
    node.headOperations { gridPos: { h: 8, w: 12, x: 4, y: 3 } },
    node.headHistory { gridPos: { h: 9, w: 6, x: 10, y: 11 } },
    node.blocksValidationTime { gridPos: { h: 9, w: 6, x: 4, y: 11 } },
    node.gasConsumedHistory { gridPos: { h: 9, w: 8, x: 16, y: 10 } },
    node.roundHistory { gridPos: { h: 9, w: 8, x: 16, y: 19 } },
    node.writtenBlockSize { gridPos: { h: 8, w: 6, x: 4, y: 20 } },
    node.alternateHeadsCount { gridPos: { h: 8, w: 6, x: 10, y: 20 } },
    node.invalidBlocksMean { gridPos: { h: 5, w: 2, x: 16, y: 0 } },
    node.branchSwitchCount { gridPos: { h: 5, w: 2, x: 18, y: 0 } },
    node.storeMergeTime { gridPos: { h: 5, w: 2, x: 20, y: 5 } },
    node.maxRound { gridPos: { h: 5, w: 2, x: 22, y: 0 } },
    node.blocksValidationMean { gridPos: { h: 5, w: 2, x: 16, y: 5 } },
    workers.chainValidatorRequestCompletionMean { gridPos: { h: 5, w: 2, x: 18, y: 5 } },
    workers.peerValidatorErrorsMean { gridPos: { h: 5, w: 2, x: 20, y: 5 } },
    p2p.incomingConnectionsMean { gridPos: { h: 5, w: 2, x: 22, y: 5 } },
  ]
)
