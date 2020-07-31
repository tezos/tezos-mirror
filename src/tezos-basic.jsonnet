local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;
local singlestat = grafana.singlestat;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;
local row = grafana.row;


local node = import './node.jsonnet';
local p2p = import './p2p.jsonnet';
local hardware = import './hardware.jsonnet';
local workers = import './workers.jsonnet';


####
# Grafana main stuffs
###
dashboard.new(
  'Tezos node test',
  tags=['tezos','storage'],
  schemaVersion=18,
  editable=true,
  time_from='now-3h',
  refresh='',
)


#Node a grid is 24 slots wide
.addPanels(
  [
    ########
    row.new(
      title='Node stats',
      repeat='',
      showTitle=true,
    ) ,
    node.buildInfo           + {gridPos: {h: 3, w: 4, x: 0, y: 0}},
    node.headLevel           + {gridPos: {h: 3, w: 4, x: 0, y: 3}},
    node.checkpointLevel     + {gridPos: {h: 3, w: 4, x: 0, y: 6}},
    node.headCycleLevel      + {gridPos: {h: 3, w: 4, x: 0, y: 9}},
    p2p.trustedPoints        + {gridPos: {h: 2, w: 4, x: 0, y: 12}},
    p2p.privateConnections   + {gridPos: {h: 2, w: 4, x: 0, y: 14}},

    node.headHistory                                                + {gridPos: {h: 8, w: 10, x: 4, y: 0}},
    node.invalidBlocksHistory                                       + {gridPos: {h: 8, w: 10, x: 4, y: 8}},

    node.logs                                                                                               + {gridPos: {h: 16, w: 10, x: 14, y: 0}},


    node.headOperations      + {gridPos: {h: 8, w: 24, x: 0, y: 14}},

    ########
    row.new(
      title='Hardware stats',
      repeat='',
      showTitle=true,
    ) + {gridPos: {h: 0, w: 8, x: 0, y: 22}},
    hardware.cpu               + {gridPos: {h: 8, w: 8, x: 0, y: 22}},
    hardware.ios                                                      + {gridPos: {h: 8, w: 8, x: 8, y: 22}},
    hardware.memory                                                                                          + {gridPos: {h:8, w:8, x:16, y:22}},

    hardware.storage           + {gridPos: {h: 8, w: 12, x: 0, y: 30}},
    hardware.fileDescriptors                                            + {gridPos: {h: 8, w: 12, x: 12, y: 30}},

    ########
    row.new(
      title='P2P stats',
      repeat='',
      showTitle=true,
    ) + {gridPos: {h: 0, w: 8, x: 0, y: 38}},
    p2p.mempoolPending     + {gridPos: {h: 8, w: 12, x: 0, y: 38}},
    p2p.totalConnections                                           + {gridPos: {h: 8, w: 12, x: 12, y: 38}},

    p2p.peers              + {gridPos: {h: 8, w: 12, x: 0, y: 46}},
    p2p.points                                                     + {gridPos: {h: 8, w: 12, x: 12, y: 46}},

    ########
    row.new(
      title='Workers stats',
      repeat='',
      showTitle=true,
    ) + {gridPos: {h: 0, w: 8, x: 0, y: 54}},
    workers.requests                             + {gridPos: {h: 8, w: 12, x: 12, y: 54}},
    workers.distributedDB                                                                 + {gridPos: {h: 8, w: 12, x: 0, y: 54}},

    workers.prevalidators                        + {gridPos: {h: 8, w: 12, x: 12, y: 62}},
    workers.peerValidators                                                                + {gridPos: {h: 8, w: 12, x: 0, y: 62}},

    workers.peerValidatorsFetchedBlockPipelines  + {gridPos: {h: 8, w: 12, x: 12, y: 70}},
    workers.peerValidatorsFetchedHeaderPipelines                                          + {gridPos: {h: 8, w: 12, x: 0, y: 70}},

    ########
    row.new(
      title='Miscellaneous',
      repeat='',
      showTitle=true,
    ) + {gridPos: {h: 0, w: 8, x: 0, y: 78}},
    node.gcOperations + {gridPos: {h: 8, w: 12, x: 0, y: 78}},
    node.gcMajorHeap                                          + {gridPos: {h: 8, w: 12, x: 12, y: 78}},

  ]
)
