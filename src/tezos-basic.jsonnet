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

local boardtitle = 'Tezos branch: ' + std.extVar('branch');

//###
// Grafana main stuffs
//##
dashboard.new(
  title=boardtitle,
  tags=['tezos', 'storage'],
  schemaVersion=18,
  editable=true,
  time_from='now-3h',
  refresh='',
)


#Node a grid is 24 slots wide
.addPanels(
  [
    //#######
    row.new(
      title='Node stats',
      repeat='',
      showTitle=true,
    ) ,
    node.buildInfo           + {gridPos: {h: 3, w: 4, x: 0, y: 0}},
    node.uptime              + {gridPos: {h: 2, w: 4, x: 0, y: 3}},
    node.headLevel           + {gridPos: {h: 3, w: 4, x: 0, y: 5}},
    node.savepointLevel      + {gridPos: {h: 3, w: 2, x: 0, y: 7}},
    node.cabooseLevel        + {gridPos: {h: 3, w: 2, x: 2, y: 7}},
    node.headCycleLevel      + {gridPos: {h: 3, w: 4, x: 0, y: 9}},
    p2p.trustedPoints        + {gridPos: {h: 2, w: 4, x: 0, y: 11}},
    p2p.privateConnections   + {gridPos: {h: 2, w: 4, x: 0, y: 13}},


    node.headHistory                                                + {gridPos: {h: 10, w: 10, x: 4, y: 0}},

    node.blocksPerSecond                                            + {gridPos: {h: 8, w: 10, x: 4, y: 10 }},

    node.logs                                                                                               + {gridPos: {h: 18, w: 10, x: 14, y: 0}},


    node.headOperations { gridPos: { h: 8, w: 14, x: 0, y: 14 } }, node.invalidBlocksHistory {gridPos: {h: 8, w: 10, x: 14, y: 14}},

    //#######
    row.new(
      title='Hardware stats',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 8, x: 0, y: 22 } },
    hardware.cpu { gridPos: { h: 8, w: 12, x: 0, y: 22 } },
    hardware.memory { gridPos: { h: 8, w: 12, x: 12, y: 22 } },


    hardware.diskFreeSpace { gridPos: { h: 8, w: 2, x: 0, y: 30 } },
    hardware.storage { gridPos: { h: 8, w: 11, x: 2, y: 30 } },
    hardware.ios { gridPos: { h: 8, w: 11, x: 13, y: 30 } },

    hardware.networkIOS { gridPos: { h: 8, w: 12, x: 0, y: 38 } },
    hardware.fileDescriptors { gridPos: { h: 8, w: 12, x: 12, y: 38 } },

    //#######
    row.new(
      title='P2P stats',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 8, x: 0, y: 46 } },
    p2p.mempoolPending { gridPos: { h: 8, w: 12, x: 0, y: 46 } },
    p2p.totalConnections { gridPos: { h: 8, w: 12, x: 12, y: 46 } },

    p2p.peers { gridPos: { h: 8, w: 12, x: 0, y: 54 } },
    p2p.points { gridPos: { h: 8, w: 12, x: 12, y: 54 } },

    //#######
    row.new(
      title='Workers stats',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 8, x: 0, y: 62 } },
    workers.requests { gridPos: { h: 8, w: 12, x: 12, y: 62 } },
    workers.distributedDB { gridPos: { h: 8, w: 12, x: 0, y: 62 } },

    workers.prevalidators { gridPos: { h: 8, w: 12, x: 12, y: 70 } },
    workers.peerValidators { gridPos: { h: 8, w: 12, x: 0, y: 70 } },

    workers.peerValidatorsFetchedBlockPipelines { gridPos: { h: 8, w: 12, x: 12, y: 78 } },
    workers.peerValidatorsFetchedHeaderPipelines { gridPos: { h: 8, w: 12, x: 0, y: 78 } },

    //#######
    row.new(
      title='Miscellaneous',
      repeat='',
      showTitle=true,
    ) + { gridPos: { h: 0, w: 8, x: 0, y: 86 } },
    node.gcOperations { gridPos: { h: 8, w: 12, x: 0, y: 86 } },
    node.gcMajorHeap { gridPos: { h: 8, w: 12, x: 12, y: 86 } }

  ]
)
