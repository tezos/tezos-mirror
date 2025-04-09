// Copyright (c) 2024 TriliTech <contact@trili.tech>

// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local dashboard = grafonnet.dashboard;
local panel = grafonnet.panel;

// Base imports
local base = import './base.jsonnet';
local profiling = import './profiling.jsonnet';

// External variables
local uid_ext = std.extVar('uid_ext');
local uid = uid_ext == 'default';

// Constants
local panelWidth = 10;
local panelHeight = 10;
local store_y = 1;
local mempool_y = 41;
local chain_validator_y = 81;
local peer_validator_y = 121;
local block_validator_y = 161;

// Create the dashboard
dashboard.new('Octez Profiling Dashboard' + if !uid && uid_ext != '' then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ (if !uid then dashboard.withUid('octez-profiling' + uid_ext) else {})
+ dashboard.withDescription('In-depth profiling for Octez function performance, showing average execution times.')
+ dashboard.withTags(['tezos', 'octez', 'profiling'])
+ dashboard.time.withFrom('now-8h')
+ dashboard.withRefresh('10s')
+ dashboard.withVariables([base.nodeInstance])

+ dashboard.withPanels(
  //#######
  grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Store Profiling')], panelWidth=20, panelHeight=20, startY=store_y)
  + [
    profiling.setHead(h=panelHeight, w=panelWidth, x=0, y=store_y),
    profiling.storeBlock(h=panelHeight, w=panelWidth, x=panelWidth, y=store_y),
    profiling.computeLiveBlocks(h=panelHeight, w=panelWidth, x=0, y=store_y + panelHeight),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Mempool Profiling')], panelWidth=20, panelHeight=20, startY=mempool_y)
  + [
    profiling.onMempoolRequest(h=panelHeight, w=2 * panelWidth, x=0, y=mempool_y),
    profiling.handleUnprocessed(h=panelHeight, w=panelWidth, x=0, y=mempool_y + panelHeight),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Chain Validator Profiling')], panelWidth=20, panelHeight=20, startY=chain_validator_y)
  + [
    profiling.onChainValidatorRequest(h=panelHeight, w=2 * panelWidth, x=0, y=chain_validator_y),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Peer Validator Profiling')], panelWidth=20, panelHeight=20, startY=peer_validator_y)
  + [
    profiling.onPeerValidatorRequest(h=panelHeight, w=2 * panelWidth, x=0, y=peer_validator_y),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Block Validator Profiling')], panelWidth=20, panelHeight=20, startY=block_validator_y)
  + [
    profiling.applyBlock(h=panelHeight, w=panelWidth, x=0, y=block_validator_y),
    profiling.applyOperations(h=panelHeight, w=panelWidth, x=panelWidth, y=block_validator_y),
    profiling.beginApplication(h=panelHeight, w=panelWidth, x=0, y=block_validator_y + panelHeight),
    profiling.beginValidation(h=panelHeight, w=panelWidth, x=panelWidth, y=block_validator_y + panelHeight),
    profiling.finalizeApplication(h=panelHeight, w=panelWidth, x=0, y=block_validator_y + 2 * panelHeight),
    profiling.finalizeValidation(h=panelHeight, w=panelWidth, x=panelWidth, y=block_validator_y + 2 * panelHeight),
    profiling.validateBlock(h=panelHeight, w=panelWidth, x=0, y=block_validator_y + 3 * panelHeight),
    profiling.validateOperation(h=panelHeight, w=panelWidth, x=panelWidth, y=block_validator_y + 3 * panelHeight),
  ]
)
