//
// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//

// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local dashboard = grafonnet.dashboard;
local panel = grafonnet.panel;

local tezt_cloud = import './tezt_cloud.jsonnet';

local panel_width = 12;
local panel_height = 8;

local node_h = panel_height;

local hardware_y = node_h + 1;
local hardware_h = panel_height * 4 + 1;

local misc_y = hardware_y + hardware_h + 1;

// External variables
local uid_ext = std.extVar('uid_ext');
local uid = uid_ext == 'default';

//###
// Grafana main stuffs
//##
dashboard.new('Tezt Cloud - Layer 1 dashboard' + if !uid && uid_ext != '' then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ (if !uid then dashboard.withUid('tezt-cloud-layer1' + uid_ext) else {})
+ dashboard.withDescription('A dashboard for Layer 1 related experiments with tezt cloud')
+ dashboard.time.withFrom('now-3h')
+ dashboard.withRefresh('20s')
+ dashboard.withVariables(tezt_cloud.datasource)
+ dashboard.withPanels(

  //#######
  grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Node stats')])
  + [
    tezt_cloud.headHistory(agg='avg', h=panel_height, w=panel_width, x=0, y=0),
    tezt_cloud.blocksValidationTime(agg='avg', h=panel_height, w=panel_width, x=panel_width, y=0),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Hardware stats')], startY=hardware_y)
  + [
    tezt_cloud.cpu(agg='avg', app_group='octez-node', h=panel_height, w=panel_width, x=0, y=hardware_y),
    tezt_cloud.cpu(agg='avg', app_group='octez-baker', h=panel_height, w=panel_width, x=panel_width, y=hardware_y),
    tezt_cloud.memory(agg='avg', app_group='octez-node', h=panel_height, w=panel_width, x=0, y=hardware_y),
    tezt_cloud.memory(agg='avg', app_group='octez-baker', h=panel_height, w=panel_width, x=panel_width, y=hardware_y),
    tezt_cloud.ios(agg='avg', app_group='octez-node', h=panel_height, w=panel_width, x=0, y=hardware_y),
    tezt_cloud.ios(agg='avg', app_group='octez-baker', h=panel_height, w=panel_width, x=panel_width, y=hardware_y),
    tezt_cloud.networkIOS(agg='avg', h=panel_height, w=panel_width * 2, x=0, y=hardware_y + 16),
  ]

  //#######
  + grafonnet.util.grid.wrapPanels(panels=[panel.row.new('Miscellaneous')], startY=misc_y)
  + [
    tezt_cloud.gcOperations(agg='avg', h=panel_height, w=panel_width, x=0, y=misc_y),
    tezt_cloud.gcMajorHeap(agg='avg', h=panel_height, w=panel_width, x=panel_width, y=misc_y),
  ]

)
