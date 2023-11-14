// Copyright (c) 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local singlestat = grafana.singlestat;
local statPanel = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local tablePanel = grafana.tablePanel;
local prometheus = grafana.prometheus;
local namespace = 'octez';
local node_instance = '{' + std.extVar('node_instance_label') + '="$node_instance"}';

//##
// P2P
//##

local mkPeersPanel(legendRightSide) =
  local disconnected = 'Disconnected peers';
  local running = 'Running peers';
  local greylisted = 'Waiting to reconnect';
  graphPanel.new(
    title='P2P peers connections',
    datasource='Prometheus',
    linewidth=1,
    format='none',
    legend_alignAsTable=true,
    legend_rightSide=legendRightSide,
    legend_avg=true,
    legend_min=true,
    legend_max=true,
    legend_show=true,
    legend_values=true,
    aliasColors={
      [disconnected]: 'light-yellow',
      [running]: 'light-green',
      [greylisted]: 'light-red',
    },
  ).addTarget(
    prometheus.target(
      namespace + '_p2p_peers_disconnected' + node_instance,
      legendFormat=disconnected,
    )
  ).addTarget(
    prometheus.target(
      namespace + '_p2p_peers_running' + node_instance,
      legendFormat=running,
    )
  ).addTarget(
    prometheus.target(
      namespace + '_p2p_points_greylisted' + node_instance,
      legendFormat=greylisted,
    )
  );


{

  trustedPoints:
    singlestat.new(
      title='Trusted points',
      datasource='Prometheus',
      format='none',
      valueName='max',
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_trusted' + node_instance,
        legendFormat='trusted points',
      )
    ),

  privateConnections:
    singlestat.new(
      title='Private Connections',
      datasource='Prometheus',
      format='none',
      valueName='max',
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_private' + node_instance,
        legendFormat='Private Connections',
      )
    ),

  peers: mkPeersPanel(true),
  peersLegendBottom: mkPeersPanel(false),

  exchangedData:
    local received = 'Received data (KB)';
    local sent = 'Sent data (KB)';
    graphPanel.new(
      title='Average data exchange (10-minute interval)',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [received]: 'blue',
        [sent]: 'green',
      },
    ).addTargets([
      prometheus.target(
        // Divide by 1,000 for KB
        'deriv(' + namespace + '_p2p_io_scheduler_total_recv' + node_instance + '[10m]) / 1000',
        legendFormat=received,
      ),
      prometheus.target(
        // Divide by 1,000 for KB
        'deriv(' + namespace + '_p2p_io_scheduler_total_sent' + node_instance + '[10m]) / 1000',
        legendFormat=sent,
      ),
    ]),

  points:
    local disconnected = 'Disconnected points';
    local running = 'Running points';
    local trusted = 'Trusted points';
    graphPanel.new(
      title='P2P points connections',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [disconnected]: 'light-red',
        [running]: 'light-green',
        [trusted]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_disconnected' + node_instance,
        legendFormat=disconnected,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_running' + node_instance,
        legendFormat=running,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_points_trusted' + node_instance,
        legendFormat=trusted,
      )
    ),

  totalConnections:
    local outgoing = 'Outgoing connections';
    local incoming = 'Incoming connections';
    graphPanel.new(
      title='P2P total connections',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_alignAsTable=true,
      legend_avg=true,
      legend_min=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [outgoing]: 'light-red',
        [incoming]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_outgoing' + node_instance,
        legendFormat=outgoing,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_incoming' + node_instance,
        legendFormat=incoming,
      )
    ),

  incomingConnectionsMean:
    statPanel.new(
      title='Incoming connections mean',
      datasource='Prometheus',
    ).addTarget(
      prometheus.target(
        namespace + '_p2p_connections_incoming' + node_instance
      )
    ),

  mempoolPending:
    local validated = 'Validated';
    local refused = 'Refused';
    local outdated = 'Outdated';
    local branch_refused = 'Branch refused';
    local branch_delayed = 'Branch delayed';
    local unprocessed = 'Unprocessed';
    graphPanel.new(
      title='Mempool status',
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
      legend_total=true,
      legend_values=true,
      logBase1Y=10,
      aliasColors={
        [validated]: 'green',
        [refused]: 'red',
        [outdated]: 'blue',
        [branch_refused]: 'light-orange',
        [branch_delayed]: 'light-red',
        [unprocessed]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_validated' + node_instance,
        legendFormat=validated,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_refused' + node_instance,
        legendFormat=refused,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_outdated' + node_instance,
        legendFormat=outdated,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_branch_refused' + node_instance,
        legendFormat=branch_refused,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_branch_delayed' + node_instance,
        legendFormat=branch_delayed,
      )
    ).addTarget(
      prometheus.target(
        namespace + '_mempool_pending_unprocessed' + node_instance,
        legendFormat=unprocessed,
      )
    ),

  connectionsTable:
    tablePanel.new(
      title='Connections',
      datasource='Prometheus',
      transform=('timeseries_to_rows'),
    ).addTargets([
      prometheus.target(
        namespace + '_p2p_connections_incoming' + node_instance,
        legendFormat='Incomming connections',
        instant=true
      ),
      prometheus.target(
        namespace + '_p2p_points_trusted' + node_instance,
        legendFormat='Trusted points',
        instant=true
      ),
      prometheus.target(
        namespace + '_p2p_connections_private' + node_instance,
        legendFormat='Private points',
        instant=true
      ),
    ]).hideColumn('Time'),


  DDBCurrentHead:
    local get_broadcasted = 'get current head broadcasted';
    local get_sent = 'get current head sent';
    local received = 'current head received';
    local get_received = 'get current head received';
    local sent = 'current head sent';
    local broadcasted = 'current head broadcasted';
    graphPanel.new(
      title='DDB current head messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_broadcasted]: 'green',
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
        [broadcasted]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_current_head_messages{action="broadcasted",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_broadcasted,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_current_head_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_current_head_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_current_head_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_current_head_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_current_head_messages{action="broadcasted",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=broadcasted,
      ),
    ]),

  DDBCurrentBranch:
    local get_sent = 'get current branch sent';
    local received = 'current branch received';
    local get_received = 'get current branch received';
    local sent = 'current branch sent';
    graphPanel.new(
      title='DDB current branch messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_current_branch_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_current_branch_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_current_branch_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_current_branch_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBBlockHeaders:
    local get_sent = 'get block headers sent';
    local received = 'block header received';
    local get_received = 'get block headers received';
    local sent = 'block header sent';
    graphPanel.new(
      title='DDB block headers messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_block_headers_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_block_header_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_block_headers_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_block_header_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBPredHeader:
    local get_sent = 'get predecessor header sent';
    local received = 'predecessor header received';
    local get_received = 'get predecessor header received';
    local sent = 'predecessor header sent';
    graphPanel.new(
      title='DDB predecessor header messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_predecessor_header_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_predecessor_header_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_predecessor_header_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_predecessor_header_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBOperations:
    local get_sent = 'get operations sent';
    local received = 'operation received';
    local get_received = 'get operations received';
    local sent = 'operation sent';
    graphPanel.new(
      title='DDB operations messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_operations_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_operation_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_operations_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_operation_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBOps4Blocks:
    local get_sent = 'get ops for blocks sent';
    local received = 'ops for block received';
    local get_received = 'get ops for blocks received';
    local sent = 'ops for block sent';
    graphPanel.new(
      title='DDB operations for blocks messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_operations_for_blocks_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_operations_for_block_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_operations_for_blocks_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_operations_for_block_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBProtocols:
    local get_sent = 'get protocols sent';
    local received = 'protocol received';
    local get_received = 'get protocols received';
    local sent = 'protocol sent';
    graphPanel.new(
      title='DDB protocols messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_protocols_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_protocol_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_protocols_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_protocol_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBProtoBranch:
    local get_sent = 'get protocol branch sent';
    local received = 'protocol branch received';
    local get_received = 'get protocol branch received';
    local sent = 'protocol branch sent';
    graphPanel.new(
      title='DDB protocol branch messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_protocol_branch_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_protocol_branch_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_protocol_branch_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_protocol_branch_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBCheckpoint:
    local get_sent = 'get checkpoint sent';
    local received = 'checkpoint received';
    local get_received = 'get checkpoint received';
    local sent = 'checkpoint sent';
    graphPanel.new(
      title='DDB checkpoint messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_checkpoint_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_checkpoint_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_checkpoint_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_checkpoint_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

  DDBDeactivate:
    local get_sent = 'get deactivate sent';
    local received = 'deactivate received';
    local get_received = 'get deactivate received';
    local sent = 'deactivate sent';
    graphPanel.new(
      title='DDB deactivate messages',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      legend_show=true,
      logBase1Y=10,
      aliasColors={
        [get_sent]: 'green',
        [received]: 'light-blue',
        [get_received]: 'blue',
        [sent]: 'light-green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_distributed_db_message_get_deactivate_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_sent,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_deactivate_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_get_deactivate_messages{action="received",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=get_received,
      ),
      prometheus.target(
        namespace + '_distributed_db_message_deactivate_messages{action="sent",' + std.extVar('node_instance_label') + '="$node_instance"}',
        legendFormat=sent,
      ),
    ]),

}
