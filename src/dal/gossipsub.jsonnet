// Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
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

local grafana = import '../../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local dashboard = grafana.dashboard;
local template = grafana.template;
local singlestat = grafana.singlestat;
local statPanel = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local tablePanel = grafana.tablePanel;
local prometheus = grafana.prometheus;
local namespace = 'dal_gs';
local node_instance = '{' + std.extVar('node_instance_label') + '="$node_instance"}';

//##
// Gossipsub related stats
//##

{

  countTopics:
    local topics = "Node's topics";
    graphPanel.new(
      title='The number of topics the node is subscribed to',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [topics]: 'blue',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_count_topics' + node_instance,
        legendFormat=topics
      ),
    ]),

  countConnections:
    local connections = 'All connections';
    local bootstrap = 'Bootstrap connections';
    graphPanel.new(
      title='The number of (bootstrap) connections',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [connections]: 'green',
        [bootstrap]: 'yellow',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_count_connections' + node_instance,
        legendFormat=connections
      ),
      prometheus.target(
        namespace + '_count_bootstrap_connections' + node_instance,
        legendFormat=bootstrap
      ),
    ]),

  workerStreams:
    local input = 'Input stream';
    local p2p = 'P2P output stream';
    local app = 'Application output stream';
    graphPanel.new(
      title='The size of different worker streams',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [input]: 'green',
        [p2p]: 'blue',
        [app]: 'yellow',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_input_events_stream_length' + node_instance,
        legendFormat=input
      ),
      prometheus.target(
        namespace + '_p2p_output_stream_length' + node_instance,
        legendFormat=p2p
      ),
      prometheus.target(
        namespace + '_app_output_stream_length' + node_instance,
        legendFormat=app
      ),
    ]),

  appMessagesDeriv:
    local received_valid = 'Received & valid';
    local received_invalid = 'Received & invalid';
    local received_unknown = 'Received & unchecked validity';
    local sent = 'Sent';
    graphPanel.new(
      title='Average sent & received app messages (1-minute interval)',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [received_valid]: 'green',
        [received_invalid]: 'red',
        [received_unknown]: 'orange',
        [sent]: 'blue',
      },
    ).addTargets([
      prometheus.target(
        'deriv(' + namespace + '_count_received_valid_messages' + node_instance + '[1m])',
        legendFormat=received_valid,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_received_invalid_messages' + node_instance + '[1m])',
        legendFormat=received_invalid,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_received_unknown_validity_messages' + node_instance + '[1m])',
        legendFormat=received_unknown,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_sent_messages' + node_instance + '[1m])',
        legendFormat=sent,
      ),
    ]),

  sentOtherMessagesDeriv:
    local grafts = 'Graft';
    local prunes = 'Prune';
    local ihaves = 'IHave';
    local iwants = 'IWant';
    graphPanel.new(
      title='Average sent control & metadata messages (1-minute interval)',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [grafts]: 'blue',
        [prunes]: 'yellow',
        [ihaves]: 'purple',
        [iwants]: 'magenta',
      },
    ).addTargets([
      prometheus.target(
        'deriv(' + namespace + '_count_sent_grafts' + node_instance + '[1m])',
        legendFormat=grafts,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_sent_prunes' + node_instance + '[1m])',
        legendFormat=prunes,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_sent_ihaves' + node_instance + '[1m])',
        legendFormat=ihaves,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_sent_iwants' + node_instance + '[1m])',
        legendFormat=iwants,
      ),
    ]),

  receivedOtherMessagesDeriv:
    local grafts = 'Graft';
    local prunes = 'Prune';
    local ihaves = 'IHave';
    local iwants = 'IWant';
    graphPanel.new(
      title='Average received control & metadata messages (1-minute interval)',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [grafts]: 'blue',
        [prunes]: 'yellow',
        [ihaves]: 'purple',
        [iwants]: 'magenta',
      },
    ).addTargets([
      prometheus.target(
        'deriv(' + namespace + '_count_received_grafts' + node_instance + '[1m])',
        legendFormat=grafts,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_received_prunes' + node_instance + '[1m])',
        legendFormat=prunes,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_received_ihaves' + node_instance + '[1m])',
        legendFormat=ihaves,
      ),
      prometheus.target(
        'deriv(' + namespace + '_count_received_iwants' + node_instance + '[1m])',
        legendFormat=iwants,
      ),
    ]),

  scoresOfPeers:
    graphPanel.new(
      title='Score of the peers connected to this node',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
    ).addTargets([
      prometheus.target(
        namespace + '_scores_of_peers' + node_instance,
        legendFormat='{{ scores_of_peers }}',
      ),
    ]),

  peersOfTopics:
    graphPanel.new(
      title='Number of peers in the mesh for each subscribed topic',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
    ).addTargets([
      prometheus.target(
        namespace + '_count_peers_per_topic' + node_instance,
        legendFormat='{{ count_peers_per_topic }}',
      ),
    ]),

}
