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
local namespace = 'dal_node';
local node_instance = '{' + std.extVar('node_instance_label') + '="$node_instance"}';

//##
// DAL node related stats
//##

{

  layer1MonitorLevels:
    local head = 'Seen L1 heads';
    local finalized = 'Finalized L1 blocks';
    graphPanel.new(
      title='Layer 1 heads & finalized blocks seen by the DAL node',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [head]: 'blue',
        [finalized]: 'green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_new_layer1_head' + node_instance,
        legendFormat=head
      ),
      prometheus.target(
        namespace + '_layer1_block_finalized' + node_instance,
        legendFormat=finalized
      ),
    ]),

  layer1MonitorRounds:
    local head = "Seen L1 heads' rounds";
    local finalized = "Finalized L1 blocks' rounds";
    graphPanel.new(
      title='Rounds of layer 1 heads & finalized blocks seen by the DAL node',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [head]: 'blue',
        [finalized]: 'green',
      },
    ).addTargets([
      prometheus.target(
        namespace + '_new_layer1_head_round' + node_instance,
        legendFormat=head
      ),
      prometheus.target(
        namespace + '_layer1_block_finalized_round' + node_instance,
        legendFormat=finalized
      ),
    ]),

  storedShards:
    local shards = 'Stored shards';
    graphPanel.new(
      title='The shards stored by this node (1-minute interval)',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [shards]: 'blue',
      },
    ).addTargets([
      prometheus.target(
        'deriv(' + namespace + '_number_of_stored_shards' + node_instance + '[1m])',
        legendFormat=shards
      ),
    ]),

  slotsAttesatationSummary:
    local attested = "Number of attested slots";
    local waiting = "Number of slots waiting for attestation";
    graphPanel.new(
      title='Number of slots waiting for attesatation and of attested slots',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
      aliasColors={
        [waiting]: 'yellow',
        [attested]: 'green',
      },
    ).addTargets([
      prometheus.target(
        'sum(' + namespace + '_slots_waiting_for_attestaion' + node_instance + ')',
        legendFormat=waiting
      ),
      prometheus.target(
        'sum(' + namespace + '_slots_attested' + node_instance + ')',
        legendFormat=attested
      ),
    ]),

  slotsWaitingAttestations:
    graphPanel.new(
      title='Indexes of slots waiting for attestation',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
    ).addTargets([
      prometheus.target(
        namespace + '_slots_waiting_for_attestaion' + node_instance,
        legendFormat='{{ slot_waiting_for_attestaion }}',
      ),
    ]),

  slotsAttested:
    graphPanel.new(
      title='Indexes of attested slots',
      datasource='Prometheus',
      legend_rightSide=false,
      linewidth=1,
      format='none',
      legend_show=true,
    ).addTargets([
      prometheus.target(
        namespace + '_slots_attested' + node_instance,
        legendFormat='{{ slot_attested }}',
      ),
    ]),

}
