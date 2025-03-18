// Copyright (c) 2022-2024 Nomadic Labs <contact@nomadic-labs.com>
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


// Grafonnet
local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';
local query = grafonnet.query;

// Base
local base = import '../base.jsonnet';
local namespace = 'dal_node';
local prometheus = base.prometheus;
local graph = base.graph;

//##
// DAL node related stats
//##

{

  layer1MonitorLevels(h, w, x, y):
    local head = 'Seen L1 heads';
    local finalized = 'Finalized L1 blocks';
    local headQuery = prometheus('new_layer1_head', legendFormat=head, namespace=namespace);
    local finalizedQuery = prometheus('layer1_block_finalized', legendFormat=finalized, namespace=namespace);
    graph.new('Layer 1 heads & finalized blocks seen by the DAL node', [headQuery, finalizedQuery], h, w, x, y)
    + graph.withQueryColor([[head, 'blue'], [finalized, 'green']]),

  layer1MonitorRounds(h, w, x, y):
    local head = "Seen L1 heads' rounds";
    local finalized = "Finalized L1 blocks' rounds";
    local headQuery = prometheus('new_layer1_head_round', legendFormat=head, namespace=namespace);
    local finalizedQuery = prometheus('layer1_block_finalized_round', legendFormat=finalized, namespace=namespace);
    graph.new('Rounds of layer 1 heads & finalized blocks seen by the DAL node', [headQuery, finalizedQuery], h, w, x, y)
    + graph.withQueryColor([[head, 'blue'], [finalized, 'green']]),

  storedShards(h, w, x, y):
    local shards = 'Stored shards';
    local shardsQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_number_of_stored_shards' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(shards);
    graph.new('The shards stored by this node (1-minute interval)', [shardsQuery], h, w, x, y)
    + graph.withQueryColor([[shards, 'blue']]),

  slotsAttesatationSummary(h, w, x, y):
    local attested = 'Number of attested slots';
    local waiting = 'Number of slots waiting for attestation';
    local attestedQuery = grafonnet.query.prometheus.new('Prometheus', 'sum(' + namespace + '_slots_attested' + base.slot_index_instance_query + ')')
                          + query.prometheus.withLegendFormat(attested);
    local waitingQuery = grafonnet.query.prometheus.new('Prometheus', 'sum(' + namespace + '_slots_waiting_for_attestaion' + base.slot_index_instance_query + ')')
                         + query.prometheus.withLegendFormat(waiting);
    graph.new('Number of slots waiting for attestation and attested slots', [attestedQuery, waitingQuery], h, w, x, y)
    + graph.withQueryColor([[waiting, 'yellow'], [attested, 'green']]),

  slotsWaitingAttestations(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', namespace + '_slots_waiting_for_attestaion' + base.slot_index_instance_query)
              + query.prometheus.withLegendFormat('Slot index: {{ slot_index }}');
    graph.new('Indexes of slots waiting for attestation', [q], h, w, x, y),


  slotsAttested(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', namespace + '_slots_attested' + base.slot_index_instance_query)
              + query.prometheus.withLegendFormat('Slot index: {{ slot_index }}');
    graph.new('Indexes of attested slots', [q], h, w, x, y),

  L1BlockProcessingTime(h, w, x, y):
    local head = 'Processing time per level';
    local query = prometheus('per_level_processing_time', legendFormat=head, namespace=namespace);
    graph.new('Processing time for each level', [query], h, w, x, y),
}
