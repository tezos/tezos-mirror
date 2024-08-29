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
local namespace = 'dal_gs';
local prometheus = base.prometheus;
local graph = base.graph;

//##
// Gossipsub related stats
//##

{

  countTopics(h, w, x, y):
    local topics = "Node's topics";
    local topicsQuery = prometheus('count_topics', legendFormat=topics, namespace=namespace);
    graph.new('The number of topics the node is subscribed to', [topicsQuery], h, w, x, y)
    + graph.withQueryColor([[topics, 'blue']]),

  countConnections(h, w, x, y):
    local connections = 'All connections';
    local bootstrap = 'Bootstrap connections';
    local connectionsQuery = prometheus('count_connections', legendFormat=connections, namespace=namespace);
    local bootstrapQuery = prometheus('count_bootstrap_connections', legendFormat=bootstrap, namespace=namespace);
    graph.new('The number of (bootstrap) connections', [connectionsQuery, bootstrapQuery], h, w, x, y)
    + graph.withQueryColor([[connections, 'green'], [bootstrap, 'yellow']]),

  workerStreams(h, w, x, y):
    local input = 'Input stream';
    local p2p = 'P2p output stream';
    local app = 'Application output stream';
    local inputQuery = prometheus('input_events_stream_length', legendFormat=input, namespace=namespace);
    local p2pQuery = prometheus('p2p_output_stream_length', legendFormat=p2p, namespace=namespace);
    local appQuery = prometheus('app_output_stream_length', legendFormat=app, namespace=namespace);
    graph.new('The size of different worker streams', [inputQuery, p2pQuery, appQuery], h, w, x, y)
    + graph.withQueryColor([[input, 'green'], [p2p, 'blue'], [app, 'yellow']]),

  appMessagesDeriv(h, w, x, y):
    local receivedValid = 'Received & valid';
    local receivedInvalid = 'Received & invalid';
    local receivedUnknown = 'Received & unchecked validity';
    local sent = 'Sent';
    local receivedValidQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_valid_messages' + base.node_instance_query + '[1m])')
                               + query.prometheus.withLegendFormat(receivedValid);
    local receivedInvalidQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_invalid_messages' + base.node_instance_query + '[1m])')
                                 + query.prometheus.withLegendFormat(receivedInvalid);
    local receivedUnknownQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_unknown_validity_messages' + base.node_instance_query + '[1m])')
                                 + query.prometheus.withLegendFormat(receivedUnknown);
    local sentQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_sent_messages' + base.node_instance_query + '[1m])')
                      + query.prometheus.withLegendFormat(sent);
    graph.new('Average sent & received app messages (1-minute interval)', [receivedValidQuery, receivedInvalidQuery, receivedUnknownQuery, sentQuery], h, w, x, y)
    + graph.withQueryColor([[receivedValid, 'green'], [receivedInvalid, 'red'], [receivedUnknown, 'orange'], [sent, 'blue']]),

  sentOtherMessagesDeriv(h, w, x, y):
    local grafts = 'Graft';
    local prunes = 'Prune';
    local ihaves = 'IHave';
    local iwants = 'IWant';
    local graftsQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_sent_grafts' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(grafts);
    local prunesQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_sent_prunes' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(prunes);
    local ihavesQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_sent_ihaves' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(ihaves);
    local iwantsQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_sent_iwants' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(iwants);
    graph.new('Average sent control & metadata messages (1-minute interval)', [graftsQuery, prunesQuery, ihavesQuery, iwantsQuery], h, w, x, y)
    + graph.withQueryColor([[grafts, 'blue'], [prunes, 'yellow'], [ihaves, 'purple'], [iwants, 'magenta']]),

  receivedOtherMessagesDeriv(h, w, x, y):
    local grafts = 'Graft';
    local prunes = 'Prune';
    local ihaves = 'IHave';
    local iwants = 'IWant';
    local graftsQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_grafts' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(grafts);
    local prunesQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_prunes' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(prunes);
    local ihavesQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_ihaves' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(ihaves);
    local iwantsQuery = grafonnet.query.prometheus.new('Prometheus', 'deriv(' + namespace + '_count_received_iwants' + base.node_instance_query + '[1m])')
                        + query.prometheus.withLegendFormat(iwants);
    graph.new('Average received control & metadata messages (1-minute interval)', [graftsQuery, prunesQuery, ihavesQuery, iwantsQuery], h, w, x, y)
    + graph.withQueryColor([[grafts, 'blue'], [prunes, 'yellow'], [ihaves, 'purple'], [iwants, 'magenta']]),

  scoresOfPeers(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', namespace + '_scores_of_peers' + base.peer_instance_query)
              + query.prometheus.withLegendFormat('{{ peer }}');
    graph.new('Score of the peers connected to this node', [q], h, w, x, y),

  peersOfTopics(h, w, x, y):
    local q = grafonnet.query.prometheus.new('Prometheus', namespace + '_count_peers_per_topic' + base.topic_instance_query)
              + query.prometheus.withLegendFormat('Topic: {{ slot_index }}, {{ pkh }}');
    graph.new('Number of peers in the mesh for each subscribed topic', [q], h, w, x, y),

}
