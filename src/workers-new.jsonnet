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
local stat = grafonnet.panel.stat;

// Base
local base = import './base.jsonnet';
local prometheus = base.prometheus;
local info = base.info;

//##
// Workers
//##

{
  peerValidatorErrorsMean(h, w, x, y):
    local system_error_query = prometheus('validator_peer_system_error')
                               + grafonnet.query.prometheus.withInstant(true);
    local too_short_locator_query = prometheus('validator_peer_too_short_locator')
                                    + grafonnet.query.prometheus.withInstant(true);
    local unavailable_protocol_query = prometheus('validator_peer_unavailable_protocol')
                                       + grafonnet.query.prometheus.withInstant(true);
    local unknown_ancestor_query = prometheus('validator_peer_unknown_ancestor')
                                   + grafonnet.query.prometheus.withInstant(true);
    local unknown_error_query = prometheus('validator_peer_unknown_error')
                                + grafonnet.query.prometheus.withInstant(true);
    local transformation = {
      id: 'calculateField',
      options: {
        mode: 'reduceRow',
        replaceFields: true,
        reducer: { reducer: 'sum' },
      },
    };
    stat.new('Peer validators errors')
    + stat.panelOptions.withGridPos(h, w, x, y)
    + stat.options.withGraphMode('area')
    + stat.options.withColorMode('value')
    + stat.queryOptions.withTargets([system_error_query, too_short_locator_query, unavailable_protocol_query, unknown_ancestor_query, unknown_error_query])
    + info.withThreshold([['0', 'green'], ['1', 'red']])
    + stat.queryOptions.withTransformations(transformation),


  chainValidatorRequestCompletionMean(h, w, x, y):
    local chainTreatment = base.namespace + '_validator_chain_last_finished_request_treatment_timestamp' + base.node_instance;
    local chainCompletion = base.namespace + '_validator_chain_last_finished_request_completion_timestamp' + base.node_instance;
    local q = query.prometheus.new('Prometheus', chainCompletion + ' - ' + chainTreatment);
    info.new('Chain validator request completion', q, h, w, x, y, instant=false)
    + stat.options.withGraphMode('area'),

}
