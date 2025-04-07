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
local timeSeries = grafonnet.panel.timeSeries;

// Base
local base = import './base.jsonnet';
local prometheus = base.prometheus;
local info = base.info;
local graph = base.graph;

//##
// Workers
//##

{

  requests(h, w, x, y):
    local blockRequestsQuery = prometheus('validator_block_worker_request_count', legendFormat='Block validator request count');
    local blockCompletionsQuery = prometheus('validator_block_worker_completion_count', legendFormat='Block validator completion count');
    local blockErrorsQuery = prometheus('validator_block_worker_error_count', legendFormat='Block validator error count');
    local chainRequestsQuery = prometheus('validator_chain_worker_request_count', legendFormat='Chain validator request count');
    local chainCompletionsQuery = prometheus('validator_chain_worker_completion_count', legendFormat='Chain validator completion count');
    local chainErrorsQuery = prometheus('validator_chain_worker_error_count', legendFormat='Chain validator error count');
    local prevalidatorRequestsQuery = prometheus('mempool_worker_request_count', legendFormat='Prevalidator request count');
    local prevalidatorCompletionsQuery = prometheus('mempool_worker_completions_count', legendFormat='Prevalidator completion count');
    local prevalidatorErrorsQuery = prometheus('mempool_worker_error_count', legendFormat='Prevalidator error count');
    graph.new('Validation workers requests', [blockRequestsQuery, blockCompletionsQuery, blockErrorsQuery, chainRequestsQuery, chainCompletionsQuery, chainErrorsQuery, prevalidatorRequestsQuery, prevalidatorCompletionsQuery, prevalidatorErrorsQuery], h, w, x, y)
    + graph.withLegendRight(calcs=['current']),

  distributedDB(h, w, x, y):
    local headersTableQuery = query.prometheus.new('Prometheus', base.namespace + '_distributed_db_requester_table_length{requester_kind="block_header",' + base.node_instance + '="$node_instance"}')
                              + query.prometheus.withLegendFormat('headers table');
    local headersSchedulerQuery =
      query.prometheus.new('Prometheus', base.namespace + '_distributed_db_requester_scheduler_length{requester_kind="block_header",' + base.node_instance + '="$node_instance"}')
      + query.prometheus.withLegendFormat('headers scheduler');
    local operationsTableQuery =
      query.prometheus.new('Prometheus', base.namespace + '_distributed_db_requester_scheduler_length{requester_kind="block_header",' + base.node_instance + '="$node_instance"}')
      + query.prometheus.withLegendFormat('operations table');
    local operationsSchedulerQuery =
      query.prometheus.new('Prometheus', base.namespace + '_distributed_db_requester_table_length{requester_kind="operations",' + base.node_instance + '="$node_instance"}')
      + query.prometheus.withLegendFormat('operations scheduler');
    local operationTableQuery =
      query.prometheus.new('Prometheus', base.namespace + '_distributed_db_requester_table_length{requester_kind="operation",' + base.node_instance + '="$node_instance"}')
      + query.prometheus.withLegendFormat('operation table');
    local operationSchedulerQuery =
      query.prometheus.new('Prometheus', base.namespace + '_distributed_db_requester_scheduler_length{requester_kind="operation",' + base.node_instance + '="$node_instance"}')
      + query.prometheus.withLegendFormat('operation scheduler');
    graph.new('DDB Workers', [headersTableQuery, headersSchedulerQuery, operationsTableQuery, operationsSchedulerQuery, operationTableQuery, operationSchedulerQuery], h, w, x, y)
    + graph.withLegendRight(calcs=['mean', 'min', 'max']),

  peerValidatorErrorsMean(h, w, x, y):
    local systemErrorQuery = prometheus('validator_peer_system_error')
                             + grafonnet.query.prometheus.withInstant(true);
    local tooShortLocatorQuery = prometheus('validator_peer_too_short_locator')
                                 + grafonnet.query.prometheus.withInstant(true);
    local unavailableProtocolQuery = prometheus('validator_peer_unavailable_protocol')
                                     + grafonnet.query.prometheus.withInstant(true);
    local unknownAncestorQuery = prometheus('validator_peer_unknown_ancestor')
                                 + grafonnet.query.prometheus.withInstant(true);
    local unknownErrorQuery = prometheus('validator_peer_unknown_error')
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
    + stat.queryOptions.withTargets([systemErrorQuery, tooShortLocatorQuery, unavailableProtocolQuery, unknownAncestorQuery, unknownErrorQuery])
    + info.withThreshold([['0', 'green'], ['1', 'red']])
    + stat.queryOptions.withTransformations(transformation),

  peerValidators(h, w, x, y):
    local connections = 'connections';
    local invalidBlocks = 'invalid blocks';
    local invalidLocator = 'invalid locator';
    local newBranchCompleted = 'new branch completed';
    local newHeadCompleted = 'new head completed';
    local onNoRequest = 'on no request count';
    local newBranch = 'fetching canceled new branch';
    local newKnownValidHead = 'fetching canceled new known valid head';
    local newUnknownHead = 'fetching canceled knew unknown head';
    local systemError = 'system error';
    local tooShortLocator = 'too short locator';
    local unavailableProtocol = 'unavailable protocol';
    local unknownAncestor = 'unknown ancestor';
    local unknownError = 'unknown error';
    local connectionsQuery = prometheus('validator_peer_connections', legendFormat=connections);
    local invalidBlocksQuery = prometheus('validator_peer_invalid_block', legendFormat=invalidBlocks);
    local invalidLocatorQuery = prometheus('validator_peer_invalid_locator', legendFormat=invalidLocator);
    local newBranchCompeletedQuery = prometheus('validator_peer_new_branch_completed', legendFormat=newBranchCompleted);
    local newHeadCompletedQuery = prometheus('validator_peer_new_head_completed', legendFormat=newHeadCompleted);
    local onNoRequestQuery = prometheus('validator_peer_on_no_request_count', legendFormat=onNoRequest);
    local newBranchQuery = prometheus('validator_peer_operations_fetching_canceled_new_branch', legendFormat=newBranch);
    local newKnownValidHeadQuery = prometheus('validator_peer_operations_fetching_canceled_new_known_valid_head', legendFormat=newKnownValidHead);
    local newUnknownHeadQuery = prometheus('validator_peer_operations_fetching_canceled_new_unknown_head', legendFormat=newUnknownHead);
    local systemErrorQuery = prometheus('validator_peer_system_error', legendFormat=systemError);
    local tooShortLocatorQuery = prometheus('validator_peer_too_short_locator', legendFormat=tooShortLocator);
    local unavailableProtocolQuery = prometheus('validator_peer_unavailable_protocol', legendFormat=unavailableProtocol);
    local unknownAncestorQuery = prometheus('validator_peer_unknown_ancestor', legendFormat=unknownAncestor);
    local UnknownError = prometheus('validator_peer_unknown_error', legendFormat=unknownError);
    graph.new('Peer validators', [connectionsQuery, invalidBlocksQuery, invalidLocatorQuery, newBranchCompeletedQuery, newHeadCompletedQuery, onNoRequestQuery, newBranchQuery, newKnownValidHeadQuery, newUnknownHeadQuery, systemErrorQuery, tooShortLocatorQuery, unavailableProtocolQuery, unknownAncestorQuery, UnknownError], h, w, x, y)
    + graph.withLogScale()
    + graph.withQueryColor([[connections, 'light-green'], [invalidBlocks, 'light-red'], [invalidLocator, 'light-orange'], [newBranchCompleted, 'light-blue'], [newHeadCompleted, 'blue'], [onNoRequest, 'white'], [newBranch, 'green'], [newKnownValidHead, 'light-yellow'], [newUnknownHead, 'light-orange'], [systemError, 'orange'], [tooShortLocator, 'brown'], [unavailableProtocol, 'light-red'], [unknownAncestor, 'yellow'], [unknownError, 'red']]),

  validatorTreatmentRequests(h, w, x, y):
    local chainPush = base.namespace + '_validator_chain_last_finished_request_push_timestamp' + base.node_instance_query;
    local chainTreatment = base.namespace + '_validator_chain_last_finished_request_treatment_timestamp' + base.node_instance_query;
    local blockPush = base.namespace + '_validator_block_last_finished_request_push_timestamp' + base.node_instance_query;
    local blockTreatment = base.namespace + '_validator_block_last_finished_request_treatment_timestamp' + base.node_instance_query;
    local chainTreatmentTimeQuery = query.prometheus.new('Prometheus', chainTreatment + ' - ' + chainPush)
                                    + query.prometheus.withLegendFormat('chain validator');
    local blockTreatmentTimeQuery = query.prometheus.new('Prometheus', blockTreatment + ' - ' + blockPush)
                                    + query.prometheus.withLegendFormat('block validator');
    graph.new('Validators Requests Treatments', [chainTreatmentTimeQuery, blockTreatmentTimeQuery], h, w, x, y)
    + graph.withLegendBottom(calcs=['mean', 'min', 'max'])
    + timeSeries.standardOptions.withUnit('s')
    + graph.withQueryColor([['chain validator', 'light-green'], ['block validator', 'light-red']]),

  validatorCompletionRequests(h, w, x, y):
    local chainTreatment = base.namespace + '_validator_chain_last_finished_request_treatment_timestamp' + base.node_instance_query;
    local chainCompletion = base.namespace + '_validator_chain_last_finished_request_completion_timestamp' + base.node_instance_query;
    local blockTreatment = base.namespace + '_validator_block_last_finished_request_treatment_timestamp' + base.node_instance_query;
    local blockCompletion = base.namespace + '_validator_block_last_finished_request_completion_timestamp' + base.node_instance_query;
    local chainCompletionTimeQuery = query.prometheus.new('Prometheus', chainCompletion + ' - ' + chainTreatment)
                                     + query.prometheus.withLegendFormat('chain validator');
    local blockCompletionTimeQuery = query.prometheus.new('Prometheus', blockCompletion + ' - ' + blockTreatment)
                                     + query.prometheus.withLegendFormat('block validator');
    graph.new('Validators Requests Completion', [chainCompletionTimeQuery, blockCompletionTimeQuery], h, w, x, y)
    + graph.withLegendBottom(calcs=['mean', 'min', 'max'])
    + timeSeries.standardOptions.withUnit('s')
    + graph.withQueryColor([['chain validator', 'light-green'], ['block validator', 'light-red']]),

  chainValidatorRequestCompletionMean(h, w, x, y):
    local chainTreatment = base.namespace + '_validator_chain_last_finished_request_treatment_timestamp' + base.node_instance;
    local chainCompletion = base.namespace + '_validator_chain_last_finished_request_completion_timestamp' + base.node_instance;
    local q = query.prometheus.new('Prometheus', chainCompletion + ' - ' + chainTreatment);
    info.new('Chain validator request completion', q, h, w, x, y, instant=false)
    + stat.options.withGraphMode('area'),

}
