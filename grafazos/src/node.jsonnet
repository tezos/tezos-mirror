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
local panel = grafonnet.panel;
local stat = panel.stat;
local timeSeries = panel.timeSeries;

// Base
local base = import './base.jsonnet';
local namespace = base.namespace;
local prometheus = base.prometheus;
local info = base.info;
local graph = base.graph;
local table = base.table;

//##
// Octez node related stats
//##

{

  releaseVersionInfo(h, w, x, y):
    local q = prometheus('version', legendFormat='{{ version }}');
    info.new('Node release version', q, h, w, x, y) + info.withName(),

  releaseCommitInfo(h, w, x, y):
    local q = query.prometheus.new('Prometheus', '(label_replace(' + namespace + '_version' + base.node_instance_query + ',"commit_hash_short" ,"$1","commit_hash","^(.{8}).*$"))')
              + query.prometheus.withLegendFormat('{{ commit_hash_short }}');
    info.new('Node release commit', q, h, w, x, y) + info.withName(),

  chainNameInfo(h, w, x, y):
    local q = prometheus('version', legendFormat='{{ chain_name }}');
    info.new('Chain name', q, h, w, x, y) + info.withName(),

  p2pVersion(h, w, x, y):
    local q = prometheus('version', legendFormat='{{ p2p_version }}');
    info.new('P2p version', q, h, w, x, y),

  distributedDbVersion(h, w, x, y):
    local q = prometheus('version', legendFormat='{{ distributed_db_version }}');
    info.new('Distributed db version', q, h, w, x, y),

  bootstrapStatus(h, w, x, y):
    local q = prometheus('validator_chain_is_bootstrapped');
    info.new('Bootstrap status', q, h, w, x, y)
    + info.withMapping([['0', 'Bootstrapping', 'red'], ['1', 'Bootstrapped', 'green']]),

  syncStatus(h, w, x, y):
    local q = prometheus('validator_chain_synchronisation_status');
    info.new('Sync status', q, h, w, x, y)
    + info.withMapping([['0', 'Unsync', 'red'], ['1', 'Sync', 'green'], ['2', 'Stuck', 'red']]),

  uptime(h, w, x, y):
    local q = query.prometheus.new('Prometheus', 'time()-(process_start_time_seconds' + base.node_instance_query + ')')
              + query.prometheus.withLegendFormat('node uptime');
    info.new('Node uptime', q, h, w, x, y)
    + stat.panelOptions.withDescription('Reflects the uptime of the monitoring of the job, not the uptime of the process.')
    + stat.standardOptions.withUnit('dtdhms')
    + stat.options.withTextMode('max'),

  headLevel(h, w, x, y):
    local q = prometheus('validator_chain_head_level', legendFormat='currrent head level');
    info.new('Current head level', q, h, w, x, y),

  savepointLevel(h, w, x, y):
    local q = prometheus('store_savepoint_level', legendFormat='current savepoint');
    info.new('Current savepoint level', q, h, w, x, y),

  checkpointLevel(h, w, x, y):
    local q = prometheus('store_checkpoint_level', legendFormat='current checkpoint');
    info.new('Current checkpoint level', q, h, w, x, y),

  cabooseLevel(h, w, x, y):
    local q = prometheus('store_caboose_level', legendFormat='current caboose');
    info.new('Current caboose level', q, h, w, x, y),

  levelsTable(h, w, x, y):
    local cycleQuery = prometheus('validator_chain_head_cycle', legendFormat='current cycle');
    local cabooseQuery = prometheus('store_caboose_level', legendFormat='current caboose');
    local checkpointQuery = prometheus('store_checkpoint_level', legendFormat='current checkpoint');
    local savepointQuery = prometheus('store_savepoint_level', legendFormat='current savepoint');
    table('Chain levels', [cycleQuery, cabooseQuery, checkpointQuery, savepointQuery], h, w, x, y),

  headCycleLevel(h, w, x, y):
    local q = prometheus('validator_chain_head_cycle', legendFormat='current cycle');
    info.new('Current cycle', q, h, w, x, y),

  headHistory(h, w, x, y):
    local q = prometheus('validator_chain_head_level', legendFormat='Head level');
    graph.new('Head level history', [q], h, w, x, y)
    + timeSeries.options.legend.withShowLegend(false),

  invalidBlocksHistory(h, w, x, y):
    local q = prometheus('store_invalid_blocks', legendFormat='Invalid blocks history');
    graph.new('Invalid blocks history', [q], h, w, x, y)
    + graph.withFixedColor('light-red')
    + graph.withLegendBottom(calcs=['lastNotNull', 'max']),

  invalidBlocksMean(h, w, x, y):
    local q = prometheus('store_invalid_blocks', legendFormat='current cycle');
    info.new('Invalid blocks mean', q, h, w, x, y, instant=false)
    + stat.options.withColorMode('value')
    + stat.options.withGraphMode('area'),

  alternateHeadsCount(h, w, x, y):
    local q = prometheus('store_alternate_heads_count', legendFormat='Alternate heads count');
    graph.new('Alternate heads count', [q], h, w, x, y)
    + timeSeries.options.legend.withShowLegend(false)
    + graph.withFixedColor('light-yellow'),

  headOperations(h, w, x, y):
    local consensusQuery = query.prometheus.new('Prometheus', base.namespace + '_validator_block_operations_per_pass{pass_id="0",' + base.node_instance + '="$node_instance"}')
                           + query.prometheus.withLegendFormat('Consensus');
    local voteQuery = query.prometheus.new('Prometheus', base.namespace + '_validator_block_operations_per_pass{pass_id="1",' + base.node_instance + '="$node_instance"}')
                      + query.prometheus.withLegendFormat('Vote');
    local anonymousQuery = query.prometheus.new('Prometheus', base.namespace + '_validator_block_operations_per_pass{pass_id="2",' + base.node_instance + '="$node_instance"}')
                           + query.prometheus.withLegendFormat('Anonymous');
    local managerQuery = query.prometheus.new('Prometheus', base.namespace + '_validator_block_operations_per_pass{pass_id="3",' + base.node_instance + '="$node_instance"}')
                         + query.prometheus.withLegendFormat('Manager');
    graph.new('Head operations', [consensusQuery, voteQuery, anonymousQuery, managerQuery], h, w, x, y)
    + graph.withLegendRight(calcs=['mean', 'lastNotNull', 'max', 'min']),

  gasConsumedHistory(h, w, x, y):
    local q = prometheus('validator_chain_head_consumed_gas', legendFormat='Gas consumed');
    graph.new('Gas consumed history', [q], h, w, x, y)
    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max', 'min']),

  blocksValidationTime(h, w, x, y):
    local completion = base.namespace + '_validator_block_last_finished_request_completion_timestamp' + base.node_instance_query;
    local treatment = base.namespace + '_validator_block_last_finished_request_treatment_timestamp' + base.node_instance_query;
    local q = query.prometheus.new('Prometheus', completion + '-' + treatment)
              + query.prometheus.withLegendFormat('Validation time');
    graph.new('Block validation time', [q], h, w, x, y)
    + graph.withLegendBottom(calcs=['mean', 'max', 'min'])
    + graph.withFixedColor('light-blue')
    + timeSeries.standardOptions.withUnit('s'),

  blocksValidationMean(h, w, x, y):
    local completion = base.namespace + '_validator_block_last_finished_request_completion_timestamp' + base.node_instance_query;
    local treatment = base.namespace + '_validator_block_last_finished_request_treatment_timestamp' + base.node_instance_query;
    local q = query.prometheus.new('Prometheus', completion + '-' + treatment)
              + query.prometheus.withLegendFormat('block validation mean');
    info.new('Block validation mean', q, h, w, x, y, instant=false)
    + stat.options.withColorMode('value')
    + stat.options.withGraphMode('area')
    + info.withThreshold([['0', 'green'], ['1', 'red']]),

  roundHistory(h, w, x, y):
    local q = prometheus('validator_chain_head_round', legendFormat='Round');
    graph.new('Round history', [q], h, w, x, y)
    + graph.withLegendBottom(calcs=['max']),

  maxRound(h, w, x, y):
    local q = prometheus('validator_chain_head_round');
    local threshold =
      [['0', 'green'], ['2', 'yellow'], ['4', 'red']];
    info.new('Max round', q, h, w, x, y, instant=false)
    + stat.options.reduceOptions.withCalcs(['max'])
    + stat.options.withColorMode('value')
    + stat.options.withGraphMode('area')
    + info.withThreshold(threshold),

  branchSwitchCount(h, w, x, y):
    local q = prometheus('validator_chain_branch_switch_count');
    info.new('Branch switch count', q, h, w, x, y, instant=false)
    + stat.options.withGraphMode('area')
    + stat.standardOptions.color.withMode('fixed'),

  storeMergeTimeGraph(h, w, x, y):
    local q = prometheus('store_last_merge_time', legendFormat='Merge Time');
    graph.new('Store merge time', [q], h, w, x, y)
    + graph.withLegendBottom(calcs=['lastNotNull', 'max'])
    + graph.withFixedColor('light-blue')
    + timeSeries.standardOptions.withUnit('s'),

  storeMergeTime(h, w, x, y):
    local q = prometheus('store_last_merge_time', legendFormat='Round');
    info.new('Store merge time', q, h, w, x, y, instant=false)
    + stat.options.withGraphMode('area')
    + stat.standardOptions.color.withMode('fixed'),

  writtenBlockSize(h, w, x, y):
    local q = prometheus('store_last_written_block_size', legendFormat='Written block size');
    graph.new('Last written block size', [q], h, w, x, y)
    + graph.withLegendBottom(calcs=['mean', 'lastNotNull', 'max', 'min'])
    + timeSeries.standardOptions.withUnit('bytes'),


  //## GC

  gcOperations(h, w, x, y):
    local minor = 'Minor collections';
    local major = 'Major collections';
    local forced = 'Forced major collections';
    local compact = 'Heap compactions';
    local minorQuery = query.prometheus.new('Prometheus', 'ocaml_gc_minor_collections' + base.node_instance_query)
                       + query.prometheus.withLegendFormat(minor);
    local majorQuery = query.prometheus.new('Prometheus', 'ocaml_gc_major_collections' + base.node_instance_query)
                       + query.prometheus.withLegendFormat(major);
    local forcedQuery = query.prometheus.new('Prometheus', 'ocaml_gc_forced_major_collections' + base.node_instance_query)
                        + query.prometheus.withLegendFormat(forced);
    local compactQuery = query.prometheus.new('Prometheus', 'ocaml_gc_compactions' + base.node_instance_query)
                         + query.prometheus.withLegendFormat(compact);
    graph.new('GC maintenance operations', [minorQuery, majorQuery, forcedQuery, compactQuery], h, w, x, y)
    + graph.withLogScale()
    + graph.withQueryColor([[minor, 'light-green'], [major, 'light-yellow'], [forced, 'light-blue'], [compact, 'light-red']]),

  gcMajorHeap(h, w, x, y):
    local major = 'Major heap';
    local majorQuery = query.prometheus.new('Prometheus', 'ocaml_gc_heap_words' + base.node_instance_query + '* 8')
                       + query.prometheus.withLegendFormat(major);
    graph.new('GC major word sizes', [majorQuery], h, w, x, y)
    + graph.withQueryColor([[major, 'light-green']])
    + timeSeries.standardOptions.withUnit('bytes')
    + timeSeries.options.legend.withShowLegend(false),

}
