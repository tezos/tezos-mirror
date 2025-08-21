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
local dashboard = grafonnet.dashboard;

local base = import './base.jsonnet';
local node = import './node.jsonnet';
local p2p = import './p2p.jsonnet';
local workers = import './workers.jsonnet';

// External variables
local uid_ext = std.extVar('uid_ext');
local uid = uid_ext == 'default';

dashboard.new('Octez compact dashboard' + if !uid && uid_ext != '' then ' (' + std.strReplace(uid_ext, '-', '') + ')' else '')
+ (if !uid then dashboard.withUid('octez-compact' + uid_ext) else {})
+ dashboard.withDescription('A compact dashboard for Octez')
+ dashboard.withTags(['tezos', 'octez'])
+ dashboard.time.withFrom('now-3h')
+ dashboard.withRefresh('20s')
+ dashboard.withVariables([base.nodeInstance])

+ dashboard.withPanels(
  [
    node.bootstrapStatus(h=3, w=3, x=0, y=0),
    node.syncStatus(h=3, w=3, x=3, y=0),
    node.chainNameInfo(h=3, w=4, x=6, y=0),
    node.releaseVersionInfo(h=3, w=3, x=10, y=0),
    node.releaseCommitInfo(h=3, w=3, x=13, y=0),
    node.uptime(h=3, w=4, x=0, y=3),
    node.headLevel(h=3, w=4, x=0, y=6),
    node.levelsTable(h=6, w=4, x=0, y=9),
    p2p.connectionsTable(h=5, w=4, x=0, y=15),
    p2p.totalConnections(h=8, w=4, x=0, y=20),
    node.headOperations(h=8, w=12, x=4, y=3),
    node.headHistory(h=9, w=6, x=10, y=11),
    node.blocksValidationTime(h=9, w=6, x=4, y=11),
    node.gasConsumedHistory(h=9, w=8, x=16, y=10),
    node.roundHistory(h=9, w=8, x=16, y=19),
    node.writtenBlockSize(h=8, w=6, x=4, y=20),
    node.alternateHeadsCount(h=8, w=6, x=10, y=20),
    node.invalidBlocksMean(h=5, w=2, x=16, y=0),
    node.branchSwitchCount(h=5, w=2, x=18, y=0),
    node.storeMergeTime(h=5, w=2, x=20, y=5),
    node.maxRound(h=5, w=2, x=22, y=0),
    node.blocksValidationMean(h=5, w=2, x=16, y=5),
    workers.chainValidatorRequestCompletionMean(h=5, w=2, x=18, y=5),
    workers.peerValidatorErrorsMean(h=5, w=2, x=20, y=5),
    p2p.incomingConnectionsMean(h=5, w=2, x=22, y=5),
  ]
)
