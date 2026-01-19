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

// Grafazos Base
local base = import './base.jsonnet';
local logs = base.logs;

// Configure logs label from command line, defaults to 'job'
local logs_label = std.extVar('logs_label');

//##
// Logs
//##
{
  // Query helper
  query(type, legendFormat):
    query.prometheus.new('Loki', '{' + logs_label + '="' + base.namespace + '-' + type + '"}')
    + query.prometheus.withLegendFormat(legendFormat),

  nodelogs(h, w, x, y):
    local q = self.query('node', 'Node logs');
    logs.new('Node logs', [q], h, w, x, y, 'loki', 'Loki'),

  bakerlogs(h, w, x, y):
    local q = self.query('baker', 'Baker logs');
    logs.new('Baker logs', [q], h, w, x, y, 'loki', 'Loki'),

  accuserlogs(h, w, x, y):
    local q = self.query('accuser', 'Accuser logs');
    logs.new('Accuser logs', [q], h, w, x, y, 'loki', 'Loki'),

  systemlogs(h, w, x, y):
    local q = query.prometheus.new('Loki', '{job="varlogs"}')
              + query.prometheus.withLegendFormat('System logs');
    logs.new('System logs', [q], h, w, x, y, 'loki', 'Loki'),

}
