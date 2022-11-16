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

local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local stat = grafana.statPanel;
local loki = grafana.loki;
local prometheus = grafana.prometheus;
local logPanel = grafana.logPanel;
local namespace = 'octez';

//##
// Logs
//##

{
  nodelogs:
    logPanel.new(
      title='Node logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="' + namespace + '-node"}',
        legendFormat='Node logs',
      )
    ),

  bakerlogs:
    logPanel.new(
      title='Baker logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="' + namespace + '-baker"}',
        legendFormat='Baker logs',
      )
    ),

  accuserlogs:
    logPanel.new(
      title='Accuser logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="' + namespace + '-accuser"}',
        legendFormat='Accuser logs',
      )
    ),

  systemlogs:
    logPanel.new(
      title='System logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="varlogs"}',
        legendFormat='System logs',
      )
    ),
}
