// Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>
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
// local grafonnet = import 'github.com/grafana/grafonnet/gen/grafonnet-latest/main.libsonnet';

// Grafazos Base
local base = import './base.jsonnet';
local logs = base.logs;


//##
// Logs
//##

{
  // GCP project ID
  local gcp_project_id = std.extVar('gcp_project'),  // Replace with your actual project ID or make this a parameter
  local gcp_datasource_uid = std.extVar('gcp_datasource_uid'),  // Replace with your actual project ID or make this a parameter

  // Query helper for Google Cloud Logging
  cloudLoggingQuery(logs_bucket, legendFormat):
    {
      aliasBy: legendFormat,
      datasource: {
        type: 'googlecloud-logging-datasource',
        uid: gcp_datasource_uid,
      },
      projectId: gcp_project_id,
      queryText: 'resource.type = "gce_instance" AND severity >= DEFAULT AND labels."compute.googleapis.com/resource_name" = $node_instance AND logName = "projects/' + gcp_project_id + '/logs/' + logs_bucket + '"',
      queryType: 'logs',
      refId: 'A',
    },

  nodelogs(h, w, x, y):
    local q = self.cloudLoggingQuery('octez_node_logs', 'Node logs');
    logs.new('Node logs', [q], h, w, x, y, 'googlecloud-logging-datasource', '-- Mixed --'),

  bakerlogs(h, w, x, y):
    local q = self.cloudLoggingQuery('octez_baker_logs', 'Baker logs');
    logs.new('Baker logs', [q], h, w, x, y, 'googlecloud-logging-datasource', '-- Mixed --'),

  accuserlogs(h, w, x, y):
    local q = self.cloudLoggingQuery('octez_accuser_logs', 'Accuser logs');
    logs.new('Accuser logs', [q], h, w, x, y, 'googlecloud-logging-datasource', '-- Mixed --'),

  systemlogs(h, w, x, y):
    local q = self.cloudLoggingQuery('syslog', 'System logs');
    logs.new('System logs', [q], h, w, x, y, 'googlecloud-logging-datasource', '-- Mixed --'),
}
