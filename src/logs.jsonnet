local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local stat = grafana.statPanel;
local loki = grafana.loki;
local prometheus = grafana.prometheus;
local logPanel = grafana.logPanel;

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
        '{job="tezos-node"}',
        legendFormat='Node logs',
      )
    ),

  metricslogs:
    logPanel.new(
      title='Metrics logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="tezos-metrics"}',
        legendFormat='Metrics logs',
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
