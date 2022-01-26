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

  bakerlogs:
    logPanel.new(
      title='Baker logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="tezos-baker"}',
        legendFormat='Baker logs',
      )
    ),

  accuserlogs:
    logPanel.new(
      title='Accuser logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="tezos-accuser"}',
        legendFormat='Accuser logs',
      )
    ),

  endorserlogs:
    logPanel.new(
      title='Endorser logs',
      datasource='Loki'
    ).addTarget(
      prometheus.target(
        '{job="tezos-endorser"}',
        legendFormat='Endorser logs',
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
