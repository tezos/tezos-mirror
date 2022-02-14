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
