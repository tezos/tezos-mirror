local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local graphPanel = grafana.graphPanel;
local statPanel = grafana.statPanel;
local prometheus = grafana.prometheus;
local namespace = 'octez';
local node_instance = '{' + std.extVar('node_instance_label') + '="$node_instance"}';

local count_query(endpoint) =
  'sum(' + namespace + '_rpc_calls_count{endpoint=~' + endpoint + ',' + std.extVar('node_instance_label') + '="$node_instance"})';

local sum_query(endpoint) =
  'sum(' + namespace + '_rpc_calls_sum{endpoint=~' + endpoint + ',' + std.extVar('node_instance_label') + '="$node_instance"})';

local average_query(endpoint) =
  sum_query(endpoint) + '/' + count_query(endpoint);


//##
// RPCs stats
//##


local graph(title, query, unit='none') =
  local chains = 'Chains';
  local blocks = 'Blocks';
  local version = 'Versions';
  local config = 'Config';
  local workers = 'Workers';
  local network = 'Network';
  local protocols = 'Protocols';
  local injection = 'Injection';
  local private = 'Private';
  local misc = 'Misc';
  graphPanel.new(
    title=title,
    datasource='Prometheus',
    linewidth=1,
    format='none',
    legend_alignAsTable=true,
    legend_current=true,
    legend_rightSide=true,
    legend_show=true,
    formatY1=unit,
    aliasColors={
      [chains]: 'light-red',
      [blocks]: 'light-blue',
      [version]: 'light-green',
      [config]: 'light-yellow',
      [network]: 'blue',
      [workers]: 'light-red',
      [injection]: 'green',
      [protocols]: 'blue',
      [private]: 'purple',
      [misc]: 'grey',
    },
  ).addTargets([
    prometheus.target(
      query("'/chains/<chain_id>/.*'"),
      legendFormat=chains
    ),
    prometheus.target(
      query("'/chains/<chain_id>/blocks/.*'"),
      legendFormat=blocks
    ),
    prometheus.target(
      query("'/version.*'"),
      legendFormat=version
    ),
    prometheus.target(
      query("'/config.*'"),
      legendFormat=config
    ),
    prometheus.target(
      query("'/network.*'"),
      legendFormat=network
    ),
    prometheus.target(
      query("'/workers.*'"),
      legendFormat=workers
    ),
    prometheus.target(
      query("'/injection.*'"),
      legendFormat=injection
    ),
    prometheus.target(
      query("'/protocols.*'"),
      legendFormat=protocols
    ),
    prometheus.target(
      query("'/private.*'"),
      legendFormat=private
    ),
    prometheus.target(
      query("'/(fetch_protocol|stats|monitor)*.'"),
      legendFormat=misc
    ),
  ]);

{

  calls: graph('RPC calls', count_query),
  durations: graph('RPC durations', average_query, 's'),
  //The total number of calls
  totalCalls:
    statPanel.new(
      title='Total of RPC calls',
      datasource='Prometheus',
      graphMode='none',
      unit='none',
      reducerFunction='last',
    ).addTarget(
      prometheus.target(
        count_query("'.*'"),
        legendFormat='total calls',
      )
    ).addThreshold({ color: 'light-yellow', value: 'Base' }),
  //The rate of calls from the last hour
  callsRate:
    statPanel.new(
      title='RPC calls rate per hour',
      datasource='Prometheus',
      graphMode='none',
    ).addTarget(
      prometheus.target(
        'sum(' + 'rate(' + namespace + '_rpc_calls_count{endpoint=~".*",' + std.extVar('node_instance_label') + '="$node_instance"}' + '[1h]))',
        legendFormat='calls rate',
      )
    ).addThreshold({ color: 'light-yellow', value: 'Base' }),
  //The total duration of all RPCs calls
  totalDuration: statPanel.new(
    title='Total of RPC calls duration',
    datasource='Prometheus',
    graphMode='none',
    unit='s',
    reducerFunction='last',
  ).addTarget(
    prometheus.target(
      sum_query("'.*'"),
      legendFormat='total duration',
    )
  ).addThreshold({ color: 'blue', value: 'Base' }),
  //The average duration of all RPC calls
  averageDuration: statPanel.new(
    title='Average of calls durations',
    datasource='Prometheus',
    graphMode='none',
    unit='s',
    reducerFunction='mean',
  ).addTarget(
    prometheus.target(
      sum_query("'.*'") + '/' + count_query("'.*'"),
      legendFormat='average duration',
    )
  ).addThreshold({ color: 'blue', value: 'Base' }),
  //The maximum of the total duration of each RPC calls
  maxTotalDuration:
    statPanel.new(
      title='Max of total calls durations',
      datasource='Prometheus',
      graphMode='none',
      unit='s',
      reducerFunction='max',
    ).addTarget(
      prometheus.target(
        'max(' + namespace + '_rpc_calls_sum{' + std.extVar('node_instance_label') + '="$node_instance"})',
        legendFormat='max total duration',
      )
    ).addThreshold({ color: 'blue', value: 'Base' }),
  //The maximum of the average duration of each RPC calls
  // i.e the duration of the longest call on average
  maxAverageDuration:
    statPanel.new(
      title='Max of average calls durations',
      datasource='Prometheus',
      graphMode='none',
      unit='s',
      reducerFunction='max',
    ).addTarget(
      prometheus.target(
        'max(' + namespace + '_rpc_calls_sum{' + std.extVar('node_instance_label') + '="$node_instance"} / ' + namespace + '_rpc_calls_count{' + std.extVar('node_instance_label') + '="$node_instance"})',
        legendFormat='max average duration',
      )
    ).addThreshold({ color: 'blue', value: 'Base' }),
}
