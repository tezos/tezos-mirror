local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local stat = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;

//##
// Hardware relates stats
//##

{
  ios:
    local reads = 'reads';
    local writes = 'writes';
    graphPanel.new(
      title='IOs',
      datasource='Prometheus',
      linewidth=1,
      format='kbytes',
      legend_alignAsTable=true,
      legend_current=true,
      legend_avg=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [reads]: 'light-green',
        [writes]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_lreads_KiB_persec_average{dimension="tezos"}',
        legendFormat=reads,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lwrites_KiB_persec_average{dimension="tezos"}',
        legendFormat=writes,
      )
    ),

  cpu:
    local load = 'Cpu load';
    graphPanel.new(
      title='Cpu actitvity',
      datasource='Prometheus',
      linewidth=1,
      format='percent',
      aliasColors={
        [load]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="tezos"}',
        legendFormat=load,
      )
    ),

  memory:
    local ram = 'Memory usage';
    local swap = 'Swap usage';
    graphPanel.new(
      title='Memory usage',
      datasource='Prometheus',
      linewidth=1,
      format='mbytes',
      legend_alignAsTable=true,
      legend_current=true,
      legend_avg=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
      aliasColors={
        [ram]: 'light-green',
        [swap]: 'light-orange',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_mem_MiB_average{dimension="tezos"}',
        legendFormat=ram,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="tezos"}',
        legendFormat=swap,
      )
    ),

  storage:
    local total = 'Total';
    local context = 'Context store';
    local context_index = 'Context index';
    local store = 'Store';
    graphPanel.new(
      title='Storage',
      datasource='Prometheus',
      linewidth=1,
      format='mbytes',
      aliasColors={
        [total]: 'light-green',
        [context]: 'light-orange',
        [context_index]: 'light-yellow',
        [store]: 'light-blue',
      },
    ).addTarget(
      prometheus.target(
        'tezos_metrics_storage_total',
        legendFormat=total,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_storage_context',
        legendFormat=context,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_storage_index',
        legendFormat=context_index,
      )
    ).addTarget(
      prometheus.target(
        'tezos_metrics_storage_store',
        legendFormat=store,
      )
    ),

  diskFreeSpace:
    stat.new(
      title='Disk free space',
      graphMode='area',
      unit='decbytes',
      reducerFunction='lastNotNull'
    ).addTarget(
      prometheus.target(
	'node_filesystem_free_bytes{mountpoint=\"/\"}',
	legendFormat= 'Available bytes on disk.',
      )
    ),

  fileDescriptors:
    local total = 'All fds';
    local sockets = 'Sockets';
    local files = 'Files';
    local pipes = 'Pipes';
    graphPanel.new(
      title='File descriptors',
      datasource='Prometheus',
      linewidth=1,
      format='none',
      decimals=0,
      legend_alignAsTable='true',
      legend_current='true',
      legend_avg='true',
      legend_min='true',
      legend_max='true',
      legend_rightSide='true',
      legend_show='true',
      legend_values='true',
      aliasColors={
        [total]: 'light-green',
        [sockets]: 'light-yellow',
        [files]: 'light-blue',
        [pipes]: 'light-orange',
      },
    ).addTarget(
      prometheus.target(
        'sum(netdata_apps_pipes_open_pipes_average{dimension="tezos"}) + sum(netdata_apps_files_open_files_average{dimension="tezos"}) + sum(netdata_apps_sockets_open_sockets_average{dimension="tezos"})',
        legendFormat=total,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_sockets_open_sockets_average{dimension="tezos"}',
        legendFormat=sockets,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_files_open_files_average{dimension="tezos"}',
        legendFormat=files,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_pipes_open_pipes_average{dimension="tezos"}',
        legendFormat=pipes,
      )
    ),

  networkIOS:
    graphPanel.new(
      title='Network traffic',
      format='Bps',
      legend_alignAsTable=true,
      legend_current=true,
      legend_avg=true,
      legend_max=true,
      legend_show=true,
      legend_values=true,
    ).addTarget(
      prometheus.target(
	"irate(node_network_receive_bytes_total[5m]) > 0",
	legendFormat="Bytes received",
      )
    ).addTarget(
      prometheus.target(
	"irate(node_network_transmit_bytes_total[5m]) > 0",
	legendFormat="Bytes transmitted",
      )
    ).addSeriesOverride(
      {
	alias:'/.*received/',
	transform:'negative-Y',
      }
    ),
}
