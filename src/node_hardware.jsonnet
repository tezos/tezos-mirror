local grafana = import '../vendors/grafonnet-lib/grafonnet/grafana.libsonnet';
local stat = grafana.statPanel;
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;

local filecheck = if std.extVar('storage_mode') == "filecheck" then true else false;


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
        'netdata_apps_lreads_KiB_persec_average{dimension="octez"}',
        legendFormat=reads,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lwrites_KiB_persec_average{dimension="octez"}',
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
        [load]: 'light-yellow',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="octez"}',
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
        'netdata_apps_mem_MiB_average{dimension="octez"}',
        legendFormat=ram,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="octez"}',
        legendFormat=swap,
      )
    ),

  storage:
    local query = if filecheck then 'netdata_filecheck_dir_size_bytes_average' else 'netdata_disk_space_GiB_average{chart="disk_space._",dimension="used"}';
    graphPanel.new(
    title='Storage',
      datasource='Prometheus',
      linewidth=1,
      format='bytes',
    ).addTarget(
      prometheus.target(
        query,
        legendFormat='{{dimension}}',
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
        'node_filesystem_free_bytes{mountpoint="/"}',
        legendFormat='Available bytes on disk.',
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
        'sum(netdata_apps_pipes_open_pipes_average{dimension="octez"}) + sum(netdata_apps_files_open_files_average{dimension="octez"}) + sum(netdata_apps_sockets_open_sockets_average{dimension="octez"})',
        legendFormat=total,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_sockets_open_sockets_average{dimension="octez"}',
        legendFormat=sockets,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_files_open_files_average{dimension="octez"}',
        legendFormat=files,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_pipes_open_pipes_average{dimension="octez"}',
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
        'irate(node_network_receive_bytes_total[5m]) > 0',
        legendFormat='Bytes received',
      )
    ).addTarget(
      prometheus.target(
        'irate(node_network_transmit_bytes_total[5m]) > 0',
        legendFormat='Bytes transmitted',
      )
    ).addSeriesOverride(
      {
        alias: '/.*received/',
        transform: 'negative-Y',
      }
    ),
}
