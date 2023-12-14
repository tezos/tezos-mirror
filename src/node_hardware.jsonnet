// Copyright (c) 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
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
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;
local filecheck = std.extVar('storage_mode') == 'filecheck';
local netdata_legacy = std.extVar('netdata') == 'legacy';

//##
// Hardware relates stats
//##

{
  ios:
    local reads = 'reads';
    local writes = 'writes';
    local reads_target =
      if netdata_legacy then 'netdata_apps_lreads_KiB_persec_average{dimension="octez"}'
      else 'netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="reads",' + std.extVar('node_instance_label') + '="$node_instance"}';
    local writes_target =
      if netdata_legacy then 'netdata_apps_lwrites_KiB_persec_average{dimension="octez"}'
      else 'netdata_app_disk_logical_io_KiB_persec_average{app_group="octez",dimension="writes",' + std.extVar('node_instance_label') + '="$node_instance"}';
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
        reads_target,
        legendFormat=reads,
      )
    ).addTarget(
      prometheus.target(
        writes_target,
        legendFormat=writes,
      )
    ),

  cpu:
    local load = 'Cpu load';
    local load_target =
      if netdata_legacy then 'netdata_apps_cpu_percentage_average{dimension="octez"}'
      else 'sum(netdata_app_cpu_utilization_percentage_average{app_group="octez",' + std.extVar('node_instance_label') + '="$node_instance"})';
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
        load_target,
        legendFormat=load,
      )
    ),

  memory:
    local ram = 'Memory usage';
    local swap = 'Swap usage';
    local ram_target =
      if netdata_legacy then 'netdata_apps_mem_MiB_average{dimension="octez"}'
      else 'netdata_app_mem_usage_MiB_average{app_group="octez",' + std.extVar('node_instance_label') + '="$node_instance"}';
    local swap_target =
      if netdata_legacy then 'netdata_apps_swap_MiB_average{dimension="octez"}'
      else 'netdata_app_swap_usage_MiB_average{app_group="octez",' + std.extVar('node_instance_label') + '="$node_instance"}';
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
        ram_target,
        legendFormat=ram,
      )
    ).addTarget(
      prometheus.target(
        swap_target,
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
    local sockets_target =
      if netdata_legacy then 'netdata_apps_sockets_open_sockets_average{dimension="octez"}'
      else 'netdata_app_fds_open_fds_average{dimension="sockets",app_group="octez",' + std.extVar('node_instance_label') + '="$node_instance"}';
    local files_target =
      if netdata_legacy then 'netdata_apps_sockets_open_files_average{dimension="octez"}'
      else 'netdata_app_fds_open_fds_average{dimension="files",app_group="octez",' + std.extVar('node_instance_label') + '="$node_instance"}';
    local pipes_target =
      if netdata_legacy then 'netdata_apps_sockets_open_pipes_average{dimension="octez"}'
      else 'netdata_app_fds_open_fds_average{dimension="pipes",app_group="octez",' + std.extVar('node_instance_label') + '="$node_instance"}';
    local total_target = 'sum(' + sockets_target + ') + sum(' + files_target + ') + sum(' + pipes_target + ')';
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
        total_target,
        legendFormat=total,
      )
    ).addTarget(
      prometheus.target(
        sockets_target,
        legendFormat=sockets,
      )
    ).addTarget(
      prometheus.target(
        files_target,
        legendFormat=files,
      )
    ).addTarget(
      prometheus.target(
        pipes_target,
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
