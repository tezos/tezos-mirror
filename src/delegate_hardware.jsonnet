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
local graphPanel = grafana.graphPanel;
local prometheus = grafana.prometheus;

//##
// Hardware relates stats
//##

{
  ios:
    local reads_accuser = 'reads accuser';
    local writes_accuser = 'writes accuser';
    local reads_baker = 'reads baker';
    local writes_baker = 'writes baker';
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
        [reads_accuser]: 'dark-yellow',
        [writes_accuser]: 'light-yellow',
        [reads_baker]: 'dark-red',
        [writes_baker]: 'light-red',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_lreads_KiB_persec_average{dimension="octez-accuser"}',
        legendFormat=reads_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lwrites_KiB_persec_average{dimension="octez-accuser"}',
        legendFormat=writes_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lreads_KiB_persec_average{dimension="octez-baker"}',
        legendFormat=reads_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lweads_KiB_persec_average{dimension="octez-baker"}',
        legendFormat=writes_baker,
      )
    ),

  cpu:
    local load_accuser = 'Cpu load accuser';
    local load_baker = 'Cpu load baker';
    graphPanel.new(
      title='Cpu actitvity',
      datasource='Prometheus',
      linewidth=1,
      format='percent',
      aliasColors={
        [load_accuser]: 'light-yellow',
        [load_baker]: 'light-red',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="octez-accuser"}',
        legendFormat=load_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="octez-baker"}',
        legendFormat=load_baker,
      )
    ),

  memory:
    local ram_accuser = 'Memory usage accuser';
    local swap_accuser = 'Swap usage accuser';
    local ram_baker = 'Memory usage baker';
    local swap_baker = 'Swap usage baker';
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
        [ram_accuser]: 'dark-yellow',
        [swap_accuser]: 'light-yellow',
        [ram_baker]: 'dark-red',
        [swap_baker]: 'light-red',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_mem_MiB_average{dimension="octez-accuser"}',
        legendFormat=ram_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="octez-accuser"}',
        legendFormat=swap_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_mem_MiB_average{dimension="octez-baker"}',
        legendFormat=ram_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="octez-baker"}',
        legendFormat=swap_baker,
      )
    ),
}
