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
    local reads_endorser = 'reads endorser';
    local writes_endorser = 'writes endorser';
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
        [reads_endorser]: 'dark-green',
        [writes_endorser]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_lreads_KiB_persec_average{dimension="tezos-accuser"}',
        legendFormat=reads_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lwrites_KiB_persec_average{dimension="tezos-accuser"}',
        legendFormat=writes_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lreads_KiB_persec_average{dimension="tezos-baker"}',
        legendFormat=reads_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lweads_KiB_persec_average{dimension="tezos-baker"}',
        legendFormat=writes_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lreads_KiB_persec_average{dimension="tezos-endorser"}',
        legendFormat=reads_endorser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_lweads_KiB_persec_average{dimension="tezos-endorser"}',
        legendFormat=writes_endorser,
      )
    ),

  cpu:
    local load_accuser = 'Cpu load accuser';
    local load_baker = 'Cpu load baker';
    local load_endorser = 'Cpu load endorser';
    graphPanel.new(
      title='Cpu actitvity',
      datasource='Prometheus',
      linewidth=1,
      format='percent',
      aliasColors={
        [load_accuser]: 'light-yellow',
        [load_baker]: 'light-red',
        [load_endorser]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="tezos-accuser"}',
        legendFormat=load_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="tezos-baker"}',
        legendFormat=load_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_cpu_percentage_average{dimension="tezos-endorser"}',
        legendFormat=load_endorser,
      )
    ),

  memory:
    local ram_accuser = 'Memory usage accuser';
    local swap_accuser = 'Swap usage accuser';
    local ram_baker = 'Memory usage baker';
    local swap_baker = 'Swap usage baker';
    local ram_endorser = 'Memory usage endorser';
    local swap_endorser = 'Swap usage endorser';
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
	[ram_endorser]: 'dark-green',
        [swap_endorser]: 'light-green',
      },
    ).addTarget(
      prometheus.target(
        'netdata_apps_mem_MiB_average{dimension="tezos-accuser"}',
        legendFormat=ram_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="tezos-accuser"}',
        legendFormat=swap_accuser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_mem_MiB_average{dimension="tezos-baker"}',
        legendFormat=ram_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="tezos-baker"}',
        legendFormat=swap_baker,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_mem_MiB_average{dimension="tezos-endorser"}',
        legendFormat=ram_endorser,
      )
    ).addTarget(
      prometheus.target(
        'netdata_apps_swap_MiB_average{dimension="tezos-endorser"}',
        legendFormat=swap_endorser,
      )
    ),

}
