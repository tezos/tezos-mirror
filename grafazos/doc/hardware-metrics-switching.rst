Hardware Metrics Sourcing: Multi-Exporter Configuration
========================================================

Overview
--------

Grafazos provides **configurable hardware metrics sourcing** for Octez Grafana dashboards, allowing users to combine any set of metric exporters:

1. **netdata** (default): Application-aware metrics from netdata daemon
2. **process-exporter**: Process-level metrics (CPU, memory, disk I/O, file descriptors) via ``namedprocess_namegroup_*`` metrics
3. **node-exporter**: System-level metrics (storage, network) via ``node_*`` metrics
4. **local-storage-exporter**: Size on disk via ``local_storage_pv_used_bytes``

``HARDWARE_SRC`` accepts a **comma-separated list** of exporters. When multiple exporters provide the same metric, all variants appear in the same Grafana panel with an ``[exporter-name]`` legend suffix.

Key Features
~~~~~~~~~~~~

- **Multi-Exporter Panels**: Multiple exporters can contribute queries to the same panel
- **Legend Suffixes**: When multiple exporters match, legends are suffixed with ``[exporter-name]``
- **Granular Selection**: Choose exactly which exporters to use (e.g., ``netdata,node-exporter``)
- **Side-by-Side Comparison**: Comparison dashboard showing netdata and prom-exporters metrics side-by-side
- **Build Integration**: Seamless integration with existing Makefile build system

Motivation
----------

Hardware metrics can be sourced from different monitoring backends. This system supports netdata, process-exporter, node-exporter, and local-storage-exporter, allowing users to combine them as needed.

Solution Approach
~~~~~~~~~~~~~~~~~

This implementation provides **flexible metric sourcing** through:

- **selectMetrics()** pattern: Each hardware metric function declares which exporters can provide it via a mapping object. All matching exporters contribute queries to the panel.
- **Environment Variable Control**: Users specify a comma-separated list of exporters via ``HARDWARE_SRC``
- **Exporter Mapping**: Each metric explicitly declares its exporter (``netdata``, ``process-exporter``, ``node-exporter``, or ``local-storage-exporter``)
- **Comparison Dashboard**: Visual reference showing both approaches side-by-side for evaluation

Configuration Parameters
------------------------

HARDWARE_SRC Parameter
~~~~~~~~~~~~~~~~~~~~~~

**Type**: Environment variable (comma-separated list)

**Default Value**: ``netdata``

**Valid Exporter Names**:

- ``netdata`` - Application-aware metrics from netdata daemon
- ``process-exporter`` - Process-level metrics (``namedprocess_namegroup_*``)
- ``node-exporter`` - System-level metrics (``node_filesystem_*``, ``node_network_*``)
- ``local-storage-exporter`` - Size on disk metrics (``local_storage_pv_used_bytes``)

**How It Works**:

1. User sets environment variable: ``export HARDWARE_SRC=process-exporter,node-exporter``
2. Makefile receives value: ``HARDWARE_SRC ?= netdata``
3. Passed to jsonnet compiler: ``--ext-str hardware_src="$(HARDWARE_SRC)"``
4. Parsed in base.jsonnet into a list of exporters (``hardware_exporters``)
5. ``selectMetrics()`` finds all matching exporters and returns queries for each
6. When multiple match, legend suffixes are added automatically

**Example Usage**:

.. code-block:: shell

    # Build with default netdata metrics
    make

    # Build with both prometheus exporters
    HARDWARE_SRC=process-exporter,node-exporter make

    # Combine netdata with node-exporter (netdata for process metrics, node-exporter for system metrics)
    HARDWARE_SRC=netdata,node-exporter make

    # Show both netdata and process-exporter in same panels (with legend suffixes)
    HARDWARE_SRC=netdata,process-exporter make

    # Build comparison dashboard (shows both side-by-side, independent of HARDWARE_SRC)
    make compare-hardware.jsonnet

LOGS_LABEL Parameter
~~~~~~~~~~~~~~~~~~~~

**Type**: Environment variable

**Default Value**: ``job``

**Purpose**: Configure which label field to filter logs by

**How It Works**:

1. User sets environment variable: ``export LOGS_LABEL=service``
2. Makefile passes to jsonnet: ``--ext-str logs_label="$(LOGS_LABEL)"``
3. Used in log filter queries: ``{$logs_label="octez-node"}``

**Example Usage**:

.. code-block:: shell

    # Filter logs by 'job' label (default)
    make octez-with-logs.jsonnet

    # Filter logs by 'service' label
    LOGS_LABEL=service make octez-with-logs.jsonnet

    # Filter logs by 'app' label
    LOGS_LABEL=app make octez-with-logs.jsonnet

Metrics Comparison
------------------

Comparison
~~~~~~~~~~

=================== ======= ================= ============= ======================
Metric Category     netdata process-exporter  node-exporter local-storage-exporter
=================== ======= ================= ============= ======================
Disk I/O (Process)  ✅      ✅
CPU Utilization     ✅      ✅
Memory Usage        ✅      ✅
Open FDs            ✅      ✅
Storage/Filesystem  ✅                         ✅            ✅
Network I/O         ✅                         ✅
=================== ======= ================= ============= ======================

Disk I/O Metrics
~~~~~~~~~~~~~~~~

**netdata Approach:**

::

    // Logical disk I/O at app_group level
    'netdata_app_disk_logical_io_KiB_persec_average{dimension="%s", app_group="%s"}'

- **Metric Name**: ``netdata_app_disk_logical_io_KiB_persec_average``
- **Granularity**: Application group aggregation
- **Dimensions**: "reads" and "writes" as dimension labels
- **Units**: KiB/sec (kilobytes per second)
- **Coverage**: Logical disk operations per app_group

**Strengths**:

- Integrated with application group awareness
- Simple aggregation level

**Weaknesses**:

- Doesn't show individual process breakdown
- Limited to application group filtering
- Logical I/O only (no physical device information)

**process-exporter Approach:**

::

    // Physical disk I/O with process-level granularity
    'rate(namedprocess_namegroup_read_bytes_total[1m])/1024',
    'rate(namedprocess_namegroup_write_bytes_total[1m])/1024'

- **Metric Name**: ``namedprocess_namegroup_*_bytes_total`` (counters)
- **Granularity**: Per-process-group with ``groupname`` label
- **Dimensions**: Read and write as separate metric series
- **Units**: Converted to KiB/sec via rate() calculation
- **Coverage**: Actual bytes read/written per process group

**Strengths**:

- Per-process-group tracking with explicit ``groupname`` label
- Shows actual bytes transferred (physical I/O)
- Standard Prometheus counter pattern (monotonic)
- Compatible with external monitoring infrastructure

**Weaknesses**:

- Requires rate() calculation for per-second metrics
- Process-exporter must be deployed separately

CPU Utilization Metrics
~~~~~~~~~~~~~~~~~~~~~~~

**netdata Approach:**

::

    'netdata_app_cpu_utilization_percentage_average{app_group="%s"}'

- **Metric Name**: ``netdata_app_cpu_utilization_percentage_average``
- **Type**: Gauge (direct percentage)
- **Units**: Percent (0-100 or 0-400 for 4 cores)
- **Coverage**: CPU usage aggregated per app_group
- **Time Aggregation**: Already averaged by netdata

**process-exporter Approach:**

::

    'namedprocess_namegroup_cpu_seconds_total'

- **Metric Name**: ``namedprocess_namegroup_cpu_seconds_total``
- **Type**: Counter (cumulative CPU time in seconds)
- **Units**: Seconds (must be converted via irate/rate)
- **Coverage**: Per-CPU-mode tracking (user, system)
- **Time Aggregation**: Requires rate() calculation

Memory Usage Metrics
~~~~~~~~~~~~~~~~~~~~

**netdata Approach:**

::

    'netdata_app_mem_usage_MiB_average{app_group="%s"}',     // RAM
    'netdata_app_swap_usage_MiB_average{app_group="%s"}'     // Swap

- **Metric Names**:

  - ``netdata_app_mem_usage_MiB_average`` (RAM)
  - ``netdata_app_swap_usage_MiB_average`` (Swap)

- **Type**: Gauge
- **Units**: MiB (mebibytes)
- **Coverage**: Memory and swap usage per app_group
- **Breakdown**: Separate metrics for RAM vs Swap

**process-exporter Approach:**

::

    'namedprocess_namegroup_memory_bytes{memtype="rss"}',    // Physical RAM
    'namedprocess_namegroup_memory_bytes{memtype="vms"}',    // Virtual memory
    'namedprocess_namegroup_memory_bytes{memtype="swap"}'    // Swap

- **Metric Names**: ``namedprocess_namegroup_memory_bytes`` (with memtype label)
- **Type**: Gauge
- **Units**: Bytes (requires /1024/1024 conversion to MiB)
- **Coverage**: RSS (resident), VMS (virtual), and Swap per groupname
- **Breakdown**: Three memory types via label matching

Storage/Filesystem Metrics
~~~~~~~~~~~~~~~~~~~~~~~~~~

**netdata Approach:**

::

    'netdata_disk_space_GiB_average'  // All disks averaged together

- **Metric Name**: ``netdata_disk_space_GiB_average``
- **Coverage**: Aggregate disk space
- **Granularity**: Single value for all filesystems
- **Units**: GiB (gibibytes)

**node-exporter Approach:**

::

    'node_filesystem_size_bytes{mountpoint=~"^/$"}'  // Root filesystem only

- **Metric Name**: ``node_filesystem_size_bytes`` (from node-exporter)
- **Coverage**: Per-filesystem granularity via mountpoint label
- **Granularity**: Individual filesystem data
- **Units**: Bytes (requires /1024/1024/1024 conversion to GiB)
- **Label**: ``device`` shows actual device (e.g., ``/dev/sda1``)

When to Use Each
~~~~~~~~~~~~~~~~

**Choose netdata when:**

- Running existing netdata infrastructure
- Need out-of-the-box application-aware metrics
- Prefer pre-aggregated, immediately-readable values
- Simple deployment without additional exporters

**Choose process-exporter when:**

- Need explicit process group identification (groupname labels)
- Need fine-grained process-level insights (individual FD counts, per-group CPU time)
- Running multiple process groups and need clear separation

**Choose node-exporter when:**

- Require device-level filesystem metrics
- Need system-level network traffic monitoring
- Want standardized Prometheus counter/gauge patterns

Usage Instructions
------------------

Default Build (netdata)
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    cd grafazos
    make

This compiles all dashboards using netdata metrics. Generated files appear in ``output/`` directory.

Build with Prometheus Exporters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    cd grafazos
    HARDWARE_SRC=process-exporter,node-exporter make

This recompiles all dashboards using process-exporter and node-exporter metrics.

Build with Multiple Exporters in Same Panel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    cd grafazos
    HARDWARE_SRC=netdata,process-exporter make

When both exporters provide the same metric (e.g., CPU), both queries appear in
the same panel with legend suffixes like ``Cpu load [netdata]`` and
``Cpu load [process-exporter]``.

Build Specific Dashboard
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    # octez-full with both prometheus exporters
    HARDWARE_SRC=process-exporter,node-exporter make octez-full.jsonnet

    # octez-basic with netdata (default)
    make octez-basic.jsonnet

    # Comparison dashboard
    make compare-hardware.jsonnet
