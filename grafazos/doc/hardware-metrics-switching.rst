Hardware Metrics Sourcing: netdata vs Prometheus Exporters
===========================================================

Overview
--------

Grafazos provides **configurable hardware metrics sourcing** for Octez Grafana dashboards, allowing users to choose between two different monitoring backends:

1. **netdata** (default): Application-aware metrics from netdata daemon
2. **prom-exporters** (alternative): System and process-level metrics from Prometheus exporters (process-exporter + node-exporter)

Additionally, the system supports **configurable log filtering labels** via the ``LOGS_LABEL`` parameter, enabling users to query logs by custom label fields.

Key Features
~~~~~~~~~~~~

- **Metric Source Switching**: Runtime selection of metric backend via ``HARDWARE_SRC`` environment variable
- **Process-Level Monitoring**: Visibility into individual process group performance
- **Self-Documenting Dashboards**: Legend labels display process group names and device identifiers at runtime
- **Side-by-Side Comparison**: Comparison dashboard showing netdata and prom-exporters metrics side-by-side
- **Flexible Backend Selection**: Default configuration uses netdata, with easy opt-in to prom-exporters
- **Build Integration**: Seamless integration with existing Makefile build system

Motivation
----------

Hardware metrics can be sourced from different monitoring backends. This system supports both netdata and Prometheus exporters, allowing users to choose their preferred monitoring infrastructure.

Solution Approach
~~~~~~~~~~~~~~~~~

This implementation provides **flexible metric sourcing** through:

- **selectMetric()** pattern: Each hardware metric function conditionally selects between netdata and prom-exporter variants
- **Environment Variable Control**: Users specify ``HARDWARE_SRC=netdata`` or ``HARDWARE_SRC=prom-exporters`` at build time
- **Process-Exporter**: The ``process-exporter`` (namedprocess_namegroup_* metrics) provides process-level monitoring with clear groupname labels
- **Comparison Dashboard**: Visual reference showing both approaches side-by-side for evaluation

Configuration Parameters
------------------------

HARDWARE_SRC Parameter
~~~~~~~~~~~~~~~~~~~~~~

**Type**: Environment variable

**Default Value**: ``netdata``

**Valid Values**:

- ``netdata`` - Use netdata application-aware metrics
- ``prom-exporters`` - Use Prometheus exporters (process-exporter + node-exporter)

**How It Works**:

1. User sets environment variable: ``export HARDWARE_SRC=prom-exporters``
2. Makefile receives value: ``HARDWARE_SRC ?= netdata``
3. Passed to jsonnet compiler: ``--ext-str hardware_src="$(HARDWARE_SRC)"``
4. Imported in base.jsonnet: ``hardware_src: std.extVar('hardware_src')``
5. Used in metric functions: ``if base.hardware_src == 'prom-exporters' then ...``
6. Compiled dashboard contains only selected metrics

**Example Usage**:

.. code-block:: shell

    # Build with default netdata metrics
    make

    # Build with prom-exporters metrics
    HARDWARE_SRC=prom-exporters make

    # Build specific dashboard with prom-exporters
    HARDWARE_SRC=prom-exporters make octez-full.jsonnet

    # Build comparison dashboard (shows both side-by-side)
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

=================== ======= ==============
Metric Category     netdata prom-exporters
=================== ======= ==============
Disk I/O (Process)  ✅      ✅
CPU Utilization     ✅      ✅
Memory Usage        ✅      ✅
Open FDs            ✅      ✅
Storage/Filesystem  ✅      ✅
Network I/O         ✅      ✅
Label Clarity       ⚠️      ✅
=================== ======= ==============

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

**prom-exporters Approach:**

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

**prom-exporters Approach:**

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

**prom-exporters Approach:**

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

**prom-exporters Approach:**

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

**Choose prom-exporters when:**

- Already using Prometheus ecosystem (node-exporter, process-exporter)
- Need explicit process group identification (groupname labels)
- Require device-level filesystem metrics
- Want standardized Prometheus counter/gauge patterns
- Need fine-grained process-level insights (individual FD counts, per-group CPU time)
- Running multiple process groups and need clear separation

Usage Instructions
------------------

Default Build (netdata)
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    cd grafazos
    make

This compiles all dashboards using netdata metrics. Generated files appear in ``output/`` directory.

Build with prom-exporters Metrics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    cd grafazos
    HARDWARE_SRC=prom-exporters make

This recompiles all dashboards substituting prom-exporter metrics for netdata equivalents.

Build Specific Dashboard
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

    # octez-full with prom-exporters
    HARDWARE_SRC=prom-exporters make octez-full.jsonnet

    # octez-basic with netdata (default)
    make octez-basic.jsonnet

    # Comparison dashboard
    make compare-hardware.jsonnet
