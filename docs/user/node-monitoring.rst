Monitoring an Octez Node
========================

Monitoring the behavior of an Octez node can be partially achieved by exploring the logs or,
more efficiently, through the RPC server. The use of RPCs is detailed in :doc:`the RPC documentation <../developer/rpc>`
and :doc:`the RPC references <../shell/rpc>`.

Most practically, however, is to use Octez Metrics to gather information and statistics, which has been integrated directly into the node
since Octez version 14. Users are now able to get metrics without using an external tool,
such as `tezos-metrics <https://gitlab.com/nomadic-labs/tezos-metrics>`_ (which is now deprecated).
The node now includes a server that registers the implemented metrics and outputs them for each received ``/metrics`` http request.
So now you can configure and launch your node with a metrics exporter.


Starting a node with monitoring
-------------------------------

Start
~~~~~

The node can be started with its metrics exporter with the option ``--metrics-addr`` which takes as a parameter ``<ADDR>:<PORT>`` or ``<ADDR>`` or ``:<PORT>``.

``<ADDR>`` and ``<PORT>`` are respectively the address and the port on which to expose the metrics.
By default, ``<ADDR>`` is ``localhost`` and ``<PORT>`` is ``9932``.

.. code-block:: shell

   octez-node run --metrics-addr=<ADDR>:<PORT> …

Note that it is possible to serve metrics on several addresses by using the option more than once.

Configure
~~~~~~~~~

You can also add this configuration to your persistent configuration file through the command line:

.. code-block:: shell

   octez-node config init --metrics-addr=<ADDR>:<PORT> ...

   #Or if the configuration file already exists
   octez-node config update --metrics-addr=<ADDR>:<PORT> ...

See :doc:`the documentation of the node configuration<./node-configuration>` for more information.

A correct setup should write an entry in the logs similar to:

::

   <date> - node.main: starting metrics server on <addr>:<port>

Gathering data
--------------

Scraping Octez Metrics
~~~~~~~~~~~~~~~~~~~~~~

Once your node is correctly set up to export metrics, you can scrape them by querying the metrics server of your node with the request ``/metrics``.

Ex.:

.. code-block:: shell

  curl http://<node_addr>:<port>/metrics

You will be presented with the list of defined and computed metrics as follows:

::

   #HELP metric description
   #TYPE metric type
   octez_metric_name{label_name=label_value} x.x


The metrics that can be exposed by the node can be listed with the command:

.. code-block:: shell

   octez-node dump-metrics


Version 14 of Octez exports metrics from various components of the node, namely:

- :doc:`The p2p layer <../shell/p2p>`
- :doc:`The store <../shell/storage>`
- :doc:`The prevalidator <../shell/prevalidation>`
- :ref:`The chain validator <chain_validator>`
- :ref:`The block validator <block_validator>`
- :ref:`The peer validator <peer_validator>`
- The distributed database
- :doc:`The RPC server <../shell/rpc>`
- The node version

Each exported metric has the following form::

   octez_subsystem_metric{label_name=label_value;...} value

Each metric name starts with ``octez`` as its namespace, followed by the a subsystem name, which is the section of the node described by the metric.
It follows the OpenMetrics specification described `here <https://openmetrics.io/>`__

A metric may provide labeled parameters which allow for different instances of the metric, with different label values.
For instance, the metric ``octez_distributed_db_requester_table_length`` has a label name ``requester_kind`` which allows this metric to have one value for each kind of requester.

::

  octez_distributed_db_requester_table_length{requester_kind="block_header"} x
  octez_distributed_db_requester_table_length{requester_kind="protocol"} y
  ...

Metrics provide information about the node in the form of a `gauge <https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#gauge>`_ that can increase or decrease (like the number of connections),
a `counter <https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#counter>`_ that can only increase (like the head level),
or a `histogram <https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#histogram>`_ used to track the size of events and how long they usually take (e.g., the time taken by an RPC call).

The label value is sometimes used to store information that can't be described by the metric value (which can only be a float). This is used for example by the ``octez_version`` metric that provides the version within the labels.

For the list of metrics provided by the node, and a typical monitoring setup, see the following page:

.. toctree::
   :maxdepth: 2

   ../developer/openmetrics


.. note::

   Most of the metrics are computed when scraped from the node. As there is no rate limiter, you should consider scraping wisely and adding a proxy for a public endpoint, to limit the impact on performance.

.. _prometheus_server:

Prometheus
~~~~~~~~~~

Scraping metrics gives you instant values of the metrics. For a more effective monitoring, you should create a time series of these metrics.

We suggest using `Prometheus <https://prometheus.io/>`_ for that purpose.

Once installed, you need to add the scraping job to the configuration file.

::

   - job_name: 'octez-exporter'
     scrape_interval: interval s
     metrics_path: "/metrics"
     static_configs:
       - targets: ['addr:port']

Prometheus is a service, so you need to start it. Note that Prometheus can also scrape metrics from several nodes!

.. code-block:: shell

   sudo systemctl start prometheus

.. _hardware_metrics:

Hardware metrics
~~~~~~~~~~~~~~~~

In addition to node metrics, you may want to gather other information and statistics for effective monitoring, such as hardware metrics.

For that purpose, we suggest using `Netdata <https://www.netdata.cloud/>`_.

To install Netdata:

.. code-block:: shell

  bash <(curl -Ss https://my-netdata.io/kickstart.sh)

Add the following at the end of ``/etc/netdata/app_groups.conf``

.. code-block:: shell

  octez: octez-node octez-validator

.. _filecheck:

Optionally, you can enable storage monitoring with ``filecheck``.

To do so, create a ``filecheck.conf`` file in ``/etc/netdata/go.d/`` and add::

  jobs:
    - name: octez-data-dir-size
      discovery_every: 30s
      dirs:
        collect_dir_size: yes
        include:
          - '/path/to/data/dir'

    - name: octez-context-size
      discovery_every: 30s
      dirs:
        collect_dir_size: yes
        include:
          - '/path/to/data/dir/context'

    - name: octez-store-size
      discovery_every: 30s
      dirs:
        collect_dir_size: yes
        include:
          - '/path/to/data/dir/store'


Then, you need to make sure that the ``netdata`` user has the correct read/write/execute permissions.
This can be achieved by adding this user to your user's group, or by defining custom rules.

To check that the setup is correct::

  #Log as netdata user
  sudo -u netdata -s

  #Go to the plugin directory
  cd /usr/libexec/netdata/plugins.d/

  #Run the debugger
  ./go.d.plugin -d -m filecheck


With a correct install, you should see lines such as::

  BEGIN 'filecheck_octez-data-dir-size.dir_size' 9999945
  SET '/path/to/data/dir/' = 48585735837
  END

Note, if you use filecheck for storage monitoring, you need to configure your dashboards accordingly. More details in the :ref:`Grafazos configuration section <grafazos_configuration>`.

.. _monitoring_logs:

Logs
~~~~

Eventually, you may want to gather the logs from the different Octez executables. To do so, we suggest to use `Loki <https://grafana.com/docs/loki/latest/>`_ and `Promtail <https://grafana.com/docs/loki/latest/send-data/promtail/>`_. Promtail is used to gather the logs from each executable of Octez and pushes them to a Loki instance, for indexing metadata about the logs.

You first need to install both tools, following `their installation instructions <https://grafana.com/docs/loki/latest/setup/install/local/>`_.

A configuration file will be required, which can be downloaded with:

.. code-block:: shell

	wget https://raw.githubusercontent.com/grafana/loki/master/cmd/loki/loki-local-config.yaml
	wget https://raw.githubusercontent.com/grafana/loki/main/clients/cmd/promtail/promtail-local-config.yaml

The config file for Loki, ``loki-local-config.yml``, can be left untouched.
However, the Promtail config file, ``promtail-local-config.yml``, requires to be adapted to get the logs needed.
For each Octez executable you want the logs from, you need to add a new job to the ``scrape_configs`` part of the config file.
For instance, to gather the logs from the node, you would add::

  - job_name: octez-node
  static_configs:
  - targets:
      - localhost
    labels:
      job: octez-node
      __path__: /path/to/file/node-logs.log

Note that it requires to redirect the logs from your node into a log file, ``/path/to/file/node-logs.log`` in this example. To do so, you can follow the guidelines from :doc:`the logging documentation <./logging>`.

You can now run both tools with their config files:

.. code-block:: shell

   ./loki-linux-amd64 -config.file=loki-local-config.yaml
   ./promtail-linux-amd64 -config.file=promtail-local-config.yaml

Dashboards
----------

Dashboards will take your node monitoring to the next level, allowing you to visualize the raw data collected with pretty, colorful graphs.

Grafana
~~~~~~~

Dashboards can be created and visualized with `Grafana <https://grafana.com/>`_. Grafana can be installed by following `these instructions <https://grafana.com/docs/grafana/latest/>`__.

Once installed and running, you should be able to reach the interface on port ``3000`` (you can change the port on the Grafana config file).

Then you need to add the configured Prometheus server (see :ref:`Prometheus <prometheus_server>`) as a data source in ``Configuration/Data sources``.

If you want to have logs on your dashboards, as described in :ref:`the logs part <monitoring_logs>`, you also need to add Loki as a data source.


Grafazos
~~~~~~~~

You can interactively create your own dashboards to monitor your node, using the Grafana GUI. Alternatively Grafana allows you to import dashboards from JSON files.

`Grafazos <https://gitlab.com/nomadic-labs/grafazos>`_ generates JSON files that you can import into the Grafana interface.

This tool generates the following dashboards:

- ``octez-compact``: A compact dashboard that gives a brief overview of the various node metrics on a single page.
- ``octez-basic``: A basic dashboard with all the node metrics.
- ``octez-with-logs``: Same as basic but also displays the node's logs. This dashboard requires to follow the instructions of :ref:`the logs part <monitoring_logs>`.
- ``octez-full``: A full dashboard with the logs and hardware data. This dashboard should be used with `Netdata <https://www.netdata.cloud/>`_ (for supporting hardware data) in addition to Promtail.

You can generate them from the sources, with your own configuration. Or you can use the JSON files, compatible with your node version found `here <https://gitlab.com/nomadic-labs/grafazos/-/packages>`_.

.. _grafazos_configuration:

The dashboards can be configured by setting environment variables before starting their generation (using ``make``).

The available variables are:

- ``BRANCH``: Used to specify the name of the branch of the node.
- ``NODE_INSTANCE_LABEL``: Used to set the name of the node instance label in the metrics.
- ``STORAGE_MODE``: To be set to ``filecheck`` if the :ref:`storage monitoring with filecheck <filecheck>` is enabled.
