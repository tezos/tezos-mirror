
Octez Metrics
=============

The Octez node is able to produce metrics information and serve them in the
`Open Metrics
<https://openmetrics.io/>`_ format, an emerging standard for exposing metrics data, especially used in cloud-based systems.

Supported Open Metrics
----------------------

The Octez node supports the following metrics, characterized by: the name of
the metric, the type of the metric as in the `open metrics specification
<https://openmetrics.io/>`__, a user friendly description on the metric and a
list of labels (that can be used to aggregate or query metrics).

For more information check the openmetrics specification: https://openmetrics.io/

.. csv-table::
   :file: metrics.csv
   :header-rows: 1
   :widths: 20, 20, 20, 10


Usage
-----

To instruct the Octez node to produce metrics, the user needs to pass the option
``--metrics-addr=<ADDR>:<PORT>``. The port specified on the command line is the port
where the integrated open metrics server will be available (9932 by default).
The address defaults to localhost.
When the option is not supplied at all, no metrics are produced.
Ex.::

  tezos-node run --metrics-addr=:9091

To query the open metrics server the user can simply query the node.

Ex.::

  curl http://<node_addr>:9091/metrics

Collecting metrics
------------------

Different third-party tools can be used to query the Octez node and collect
metrics from it. Let us illustrates this with the example of a `Prometheus
server <https://prometheus.io/docs/introduction/overview/>`_.

Update the Prometheus configuration file (typically, ``prometheus.yml``)
to add a "scrape job" - that is how Prometheus is made aware of a new data
source - using adequate values:

- job_name: Use a unique name among other scrape jobs. All metrics collected
  through this job will have automatically a ‘job’ label with this value added
  to it
- targets: The URL of Octez node.

::

  - job_name: 'tezos-metrics'
      scheme: http
      static_configs:
        - targets: ['localhost:9091']


Monitoring the node with metrics
--------------------------------

Once the node is correctly set up to export metrics
and those are collected by a `Prometheus server <https://prometheus.io/docs/introduction/overview/>`_,
you can graphically monitor your node with a `Grafana dashboard <https://grafana.com/>`_.

Dashboards suited for Octez can be easily built with the `Grafazos <https://gitlab.com/nomadic-labs/grafazos/>`_ tool.
Grafazos provides several ready-to-use dashboards for Octez on the `Grafazos packages page <https://gitlab.com/nomadic-labs/grafazos/-/packages>`__, as plain JSON files.
Their sources are also available as `jsonnet <https://jsonnet.org/>`__ files, that can be adjusted to build customized dashboards, if needed:


- ``octez-basic``: A basic dashboard with all the node metrics
- ``octez-full``: A full dashboard with the logs and hardware data.
  This dashboard should be used with `Netdata <https://www.netdata.cloud/>`_  (for supporting hardware data) and `Promtail <https://grafana.com/docs/loki/latest/clients/promtail/>`_ (for exporting the logs).
- ``octez-compact``: A compact dashboard that gives a brief overwiev of the various node metrics on a single page.
