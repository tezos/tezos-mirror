Grafazos: Grafana dashboards for Octez node monitoring.
=======================================================

Grafazos allows to generate dashboards from jsonnet programs.

It relies on the metrics exposed by the Octez node, see :doc:`../developer/openmetrics`, to build grafana dashboards.

Resources
---------

Jsonnet
~~~~~~~

To build the dashboards, you need to install ``jsonnet`` with version ``0.18.0`` minimum.
We recommend to use https://github.com/google/go-jsonnet.

Grafonnet
~~~~~~~~~

``Grafonnet`` is the jsonnet library used to generate grafana dashboards.

``Grafonnet`` should be installed using `jsonnet-bundler <https://github.com/jsonnet-bundler/jsonnet-bundler/>`__.

To do so, run ``go install -a github.com/jsonnet-bundler/jsonnet-bundler/cmd/jb@latest`` and update your environment variables such as ``export GOPATH=$HOME/go`` and ``export PATH=$PATH:$GOPATH/bin``.

Once ``jsonnet-bundler`` installed you can import dependencies (here ``grafonnet``) by running the following:

.. code-block:: shell

    jb install

Tools
-----

- Emacs mode: https://github.com/tminor/jsonnet-mode

Build
-----

To create the dashboards:

.. code-block:: shell

    make

The dashboards will be availbale in the ``grafazos/output/`` folder.

You can also create a specific dashboard with


.. code-block:: shell

    make dashboard_name

with ``dashboard_name`` one of:

  - ``compact``: a single page compact dashboard which aims to give a
    brief overview of the node's health,
  - ``basic``: a simple dashboard displaying detailed node's metrics,
  - ``logs``: same as ``basic`` but also displaying node's logs (thanks to `Loki and promtail <https://github.com/grafana/loki>`__)
  - ``full``: same as ``logs`` but also displaying hardware metrics (thanks to `netdata <https://www.netdata.cloud/>`__)
  - ``dal-basic``: a simple dashboard displaying some DAL node's metrics,

To create the dashboards for a different branch

.. code-block:: shell

    BRANCH=foo make

By default, the instance names label is set to ``instance``. If you are
using a particular label for the instance names, you can use `NODE_INSTANCE_LABEL` to set it:

.. code-block:: shell

    NODE_INSTANCE_LABEL=my_instance_label


By default, the logs are filtered using the ``job`` label. If you need to use
a different label for filtering logs (e.g., ``service``), you can configure it with `LOGS_LABEL`:

.. code-block:: shell

    LOGS_LABEL=app


If you need hardware metrics, note that, by default, the storage stat considered is the used
space of the whole disk. Optionnally you can enable storage monitoring with ``filecheck``:

.. code-block:: shell

    STORAGE_MODE=filecheck


By default, all mountpoints are monitored. If you want to selectively change mountpoints
to ``/`` and another specific mountpoint, you can enable and add it with the ``MOUNTPOINT`` variable:
.. code-block:: shell

    MOUNTPOINT="/opt"

By default, the logs are fetched from Loki. If you want to use GCP logs, you can enable it with
the ``LOGSRC`` variable, the GCP project id with the ``GCP`` variable and the
``GCP_DATASOURCE_UID`` for it's unique id in grafana.

.. code-block:: shell

    LOGSRC="gcp"
    GCP_PROJECT_ID="project_id"
    GCP_DATASOURCE_UID="datasource_uid"

By default, dashboards are created without uid, resulting in a random one assigned by grafana.
You can set a specific uid with the ``UID`` variable, which will be concatenated to a default uid
set in each dasboard's definition.
If ``UID`` is set to an empty string, then the uid is set to the default uid value set for each
dashboard.

.. code-block:: shell

    UID="some_string"
