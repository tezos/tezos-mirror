Tezt: Long Tests and Performance Regression Test Framework
============================================================

:doc:`Tezt <tezt>` can also be used for long tests.
Here are the differences with regular Tezt tests:

- long tests are not run in the CI but on dedicated machines with
  stable, predictable performance, and with no global timeout like in
  the CI;
- long tests are declared in :src:`tezt/long_tests/main.ml` instead of
  :src:`tezt/tests/main.ml`;
- long tests are registered with ``Long_test.register`` instead
  of ``Test.register``;
- long tests have easy access to a Performance Regression Test framework
  which provides these features:

  - Persist measurement samples (in an `InfluxDB <https://github.com/influxdata/influxdb>`_ database) such as how long it takes
    to do something. These samples will be used to prevent regressions of
    performance.
  - Provide easy access to a `Grafana <https://github.com/grafana/grafana>`_ instance that can be updated to
    create graphs displaying the samples sent to InfluxDB;
  - Call Slack webhooks to send alerts when a performance regression has
    been detected.

Adding a Long Test
------------------

To add a long test, you need to register it with
``Long_test.register`` (respectively
``Long_test.register_with_protocol``) instead of ``Test.register``
(respectively ``Protocol.register_test``). The test must be registered
in :src:`tezt/long_tests/main.ml` instead of
:src:`tezt/tests/main.ml`. ``Long_test.register`` is very similar to
``Test.register``. The main difference is that you have to declare a
timeout (that should be significantly overestimated, see the
documentation of the ``timeout`` type in
:src:`tezt/lib_performance_regression/long_test.mli`) and that ``Long_test.register``
will handle InfluxDB, Grafana and Slack alerts.  Just like regular
Tezt tests, your test should be implemented in the same file with other
thematically-related tests, with only a single line in
:src:`tezt/long_tests/main.ml`.

If you declare a test in :src:`tezt/long_tests/main.ml` and merge it
into ``master``, it will be automatically run on dedicated machines
that regularly pull the latest version of ``master`` and run long
tests. If your test takes a significant time to run (days), you should
however ask that a new dedicated machine is created to run your test.
Please ask on the ``#tests`` Slack channel of ``tezos-dev`` before
merging.

.. _performance_regression_test_fw:

Performance Regression Test framework: Time Series, Alerts and Graphs
---------------------------------------------------------------------

Long tests can use functions from the ``Long_test`` module
(:src:`tezt/lib_performance_regression/long_test.mli`) to send data points to InfluxDB,
which is a time-series database. A time-series database stores values
annotated with a timestamp. In the particular case of InfluxDB,
data points are composed of:

- a timestamp (usually the time the measure was taken);
- a measurement name (such as ``client load time``);
- optionally, some tags that can be used to refine the measurement
  name (such as a tag ``mode`` equal to ``client``, ``mockup`` or
  ``proxy``);
- fields, which are composed of field names and field values (such as
  a field ``duration`` equal to how long it took for the client to
  load).

The ``Long_test`` module allows you to send any data point using the
``add_data_point`` function. They will be sent at the end of the test
execution. You can query statistics about data points from previous
tests using function ``get_previous_stats`` to compare them with your
new data points and send an alert on Slack with function ``alert`` if
you detect that something is wrong. Note that function
``check_regression``, which is essentially a combination of
``get_previous_stats`` and ``alert``, can be used in most cases.  And
if all you want to do is measure the time it takes for a chunk of code
to run though, you can use the ``time`` function instead, which does
everything for you: measure time, send data points, compare them with
previous runs, and send an alert if the difference is too large.

The ``Long_test`` module also provides function
``update_grafana_dashboard`` called in
:src:`tezt/long_tests/main.ml` with a specification to create/overwrite a dashboard
in `Nomadic Labs' Grafana <https://grafana.nomadic-labs.cloud/d/longtezts>`_.
Default is named ``Long Tezts`` but you can add additional dashboards using the
``Long_test.update_grafana_dashboard`` function. To add a dashboard for your tests, define
it next to your test (in the same file), and declare it in the call to
``update_grafana_dashboard`` in :src:`tezt/long_tests/main.ml`.

As always in Tezt, the above functions try to provide flexibility.
The ``time`` function in particular is parameterized by settings like
the number of times the test should be repeated, how many previous
data points should be fetched from InfluxDB when comparing with new
measurements, how much of a difference to tolerate before alerting,
etc. ``time`` itself being a combination of other lower-level
functions that are also provided and which you can combine to fit your
needs. And of course you can contribute to improve them.

Automated long tezts logs are available in `Nomadic Labs' public S3 bucket browser
<https://logs.nomadic-labs.cloud/#PRT/master/>`_. In case of InfluxDB issues, you can inspect
metrics in the `InfluxDB dashboard <https://grafana.nomadic-labs.cloud/d/influxdb/>`_.
InfluxDB itself is private and direct access via CLI is restricted to administrators.

Example
-------

See :src:`tezt/long_tests/prt_client.ml`, which is a very simple test
that measures how long it takes for ``octez-client`` to load.  It uses
``Long_test.time_lwt`` to measure how long it takes for
``Client.version`` to run and to emit alerts if this time is
significantly higher than usual. It also defines a graph of this time.
This test and its graph are registered in
:src:`tezt/long_tests/main.ml`.

Providing Large Data
--------------------

Your test may require data that is too large to commit in
``tezos/tezos``. For example, a benchmark in which measurement is
dependent on some block's context would need to load the same data
directory on each execution.

There is an Amazon S3 bucket where you can
upload your data which will be made available for your test. Data
will be synchronized with the server your tests will be running on.

For security reasons, this storage has its access limited to
authorized people. If you want to upload data, please contact
Jérémie Goldberg (@jgonlabs) or anyone with admin access on
the Tezos AWS account to allow you to do so.

Please note that the S3 storage root folder is mounted in ``/s3data/``.
E.g. if your file is under ``/myfolder/myfile`` in the Amazon bucket, your
tests will find it under ``/s3data/myfolder/myfile``.

Testing Your Benchmarks Locally
-------------------------------

When developing a benchmark depending on the Performance Regression Test
framework, it can be useful to test it using development backends so that
your tests does not impact production ones.

The Performance Regression Test framework now contains a setup that can
automatically provision and configure InfluxDB and Grafana instances using
Docker Compose.

Provisioning InfluxDB and Grafana
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following steps assume that you already installed ``Docker`` as well
as ``docker-compose`` and correctly configured it. For more information
on this subject, please refer to:

- https://docs.docker.com/engine/install/#desktop
- https://docs.docker.com/compose/

From the root folder of ``tezos`` run the following commands from a terminal
to start the Docker containers in background:

.. code-block:: shell

    docker-compose -f tezt/lib_performance_regression/local-sandbox/docker-compose.yml up -d

After containers have been started, you can test that InfluxDB is properly started and
that the ``performance_regression`` database has been automatically created::

    curl --get http://localhost:8086/query\?pretty\=true --data-urlencode "q=show databases"

The command should display the following:

.. code-block:: json

    {
        "results": [
            {
                "statement_id": 0,
                "series": [
                    {
                        "name": "databases",
                        "columns": [
                            "name"
                        ],
                        "values": [
                            [
                                "performance_regression"
                            ],
                            [
                                "_internal"
                            ]
                        ]
                    }
                ]
            }
        ]
    }

Also, you should be able to connect to the Grafana web UI by connecting to
http://localhost:3000 in your browser. By going to the ``Datasources`` menu in the
webapp configuration (http://localhost:3000/datasources),
you can see that an InfluxDB datasource has been pre-configured
and is connected to the ``performance_regression`` database.

Note that as security does not really matter for tests, it has been disable for ease.
This is why you can connect to the Grafana web app with full privileges or send requests
to InfluxDB without having to authenticate.

To stop the container, simply run::

    docker-compose -f tezt/lib_performance_regression/local-sandbox/docker-compose.yml down

The created containers use persistent Docker volumes, so that data stored in the database
and created dashboards will be preserved between container runs. To permanently remove these
docker volumes, run the command ``docker volume rm local-sandbox_influxdb local-sandbox_grafana``.

Configuring and Running Tezt Long Tests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For more information about how to use the configuration file, please refer
to the :package-api:`Long test module API <tezt-tezos/Tezt_tezos_tezt_performance_regression/index.html>`.

A predefined configuration has already been shipped in :src:`tezt/lib_performance_regression/local-sandbox/tezt_config.json`.
It allows to use the InfluxDB and Grafana instances set up by the
Docker compose file presented in the previous section.

All content related to Grafana and InfluxDB has already been set and can be used as is.

Other aspects of the configuration (for example the ``test_data_path``) should be updated to match the needs
of your local machine.

To run Tezt long tests, run the following command::

    TEZT_CONFIG=tezt/lib_performance_regression/local-sandbox/tezt_config.json dune exec tezt/long_tests/main.exe
