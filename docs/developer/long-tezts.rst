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
  wich provides these features:

  - Persist measurement samples (in an InfluxDB) such as how long it takes
    to do something. These samples will be used to prevent regressions of
    performance.
  - Provide easy access to a Grafana instance that can be updated to
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
``update_grafana_dashboard`` which is called in
:src:`tezt/long_tests/main.ml` with a specification of graphs to
display in Grafana at https://graf.nomadic-labs.com/?orgId=1 (in the
"Long Tezts" dashboard by default but you can create your own
dashboards). To add graphs for your tests, define them next to your
test in the same file as your test, and declare them in
:src:`tezt/long_tests/main.ml` in the call to
``update_grafana_dashboard``.

As always in Tezt, the above functions try to provide flexibility.
The ``time`` function in particular is parameterized by settings like
the number of times the test should be repeated, how many previous
data points should be fetched from InfluxDB when comparing with new
measurements, how much of a difference to tolerate before alerting,
etc. ``time`` itself being a combination of other lower-level
functions that are also provided and which you can combine to fit your
needs. And of course you can contribute to improve them.

Example
-------

See :src:`tezt/long_tests/prt_client.ml`, which is a very simple test
that measures how long it takes for ``tezos-client`` to load.  It uses
``Long_test.time_lwt`` to measure how long it takes for
``Client.version`` to run and to emit alerts if this time is
significantly higher than usual. It also defines a graph of this time.
This test and its graph are registered in
:src:`tezt/long_tests/main.ml`.

One-Shot Tests
--------------

You may be interested in running some long tests using this framework
on your own branch instead of ``master``.

See documentation in the README `here <https://gitlab.com/nomadic-labs/iac/terraform/tf-aws-performance-regression-oneshot-instance>`_

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
framework, it can be useful to test it using a development database so
that your tests does not impact the production database.

This section describes how to easily set up an InfluxDB database so that the
framework can operate with it.

The following steps assume that you already installed Docker and correctly
configured it. For more information on this subject, please refer to:
https://docs.docker.com/engine/install/#desktop

We will first install and bootstrap an InfluxDB database. This can be done
using the official Docker image: https://hub.docker.com/_/influxdb

From a terminal, run the following commands::

    mkdir $HOME/influxdb

    docker run -d -p 8086:8086 \
      -v $HOME/influxdb/data:/var/lib/influxdb2 \
      -v $HOME/influxdb/config:/etc/influxdb2 \
      -e DOCKER_INFLUXDB_INIT_MODE=setup \
      -e DOCKER_INFLUXDB_INIT_USERNAME=<user> \
      -e DOCKER_INFLUXDB_INIT_PASSWORD=<password> \
      -e DOCKER_INFLUXDB_INIT_ORG=my-org \
      -e DOCKER_INFLUXDB_INIT_BUCKET=my-bucket \
      influxdb:1.8

This will download an image of the version 1.8 of InfluxDB and start a
container with it. Version 1.8 is mandatory as the framework does not
support newer versions for now.

Of course, ``<user>`` and ``<password>`` should be replaced by values of your choice.

When the container is bootstrapped, you need to create the database
that will be used by the framework.

Run the following command to connect to the InfluxDB server and create
a database named ``prt``::

    curl -X POST http://localhost:8086/query\?pretty\=true \
    --user "<user>:<password>" \
    --data-urlencode "q=create database prt"

After the database is created, you can use the following JSON
configuration to set up the framework with your local database:

``tezt_config.json``:

.. code-block:: json

    {
      "influxdb": {
        "url": "http://localhost:8086",
        "database": "prt",
        "username": "<user>",
        "password": "<password>"
      }
    }

For more information about the configuration file, please refer
to the `Long test module API <https://gitlab.com/tezos/tezos/-/blob/master/tezt/long_tests/long_test.mli>`__.



