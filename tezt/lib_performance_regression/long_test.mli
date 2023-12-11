(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Long test registration and helpers. *)

(** {2 Registering Tests} *)

(** Timeout specifications.

    Contrary to regular tests, long tests must have a timeout.
    Indeed, as long tests can run for days, there is no global timeout.
    So if a test got stuck and never finished, the whole process would
    get stuck forever.

    For instance, [Hours 10] means that your test should fail
    if it has been running for more than 10 hours.

    Timeouts are only a safety measure to prevent a test from blocking forever.
    They are not meant to be used as a way to check that a test runs in a given
    amount of time. They should thus significantly be over-estimated.
    A good rule of thumb is to:
    - multiply by 10 for tests that take minutes to run;
    - multiply by 5 for tests that take hours to run;
    - multiply by 2 for tests that take days to run.
    For instance, a test that usually takes between 10 to 20 hours to run
    could have a timeout of about 3 days.

    You should still try to avoid waiting for events that may not happen,
    and if you have to, try to also wait for another event that indicates
    that the first event will never happen. In other words, try to
    detect failures early. *)
type timeout = Seconds of int | Minutes of int | Hours of int | Days of int

(** Machines on which to run a given test.

    Tests that are very long (days) should run on their own executor,
    otherwise shorter tests will not be run very often.

    Values of type [executor] are just tags that are added to tests.
    Executors will execute tests which have the tag that correspond to themselves
    (e.g. the x86 executor 1 will execute tests tagged with [x86_executor1]).
    If you specify an empty list of executors, the test will not run on any executor. *)
type executor

(** AMD64 executor number 1. *)
val x86_executor1 : executor

(** AMD64 executor number 2. *)
val x86_executor2 : executor

(** Executor for the block-replay semantic regression tests *)
val block_replay_executor : executor

(** Wrapper over [Test.register] to register a performance regression test.

    Differences with [Test.register] are:
    - the [timeout] parameter;
    - if the test fails, an alert is emitted;
    - tag ["long"] is added;
    - [team] is added to tags and is used to decide where to send alerts;
    - [executors] specifies which machines shall run the test;
    - data points created with [add_data_point] and [measure] are pushed
      at the end of the test (whether it succeeds or not).

    Because an alert is emitted in case of failure, such tests are meant to be run
    not on the CI (which already sends e-mails in case a job fails) but on custom
    architectures. Although it can also make sense to emit alerts from the CI for
    a scheduled pipeline if nobody receives (or read) its alerts.

    Alerts are only sent if the relevant configuration is set (see {!alert}).
    [team] specifies which Slack webhook to use to send alerts.
    If [team] is not specified, or if [team] doesn't have a corresponding
    entry in the configuration file, the default Slack webhook is used.

    @raise Invalid_arg if [title] contains a newline character. *)
val register :
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?uses:Uses.t list ->
  ?uses_node:bool ->
  ?uses_client:bool ->
  ?uses_admin_client:bool ->
  ?team:string ->
  executors:executor list ->
  timeout:timeout ->
  (unit -> unit Lwt.t) ->
  unit

(** {2 Alerts} *)

(** category of an alert message *)
type category = string

(** Emit an alert.

    The alert is sent to a Slack channel.
    It is also written in logs with [Log.error], prefixed with ["Alert: "].

    Be careful with alert fatigue: only send alerts that really need to be acted on.
    Each test can only send a maximum of 2 alerts, and the total number of alerts
    for all tests cannot exceed 100. Alerts can be sent outside of tests, in which
    case only the global limit applies.

    If an alert for [category] was already sent less than [rate_limit_per_category]
    seconds ago, the alert will not be sent. See the Configuration section for
    documentation about [rate_limit_per_category].

    Default [category] is [""].
*)
val alert : ?category:category -> ('a, unit, string, unit) format4 -> 'a

(** Same as [alert], but also log an exception.

    The alert itself does not contain the exception, as it could contain
    sensitive data. *)
val alert_exn : exn -> ('a, unit, string, unit) format4 -> 'a

(** {2 Emitting And Retrieving Data Points} *)

(** Add a data point to be sent at the end of the current test.

    If data points cannot be pushed at the end of the test, an alert is emitted.

    @raise Invalid_arg if no test is currently running, or if it was not registered
    with [Long_test.register] or [Long_test.register_with_protocol]. *)
val add_data_point : InfluxDB.data_point -> unit

(** Wrapper over [InfluxDB.query].

    This wrapper takes care of:
    - providing the configuration;
    - emitting alerts in case something goes wrong;
    - adding a [test = "test title"] clause to the innermost SELECT of the query.

    This wrapper passes the query result to a function.
    The intended behavior of this function is to extract the results,
    e.g. check that they contain the expected number of series
    and the expected columns. If this function raises an exception,
    an alert is emitted and [query] returns [None].
    [query] also returns returns [None] if InfluxDB is not configured or if
    it failed to perform the query.

    If [log] is [true], log the results using [Log.debug]. Default is [false]. *)
val query :
  InfluxDB.select ->
  (InfluxDB.result_data_point list list -> 'a) ->
  'a option Lwt.t

module Stats : sig
  (** Statistics to retrieve with [get_previous_stats]. *)

  (** Specifications for which stats to retrieve. *)
  type _ t

  (** InfluxQL function MEAN. *)
  val mean : float t

  (** InfluxQL function MEDIAN. *)
  val median : float t

  (** InfluxQL function STDDEV (standard deviation). *)
  val stddev : float t

  (** Carthesian product of two stats. *)
  val _2 : 'a t -> 'b t -> ('a * 'b) t

  (** Carthesian product of three stats. *)
  val _3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
end

(** Retrieve statistics about measurements made in previous test runs.

    Usage: [get_previous_stats measurement field stats]

    Example: [get_previous_stats "rpc" "duration" Stats.(_3 mean median stddev)]

    This retrieves statistics specified by [stats] for the field named [field]
    of measurement named [measurement]. Only the [limit] most recent data points
    are considered in the statistics.

    [tags] is a list of [(tag, value)] pairs.
    If [tags] is specified, only data points that are tagged
    with [tag] equal to [value] for all [tags] are returned.

    This returns [None] if:
    - InfluxDB is not configured;
    - the data points cannot be retrieved (in which case an alert is also emitted);
    - less than [minimum_count] data points exist (default value is [3]).

    Otherwise it returns [Some (count, average)]
    where [count] is the number of data points which were used
    (with [minimum_count <= count <= limit]).

    @raise Invalid_arg if no test is currently running, or if it was not registered
    with [Long_test.register] or [Long_test.register_with_protocol]. *)
val get_previous_stats :
  ?limit:int ->
  ?minimum_count:int ->
  ?tags:(InfluxDB.tag * string) list ->
  InfluxDB.measurement ->
  InfluxDB.field ->
  'a Stats.t ->
  (int * 'a) option Lwt.t

(** Get the list of data points that were added for a given measurement but not sent yet.

    Those data points are pending: they have not been sent to InfluxDB yet.
    Those data points will be sent at the end of the current test.
    The order of the resulting list is unspecified.

    [tags] is a list of [(tag, value)] pairs.
    If [tags] is specified, only data points that are tagged
    with [tag] equal to [value] for all [tags] are returned.

    @raise Invalid_arg if no test is currently running, or if it was not registered
    with [Long_test.register] or [Long_test.register_with_protocol]. *)
val get_pending_data_points :
  ?tags:(InfluxDB.tag * string) list ->
  InfluxDB.measurement ->
  InfluxDB.data_point list

(** {2 Regression Testing} *)

(** Which statistics to compare with previous data points. *)
type check = Mean | Median

(** Compare data points from the current test with previous tests.

    Usage: [check_regression measurement field]

    This compare two sets of data points:
    - the set of current data points;
    - the set of previous data points.

    The set of current data points is [data_points], which defaults to the
    current pending data points. Only data points for [measurement] are used,
    and only if they have a field [field] with a float value.

    [tags] is a list of [(tag, value)] pairs.
    If [tags] is specified, only data points that are tagged
    with [tag] equal to [value] for all [tags] are used.

    If the set of data points to use is empty, the function
    returns immediately without doing anything.

    The set of previous data points is obtained by querying InfluxDB.
    If InfluxDB is not configured, or if less than [minimum_previous_count]
    previous data points exist, the function returns without doing anything else.
    If more than [minimum_previous_count] previous data points are available,
    the last [previous_count] points are used.

    The mean or median (depending on [check]) of those two sets is compared.
    If the value for the current data points is more than the value for
    the previous data points multiplied by [1. +. margin], an alert is emitted.

    If [stddev] is [true], this also logs the standard deviation of the previous
    data points.

    Default values:
    - [previous_count = 10]
    - [minimum_previous_count = 3]
    - [margin = 0.2]
    - [check = Mean]
    - [stddev = false]

    @raise Invalid_arg if no test is currently running, or if it was not registered
    with [Long_test.register] or [Long_test.register_with_protocol]. *)
val check_regression :
  ?previous_count:int ->
  ?minimum_previous_count:int ->
  ?margin:float ->
  ?check:check ->
  ?stddev:bool ->
  ?data_points:InfluxDB.data_point list ->
  ?tags:(InfluxDB.tag * string) list ->
  InfluxDB.measurement ->
  InfluxDB.field ->
  unit Lwt.t

(** Do something and measure how long it takes.

    Usage: [time measurement f]

    This executes [f], measures the [time] it takes to run, adds
    a data point for [measurement] with field ["duration"] equal to [time],
    and uses [check_regression] to compare with previous values.
    If [f] raises an exception, data points are not pushed and the
    exception is propagated.

    If [repeat] is specified, call [f] [repeat] times to obtain as many
    data points.

    See {!check_regression} for documentation about other optional parameters.

    @raise Invalid_arg if no test is currently running, or if it was not registered
    with [Long_test.register] or [Long_test.register_with_protocol]. *)
val time :
  ?previous_count:int ->
  ?minimum_previous_count:int ->
  ?margin:float ->
  ?check:check ->
  ?stddev:bool ->
  ?repeat:int ->
  ?tags:(string * string) list ->
  InfluxDB.measurement ->
  (unit -> unit) ->
  unit Lwt.t

(** Same as {!time}, but instead of measuring the duration taken
    by [f ()] execution, delegates this responsability to [f] itself.

    In this case, [f] represents a thunk that executes an expression
    or a program and evaluates in the duration taken by its execution.*)
val measure_and_check_regression :
  ?previous_count:int ->
  ?minimum_previous_count:int ->
  ?margin:float ->
  ?check:check ->
  ?stddev:bool ->
  ?repeat:int ->
  ?tags:(string * string) list ->
  InfluxDB.measurement ->
  (unit -> float) ->
  unit Lwt.t

(** Same as {!time}, but for functions that return promises.

    Note that other concurrent promises may slow down the measured function
    and result in inaccurate measurements. *)
val time_lwt :
  ?previous_count:int ->
  ?minimum_previous_count:int ->
  ?margin:float ->
  ?check:check ->
  ?stddev:bool ->
  ?repeat:int ->
  ?tags:(string * string) list ->
  InfluxDB.measurement ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t

(** Same as {!time_lwt}, but instead of measuring the duration taken
    by [f ()] execution, delegates to [f] itself.

    In this case, [f] represents a thunk that executes an expression
    or a program and evaluates in the duration taken by its execution.*)
val measure_and_check_regression_lwt :
  ?previous_count:int ->
  ?minimum_previous_count:int ->
  ?margin:float ->
  ?check:check ->
  ?stddev:bool ->
  ?repeat:int ->
  ?tags:(string * string) list ->
  InfluxDB.measurement ->
  (unit -> float Lwt.t) ->
  unit Lwt.t

(** {2 Graphs} *)

(** Wrapper over [Grafana.update_dashboard].

    This wrapper takes care of:
    - providing the configuration;
    - prepending the measurement prefix configured for InfluxDB;
    - running [Lwt_main.run] for you.

    Does nothing if Grafana or InfluxDB are not configured.

    @raise Invalid_arg if the dashboard UID is invalid. *)
val update_grafana_dashboard : Grafana.dashboard -> unit

(** {2 Configuration} *)

(** This module should be initialized with [init] to read the configuration file.
    If not, default values are used.

    If you don't need to send alerts, send data points, and retrieve
    data points, you do not need to write a configuration file.
    If you do intend to run the tests yourself, you should store data points
    in your own database though. Indeed, time measurements only make sense
    when they are compared with other measurements that have been run on
    the same hardware.

    This module sends data points to an InfluxDB database, queries data points
    from this database, and sends alerts to a Slack webhook. To configure this
    integration, write a configuration file in one of the following locations:

    - $TEZT_CONFIG
    - ./tezt_config.json
    - $HOME/.tezt_config.json

    More precisely, if environment variable TEZT_CONFIG is set to a non-empty
    value, the location is read from the file located at this value.
    If this variable does not denote a valid configuration file, the program exits.
    If this variable is empty or not set, other configuration files are tried:
    the first one which exists is read. If the first one that exists is not
    a valid configuration file, the program exits. If no configuration file exists,
    default configuration values are used.

    The contents of this file should look like:

{v
    {
        "alerts": {
            "slack_webhook_urls": {
                "default": "https://hooks.slack.com/services/XXX/XXX/XXX",
                "p2p": "https://hooks.slack.com/services/XXX/XXX/XXX",
                "shell": "https://hooks.slack.com/services/XXX/XXX/XXX"
            },
            "max_total": 100,
            "max_by_test": 2,
            "gitlab_project_url": "https://gitlab.com/org/repo",
            "timeout": 20,
            "rate_limit_per_category": 84600,
            "last_alerts_filename", "last_alerts.json",
            "max_alert_size": 1000,
            "max_alert_lines": 20
        },
        "influxdb": {
            "url": "https://localhost:8086",
            "database": "db",
            "credentials": {
                "username": "user",
                "password": "password"
            },
            "measurement_prefix": "tezt_",
            "tags": { "commit_hash": "12345678" },
            "timeout": 20
        },
        "grafana": {
            "url": "https://localhost/api",
            "api_token": "123456789",
            "data_source": "InfluxDB",
            "timeout": 20
        },
        "test_data_path" : "/path/to/the/test_data_path"
    }
v}

    where:

    - [alerts] configures how to send alerts
      (optional, alerts are not sent if not specified);
      - [slack_webhook_urls] maps team names to Slack webhook URLs
        (it must contain at least a team named ["default"],
        see the [?team] argument of {!register});
      - [max_total] is the maximum number of alerts that the process may send,
        after which alerts are not sent to Slack
        (optional, default value is 100);
      - [max_by_test] is the maximum number of alerts that a given test may send,
        after which alerts are not sent to Slack
        (optional, default value is 2);
      - [gitlab_project_url], is used to add, in Slack alerts,
        a link to create a GitLab issue to fix the alert
        (optional, links are not added if not specified);
      - [timeout] is how long, in seconds, to wait when sending alerts before
        giving up (optional, default value is 20);
      - [rate_limit_per_category] is the minimum delay between two alerts
        for the same category, in seconds (optional, default value is one day);
      - [last_alerts_filename] is the name of the file where to store the
        last time an alert was sent for each category
        (optional, default value is ["last_alerts.json"]);
      - [max_alert_size] is the maximum length of alert messages, in bytes:
        messages which are longer are truncated to this size and an ellipsis
        (["[...]"]) is appended (optional, default value is 1000);
      - [max_alert_lines] is the maximum number of lines of alert messages:
        messages with more lines are truncated and an ellipsis is appended
        (optional, default value is 20);

    - [influxdb] configures how to send and query data points
      (optional, data points are not sent nor queried if not specified);
      - [url] is the base URL of the InfuxDB API;
      - [database] is the name of the InfluxDB database;
      - [credentials] contains the credentials needed to authenticate to the database.
        (optional, can be omitted for test databases that don't need authentication);
        - [username] is the username used to log in the InfluxDB database;
        - [password] is the password used to log in the InfluxDB database;
      - [measurement_prefix] is a prefix which is prepended to all measurement names
        to prevent accidentally polluting other data of the database - it is
        automatically prepended both when sending data points and in SELECT queries;
        (optional, default value is ["tezt_"]);
      - [tags] is a list of tags to add to all data points that are sent,
        such as the commit hash or the current version number
        (optional, default is no tags);
      - [timeout] is how long, in seconds, to wait when sending
        and querying data points before giving up
        (optional, default value is 20);

    - [grafana] configures how to update graph dashboards
      (optional, dashboards are not updated if not specified);
      - [url] is the base URL of the Grafana API (ending with [/api]);
      - [api_token] is an API token the Grafana admin can create in the Grafana interface
        (optional, If the grafana instance is run in insecure mode (no authentication required),
        it can be ommited. This is particularly useful to connect to a Grafana test instance);
      - [data_source] is the name of the InfluxDB data source configured by the Grafana admin;
      - [timeout] is how long, in seconds, to wait when updating dashboards before giving up
        (optional, default value is 20).

    - [test_data_path] configures the folder in which large data are stored
      (optional, default value is "/s3data" which is the folder used by
      the default executor).

    Note that for Grafana dashboards to be updated, InfluxDB also needs to be configured,
    because measurements in Grafana queries are prefixed with the measurement prefix
    configured for InfluxDB. *)

(** Read configuration file.

    Please, be sure this function is called at first before using
    any function from [Long_test] to avoid undesired behaviour regarding
    the loading of the configuration. *)
val init : unit -> unit

(** Get the [test_data_path] field from the configuration. *)
val test_data_path : unit -> string

(** {2 Internal Functions for Debugging} *)

(** Same as {!query}, but without modifying the query.

    Warning: contrary to {!query} this doesn't automatically add the
    [test = "test title"] clause, so you may get results from other tests. *)
val unsafe_query :
  InfluxDB.select ->
  (InfluxDB.result_data_point list list -> 'a) ->
  'a option Lwt.t

(** Perform a query with {!unsafe_query} and log the query and its result. *)
val log_unsafe_query : InfluxDB.select -> unit Lwt.t
