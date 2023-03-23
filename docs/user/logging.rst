Logging
=======

Logging features in Octez allow to monitor the execution of Octez binaries, informing in real
time about events of interest, such as errors, completion of certain steps,
etc. This is why various software components emit *events* throughout the
codebase (see :doc:`../developer/event_logging_framework`), the logging
framework dispatches them to an arbitrary number of (active) *sinks* which can
filter print, store, or otherwise handle events.
The logging framework can be configured with environment variables, which specify how events are mapped to sinks.
Some Octez binaries provide additional configurations means.

Events
------

Events have:

- a *name*
- a *section*; a hierarchical event classification mechanism, i.e. a path-like
  list of strings, sometimes the name is appended to make a fully-qualified
  name,
- a *level* quantifying the relative importance of the event (Debug, Info,
  Notice, Warning, Error, and Fatal — inspired from the
  `Syslog standard severity levels <https://en.wikipedia.org/wiki/Syslog#Severity_level>`_),
- and, as *contents*, arbitrary structured data. They can be serialized to any
  format supported by the data-encoding library (e.g. JSON) and pretty-printed
  for human readers.

Events are by default “lazy”: if no active sink accepts their section and level,
they are not emitted at all. This means that outputting *more* events
implies more CPU and memory usage.

*“Legacy events”* are the events that use the old & deprecated logging API;
their contents all have the same structure: they are just human-readable
text. The API makes that these events are evaluated even if they are not
consumed by a sink; i.e. they are not lazily evaluated.  The codebase is in the
process of getting rid of them.

Sink Configuration
-------------------

The logging framework refers to sinks with URIs (a.k.a.  structured strings of
the form ``schema://path?query``): the schema of the URI is used to find the
appropriate sink-implementation, the rest of the URI is used to configure the
particular instance. There can be as many instances of a given implementation as
needed, except for the (legacy) *Lwt-Log Sink* which should be activated at most
once.

(Legacy) Lwt-Log Sink
~~~~~~~~~~~~~~~~~~~~~

This sink is the one currently activated by default:

-  Schema: ``lwt-log``
-  URI-Configuration: **none**; (for now) can only use the legacy
   ``TEZOS_LOG`` environment variable or the specific section in the
   node’s configuration file (see below).

This sink can only output pretty-printed versions of the events.

File-Descriptor Sinks
~~~~~~~~~~~~~~~~~~~~~

This is configurable and duplicatable replacement for the lwt-log-sink;
it allows to output events to regular Unix file-descriptors. It is
actually a *family* of three URI-schemes:

-  ``file-descriptor-path://`` to output to a file,
-  ``file-descriptor-stdout://`` to output to ``stdout`` / ``1``, or
-  ``file-descriptor-stderr://`` to output to ``stderr`` / ``2``.

*Note:* ``-stdout`` and ``-stderr`` schemes are there for convenience
and for making the API consistent; most Unix-ish systems nowadays have
``/dev/std{out,err}`` and ``/dev/fd/[0-9]+`` special files which can be
used with ``file-descriptor-path`` (with some precautions).

The path of the URI is used by ``file-descriptor-path`` to choose the
path to write to and ignored for the other two.

The query of the URI is used to further configure the sink instance.

Common options:

-  ``level-at-least=<loglevel>`` the minimal log-level that the sink
   will output.
-  ``section-prefix`` can be given many times and defines a list of
   pairs ``<section-prefix>:<level-threshold>`` which can be used to
   setup more precise filters. ``level-at-least=info`` can be understood
   as ``section-prefix=:info``, the empty section prefix matches all
   sections. To exclude a specific section use the ``none`` filter, for
   example ``section-prefix=p2p:none``. To define a filter only for
   one specific section ``section-prefix=p2p:debug&section-prefix=:none``
-  ``format=<value>`` the output format used; acceptable values are:

   -  ``one-per-line`` (the default): output JSON objects, one per line,
   -  ``netstring``: use the Netstring format
      (cf. `Wikipedia <https://en.wikipedia.org/wiki/Netstring>`__) to
      separate JSON records,
   -  ``pp`` to output the events pretty-printed, one per line, using a
      format compatible with
      `RFC-5424 <https://www.rfc-editor.org/rfc/rfc5424#section-6>`__ (or
      Syslog).

Options available only for the ``file-descriptor-path://`` case:

-  ``with-pid=<bool>`` when ``true`` adds the current process-id to the
   file path provided (for instance, useful for the node when not
   running in ``--singleprocess`` mode).
-  ``fresh=<bool>`` when ``true`` smashes the content of the file if it
   already exists instead of appending to it.
-  ``chmod=<int>`` sets the access-rights of the file at creation time
   (default is ``0o600``, provided
   `Umask <https://en.wikipedia.org/wiki/Umask>`__ allows it).
- ``daily-logs=<int>`` sets up a rotation for log files, creating a file for
  each day. The parameter is the number of days these logs should be kept, and
  hence the number of files. The day of the year with format ["yyyymmdd"] is
  added as a suffix to the filename, prefixed by a dash. We recomend not to put
  your tezos logs in ``/var/log`` if you use this option, as you system would
  use ``logrotate`` automatically.
- ``create-dirs=<bool>`` when ``true`` allows to create the directory where
  the log files are stored and all its parents recursively if they don't
  exist.

Examples:

-  ``file-descriptor-path:///the/path/to/write.log?format=one-per-line&level-at-least=notice&with-pid=true&chmod=0o640``
   → Executables will write all log events of level at least ``Notice``
   to a file ``/the/path/to/write-XXXX.log`` where ``XXXX`` is the PID,
   the file will be also readable by the user’s group (``0o640``).
-  ``file-descriptor-stderr://?format=netstring`` → Executables will
   write to ``stderr`` JSON blobs *“packetized” as* Netstrings.
-  ``file-descriptor-path:///dev/fd/4?section-prefix=rpc:debug`` →
   Executables will write to the file-descriptor ``4`` likely opened by
   a parent monitoring process. The reader will only receive the logs
   from the section ``rpc`` (but all of them including ``Debug``).
-  ``file-descriptor-path:///the/path/to/write.log?section-prefix=rpc:debug&section-prefix=validator:debug&section-prefix=:none"``
   → Write only sections validator and rpc at debug level but exclude all
   other sections from the stream.
- ``"file-descriptor-path:///tmp/node-logs/node.log?daily-logs=5&create-dirs=true&section-prefix=:info"``
   sets up daily log files with a history of up to 5 days and verbosity level
   ``info`` for all logs. Files will be named ``node-19700101.log`` in an
   example of a file produced in 1970, January, the 1st. The log directory
   ``node-logs`` will be automatically created if it doesn't exist.

The format of the events is (usually minified):

.. code:: javascript

   {"fd-sink-item.v0":
     {"hostname": <host-name>,
      "time_stamp": <float-seconds-since-epoch>,
      "section":[ <list-of-strings> ],
      "event":
        <event-specific-json> } }


Additionally, the ``"hostname"`` field can be customized with environment
variable ``TEZOS_EVENT_HOSTNAME``; Its default value is the hostname of the
device the node is running on.

To store rotated logs, there is the ``daily-logs`` option to create logs files on
a daily basis. However, it is also possible to use ``logrotate`` by putting the
log file in ``/var/log/tezos/sink.log``, for exemple. The following
configuration can then be put in ``/etc/logrotate.d/tezos/sink.log``:

.. code::

  /var/log/tezos/sink.log {
          daily
          copytruncate
          rotate 4
          compress
  }

File-Tree Sink
~~~~~~~~~~~~~~

This is a sink that dumps events as JSON files (same format as above)
in a directory structure guided by the section of the events. It can be
useful for testing the logging framework itself, or for off-line
post-mortem analysis for instance.

The URI scheme is ``unix-files``, the path is the top-level directory in
which the JSON files will be written.

The query of the URI allows one to filter the events early on.

-  ``level-at-least=<loglevel>`` the minimal log-level that the sink
   will output.
-  ``name-matches=<regexps>`` comma-separated-list of POSIX regular
   expressions on the name of the events.
-  ``name=<names>`` comma-separated-list of event names matched
   *exactly*.
-  ``section=<sections>`` comma-separated-list of event sections matched
   *exactly*.
-  ``no-section=<bool>`` when true only catch the events that have an
   empty section.

Example: ``unix-files:///the/path/to/write?level-at-least=info`` (the
path should be inexistent or already a directory).

The directory structure is as follows:
``<section-dirname>/<event-name>/<YYYYMMDD>/<HHMMSS-MMMMMM>/<YYYYMMDD-HHMMSS-MMMMMM-xxxx.json>``
where ``<section-dirname>`` is either ``no-section`` or
``section-<section-name>``.

Global Defaults
---------------

By default, the Octez binaries generate **user logs** as follows:

- ``lwt-log://`` sinks are activated by default and configured to
  output events of level at least ``Notice``. Their goal is the stdout logging.

The node additionally generates by default more detailed **internal logs** as follows:

- A file-descriptor-sink is activated to store logs from last
  seven days with an ``Info`` level. As defined in the `XDG Base Directory Specification <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>`_,
  logs are written in a path relative to a configurable state directory
  ``$XDG_STATE_HOME``, or if this variable is undefined, to
  ``$HOME/.local/state``. In the case of the Octez node, the precise path is
  ``$XDG_STATE_HOME/octez/node/logs/``, and defaults to
  ``$HOME/.local/state/octez/node/logs/``.




JSON Configuration Format
-------------------------

A configuration JSON blob, is an object with one field ``"active_sinks"``
which contains a list of URIs:

.. code:: javascript

   {
     "active_sinks": [ <list-of-sink-URIs> ]
   }

The URIs are discriminated among the sink implementations above using
their schemes and activated.

It is used in various places: node configuration file,
logging-configuration RPC, etc.

Environment Variables
---------------------

The logging framework can be configured with environment variables
before starting an Octez executable (e.g., the node). Those variables work on all the code using the
``tezos-stdlib-unix`` library as long as ``Internal_event_unix.init`` is
called; this should include *all* the regular ``octez-*`` binaries.

-  ``TEZOS_EVENTS_CONFIG`` must be a whitespace-separated list of URIs:

   -  URIs that have a schema are activated.
   -  URIs without a schema, i.e. simple paths, are understood as paths
      to configuration JSON files (format above) to load (which
      themselves activate sinks).

-  ``TEZOS_LOG`` and ``LWT_LOG`` (with lower priority) contain “rules”
   to configure the ``lwt-log://`` sink. The rules are expressed with a
   DSL documented at
   `Lwt_log_core <https://ocsigen.org/lwt/3.2.1/api/Lwt_log_core>`__:

   -  rules are separated by semi-colons ``;``,
   -  each rule has the form ``pattern -> level``,
   -  a pattern is a minimalist glob-expression on the ``section.name`` of
      the event, e.g. ``rpc*`` for all events whose section.name starts
      with ``rpc``,
   -  rules are ordered, i.e., the first pattern that matches, from left to
      right, fires the corresponding rule.

-  ``TEZOS_EVENT_HOSTNAME`` is used by the file-descriptor-sink to tweak the JSON
   output (see above).

- ``XDG_STATE_HOME``, in the `XDG Base Directory Specification
  <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>`_,
  defines the base directory relative to which user-specific state files should
  be stored. In Octez, it is used by default to store internal logs. If
  $XDG_STATE_HOME is either not set or empty, ``$HOME/.local/state`` is used.

As the Irmin context backend uses an internal and specific logging
system, it is possible to configure it through the ``TEZOS_CONTEXT``
environment variable, see :ref:`context_component`.


.. _configure_node_logging:

Node-Specific Configuration
---------------------------

The node supports some additional means to configure logging, besides environment variables.

Configuration File
~~~~~~~~~~~~~~~~~~

See ``octez-node config --help`` for the full schema of the node’s JSON
configuration file.

In particular the fields:

-  ``"internal-events"`` contains a configuration of the sinks (format
   above).
-  ``"log"`` is an object which defines the configuration of the
   ``lwt-log://`` sink; one can redirect the output to a file, set the
   rules, and change the formatting template.

Command Line Options
~~~~~~~~~~~~~~~~~~~~

See ``octez-node run --help``, the ``lwt-log://`` sink configuration can
be also changed with 2 options:

-  ``-v`` / ``-vv``: set the global log level to ``Info`` or ``Debug``
   respectively.
-  ``--log-output``: set the output file.

RPC ``/config/logging``
~~~~~~~~~~~~~~~~~~~~~~~

The node exposes an administrative ``PUT`` endpoint:
``/config/logging``.

The input schema is the JSON configuration of the sinks. It
deactivates all current sinks and activates the ones provided **except**
the ``lwt-log://`` sink that is left untouched.

Example: (assuming the ``lwt-log://`` is active not to miss other
events) this call adds a sink to suddenly start pretty-printing all
``rpc`` events to a ``/tmp/rpclogs`` file:

::

   octez-client rpc put /config/logging with \
     '{ "active_sinks": [ "file-descriptor-path:///tmp/rpclogs?section-prefix=rpc:debug&format=pp&fresh=true" ] }'

Client and baker configuration
------------------------------

For now, ``octez-client``, ``octez-{baker,accuser}-*``, etc.
can only be configured using the environment variables.

There is one common option ``--log-requests`` which can be used to trace
all the interactions with the node (but it does *not* use the logging
framework).

Processing Structured Events
----------------------------

This is work-in-progress, see:

-  ``octez-admin-client show event-logging`` outputs the configuration
   currently understood by ``octez-admin-client`` (hence through the
   ``TEZOS_EVENTS_CONFIG`` variable) and lists all the events it knows
   about.
-  ``octez-admin-client output schema of <Event-Name> to <File-path>``
   get the JSON-Schema for an event.

Example:
``octez-admin-client output schema of block-seen-alpha to block-seen-alpha.json``
