(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** An implementation of {!Tezos_base.Internal_event.SINK} which
    writes the events as JSON or pretty printed into a single file-descriptor.

    An URI scheme is used to describe such file descriptors:
    - ["file-descriptor-path"] outputs to the file at the URI path
    - ["file-descriptor-stdout"] outputs to [stdout]
    - ["file-descriptor-stderr"] outputs to [stderr]
    - ["file-descriptor-syslog"] ouputs to a [syslog] facility

    Available options are

    - ["level-at-least"] the minimal log-level that the sink will
      output (see {!Tezos_event_logging.Internal_event.level}).
    - ["section-prefix"] can be given many times and defines a list of pairs
      ["<section-prefix>:<level-threshold>"] which can be used to setup more
      precise filters. ["level-at-least=info"] is understood as
      ["section-prefix=:info"] (the empty section prefix matches all
      sections). To exclude completely a section from the log stream that the
      sink will output, you can use the special level-threshold "none".

    - ["format"] the output format used.Note that syslog output will ignore this
      option and use the syslog formatting. Possible values are
      - ["one-per-line"] (the default),
      - ["netstring"] (see {{:https://en.wikipedia.org/wiki/Netstring}The
        Netstring format}) (both to separate JSON records),
      - ["pp-rfc5424"] to output the events pretty-printed as text using
        {{:https://www.rfc-editor.org/rfc/rfc5424}RFC 5424}
      - ["pp-short"] to output the events pretty-printed in a shorter and more
        user-friendly fashion.

    Options available only for ["file-descriptor-path://"]:

    - ["with-pid=true"] adds the current process id to the file path provided.
    - ["fresh=true"] smashes the content of the file if it already
      exists instead of appending to it.
    - ["chmod=<INT>"] sets the access-rights of the file at creation
      time (default is [0o600], provided
      {{:https://en.wikipedia.org/wiki/Umask}[umask]} allows it).
    - ["daily-logs=<INT>"] sets up a rotation for log files, keeping only the
      last N days where N is the given parameter. It creates a file for each
      day and and adds the day of the year with format ["yyyymmdd"] to the
      path provided.
    - ["create-dirs=true"] allows to create the directory where the log
      files are stored and all its parents recursively if they don't exist.

    Option available only for ["file-descriptor-syslog://"]:

    - ["facility=<facility>"] is the targeted syslog output. [User] is the
    default value if no value is provided. See
    {{:https://www.rfc-editor.org/rfc/rfc3164}RFC 3164} for more information.
    The possible values are defined in module [Syslog].

    Examples:

    - [export TEZOS_EVENTS_CONFIG="file-descriptor-path:///the/path/to/write.log?format=one-per-line&section-prefix=p2p.maintenance:none&with-pid=true&chmod=0o640"]:
      By default all executables will write all log events of level at least [Info]
      to a file ["/the/path/to/write-XXXX.log"] where ["XXXX"] is the
      process ID, the file will be also readable by the user's group ([0o640]).
      The maintenance module will be excluded from the stream.
    - [export TEZOS_EVENTS_CONFIG="file-descriptor-path:///the/path/to/write.log?section-prefix=rpc:debug&section-prefix=validator:debug&section-prefix=:none"]:
      Write only sections validator and rpc at debug level but exclude all other
      sections from the stream.
    - [export TEZOS_EVENTS_CONFIG="file-descriptor-path:///the/path/to/write.log?format=one-per-line&level-at-least=notice&with-pid=true&chmod=0o640"]:
      Executables will write all log events of level at least [Notice]
      to a file ["/the/path/to/write-XXXX.log"] where ["XXXX"] is the
      process ID, the file will be also readable by the user's group ([0o640]).
    - ["export TEZOS_EVENTS_CONFIG=file-descriptor-stderr://?format=netstring"]
      Executables will write to [stderr].
    - [export TEZOS_EVENTS_CONFIG="file-descriptor-path:///dev/fd/4?format=netstring"]
      Executables will write to the [4] file-descriptor likely opened
      by a parent monitoring process (non-standard feature available
      on mainstream UNIX hosts, e.g. Linux and MacOSX).

    - [export TEZOS_EVENT_HOSTNAME="hostname"]
      The [hostname] will be used in the JSON representation of the event.
      By default, it is the hostname given by [Unix.gethostname ()].
    - ["export TEZOS_EVENTS_CONFIG=file-descriptor-path:///tmp/node.log?daily-logs=5&section-prefix=:info"]
      sets up log files with a rotation of 5 days and verbosity level [info] for
      all logs. Files will be named [node-19700101.log] in an example of a file
      produced in 1970, January, the 1st.

*)

type t

module Sink_implementation_path : Internal_event.SINK with type t = t

module Sink_implementation_stdout : Internal_event.SINK with type t = t

module Sink_implementation_stderr : Internal_event.SINK with type t = t
