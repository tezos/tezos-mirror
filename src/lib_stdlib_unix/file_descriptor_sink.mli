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

    It is registered with the URI scheme ["file-descriptor-path"] to
    output to a file or
    ["file-descriptor-stdout"]/["file-descriptor-stderr"] for [stdout]
    and [stderr] respectively.

    Available options are

    - ["level-at-least"] the minimal log-level that the sink will
      output (see {!Tezos_event_logging.Internal_event.level}).
    - ["section-prefix"] can be given many times and defines a list of pairs
      ["<section-prefix>:<level-threshold>"] which can be used to setup more
      precise filters. ["level-at-least=info"] is understood as
      ["section-prefix=:info"] (the empty section prefix matches all
      sections).
    - ["format"] the output format used;
      acceptable values are ["one-per-line"] (the default),
      ["netstring"] (see {{:https://en.wikipedia.org/wiki/Netstring}The
      Netstring format}) (both to separate JSON records), {i or} ["pp"] to
      output the events pretty-printed as text using the [syslog] format.

    Options available only for ["file-descriptor-path://"]:

    - ["with-pid=true"] adds the current process id to the file path provided.
    - ["fresh=true"] smashes the content of the file if it already
      exists instead of appending to it.
    - ["chmod=<INT>"] sets the access-rights of the file at creation
      time (default is [0o600], provided
      {{:https://en.wikipedia.org/wiki/Umask}[umask]} allows it).

    Examples:

    - ["export TEZOS_EVENTS_CONFIG=file-descriptor-path:///the/path/to/write.log?format=one-per-line&level-at-least=notice&with-pid=true&chmod=0o640"]:
      Executables will write all log events of level at least [Notice]
      to a file ["/the/path/to/write-XXXX.log"] where ["XXXX"] is the
      process ID, the file will be also readable by the user's group ([0o640]).
    - ["export TEZOS_EVENTS_CONFIG=file-descriptor-stderr://?format=netstring"]
      Executables will write to [stderr].
    - ["export TEZOS_EVENTS_CONFIG=file-descriptor-path:///dev/fd/4?format=netstring"]
      Executables will write to the [4] file-descriptor likely opened
      by a parent monitoring process (non-standard feature available
      on mainstream UNIX hosts, e.g. Linux and MacOSX).

    - ["export TEZOS_EVENT_HOSTNAME=hostname"]
      The [hostname] will be used in the JSON representation of the event.
      By default, it is the hostname given by [Unix.gethostname ()].

*)

type t

module Sink_implementation_path : Internal_event.SINK with type t = t

module Sink_implementation_stdout : Internal_event.SINK with type t = t

module Sink_implementation_stderr : Internal_event.SINK with type t = t
