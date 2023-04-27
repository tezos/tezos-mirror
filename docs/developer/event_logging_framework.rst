Using The Event Logging Framework
---------------------------------

Developers of most modules should consider generating useful information
for the :doc:`Logging subsystem <../user/logging>`.

In the Octez code base, logging is instrumented using an asynchronous event
system, where log events are emitted in various part of the code and consumed by
the so called sinks.  This is done by using the :opam:`tezos-event-logging` library.

We use two levels of abstraction to define these events. Heavy events are
defined using the most generic API and have the full expressive power and
structure. Simple events are record-like structures which
are constructed on top of the generic event api, and are meant to be less
verbose and more developer friendly.


Adding Events
~~~~~~~~~~~~~

Simple Events
^^^^^^^^^^^^^

Simple events are record-like structures which are constructed directly
(i.e. there is no high-level OCaml type, and parsing events is not accessible
programmatically). The API is less generic than the “Heavy” Events'.

See for instance
:src:`src/lib_node_config/config_file.ml` (search for the string ``declare_``):

-  one declares typed-record events with
   ``Internal_event.Simple.declare_<number-of-fields>``,
-  and they are called with
   ``val Simple.emit: 'a Simple.t -> 'a -> (unit, trace) result Lwt.t``.

Please read the guidelines that are documented in
`the Internal_event.Simple module. <../api/odoc/_html/tezos-event-logging/Tezos_event_logging/Internal_event/Simple/index.html>`__


“Heavy” Events
^^^^^^^^^^^^^^

The most generic API for defining events ``Internal_event.Make`` gives full
expressive power and structure.

See for instance:
:src:`src/proto_alpha/lib_delegate/client_baking_blocks.ml` (module
``Block_seen_event``):

-  one needs to call the functor
   ``module Event = Internal_event.Make(Definition)`` where
   ``Definition: Internal_event.EVENT_DEFINITION``;
-  this provides
   ``val Event.emit: ?section:Section.t -> Block_seen_event.t -> (unit, trace) result Lwt.t``.

Creating Sinks
~~~~~~~~~~~~~~

A sink is a module of type ``Internal_event.SINK`` which is registered
within the system (just like events or errors kinds) using:
``let () = Internal_event.All_sinks.register (module My_sink)``. Its
chosen URI-scheme should be unique.

See for instance:

-  The (simplest) module ``Lwt_log_sink``:
   :src:`src/lib_event_logging/internal_event.ml`,
-  or, the much more complex,
   :src:`src/lib_stdlib_unix/file_descriptor_sink.ml`.
