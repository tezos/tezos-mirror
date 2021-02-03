Using The Event Logging Framework
---------------------------------

Developers of most Tezos modules should consider generating useful information
for the :doc:`Logging subsystem <./user/logging>`. This is done by using the
`tezos-event-logging` library to define and emit events, create sinks, and so
on. This page shows how this can be done.

Adding Events
~~~~~~~~~~~~~

Simple Events
^^^^^^^^^^^^^

Simple events are record-like structures which are constructed directly
(i.e. there is no high-level OCaml type, and parsing events is not accessible
programmatically). The API is a bit less lazy than with the generic API (the
fields of the events are still evaluated every time, which is usually not an
issue since they are evaluated anyway by the application code itself).

See for instance
:src:`bin_node/node_config_file.ml#L1055`:

-  one declares typed-record events with
   ``Internal_event.Simple.declare_<number-of-fields>``,
-  and they are called with
   ``val Simple.emit: 'a Simple.t -> 'a -> (unit, trace) result Lwt.t``.

Please read the guidelines that are documented in
`the Internal_event.Simple module. <../api/odoc/_html/tezos-event-logging/Tezos_event_logging/Internal_event/Simple/index.html>`__


“Heavy” Events
^^^^^^^^^^^^^^
The most generic API for defining events ``Internal_event.Make`` gives full
expressive power and structure while being 100% lazy.

See for instance:
:src:`proto_alpha/lib_delegate/client_baking_blocks.ml#L100`:

-  one needs to call the functor
   ``module Event = Internal_event.Make(Definition)`` where
   ``Definition: Internal_event.EVENT_DEFINITION``;
-  this provides
   ``val Event.emit: ?section:Section.t -> (unit -> Block_seen_event.t) -> (unit, trace) result Lwt.t``.

Creating Sinks
~~~~~~~~~~~~~~

A sink is a module of type ``Internal_event.SINK`` which is registered
within the system (just like events or errors kinds) using:
``let () = Internal_event.All_sinks.register (module My_sink)``. Its
chosen URI-scheme should be unique.

See for instance:

-  The (simplest) ``Lwt_log_sink``:
   :src:`lib_event_logging/internal_event.ml$L1530`,
-  or, the much more complex,
   :src:`lib_stdlib_unix/file_descriptor_sink.ml`.
