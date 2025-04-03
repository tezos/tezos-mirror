==========
RPC How-To
==========

This document explains how to declare, register, and serve RPC
entry-points in the Octez codebase. A lot of the complexity comes from
the following facts:

- Resto was developped in a separate repository
- the code of Octez was split by OS usage (Unix vs others)
- Resto and the RPC stack in general have accumulated significant
  technical debt

For background knowledge about the RPC architecture, see :doc:`./rpc_architecture`.
 
The main operations related to the RPC lifetime, explained below, are:
- declaring an RPC (service): describing the service, including a textual description and the types and de/serialization of arguments and return value
- declaring an RPC directory that bundles multiple services 
- registering RPCs: associating a handler to each service
- serving an RPC directory: launching a server listening and serving RPC requests
- calling an RPC: using externals tools to invoke Tezos RPCs

How to declare a service
========================
Declaring a service is the same as declaring a module signature. This includes a textual description and the types and de/serialization of its inputs/outputs.
The short answer is: call ``{get,post,put,…}_service`` in 
:package-api:`Tezos_rpc.Service <octez-libs/Tezos_rpc/Service/index.html>`.
Usage examples :ref:`below <declare_rpc_examples>`.

These functions are defined in :src:`src/lib_rpc/RPC_service.ml` as a
wrapper (for de/serialisation and error management) around
``Resto.MakeService`` (from :src:`resto/src/resto.ml`).

The call returns a
``('method,'prefix,'params,'query,'input,'output) service`` where the
parameters have the following meaning:

- ``'method`` (a.k.a. ``'m``): one of :literal:`[\`GET]`,
  :literal:`[\`POST]`, :literal:`[\`PUT]`, etc. matching the service’s
  method. (See https://en.wikipedia.org/wiki/HTTP#Request_methods.)
- ``'prefix`` (a.k.a. ``'pr``): the type of arguments passed to the
  service handler, which the client passes as part of the path. Unlike
  ``'params`` (see next), the ``'prefix`` parameters are set when
  declaring the directory of services (see later) rather than in this
  here declaration. (See example below.)
- ``'params`` (a.k.a. ``'p``): the type of arguments passed to the
  service handler, which the client passes as part of the path of the
  URL. Think ``/blocks/<id>/operations/<index>``.
- ``'query`` (a.k.a. ``'q``): the type of arguments passed to the
  service handler, which the client passes as part of the query
  parameters of the URL. Think ``?order=ascending&sort=author``. (See
  https://en.wikipedia.org/wiki/Query_string.)
- ``'input`` (a.k.a. ``'i``): the type of arguments passed to the
  service handler, which the client passes as the request’s body. (See
  https://en.wikipedia.org/wiki/HTTP_message_body.)
- ``'output`` (a.k.a. ``'o``): the type of values the service handler
  returns. The handler is actually expected to return values of the type
  ``'output tzresult`` which is what the :src`src/lib_rpc/RPC_service.ml`
  wrapper provides: the services in ``resto/src/resto.ml`` have an
  additional type parameter for ``'error`` which is hard-coded in
  :src:`src/lib_rpc/RPC_service.ml`. (See
  https://en.wikipedia.org/wiki/HTTP_message_body.)

These ``service`` values are *description* of services: they list the
number and type of arguments, their de/serialisation format, the type of
the return value, and some documentation. They do not have an associated
handler: this is registered separately (see :ref:`howto_register`). Think of these
values as the items in a ``.mli`` file: a description of inputs and
outputs without code.

``'prefix`` vs ``'params``
--------------------------

Both the ``'prefix`` and ``'params`` type parameters describe arguments
passed to the service handler. They differ in that ``'prefix`` is
provided by the directory that multiple services are bundled under
whereas ``'params`` is provided by the service itself.

Thus the prefix can be used for common arguments that a set of services
use. Just a way to avoid repeating some common argument. E.g., if we
wanted to have a version number in all the RPC entry-points
(``/<version>/chain/<chain-id>/heads``) that would be the way to do it.

The prefix can also be used when an argument cannot be described at the point
where the service is declared but it is available when the service is
registered (see :ref:`howto_register`) as described in the documentation of ``subst`` in
:src:`resto/src/resto.mli`.

The ``'prefix`` is always a subset of the ``'params``. Meaning that if
``'prefix`` parameters exist, they also appear (in duplicate) in the
``'params``. This is enforced by construction of paths (see the module
``Path`` in :src:`resto/src/resto.ml`). The ``'prefix`` is used to enforce
that the directory does define the necessary parameters (thus you cannot
register a service in a directory without providing the descriptions for
the prefix parameters, see :ref:`howto_register`); the ``'params`` is used to enforce that
the service handler has the matching number of parameters.

.. _declare_rpc_examples:

Usage example
-------------

:src:`src/lib_p2p_services/p2p_services.ml` declares all the p2p-related
RPC services. Excerpt:

::

   let list =
     Tezos_rpc.Service.get_service  (* a GET service *)
       ~description:"List the running P2P connection."
       ~query:Tezos_rpc.Query.empty  (* no qeury parameters *)
       ~output:(Data_encoding.list connection_info_encoding)  (* what the service returns
                                                                 and how it's de/serialised *)
       Tezos_rpc.Path.(root / "network" / "connections")  (* plain path without parameters *)

   let kick =
     Tezos_rpc.Service.delete_service  (* a DELETE service *)
       ~query:wait_query  (* a query parameter defined earlier in the file as a flag (yes/no) *)
       ~output:Data_encoding.empty  (* no value returned *)
       ~description:
         "Forced close of the current P2P connection to the given peer."
       Tezos_rpc.Path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)
           (* a path with an argument identifying a peer *)

And the matching ``.mli`` excerpt:

::

   val list :
     ( [`GET]  (* method: get *)
     , unit  (* prefix: none *)
     , unit  (* params: none *)
     , unit  (* query: none *)
     , unit  (* input: none *)
     , connection_info list  (* output *)
     ) Tezos_rpc.Service.t

   val kick :
    ( [`DELETE]  (* method: delete *)
    , unit  (* prefix: none *)
    , unit * P2p_peer.Id.t  (* params: one parameter *)
    , < wait : bool >  (* query: one parameter *)
    , unit  (* input: none *)
    , unit  (* output: none *)
    ) Tezos_rpc.Service.t

Note that params (and prefix) parameters are represented as nested tuples
of parameters. Zero parameters is represented as ``unit``, a single
``x`` parameter is represented as ``unit * x``, two parameters ``x`` and
``y`` are represented as ``(unit * x) * y``, etc.

Note that query parameters are represented as objects.
This helps naming the different components of the query (it could also have been a record).
In our case, it’s only a single query argument.

Another usage example
---------------------

:src:`src/lib_shell_services/chain_services.ml` declares all the
chain-data-query RPC services. Excerpt:

::

     module Levels = struct
       (* define a path for that part of the services, as a kind of hierarchy of
          paths matching a hierarchy of concepts *)
       let path = Tezos_rpc.Path.(path / "levels")

       let checkpoint =
         Tezos_rpc.Service.get_service  (* GET *)
           ~description:"The current checkpoint for this chain."
           ~query:Tezos_rpc.Query.empty  (* no query *)
           ~output:block_descriptor_encoding  (* output de/serialisation *)
           Tezos_rpc.Path.(path / "checkpoint")  (* sub-path *)

       let savepoint = …

       let caboose = …
     end

And the matching ``.mli`` excerpt:

::

   type prefix = unit * chain
   …
   module Levels : sig
     val checkpoint :
       ( [`GET]  (* method: get *)
       , prefix  (* prefix: one parameter (defined above) *)
       , prefix  (* params: same as the prefix, no additional service-specific parameters *)
       , unit  (* query: none *)
       , unit  (* input: none *)
       , Block_hash.t * int32  (* output *)
       ) Tezos_rpc.Service.t

     val savepoint : …

     val caboose : …
   end

.. _howto_register:

How to register services by declaring a directory
=================================================

Directories are sets of services, each with a handler.
Therefore, declaring the directory and registering its services is done at the same time.

More specifically, you:

1. start with the empty directory ``Tezos_rpc.Directory.empty``

2. populate the directory by calling the registration functions in
   :package-api:`Tezos_rpc.Directory <octez-libs/Tezos_rpc/Directory/index.html>`

   There are a variety of registration functions in
   ``Tezos_rpc.Directory`` depending on the number of path parameter the
   service has, whether the service can fail with an error or not, etc.
   E.g., ``register2`` registers a 2-parameter service which may fail.
   E.g., ``lwt_register1`` registers a 1-parameter service which cannot
   fail (its handler uses lwt as its monad, hence the prefix).

3. may combine multiple directories together by calling
   ``Tezos_rpc.Directory.merge``.

   Note that the merging of directories may fail by raising an exception
   if there are services registered for conflicting paths. You can use
   the other directory combinator ``prefix`` to put a whole directory
   under a given namespace. (You can also use ``map`` to provide prefix
   parameters (see above).)

``Tezos_rpc.Directory`` is the module for the file
:src:`src/lib_rpc/RPC_directory.ml` and it is a thin wrapper around
:package-api:`Resto_directory.Make <octez-libs/Resto_directory/index.html>` which is in :src:`resto/src/resto_directory.ml`.
The wrapper instantiates the functor with de/serialisation functions and
shadows the ``*register*`` functions with some error-handling features.

.. _usage-example-1:

Usage example
-------------

:src:`src/lib_p2p/p2p_directory.ml` assembles the p2p-related services
defined in :src:`src/lib_p2p_services/`. More specifically, it provides a
``build_rpc_directory`` function which returns a directory of the
p2p-related services.

::

   let build_rpc_directory net =
     let dir = Tezos_rpc.Directory.empty in
     … (* some registrations *)
     let dir =
       Tezos_rpc.Directory.lwt_register1
         dir  (* the dir being populated *)
         P2p_services.Connections.S.kick  (* the pre-declared service being registered *)
         (fun peer_id q () ->  (* the handler for the service, with the different
                                  parameters: the path parameter, the query parameter *)
           match P2p.pool net with
           | None -> Lwt.return_unit
           | Some pool -> …)
     in
     let dir =
       Tezos_rpc.Directory.register0
         dir
         P2p_services.Connections.S.list
         (fun () () ->
           match P2p.pool net with
           | None -> tzfail P2p_errors.P2p_layer_disabled
           | Some pool -> …)
     in
     … (* more registrations *)
     dir

Note the differences between the two registrations. The ``kick``
registration uses ``lwt_register1`` because it cannot fail (it’s “lwt
only”) and it takes one (1) path parameter. The ``list`` registration
uses ``register0`` because it can fail (general case, no prefix) and it
takes zero (0) path parameters.

Also note that the helper functions for registration convert between the
nested-tuples representation of the ``'params`` parameters and the curried
representation of parameters for the handler function. E.g., the handler
for ``kick`` takes a ``peer_id`` parameter instead of ``((), peer_id)``.

Additional usage example
------------------------

:src:`src/lib_shell/node.ml` brings in multiple directories from different
parts of the code.

::

   let build_rpc_directory ~node_version ~commit_info node =
     let dir : unit Tezos_rpc.Directory.t ref = ref Tezos_rpc.Directory.empty in
     let merge d = dir := Tezos_rpc.Directory.merge !dir d in
     merge (Chain_directory.build_rpc_directory node.validator) ;
     merge (P2p_directory.build_rpc_directory node.p2p) ;
     …  (* more directories being merged *)
     !dir

How to serve a directory
========================

Serving a directory involves configuring and launching a server that listens to a port and handles RPC requests.

First, get a ``server`` value by calling
``Tezos_rpc_http.RPC_server.init_server``. This function takes a
directory (see :ref:`howto_register`).

Then, call ``Tezos_rpc_http.RPC_server.launch``. This function takes the
``server`` value initialised above as well as some server-configuration
parameters (think port number and such). The executable which calls this
function now serves the RPC services registered in the directory.

The responsibilities are handled as follows:

- :package-api:`Tezos_rpc_http.RPC_server <octez-libs/Tezos_rpc_http_server/RPC_server/index.html>`
  provides a thin wrapper (for de/serialisation and logging) around ``Resto_server``.
- ``Resto_server`` translates the directory into a callback: it takes an
  HTTP request and finds the matching handler to call.
- ``Resto_server`` provides a thin wrapper (error management, startup and teardown, some
  de/serialisation glue, etc.) around Cohttp.
- Cohttp does the low-level HTTP management (parsing HTTP requests, printing HTTP responses,
  populating headers, etc.) and delegates the actual network management
  (sockets, connections, etc.) to Conduit.
- Conduit does the bind/accept/etc. dance.

For details about RPC handling and RPC server initialization, see :doc:`./rpc_architecture`.

.. _usage-example-2:

Usage example
-------------

:src:`src/bin_node/node_run_command.ml` spins up the RPC server which is
part of the ``octez-node`` executable.

::

   let launch_rpc_server (config : Config_file.t) dir rpc_server_kind addr =
     … (* some things ommitted for scope *)
     let server =
       RPC_server.init_server
         ~cors
         ?acl
         ~media_types:(Media_type.Command_line.of_command_line media_types)
         dir  (* this is the directory of services, passed to the server *)
     in
     … (* some things ommitted for scope *)
     RPC_server.launch
       ~host
       server  (* this is the server (with its directory) being started *)
       ~callback
       ~max_active_connections:config.rpc.max_active_rpc_connections
       mode

Where ``dir`` is initialised in another part of the code as

::

     let dir = Node.build_rpc_directory ~node_version ~commit_info node in
     let dir = Node_directory.build_node_directory config dir in
     let dir =
       Tezos_rpc.Directory.register_describe_directory_service
         dir
         Tezos_rpc.Service.description_service
     in

How to call RPCs
================

In principle, you can call RPCs using curl or whichever HTTP client, but it can be difficult to
de/serialise arguments and responses. It is even more difficult when
using the ``application/octet-stream`` media type. Although
:ref:`octez-codec <octez-codec>`, the executable from ``src/bin_codec``, can help, it is
still difficult and further explanations are beyond the scope of this
document.

Instead, you can use the :ref:`octez-client <howtouse_tezos_client>` executable. This provides some
safety checks, some UI/UX niceties, and some built-in de/serialisation.
The ``octez-client`` executable uses abstractions similar to the node’s
RPC server in order to make RPC calls.

The stack is:

- ``src/bin_client/*`` defines the actual UI of the ``octez-client``
  binary: the commands, the parameters, etc. These commands use a
  client-context (variable name: ``cctxt``) (see :ref:`details <rpc_cctx>`) to actually make
  the call.
- :src:`src/lib_rpc_http/RPC_client_unix.ml` instantiates an ``RPC_client``
  (see next item) with the actual underlying calling method. The actual
  underlying calling method is a thin wrapper around the one provided by
  cohttp-client.
- :src:`src/lib_rpc_http/RPC_client.ml` provides a thin wrapper (error
  management, de/serialisation, some media-type dispatch) around
  ``resto_client`` to package it into a client-context.
- :src:`src/lib_rpc/RPC_context.ml` defines a client-context: a simple
  class (in the OOP sense) with methods to perform RPC calls.

  - The main idea behind this abstraction is roughly dependency
    injection: code can handle all the logic of calling RPC entry-points
    and using the returned value but the actual backend is passed
    dynamically as a client-context parameter. This is a leftover from a
    time we were trying to separate the code into native-vs-javascript
    parts and it could be greatly simplified.
  - The methods for performing the calls take a service as an argument
    as well as all the arguments (path, query, body) that the service
    expects. It computes the correct path based on the service
    declaration.

How to register an RPC in the protocol
======================================

Shortly: declaring and registering an RPC in the protocol is the same as for other services but is done inside the
plugin part of the protocol so the protocol services can be patched without having to
inject a new protocol.

