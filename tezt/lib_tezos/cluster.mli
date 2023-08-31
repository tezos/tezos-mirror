(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Set up networks involving large amount of nodes. *)

(** The applications of this module range from running small cliques to
    complex network topologies involving as many nodes as your computer can handle.

    The general approach is to create some nodes, then declare connections
    using functions such as [connect], [clique], [ring] or [star], and finally
    start all nodes with [start].

    For instance, here is how to create a star topology, with a center node
    connected to 5 other nodes:
    [
      let center = Node.create [] in
      let other_star_nodes = Cluster.create 5 [] in
      Cluster.star center other_star_nodes;
    ]
    Here is how to connect the center node of this star to a clique of 4 other nodes:
    [
      let clique_nodes = Cluster.create 4 [] in
      Cluster.clique clique_nodes;
      Cluster.connect [center] clique_nodes;
    ]
    Finally, here is how to start the network:
    [
      Cluster.start (clique_nodes @ center :: other_star_nodes)
    ]
*)

(** {2 Batch Node Creation} *)

(** Create several nodes.

    Usage: [create count ?rpc_local arguments]

    Create [count] nodes (using [Node.create]) sharing the same [?rpc_local]
    and [arguments].
    Each node is named [name ^ "." ^ i] where [i] is the index of the node
    in the resulting list, starting from 1. *)
val create :
  ?path:string ->
  ?name:string ->
  int ->
  ?rpc_local:bool ->
  Node.argument list ->
  Node.t list

(** {2 Topologies} *)

(** Connect two nodes together.

    Usage: [symmetric_add_peer a b]

    Same as [Node.add_peer a b; Node.add_peer b a]. *)
val symmetric_add_peer : Node.t -> Node.t -> unit

(** Connect all nodes of a cluster to all nodes of another cluster.

    Usage: [connect a b]

    Add all nodes of [b] as [--peer] arguments to all nodes of [a],
    and all nodes of [a] as [--peer] arguments to all nodes of [b].
    This is a generalization of [symmetric_add_peer] for lists. *)
val connect : Node.t list -> Node.t list -> unit

(** Connect all nodes of a cluster together.

    For a cluster [node1; node2; ...; nodeN], this adds:
    - [node2; ...; nodeN] as [--peer]s to [node1] and vice-versa;
    - [node3; ...; nodeN] as [--peer]s to [node2] and vice-versa;
    - etc. *)
val clique : Node.t list -> unit

(** Connect nodes to form a ring.

    For a cluster [node1; node2; ...; nodeN], this adds:
    - [node2] as [--peer] to [node1] and vice-versa;
    - [node3] as [--peer] to [node2] and vice-versa;
    - ...
    - and finally [node1] as [--peer] to [nodeN] and vice-versa. *)
val ring : Node.t list -> unit

(** Connect nodes to form a star.

    Usage: [star center other_nodes]

    Add all [other_nodes] as [--peer]s to [center] and vice-versa. *)
val star : Node.t -> Node.t list -> unit

(** {2 Meta-Topologies} *)

(** The following functions are similar to the above functions
    that create topologies, except that they are parameterized by:
    - ['a], the type of nodes (usually [Node.t]);
    - a connection function to create arrows in the graph.

    All non-meta functions are actually instances of their corresponding
    meta functions with ['a = Node.t] and [symmetric_add_peer] as
    the connection function. For instance, [clique] is the same
    as [meta_clique symmetric_add_peer].

    Here are examples of other useful instantiations:
    - use [Node.add_peer] as the connection function,
      to create one-way arrows instead of summetry ones
      (not very useful if nodes are in private mode);
    - use [connect] as the connection function to connect clusters
      together (['a] is then [Node.t list]). *)

(** Meta-version of {!connect}.

    For each [a] in the first list and [b] in the second list,
    this calls the connection function on [a] and [b]. *)
val meta_connect : ('a -> 'a -> unit) -> 'a list -> 'a list -> unit

(** Meta-version of {!clique}.

    For each pair [a, b] where [a] and [b] are part of the list
    and [a] is before [b] in the list, this calls the connection function
    on [a] and [b]. *)
val meta_clique : ('a -> 'a -> unit) -> 'a list -> unit

(** Lwt-version of {!meta_clique}. *)
val meta_clique_lwt : ('a -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

(** Meta-version of {!ring}.

    For a list [x1; x2; ...; xN], this calls the connection function
    on the following pairs: [x1, x2], [x2, x3], ..., [x(N-1), xN], and [xN, x1]. *)
val meta_ring : ('a -> 'a -> unit) -> 'a list -> unit

(** Meta-version of {!star}.

    Usage: [star f center outer]

    This calls [f center x] for all [x] in [outer]. *)
val meta_star : ('a -> 'b -> unit) -> 'a -> 'b list -> unit

(** {2 Running} *)

(** Start a cluster of nodes.

    This runs [Node.identity_generate], [Node.config_init], [Node.run]
    and [Node.wait_for_ready] for all nodes. This is similar to
    iterating [Node.init] except that the node are already created.

    If [public] is [false] (which is the default), nodes are started with [--private_mode].
    Set [public] to [true] to allow the topology to change over time.
    If you need some nodes to be in private mode, and some nodes not to,
    set [public] to [true], and give the [Private_mode] argument to [Cluster.create]
    and/or [Node.create].

    If [?wait_connections] is set, the function will return only after
    all nodes in the topology are connected to their expected number of
    neighbors. *)
val start :
  ?public:bool ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?wait_connections:bool ->
  Node.t list ->
  unit Lwt.t
