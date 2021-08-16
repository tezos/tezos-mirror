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

    Usage: [create count arguments]

    Create [count] nodes (using [Node.create]) sharing the same [arguments].
    Each node is named [name ^ "." ^ i] where [i] is the index of the node
    in the resulting list, starting from 1. *)
val create :
  ?path:string -> ?name:string -> int -> Node.argument list -> Node.t list

(** {2 Topologies} *)

(** Connect all nodes of a cluster to all nodes of another cluster.

    Usage: [connect a b]

    Add all nodes of [b] as [--peer] arguments to all nodes of [a].
    This is a generalization of [Node.add_peer] for lists. *)
val connect : Node.t list -> Node.t list -> unit

(** Connect all nodes of a cluster together.

    For a cluster [node1; node2; ...; nodeN], this adds:
    - [node2; ...; nodeN] as [--peer]s to [node1];
    - [node3; ...; nodeN] as [--peer]s to [node2];
    - etc. *)
val clique : Node.t list -> unit

(** Connect nodes to form a ring.

    For a cluster [node1; node2; ...; nodeN], this adds:
    - [node2] as [--peer] to [node1];
    - [node3] as [--peer] to [node2];
    - ...
    - and finally [node1] as [--peer] to [nodeN]. *)
val ring : Node.t list -> unit

(** Connect nodes to form a star.

    Usage: [star center other_nodes]

    Add all [other_nodes] as [--peer]s to [center]. *)
val star : Node.t -> Node.t list -> unit

(** {2 Meta-Topologies} *)

(** The following functions are similar to the above functions
    that create topologies, except that they connect clusters using {!connect}
    instead of nodes using [Node.add_peer].

    For instance, a meta-clique [a; b; c] connects:
    - all nodes of [a] with all nodes of [b] and [c];
    - all nodes of [b] with all nodes of [c].
    The result itself is not necessarily a clique since nodes of each sub-cluster
    are not necessarily connected between themselves. *)

(** Meta-version of {!clique}. *)
val meta_clique : Node.t list list -> unit

(** Meta-version of {!ring}. *)
val meta_ring : Node.t list list -> unit

(** Meta-version of {!star}. *)
val meta_star : Node.t list -> Node.t list list -> unit

(** {2 Running} *)

(** Start a cluster of nodes.

    This runs [Node.identity_generate], [Node.config_init] and [Node.run] for all nodes.
    This is similar to iterating [Node.init] except that the node are already created.

    If [public] is [false] (which is the default), nodes are started with [--private_mode].
    Set [public] to [true] to allow the topology to change over time.
    If you need some nodes to be in private mode, and some nodes not to,
    set [public] to [true], and give the [Private_mode] argument to [Cluster.create]
    and/or [Node.create]. *)
val start : ?public:bool -> Node.t list -> unit Lwt.t
