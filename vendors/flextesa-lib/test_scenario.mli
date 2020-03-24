(** Build and manage Network Sandboxes. *)

open Internal_pervasives

module Inconsistency_error : sig
  type t =
    [ `Empty_protocol_list
    | `Too_many_protocols of Tezos_protocol.t list
    | `Too_many_timestamp_delays of Tezos_protocol.t list ]

  val should_be_one_protocol :
       'a list
    -> ( 'a
       , [> `Empty_protocol_list
         | `Too_many_protocols of 'a list
         | `Too_many_timestamp_delays of 'a list ] )
       Asynchronous_result.t

  val pp :
       Caml.Format.formatter
    -> [< `Empty_protocol_list
       | `Too_many_protocols of 'a Base.List.t
       | `Too_many_timestamp_delays of 'a Base.List.t ]
    -> unit
end

(** Build {i static} tezos network topologies. *)
module Topology : sig
  type node = Tezos_node.t

  type _ t = private
    | Mesh : {size: int} -> node list t
    | Bottleneck :
        {name: string; left: 'a network; right: 'b network}
        -> ('a * node * 'b) t
    | Net_in_the_middle :
        {middle: 'm network; left: 'a network; right: 'b network}
        -> ('a * 'm * 'b) t

  and 'a network = {topology: 'a t; name: string}

  val mesh : string -> int -> node list network
  val sub : string -> 'a t -> 'a network

  val bottleneck :
    string -> 'a network -> 'b network -> ('a * node * 'b) network

  val node_count : 'a t -> int
  val node_ids : 'a t -> 'a -> string list

  val net_in_the_middle :
    string -> 'a network -> 'b network -> 'c network -> ('b * 'a * 'c) network

  val build :
       ?external_peer_ports:int list
    -> ?base_port:int
    -> make_node:(   string
                  -> expected_connections:int
                  -> rpc_port:int
                  -> p2p_port:int
                  -> int list
                  -> node)
    -> 'a network
    -> 'a
end

(** Start networks from (and manipulate) {!Topology.t} values. *)
module Network : sig
  type t = private {nodes: Tezos_node.t list}

  val make : Tezos_node.t list -> t

  val start_up :
       ?do_activation:bool
    -> ?check_ports:bool
    -> < Base_state.base
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; console: Console.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client_exec:Tezos_executable.t
    -> t
    -> ( unit
       , [> `Empty_protocol_list
         | System_error.t
         | Process_result.Error.t
         | Process_result.Error.t
         | `Too_many_protocols of Tezos_protocol.t list
         | `Too_many_timestamp_delays of Tezos_protocol.t list
         | `Waiting_for of string * [`Time_out] ] )
       Asynchronous_result.t
  (** Start the nodes, bootstrap the client, and (potentially)
      activate the protocol of a network.

      - [?do_activation]: if [true] the start-up will try to activate
        the protocol, if [false] it will do it only if the current
        level of the chain is [0].
 *)
end

val network_with_protocol :
     ?do_activation:bool
  -> ?node_custom_network:[`Json of Ezjsonm.value]
  -> ?external_peer_ports:int list
  -> ?base_port:int
  -> ?size:int
  -> ?protocol:Tezos_protocol.t
  -> ?nodes_history_mode_edits:([> `Empty_protocol_list
                                | System_error.t
                                | Process_result.Error.t
                                | Process_result.Error.t
                                | `Too_many_protocols of Tezos_protocol.t list
                                | `Too_many_timestamp_delays of
                                  Tezos_protocol.t list
                                | `Waiting_for of string * [`Time_out] ]
                                as
                                'errors)
                               Tezos_node.History_modes.edit
  -> < env_config: Environment_configuration.t
     ; paths: Paths.t
     ; console: Console.t
     ; runner: Running_processes.State.t
     ; .. >
     Base_state.t
  -> node_exec:Tezos_executable.t
  -> client_exec:Tezos_executable.t
  -> (Tezos_node.t list * Tezos_protocol.t, 'errors) Asynchronous_result.t
(** [network_with_protocol] is a wrapper simply starting-up a
    {!Topology.mesh}. See {!Network.start_up} for details on some arguments. *)

(** Run queries on running networks. *)
module Queries : sig
  val all_levels :
       ?chain:string
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> nodes:Tezos_node.t list
    -> ( (string * [> `Failed | `Level of int | `Null | `Unknown of string])
         list
       , [> System_error.t] )
       Asynchronous_result.t
  (** Get the current chain level for all the nodes, returns {i
      node-ID × level } values. *)

  val wait_for_all_levels_to_be :
       ?attempts_factor:float
    -> ?chain:string
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> attempts:int
    -> seconds:float
    -> Tezos_node.t list
    -> [< `At_least of int | `Equal_to of int]
    -> ( unit
       , [> System_error.t | `Waiting_for of string * [`Time_out]] )
       Asynchronous_result.t
  (** Try-sleep-loop waiting for all given nodes to reach a given level. *)
end
