(** This module provides modules to create distribtued computations.
    Distributed comutations are described using the {!module-type:Process}.
    {!module-type:Process} provides a monadic interface to describe distributed computations.

    @author essdotteedot <essdotteedot_at_gmail_dot_com>
    @version 0.6.0
*)

(** Some nomenclature :
    - Node : A node corresponds to a operating system process. There can be many nodes on a
             single machine.

    - Process : A process corresponds to a light weight thread (i.e., user space cooperative threads).
                There can be many processes on a single Node.

*)

(** This module provides a type representing a node id. *)
module Node_id : sig
  (** The abstract type representing a node id. *)
  type t

  (** [get_name node] returns the name of the node. *)
  val get_name : t -> string
end

(** This module provides a type representing a process id. *)
module Process_id : sig
  (** The abstract type representing a process id. *)
  type t
end

(** Abstract type which can perform monadic concurrent IO. *)
module type Nonblock_io = sig
  (** The monadic light weight thread type returning value ['a]. *)
  type 'a t

  (** An unbounded stream holding values of ['a]. *)
  type 'a stream

  (** A type to represent a non-blocking input channel *)
  type input_channel

  (** A type to represent a non-blocking output channel *)
  type output_channel

  (** A type to represent a server *)
  type server

  (** Exception raised by {!val:timeout} operation *)
  exception Timeout

  type level =
    | Debug
    | Info
    | Warning
    | Error  (** Type to represent levels of a log message. *)

  (** The name of the implementation, for logging purposes. *)
  val lib_name : string

  (** The version implementation, for logging purposes. *)
  val lib_version : string

  (** A description of the implementation (e.g., the url of the code repository ), for logging purposes. *)
  val lib_description : string

  (** [return v] creates a light weight thread returning [v]. *)
  val return : 'a -> 'a t

  (** [bind t f] is a thread which first waits for the thread [t] to terminate and then, if the thread succeeds,
      behaves as the application of function [f] to the return value of [t]. If the thread [t] fails, [bind t f] also fails,
      with the same exception.
  *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [fail e] is a thread that fails with the exception [e]. *)
  val fail : exn -> 'a t

  (** [catch t f] is a thread that behaves as the thread [t ()] if this thread succeeds.
      If the thread [t ()] fails with some exception, [catch t f] behaves as the application of [f] to this exception.
  *)
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  (** [async f starts] a thread without waiting for the result. *)
  val async : (unit -> unit t) -> unit

  (** [create ()] returns a new stream and a push function. *)
  val create_stream : unit -> 'a stream * ('a option -> unit)

  (** [get st] removes and returns the first element of the stream, if any. Will block if the stream is empty. *)
  val get : 'a stream -> 'a option t

  (** [stream_append s1 s2] returns a [stream] which returns all elements of [s1], then all elements of [s2]. *)
  val stream_append : 'a stream -> 'a stream -> 'a stream

  (** [close ch] closes the given channel immediately. *)
  val close_input : input_channel -> unit t

  (** [close ch] closes the given channel. It performs all pending actions, flushes it and closes it. *)
  val close_output : output_channel -> unit t

  (** [read_value ic] reads a marshalled value from [ic]. *)
  val read_value : input_channel -> 'a t

  (** [write_value oc ?flags x] marshals the value [x] to [oc]. *)
  val write_value :
    output_channel -> ?flags:Marshal.extern_flags list -> 'a -> unit t

  (** [open_connection addr] opens a connection to the given address and returns two channels for using it. *)
  val open_connection : Unix.sockaddr -> (input_channel * output_channel) t

  (** [establish_server ?backlog sockaddr f] creates a server which will listen for incoming connections.
      New connections are passed to [f]. Note that [f] must not raise any exception. Backlog is the argument passed to Lwt_unix.listen.
  *)
  val establish_server :
    ?backlog:int ->
    Unix.sockaddr ->
    (Unix.sockaddr -> input_channel * output_channel -> unit t) ->
    server t

  (** [shutdown_server server] will shutdown [server]. *)
  val shutdown_server : server -> unit t

  (** [log level message_formatter] logs a message at the specified level using the formatter provided. *)
  val log : level -> (unit -> string) -> unit t

  (** [sleep d] is a thread that remains suspended for [d] seconds and then terminates. *)
  val sleep : float -> unit t

  (** [timeout d] is a thread that remains suspended for [d] seconds and then fails with ​{!exception:Distributed.Nonblock_io.Timeout}. *)
  val timeout : float -> 'a t

  (** [pick l] behaves as the first thread in l to terminate. If several threads are already terminated, one is chosen at random. Cancels all sleeping threads when one terminates. *)
  val pick : 'a t list -> 'a t

  (** [at_exit fn] will call fn on program exit. *)
  val at_exit : (unit -> unit t) -> unit
end

(** The abstract type representing the messages that will be sent between processes. *)
module type Message_type = sig
  (** Abstract type representing the messages that will be sent between processes. *)
  type t

  (** [string_of_message msg] returns the [string] representation of [msg]. *)
  val string_of_message : t -> string
end

(** A unit of computation which can be executed on a local or remote host, is monadic. *)
module type Process = sig
  (** Exception that is raised if {!val:run_node} is called more than once. *)
  exception Init_more_than_once

  (** Exception that is raised when {!val:spawn}, {!val:broadcast}, {!val:monitor} are called with an invalid node or if {!val:send}
      is called with a process which resides on an unknown node.
  *)
  exception InvalidNode of Node_id.t

  (** Exception that is raised when {!val:add_remote_node} or {!val:remove_remote_node} is called on a node that is operating in local only mode. *)
  exception Local_only_mode

  (** The abstract monadic type representing a computation returning ['a]. *)
  type 'a t

  (** Abstract type for monadic concurrent IO returning ['a]. *)
  type 'a io

  (** The abstract type representing the messages that will be sent between processes. *)
  type message_type

  (** The abstract type representing a non-empty list of matchers to be used with {!val:receive} function. *)
  type 'a matcher_list

  (** The abstract type representing a monitor_ref that is returned when a processes is monitored and can be used to unmonitor it. *)
  type monitor_ref

  (** Reason for process termination. *)
  type monitor_reason =
    | Normal of Process_id.t  (** Process terminated normally. *)
    | Exception of Process_id.t * exn  (** Process terminated with exception. *)
    | UnkownNodeId of Process_id.t * Node_id.t
        (** An operation failed because the remote node id is unknown. *)
    | NoProcess of Process_id.t
        (** Attempted to monitor a process that does not exist. *)

  (** The configuration of a node to be run as a remote node i.e., one that can both send an receive messages with other nodes. *)
  module Remote_config : sig
    type t = {
      remote_nodes : (string * int * string) list;
          (** The initial list of remote nodes which this node can send messages to. A list of external ip address/port/node name triplets.*)
      local_port : int;  (** The port that this node should run on. *)
      connection_backlog : int;
          (** The the argument used when listening on a socket. *)
      node_name : string;  (** The name of this node. *)
      node_ip : string;  (** The external ip address of this node. *)
    }
  end

  (** The configuration of a node to be run as a local node i.e., one that can not send or receive messages with other nodes. *)
  module Local_config : sig
    type t = {node_name : string  (** The name of this node. *)}
  end

  (** The configuration of a node. Can be one of {!node_config.Local} or {!node_config.Remote}. *)
  type node_config = Local of Local_config.t | Remote of Remote_config.t

  (** [return v] creates a computation returning [v]. *)
  val return : 'a -> 'a t

  (** [c >>= f] is a computation which first waits for the computation [c] to terminate and then, if the computation succeeds,
      behaves as the application of function [f] to the return value of [c]. If the computation [c] fails, [c >>= f] also fails,
      with the same exception.
  *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  type proc_rep = Fun of (unit -> unit t) | Registered of string

  val register : string -> (Process_id.t -> unit -> unit t) -> unit t

  (** [fail e] is a process that fails with the exception [e]. *)
  val fail : exn -> 'a t

  (** [catch p f] is a process that behaves as the process [p ()] if this process succeeds.
      If the process [p ()] fails with some exception, [catch p f] behaves as the application of [f] to this exception.
  *)
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  (** [spawn monitor name node_id process] will spawn [process] on [node_id] returning the {!type:Process_id.t} associated with the newly spawned process.
      If [monitor] is true (default value is false) then the spawned process will also be monitored and the associated {!type:monitor_ref} will be
      returned.

      If [node_id] is an unknown node then {!exception:InvalidNode} exception is raised.
  *)
  val spawn :
    ?monitor:bool ->
    Node_id.t ->
    proc_rep ->
    Process_id.t ->
    (Process_id.t * monitor_ref option) t

  (** [case match_fn] will create a {!type:matcher_list} which will use [match_fn] to match on potential messages.
      [match_fn] should return [None] to indicate no match or [Some handler] where [handler] is the function
      that should be called to handle the matching message.
  *)
  val case : (message_type -> (unit -> 'a t) option) -> 'a matcher_list

  (** [termination_case handler] will create a {!type:matcher_list} which can use used to match against [termination_reason] for a
      process that is being monitored. If this process is monitoring another process then providing this matcher in the list
      of matchers to {!val:receive} will allow this process to act on the termination of the monitored process.

      NOTE : when a remote process (i.e., one running on another node) raises an exception you will not be able
      to pattern match on the exception . This is a limitation of the Marshal OCaml module :
      " Values of extensible variant types, for example exceptions (of extensible type exn), returned by the unmarshaller
        should not be pattern-matched over through [match ... with] or [try ... with], because unmarshalling does not preserve the
        information required for matching their constructors. Structural equalities with other extensible variant values does not work either.
        Most other uses such as Printexc.to_string, will still work as expected. "

      See http://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html.
  *)
  val termination_case : (monitor_reason -> 'a t) -> 'a matcher_list

  (** [a_matcher |. b_matcher] is a {!type:matcher_list} consiting of the matchers in [a_matcher] followed by the matchers
      in [b_matcher].
   *)
  val ( |. ) : 'a matcher_list -> 'a matcher_list -> 'a matcher_list

  (** [receive timeout matchers] will wait for a message to be sent to this process which matches one of matchers provided in
      [matchers]. The first matching matcher in [matchers] will used process the matching message returning [Some result] where
      [result] is result of the matcher processing the matched message. All the other non-matching messages are left in the same
      order they came in.

      If a time out is provided and no matching messages has arrived in the time out period then None will be returned.

      If the [matchers] is empty then an {!exception:Empty_matchers} exception is raised.
  *)
  val receive : ?timeout_duration:float -> 'a matcher_list -> 'a option t

  (** [receive_loop timeout matchers] is a convenience function which will loop until a matcher in [matchers] returns false. *)
  val receive_loop : ?timeout_duration:float -> bool matcher_list -> unit t

  (** [send process_id msg] will send, asynchronously, message [msg] to the process with id [process_id] (possibly running on a remote node).

      If [process_id] is resides on an unknown node then {!exception:InvalidNode} exception is raised.

      If [process_id] is an unknown process but the node on which it resides is known then send will still succeed (i.e., will not raise any exceptions).
  *)
  val send : Process_id.t -> message_type -> unit t

  (** [pid >! msg] is equivalent to [send pid msg]. [>!] is an infix alias for [send]. *)
  val ( >! ) : Process_id.t -> message_type -> unit t

  (** [broadcast node_id msg] will send, asynchronously, message [msg] to all the processes on [node_id].

      If [node_id] is an unknown node then {!exception:InvalidNode} exception is raised.
  *)
  val broadcast : Node_id.t -> message_type -> unit t

  (** [monitor pid] will allows the calling process to monitor [pid]. When [pid] terminates (normally or abnormally) this monitoring
      process will receive a [termination_reason] message, which can be matched in [receive] using [termination_matcher]. A single
      process can be monitored my multiple processes.

      If [process_id] is resides on an unknown node then {!exception:InvalidNode} exception is raised.
  *)
  val monitor : Process_id.t -> monitor_ref t

  (** [unmonitor mref] will cause this process to stop monitoring the process which is referenced by [mref].
      If the current process is not monitoring the process referenced by [mref] then [unmonitor] is a no-op.

      If process being unmonitored as indicated by [monitor_ref] is resides on an unknown node then {!exception:InvalidNode} exception is raised.
  *)
  val unmonitor : monitor_ref -> unit t

  (** [get_self_pid process] will return the process id associated with [process]. *)
  val get_self_pid : Process_id.t t

  (** [get_self_node process] will return the node id associated with [process]. *)
  val get_self_node : Node_id.t t

  (** [get_remote_node node_name] will return the node id associated with [name], if there is no record of a node with [name] at
      this time then [None] is returned.
  *)
  val get_remote_node : string -> Node_id.t option t

  (** The list of all nodes currently active and inactive. *)
  val get_remote_nodes : Node_id.t list t

  (** [add_remote_node ip port name] will connect to the remote node at [ip]:[port] with name [name] and add it to the current nodes list of connected remote nodes.
      The newly added node id is returned as the result. Adding a remote node that already exists is a no-op.

      If the node is operating in local only mode then {!exception:Local_only_mode} is raised.
  *)
  val add_remote_node : string -> int -> string -> Node_id.t t

  (** [remove_remote_node node_id] will remove [node_id] from the list of connected remote nodes.

      If the node is operating in local only mode then {!exception:Local_only_mode} is raised.
  *)
  val remove_remote_node : Node_id.t -> unit t

  (** [lift_io io] lifts the [io] computation into the process. *)
  val lift_io : 'a io -> 'a t

  (** [run_node process node_monitor_fn node_config] performs the necessary bootstrapping to start this
      node according to [node_config]. If provided, runs the initial [process] returning the resulting [io].

      If it's called more than once then an exception of {!exception:Init_more_than_once} is raised.
  *)
  val run_node : ?process:(unit -> unit t) -> node_config -> unit io
end

(** Functor to create a module of type {!module-type:Process} given a message module [M] of type {!module-type:Message_type}. *)
module Make (I : Nonblock_io) (M : Message_type) :
  Process with type message_type = M.t and type 'a io = 'a I.t
