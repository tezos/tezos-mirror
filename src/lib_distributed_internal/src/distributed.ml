module Potpourri = struct
  let get_option (v : 'a option) : 'a =
    match v with None -> assert false (*BISECT-IGNORE*) | Some v' -> v'

  let map_default_option (f : 'a -> 'b) (default_value : 'b) (opt : 'a option) :
      'b =
    match opt with Some v -> f v | None -> default_value

  let post_incr (v : int ref) : int =
    let res = !v in
    v := !v + 1 ;
    res

  let of_option (f : unit -> 'a) : 'a option = try Some (f ()) with _ -> None

  let pp_list ?first ?last ?sep (items : 'a list)
      (string_of_item : 'a -> string) formatter : unit =
    if first <> None then Format.fprintf formatter (get_option first) else () ;
    List.iter
      (fun i ->
        let s = string_of_item i in
        Format.fprintf formatter "%s" s ;
        if sep <> None then Format.fprintf formatter (get_option sep) else ())
      items ;
    if last <> None then Format.fprintf formatter (get_option last) else ()
end

module Node_id = struct
  type t = {ip : Unix.inet_addr option; port : int option; name : string}

  let make_local_node name = {ip = None; port = None; name}

  let make_remote_node ipStr port name =
    {ip = Some (Unix.inet_addr_of_string ipStr); port = Some port; name}

  let is_local node local_node =
    node.ip = local_node.ip && node.port = local_node.port

  let get_name {name; _} = name

  let print_string_of_node node formatter =
    let string_of_ip =
      if node.ip = None then "None"
      else Unix.string_of_inet_addr @@ Potpourri.get_option node.ip
    in
    let string_of_port =
      if node.port = None then "None"
      else string_of_int @@ Potpourri.get_option node.port
    in
    Format.fprintf
      formatter
      "{ip : %s ; port : %s ; name : %s}"
      string_of_ip
      string_of_port
      node.name

  let get_ip {ip; _} = ip

  let get_port {port; _} = port
end

module Node_id_seeded_hash_type = struct
  type t = Node_id.t

  let equal (n1 : t) (n2 : t) : bool =
    (Node_id.get_ip n1, Node_id.get_port n1)
    = (Node_id.get_ip n2, Node_id.get_port n2)

  (* See [src/lib_base/tzPervasives.ml] for an explanation *)
  [@@@ocaml.warning "-32"]

  let hash (seed : int) (n : t) : int =
    Hashtbl.seeded_hash seed (Node_id.get_ip n, Node_id.get_port n)

  let seeded_hash = hash

  [@@@ocaml.warning "+32"]
end

module Node_id_hashtbl = Hashtbl.MakeSeeded (Node_id_seeded_hash_type)

module Process_id = struct
  type t = {node : Node_id.t; proc_id : int}

  let make nid pid = {node = nid; proc_id = pid}

  let make_local name next_process_id =
    {
      node = Node_id.make_local_node name;
      proc_id = Potpourri.post_incr next_process_id;
    }

  let make_remote ipStr port name next_process_id =
    {
      node = Node_id.make_remote_node ipStr port name;
      proc_id = Potpourri.post_incr next_process_id;
    }

  let is_local {node; _} local_node = Node_id.is_local node local_node

  let get_node {node; _} = node

  let get_id {proc_id; _} = proc_id

  let print_string_of_pid p formatter =
    Format.fprintf formatter "{node : " ;
    Node_id.print_string_of_node p.node formatter ;
    Format.fprintf formatter " ; id : %d}" p.proc_id
end

module Process_id_seeed_hash_type = struct
  type t = Process_id.t

  let equal (p1 : t) (p2 : t) : bool =
    let p1_ip = Node_id.get_ip @@ Process_id.get_node p1 in
    let p1_port = Node_id.get_port @@ Process_id.get_node p1 in
    let p1_id = Process_id.get_id p1 in

    let p2_ip = Node_id.get_ip @@ Process_id.get_node p2 in
    let p2_port = Node_id.get_port @@ Process_id.get_node p2 in
    let p2_id = Process_id.get_id p2 in

    (p1_ip, p1_port, p1_id) = (p2_ip, p2_port, p2_id)

  (* See [src/lib_base/tzPervasives.ml] for an explanation *)
  [@@@ocaml.warning "-32"]

  let hash (seed : int) (p : t) : int =
    let p_ip = Node_id.get_ip @@ Process_id.get_node p in
    let p_port = Node_id.get_port @@ Process_id.get_node p in
    let p_id = Process_id.get_id p in
    Hashtbl.seeded_hash seed (p_ip, p_port, p_id)

  let seeded_hash = hash

  [@@@ocaml.warning "+32"]
end

module Process_id_hashtbl = Hashtbl.MakeSeeded (Process_id_seeed_hash_type)

module type Nonblock_io = sig
  type 'a t

  type 'a stream

  type input_channel

  type output_channel

  type server

  type level = Debug | Info | Warning | Error

  exception Timeout

  val lib_name : string

  val lib_version : string

  val lib_description : string

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val fail : exn -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val async : (unit -> unit t) -> unit

  val create_stream : unit -> 'a stream * ('a option -> unit)

  val get : 'a stream -> 'a option t

  val stream_append : 'a stream -> 'a stream -> 'a stream

  val close_input : input_channel -> unit t

  val close_output : output_channel -> unit t

  val read_value : input_channel -> 'a t

  val write_value :
    output_channel -> ?flags:Marshal.extern_flags list -> 'a -> unit t

  val open_connection : Unix.sockaddr -> (input_channel * output_channel) t

  val establish_server :
    ?backlog:int ->
    Unix.sockaddr ->
    (Unix.sockaddr -> input_channel * output_channel -> unit t) ->
    server t

  val shutdown_server : server -> unit t

  val log : level -> (unit -> string) -> unit t

  val sleep : float -> unit t

  val timeout : float -> 'a t

  val pick : 'a t list -> 'a t

  val at_exit : (unit -> unit t) -> unit
end

module type Message_type = sig
  type t

  val string_of_message : t -> string
end

module type Process = sig
  exception Init_more_than_once

  exception InvalidNode of Node_id.t

  exception Local_only_mode

  type 'a io

  type 'a t

  type message_type

  type 'a matcher_list

  type monitor_ref

  type monitor_reason =
    | Normal of Process_id.t
    | Exception of Process_id.t * exn
    | UnkownNodeId of Process_id.t * Node_id.t
    | NoProcess of Process_id.t

  module Remote_config : sig
    type t = {
      remote_nodes : (string * int * string) list;
      local_port : int;
      connection_backlog : int;
      node_name : string;
      node_ip : string;
    }
  end

  module Local_config : sig
    type t = {node_name : string}
  end

  type node_config = Local of Local_config.t | Remote of Remote_config.t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  type proc_rep = Fun of (unit -> unit t) | Registered of string

  val register : string -> (Process_id.t -> unit -> unit t) -> unit t

  val fail : exn -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val spawn :
    ?monitor:bool ->
    Node_id.t ->
    proc_rep ->
    Process_id.t ->
    (Process_id.t * monitor_ref option) t

  val case : (message_type -> (unit -> 'a t) option) -> 'a matcher_list

  val termination_case : (monitor_reason -> 'a t) -> 'a matcher_list

  val ( |. ) : 'a matcher_list -> 'a matcher_list -> 'a matcher_list

  val receive : ?timeout_duration:float -> 'a matcher_list -> 'a option t

  val receive_loop : ?timeout_duration:float -> bool matcher_list -> unit t

  val send : Process_id.t -> message_type -> unit t

  val ( >! ) : Process_id.t -> message_type -> unit t

  val broadcast : Node_id.t -> message_type -> unit t

  val monitor : Process_id.t -> monitor_ref t

  val unmonitor : monitor_ref -> unit t

  val get_self_pid : Process_id.t t

  val get_self_node : Node_id.t t

  val get_remote_node : string -> Node_id.t option t

  val get_remote_nodes : Node_id.t list t

  val add_remote_node : string -> int -> string -> Node_id.t t

  val remove_remote_node : Node_id.t -> unit t

  val lift_io : 'a io -> 'a t

  val run_node : ?process:(unit -> unit t) -> node_config -> unit io
end

module Make (I : Nonblock_io) (M : Message_type) :
  Process with type message_type = M.t and type 'a io = 'a I.t = struct
  exception Init_more_than_once

  exception InvalidNode of Node_id.t

  exception Local_only_mode

  type 'a io = 'a I.t

  type message_type = M.t

  type monitor_ref = Monitor_Ref of int * Process_id.t * Process_id.t
  (* unique id, the process doing the monitoring and the process being monitored *)

  type monitor_reason =
    | Normal of Process_id.t
    | Exception of Process_id.t * exn
    | UnkownNodeId of Process_id.t * Node_id.t
    | NoProcess of Process_id.t

  module Remote_config = struct
    type t = {
      remote_nodes : (string * int * string) list;
      local_port : int;
      connection_backlog : int;
      node_name : string;
      node_ip : string;
    }
  end

  module Local_config = struct
    type t = {node_name : string}
  end

  type node_config = Local of Local_config.t | Remote of Remote_config.t

  module Monitor_ref_order_type = struct
    type t = monitor_ref

    let compare (Monitor_Ref (id1, _, _) : t) (Monitor_Ref (id2, _, _) : t) :
        int =
      id1 - id2
  end

  module Monitor_ref_set = Set.Make (Monitor_ref_order_type)

  type message =
    | Data of Process_id.t * Process_id.t * message_type
      (* sending process id, receiving process id and the message *)
    | Broadcast of
        Process_id.t
        * Node_id.t
        * message_type (* sending process id, receiving node and the message *)
    | Proc of proc_rep * Process_id.t * Process_id.t
      (* the process to be spawned elsewhere and the process that requested the spawning *)
    | Spawn_monitor of proc_rep * Process_id.t * Process_id.t * Process_id.t
      (* the process to be spawned elsewhere, the monitoring process and the process that requested the spawning.*)
    | Node of Node_id.t
      (* initial message sent to remote node to identify ourselves *)
    | Exit of Process_id.t * monitor_reason
      (* process that was being monitored and the reason for termination *)
    | Monitor of Process_id.t * Process_id.t * Process_id.t
      (* the process doing the monitoring and the id of the process to be monitored and the process that requested the monitoring *)
    | Unmonitor of monitor_ref * Process_id.t
      (* process to unmonitor and the process that requested the unmonitor *)
    | Proc_result of Process_id.t * Process_id.t
      (* result of spawning a process and the receiver process id *)
    | Spawn_monitor_result of message option * monitor_ref * Process_id.t
      (* result of spawning and monitoring a process and the receiver process id *)
    | Monitor_result of
        message option
        * monitor_ref
        * Process_id.t (* result of monitor and the receiving process *)
    | Unmonitor_result of monitor_ref * Process_id.t
  (* monitor ref that was requested to be unmonitored and the receiving process *)

  and node_state = {
    mailboxes : (int, message I.stream * (message option -> unit)) Hashtbl.t;
    remote_nodes : I.output_channel Node_id_hashtbl.t;
    monitor_table : Monitor_ref_set.t Process_id_hashtbl.t;
    local_node : Node_id.t;
    monitor_ref_id : int ref;
    config : Remote_config.t option ref;
    log_buffer : Buffer.t;
    log_formatter : Format.formatter;
    est_in_ch : I.input_channel option ref;
        (* input channel from calling I.establish_server, store here so we can close when server exits*)
    est_out_ch : I.output_channel option ref;
        (* output_channel channel from calling I.establish_server, store here so we can close when server exits*)
    node_server : I.server option ref;
        (* the server from calling I.establish_server, store here so we can close listening socket when server exits *)
    next_process_id : int ref;
    registered_funs : (string, Process_id.t -> unit -> unit t) Hashtbl.t;
  }

  and 'a t = node_state * Process_id.t -> (node_state * Process_id.t * 'a) io

  and proc_rep = Fun of (unit -> unit t) | Registered of string

  type 'a matcher = message -> (unit -> 'a t) option

  type 'a matcher_list =
    | Matcher of 'a matcher
    | Matchers of 'a matcher * 'a matcher_list

  let initalised = ref false

  let dist_lib_version = "0.6.0"

  let print_string_of_termination_reason (reason : monitor_reason)
      (formatter : Format.formatter) : unit =
    match reason with
    | Normal pid ->
        Format.fprintf formatter "{termination reason : normal ; pid : " ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter "}"
    | Exception (pid, e) ->
        Format.fprintf
          formatter
          "{termination reason : exception %s ; pid : "
          (Printexc.to_string e) ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter "}"
    | UnkownNodeId (pid, n) ->
        Format.fprintf formatter "{termination reason : unknown node id " ;
        Node_id.print_string_of_node n formatter ;
        Format.fprintf formatter " ; pid : " ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter "}"
    | NoProcess p ->
        Format.fprintf formatter "{termination reason : unknown process" ;
        Process_id.print_string_of_pid p formatter ;
        Format.fprintf formatter "}"

  let print_string_of_monitor_ref (Monitor_Ref (id, pid, monitee_pid))
      (formatter : Format.formatter) : unit =
    Format.fprintf formatter "{id : %d ; monitor process : " id ;
    Process_id.print_string_of_pid pid formatter ;
    Format.fprintf formatter " ; monitee process : " ;
    Process_id.print_string_of_pid monitee_pid formatter ;
    Format.fprintf formatter "}"

  let print_string_of_monitor_notification (Monitor_Ref (id, pid, monitee_pid))
      (reason : monitor_reason) (formatter : Format.formatter) : unit =
    Format.fprintf formatter "{id : %d ; monitor process : " id ;
    Process_id.print_string_of_pid pid formatter ;
    Format.fprintf formatter " ; monitee process" ;
    Process_id.print_string_of_pid monitee_pid formatter ;
    Format.fprintf formatter " ; reason : " ;
    print_string_of_termination_reason reason formatter ;
    Format.fprintf formatter "}"

  let rec print_string_of_message (m : message) (formatter : Format.formatter) :
      unit =
    match m with
    | Data (sender, recver, msg) ->
        Format.fprintf formatter "Data : {sender pid : " ;
        Process_id.print_string_of_pid sender formatter ;
        Format.fprintf formatter " ; receiver pid : " ;
        Process_id.print_string_of_pid recver formatter ;
        Format.fprintf formatter " ;  message : %s}" (M.string_of_message msg)
    | Broadcast (sender, recv_node, msg) ->
        Format.fprintf formatter "Broadcast : {sender pid : " ;
        Process_id.print_string_of_pid sender formatter ;
        Format.fprintf formatter " ; receiver node : " ;
        Node_id.print_string_of_node recv_node formatter ;
        Format.fprintf formatter " ; message : %s}" (M.string_of_message msg)
    | Proc (_, sender_pid, _) ->
        Format.fprintf formatter "Proc { <process> ; sender pid : " ;
        Process_id.print_string_of_pid sender_pid formatter
    | Spawn_monitor (_, pid, sender, _) ->
        Format.fprintf formatter "Spawn and monitor {<process> ; monitor pid : " ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter " ; sender pid " ;
        Process_id.print_string_of_pid sender formatter ;
        Format.fprintf formatter "}"
    | Node nid ->
        Format.fprintf formatter "Node " ;
        Node_id.print_string_of_node nid formatter
    | Exit (pid, mreason) ->
        Format.fprintf formatter "Exit : {exit pid : " ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter " ; reason : " ;
        print_string_of_termination_reason mreason formatter ;
        Format.fprintf formatter "}"
    | Monitor (monitor_pid, monitee_pid, sender) ->
        Format.fprintf formatter "Monitor : {monitor pid : " ;
        Process_id.print_string_of_pid monitor_pid formatter ;
        Format.fprintf formatter " ; monitee pid : " ;
        Process_id.print_string_of_pid monitee_pid formatter ;
        Format.fprintf formatter " ; sender pid : " ;
        Process_id.print_string_of_pid sender formatter ;
        Format.fprintf formatter "}"
    | Unmonitor (mref, sender) ->
        Format.fprintf
          formatter
          "Unmonitor : {monitor reference to unmonitor : " ;
        print_string_of_monitor_ref mref formatter ;
        Format.fprintf formatter " ; sender pid : " ;
        Process_id.print_string_of_pid sender formatter ;
        Format.fprintf formatter "}"
    | Proc_result (pid, recv_pid) ->
        Format.fprintf formatter "Proc result {spawned pid : " ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter " ; receiver pid : " ;
        Process_id.print_string_of_pid recv_pid formatter ;
        Format.fprintf formatter "}"
    | Spawn_monitor_result (monitor_msg, monitor_res, receiver) ->
        Format.fprintf formatter "Spawn and monitor result {monitor message : " ;
        Potpourri.map_default_option
          (fun m -> print_string_of_message m formatter)
          ()
          monitor_msg ;
        Format.fprintf formatter " ; monitor result : " ;
        print_string_of_monitor_ref monitor_res formatter ;
        Format.fprintf formatter " : receiver pid : " ;
        Process_id.print_string_of_pid receiver formatter ;
        Format.fprintf formatter "}"
    | Monitor_result (monitor_msg, monitor_res, receiver) ->
        Format.fprintf formatter "Monitor result {monitor message : " ;
        Potpourri.map_default_option
          (fun m -> print_string_of_message m formatter)
          ()
          monitor_msg ;
        Format.fprintf formatter " ; monitor result : " ;
        print_string_of_monitor_ref monitor_res formatter ;
        Format.fprintf formatter " ; receiver pid : " ;
        Process_id.print_string_of_pid receiver formatter ;
        Format.fprintf formatter "}"
    | Unmonitor_result (mref, pid) ->
        Format.fprintf
          formatter
          "Unmonitor result : {monittor reference to unmonitor: " ;
        print_string_of_monitor_ref mref formatter ;
        Format.fprintf formatter " ; receiver pid : " ;
        Process_id.print_string_of_pid pid formatter ;
        Format.fprintf formatter "}"

  let print_string_of_config (c : node_config) (formatter : Format.formatter) :
      unit =
    match c with
    | Local l ->
        Format.fprintf
          formatter
          "{node type : local ; node name : %s}"
          l.Local_config.node_name
    | Remote r ->
        let print_remote_nodes () =
          Potpourri.pp_list
            ~first:"["
            ~last:"]"
            ~sep:";"
            r.Remote_config.remote_nodes
            (fun (ip, port, name) ->
              Format.sprintf "%s:%d, name : %s" ip port name)
            formatter
        in
        Format.fprintf formatter "{node type : remote ; remote nodes : " ;
        print_remote_nodes () ;
        Format.fprintf
          formatter
          " ; local port : %d ; connection backlog : %d ; node name : %s ; \
           node ip : %s}"
          r.Remote_config.local_port
          r.Remote_config.connection_backlog
          r.Remote_config.node_name
          r.Remote_config.node_ip

  let log_msg (ns : node_state) (level : I.level) ?exn (action : string) ?pid
      (details : unit -> unit) : unit I.t =
    let time_str () =
      let time_float = Unix.gettimeofday () in
      let time_record = Unix.gmtime time_float in
      Format.fprintf
        ns.log_formatter
        "[%d-%02d-%02d-%02d:%02d:%02d:%03.0f]"
        (1900 + time_record.Unix.tm_year)
        (1 + time_record.Unix.tm_mon)
        time_record.Unix.tm_mday
        time_record.Unix.tm_hour
        time_record.Unix.tm_min
        time_record.Unix.tm_sec
        (mod_float (time_float *. 1000.) 1000.)
    in
    let backtrace_str () =
      if Printexc.backtrace_status () then Printexc.get_backtrace () else ""
    in
    let str_of_log_bugger () =
      let str_contents = Buffer.contents ns.log_buffer in
      Buffer.reset ns.log_buffer ;
      str_contents
    in
    let print_log_msg () =
      time_str () ;
      Format.fprintf ns.log_formatter "[Node : " ;
      Node_id.print_string_of_node ns.local_node ns.log_formatter ;
      (match pid with
      | None -> ()
      | Some pid' -> Format.fprintf ns.log_formatter "|Process : %d" pid') ;
      Format.fprintf ns.log_formatter "] [Action : %s] [Details : " action ;
      details () ;
      Format.fprintf ns.log_formatter "]" ;
      match exn with
      | None -> ()
      | Some exn' ->
          Format.fprintf
            ns.log_formatter
            " [Exception : %s] [Backtrace : %s]"
            (Printexc.to_string exn')
            (backtrace_str ())
    in
    I.log level (fun () ->
        print_log_msg () ;
        str_of_log_bugger ())

  let safe_close_channel (ns : node_state)
      (ch : [`Out of I.output_channel | `In of I.input_channel])
      (action : string) (details : unit -> unit) : unit I.t =
    let open I in
    catch
      (fun () ->
        match ch with
        | `Out out_ch -> close_output out_ch
        | `In in_ch -> close_input in_ch)
      (fun e -> log_msg ns Warning ~exn:e action details)

  let at_exit_handler ns () =
    let open I in
    log_msg ns Info "at exit handler" (fun () ->
        Format.fprintf ns.log_formatter "at exit handler started")
    >>= fun () ->
    (match !(ns.node_server) with
    | Some serv ->
        catch
          (fun () -> shutdown_server serv)
          (fun exn ->
            log_msg ns Warning ~exn "node shutting down" (fun () ->
                Format.fprintf
                  ns.log_formatter
                  "error while shutting down server"))
    | None -> return ())
    >>= fun () ->
    (match (!(ns.est_in_ch), !(ns.est_out_ch)) with
    | Some est_in, None ->
        safe_close_channel ns (`In est_in) "node shutting down" (fun () ->
            Format.fprintf
              ns.log_formatter
              "error while closing remote connection")
    | None, Some est_out ->
        safe_close_channel ns (`Out est_out) "node shutting down" (fun () ->
            Format.fprintf
              ns.log_formatter
              "error while closing remote connection")
    | Some est_in, Some est_out ->
        safe_close_channel ns (`In est_in) "node shutting down" (fun () ->
            Format.fprintf
              ns.log_formatter
              "error while closing remote connection")
        >>= fun _ ->
        safe_close_channel ns (`Out est_out) "node shutting down" (fun () ->
            Format.fprintf
              ns.log_formatter
              "error while closing remote connection")
    | _ -> return ())
    >>= fun () ->
    Node_id_hashtbl.fold
      (fun _ out_ch _ ->
        safe_close_channel ns (`Out out_ch) "node shutting down" (fun () ->
            Format.fprintf
              ns.log_formatter
              "error while closing remote connection"))
      ns.remote_nodes
      (return ())
    >>= fun () ->
    log_msg ns Info "at exit handler" (fun () ->
        Format.fprintf ns.log_formatter "at exit handler finished")

  let return (v : 'a) : 'a t = fun (ns, pid) -> I.return (ns, pid, v)

  let ( >>= ) (p : 'a t) (f : 'a -> 'b t) : 'b t =
   fun (ns, pid) -> I.(p (ns, pid) >>= fun (ns', pid', v) -> (f v) (ns', pid'))

  let fail (e : exn) : 'a t = fun _ -> I.fail e

  let catch (p : unit -> 'a t) (handler : exn -> 'a t) : 'a t =
   fun (ns, pid) ->
    I.catch (fun () -> (p ()) (ns, pid)) (fun e -> (handler e) (ns, pid))

  let register (name : string) (f : Process_id.t -> unit -> unit t) : unit t =
   fun (ns, pid) ->
    Hashtbl.add ns.registered_funs name f ;
    I.return (ns, pid, ())

  let lift_io (io_comp : 'a io) : 'a t =
   fun (ns, pid) -> I.(io_comp >>= fun res -> return (ns, pid, res))

  let send_monitor_response (ns : node_state)
      (monitors : Monitor_ref_set.t option)
      (termination_reason : monitor_reason) : unit io =
    let open I in
    let send_monitor_response_local (Monitor_Ref (_, pid, _)) =
      match
        Potpourri.of_option @@ fun () ->
        Hashtbl.find ns.mailboxes (Process_id.get_id pid)
      with
      | None -> return ()
      | Some (_, push_fn) ->
          return @@ push_fn @@ Some (Exit (pid, termination_reason))
    in

    let send_monitor_response_remote
        (Monitor_Ref (_, monitoring_process, monitored_process) as mref) =
      catch
        (fun () ->
          match
            Potpourri.of_option @@ fun () ->
            Node_id_hashtbl.find
              ns.remote_nodes
              (Process_id.get_node monitoring_process)
          with
          | None ->
              log_msg ns Info "sending remote monitor notification" (fun () ->
                  Format.fprintf ns.log_formatter "monitor reference " ;
                  print_string_of_monitor_ref mref ns.log_formatter ;
                  Format.fprintf ns.log_formatter " remote node " ;
                  Node_id.print_string_of_node
                    (Process_id.get_node monitoring_process)
                    ns.log_formatter ;
                  Format.fprintf
                    ns.log_formatter
                    " is down, skipping sending monitor message")
          | Some out_ch ->
              write_value out_ch (Exit (monitored_process, termination_reason))
              >>= fun () ->
              log_msg ns Info "sending remote monitor notification" (fun () ->
                  Format.fprintf
                    ns.log_formatter
                    "sent monitor notification for monitor ref " ;
                  print_string_of_monitor_ref mref ns.log_formatter ;
                  Format.fprintf ns.log_formatter " to remote node " ;
                  Node_id.print_string_of_node
                    (Process_id.get_node monitoring_process)
                    ns.log_formatter))
        (fun e ->
          log_msg
            ns
            ~exn:e
            Error
            "sending remote monitor notification"
            (fun () ->
              Format.fprintf ns.log_formatter "monitor reference " ;
              print_string_of_monitor_ref mref ns.log_formatter ;
              Format.fprintf
                ns.log_formatter
                ", error sending monitor message to remote node " ;
              Node_id.print_string_of_node
                (Process_id.get_node monitoring_process)
                ns.log_formatter ;
              Format.fprintf ns.log_formatter ", removing node")
          >>= fun () ->
          return
          @@ Node_id_hashtbl.remove
               ns.remote_nodes
               (Process_id.get_node monitoring_process))
    in

    let iter_fn (Monitor_Ref (_, pid, _) as mref) _ =
      if Process_id.is_local pid ns.local_node then
        send_monitor_response_local mref >>= fun () ->
        log_msg ns Debug "sent local monitor notification" (fun () ->
            print_string_of_monitor_notification
              mref
              termination_reason
              ns.log_formatter)
      else
        log_msg ns Debug "start sending remote monitor notification" (fun () ->
            Format.fprintf ns.log_formatter "monitor reference : " ;
            print_string_of_monitor_notification
              mref
              termination_reason
              ns.log_formatter)
        >>= fun () ->
        send_monitor_response_remote mref >>= fun () ->
        log_msg
          ns
          Debug
          "finished sending remote monitor notification"
          (fun () ->
            Format.fprintf ns.log_formatter "monitor reference : " ;
            print_string_of_monitor_notification
              mref
              termination_reason
              ns.log_formatter)
    in

    match monitors with
    | None -> return ()
    | Some monitors' -> Monitor_ref_set.fold iter_fn monitors' (return ())

  let run_process' (ns : node_state) (pid : Process_id.t) (p : unit t) : unit io
      =
    let open I in
    catch
      (fun () ->
        log_msg ns Debug "starting process" (fun () ->
            Process_id.print_string_of_pid pid ns.log_formatter)
        >>= fun () ->
        p (ns, pid) >>= fun _ ->
        log_msg ns Debug "process terminated successfully" (fun () ->
            Process_id.print_string_of_pid pid ns.log_formatter)
        >>= fun () ->
        send_monitor_response
          ns
          ( Potpourri.of_option @@ fun () ->
            Process_id_hashtbl.find ns.monitor_table pid )
          (Normal pid)
        >>= fun () ->
        Process_id_hashtbl.remove ns.monitor_table pid ;
        return @@ Hashtbl.remove ns.mailboxes (Process_id.get_id pid))
      (fun e ->
        log_msg ns ~exn:e Error "process failed with error" (fun () ->
            Process_id.print_string_of_pid pid ns.log_formatter)
        >>= fun () ->
        Hashtbl.remove ns.mailboxes (Process_id.get_id pid) ;
        (match e with
        | InvalidNode n ->
            send_monitor_response
              ns
              ( Potpourri.of_option @@ fun () ->
                Process_id_hashtbl.find ns.monitor_table pid )
              (UnkownNodeId (pid, n))
        | _ ->
            send_monitor_response
              ns
              ( Potpourri.of_option @@ fun () ->
                Process_id_hashtbl.find ns.monitor_table pid )
              (Exception (pid, e)))
        >>= fun () -> return @@ Process_id_hashtbl.remove ns.monitor_table pid)

  let sync_send pid ns ?flags out_ch msg_create_fn response_fn =
    let open I in
    let remote_config = Potpourri.get_option !(ns.config) in
    let new_pid =
      Process_id.make_remote
        remote_config.Remote_config.node_ip
        remote_config.Remote_config.local_port
        remote_config.Remote_config.node_name
        ns.next_process_id
    in
    let new_mailbox, push_fn = I.create_stream () in
    Hashtbl.replace
      ns.mailboxes
      (Process_id.get_id new_pid)
      (new_mailbox, push_fn) ;
    let msg_to_send = msg_create_fn new_pid in
    log_msg ns ~pid Debug "sync send start" (fun () ->
        Format.fprintf ns.log_formatter "created new process " ;
        Process_id.print_string_of_pid new_pid ns.log_formatter ;
        Format.fprintf ns.log_formatter " for sync send of " ;
        print_string_of_message msg_to_send ns.log_formatter)
    >>= fun () ->
    write_value out_ch ?flags msg_to_send >>= fun () ->
    get new_mailbox >>= fun result_pid ->
    Hashtbl.remove ns.mailboxes (Process_id.get_id new_pid) ;
    log_msg ns ~pid Debug "sync send end" (fun () ->
        Format.fprintf ns.log_formatter "process " ;
        Process_id.print_string_of_pid new_pid ns.log_formatter ;
        Format.fprintf ns.log_formatter " finished for sync send of " ;
        print_string_of_message msg_to_send ns.log_formatter)
    >>= fun () -> response_fn (Potpourri.get_option result_pid)
  (* we do not send None on mailboxes *)

  let monitor_helper (ns : node_state) (monitor_pid : Process_id.t)
      (monitee_pid : Process_id.t) : message option * monitor_ref =
    let new_monitor_ref =
      Monitor_Ref
        (Potpourri.post_incr ns.monitor_ref_id, monitor_pid, monitee_pid)
    in
    match
      Potpourri.of_option @@ fun () ->
      Hashtbl.find ns.mailboxes (Process_id.get_id monitee_pid)
    with
    | None -> (Some (Exit (monitee_pid, NoProcess monitee_pid)), new_monitor_ref)
    | Some _ ->
        (match
           Potpourri.of_option @@ fun () ->
           Process_id_hashtbl.find ns.monitor_table monitee_pid
         with
        | None ->
            Process_id_hashtbl.add
              ns.monitor_table
              monitee_pid
              (Monitor_ref_set.of_list [new_monitor_ref])
        | Some curr_monitor_set ->
            Process_id_hashtbl.replace
              ns.monitor_table
              monitee_pid
              (Monitor_ref_set.add new_monitor_ref curr_monitor_set)) ;
        (None, new_monitor_ref)

  let monitor_response_handler (ns : node_state)
      (res : message option * monitor_ref) : monitor_ref =
    match res with
    | Some msg, (Monitor_Ref (_, monitor_pid, _) as mref) ->
        let _, push_fn =
          Hashtbl.find ns.mailboxes (Process_id.get_id monitor_pid)
        in
        (* process is currently running so mailbox must be present *)
        push_fn @@ Some msg ;
        mref
    | None, (Monitor_Ref (_, _, monitee_pid) as mref) ->
        (match
           Potpourri.of_option @@ fun () ->
           Process_id_hashtbl.find ns.monitor_table monitee_pid
         with
        | None ->
            Process_id_hashtbl.add
              ns.monitor_table
              monitee_pid
              (Monitor_ref_set.of_list [mref])
        | Some curr_monitor_set ->
            Process_id_hashtbl.replace
              ns.monitor_table
              monitee_pid
              (Monitor_ref_set.add mref curr_monitor_set)) ;
        mref

  let monitor_local (ns : node_state) (monitor_pid : Process_id.t)
      (monitee_pid : Process_id.t) : monitor_ref =
    monitor_response_handler ns @@ monitor_helper ns monitor_pid monitee_pid

  let make_new_pid (node_to_spwan_on : Node_id.t) (ns : node_state) :
      Process_id.t =
    if !(ns.config) = None then
      Process_id.make_local
        (Node_id.get_name node_to_spwan_on)
        ns.next_process_id
    else
      let remote_config = Potpourri.get_option !(ns.config) in
      Process_id.make_remote
        remote_config.Remote_config.node_ip
        remote_config.Remote_config.local_port
        remote_config.Remote_config.node_name
        ns.next_process_id

  let spawn ?(monitor = false) (node_id : Node_id.t) (p : proc_rep)
      (pid_to_send : Process_id.t) : (Process_id.t * monitor_ref option) t =
    let open I in
    fun (ns, pid) ->
      let proc = p in
      let p =
        match p with
        | Fun p -> fun _ -> p
        | Registered n -> Hashtbl.find ns.registered_funs n
      in
      if Node_id.is_local node_id ns.local_node then (
        let new_pid = make_new_pid node_id ns in
        Hashtbl.replace
          ns.mailboxes
          (Process_id.get_id new_pid)
          (I.create_stream ()) ;
        if monitor then (
          let monitor_res = monitor_local ns pid new_pid in
          async (fun () -> run_process' ns new_pid (p pid ())) ;
          log_msg
            ~pid:(Process_id.get_id pid)
            ns
            Debug
            "spawned and monitored local process"
            (fun () ->
              Format.fprintf ns.log_formatter "result pid " ;
              Process_id.print_string_of_pid new_pid ns.log_formatter ;
              Format.fprintf ns.log_formatter ", result monitor reference : " ;
              print_string_of_monitor_ref monitor_res ns.log_formatter)
          >>= fun () -> return (ns, pid, (new_pid, Some monitor_res)))
        else (
          async (fun () -> run_process' ns new_pid (p pid ())) ;
          log_msg
            ~pid:(Process_id.get_id pid)
            ns
            Debug
            "spawned local process"
            (fun () ->
              Format.fprintf ns.log_formatter "result pid " ;
              Process_id.print_string_of_pid new_pid ns.log_formatter)
          >>= fun () -> return (ns, pid, (new_pid, None))))
      else
        match
          Potpourri.of_option @@ fun () ->
          Node_id_hashtbl.find ns.remote_nodes node_id
        with
        | Some out_ch ->
            if monitor then
              log_msg
                ~pid:(Process_id.get_id pid)
                ns
                Debug
                "spawning and monitoring remote process"
                (fun () ->
                  Format.fprintf ns.log_formatter "on remote node " ;
                  Node_id.print_string_of_node node_id ns.log_formatter ;
                  Format.fprintf ns.log_formatter ", local process " ;
                  Process_id.print_string_of_pid pid ns.log_formatter)
              >>= fun () ->
              sync_send
                (Process_id.get_id pid)
                ns
                ~flags:[Marshal.Closures]
                out_ch
                (fun receiver_pid ->
                  Spawn_monitor (proc, pid, receiver_pid, pid_to_send))
                (fun res ->
                  let (Monitor_Ref (_, _, monitored_proc) as mref) =
                    match res with
                    | Spawn_monitor_result (_, mr, _) -> mr
                    | _ -> assert false
                  in
                  (*BISECT-IGNORE*)
                  log_msg
                    ~pid:(Process_id.get_id pid)
                    ns
                    Debug
                    "spawned and monitored remote process"
                    (fun () ->
                      Format.fprintf ns.log_formatter "spawned on remote node " ;
                      Node_id.print_string_of_node node_id ns.log_formatter ;
                      Format.fprintf ns.log_formatter " : result pid " ;
                      Process_id.print_string_of_pid
                        monitored_proc
                        ns.log_formatter ;
                      Format.fprintf
                        ns.log_formatter
                        ", result monitor reference : " ;
                      print_string_of_monitor_ref mref ns.log_formatter)
                  >>= fun () -> return (ns, pid, (monitored_proc, Some mref)))
            else
              log_msg
                ~pid:(Process_id.get_id pid)
                ns
                Debug
                "spawning remote process"
                (fun () ->
                  Format.fprintf ns.log_formatter "on remote node " ;
                  Node_id.print_string_of_node node_id ns.log_formatter ;
                  Format.fprintf ns.log_formatter ", local process " ;
                  Process_id.print_string_of_pid pid ns.log_formatter)
              >>= fun () ->
              sync_send
                (Process_id.get_id pid)
                ns
                ~flags:[Marshal.Closures]
                out_ch
                (fun receiver_pid -> Proc (proc, receiver_pid, pid_to_send))
                (fun res ->
                  let remote_proc_pid =
                    match res with Proc_result (r, _) -> r | _ -> assert false
                  in
                  (*BISECT-IGNORE*)
                  log_msg
                    ~pid:(Process_id.get_id pid)
                    ns
                    Debug
                    "spawned remote process"
                    (fun () ->
                      Format.fprintf ns.log_formatter "on remote node " ;
                      Node_id.print_string_of_node node_id ns.log_formatter ;
                      Format.fprintf ns.log_formatter " : result pid " ;
                      Process_id.print_string_of_pid
                        remote_proc_pid
                        ns.log_formatter)
                  >>= fun () -> return (ns, pid, (remote_proc_pid, None)))
        | None ->
            log_msg
              ~pid:(Process_id.get_id pid)
              ns
              Error
              "failed to spawn process on remote node"
              (fun () ->
                Format.fprintf ns.log_formatter "remote node " ;
                Node_id.print_string_of_node node_id ns.log_formatter ;
                Format.fprintf ns.log_formatter ", is unknown, local process " ;
                Process_id.print_string_of_pid pid ns.log_formatter)
            >>= fun () -> fail @@ InvalidNode node_id

  let case (match_fn : message_type -> (unit -> 'a t) option) : 'a matcher_list
      =
    let matcher = function Data (_, _, msg) -> match_fn msg | _ -> None in
    Matcher matcher

  let termination_case (handler_fn : monitor_reason -> 'a t) : 'a matcher_list =
    let matcher = function
      | Exit (_, reason) -> Some (fun () -> handler_fn reason)
      | _ -> None
    in
    Matcher matcher

  let rec ( |. ) (first_matchers : 'a matcher_list)
      (second_matchers : 'a matcher_list) : 'a matcher_list =
    match (first_matchers, second_matchers) with
    | Matcher matcher, Matcher matcher' -> Matchers (matcher, Matcher matcher')
    | Matcher matcher, Matchers (matcher', matchers) ->
        Matchers (matcher, Matchers (matcher', matchers))
    | Matchers (matcher, matchers), matchers' ->
        Matchers (matcher, matchers |. matchers')

  let receive ?timeout_duration (matchers : 'a matcher_list) : 'a option t =
    let open I in
    let temp_stream, temp_push_fn = create_stream () in
    let result = ref None in
    let mailbox_cleaned_up = ref false in

    let restore_mailbox ns pid =
      mailbox_cleaned_up := true ;
      let mailbox', old_push_fn =
        Hashtbl.find ns.mailboxes (Process_id.get_id pid)
      in
      temp_push_fn None ;
      (* close new stream so we can append new and old *)
      Hashtbl.replace
        ns.mailboxes
        (Process_id.get_id pid)
        (stream_append temp_stream mailbox', old_push_fn)
    in

    let test_match ns pid matcher candidate_msg no_match_fn =
      match matcher candidate_msg with
      | None -> no_match_fn ()
      | Some fn ->
          restore_mailbox ns pid ;
          result := Some (fn ()) ;
          true
    in

    let rec iter_fn ns pid match_fns candidate_msg =
      match match_fns with
      | Matcher matcher ->
          test_match ns pid matcher candidate_msg (fun () ->
              temp_push_fn (Some candidate_msg) ;
              false)
      | Matchers (matcher, xs) ->
          test_match ns pid matcher candidate_msg (fun () ->
              iter_fn ns pid xs candidate_msg)
    in

    let rec iter_stream iter_fn stream =
      get stream >>= fun v ->
      if iter_fn (Potpourri.get_option v) then return ()
      else iter_stream iter_fn stream
    in

    (* a None is never sent, see send function below. *)
    let do_receive_blocking (ns, pid) =
      let mailbox, _ = Hashtbl.find ns.mailboxes (Process_id.get_id pid) in
      iter_stream (iter_fn ns pid matchers) mailbox >>= fun () ->
      (Potpourri.get_option !result) (ns, pid) >>= fun (ns', pid', result') ->
      return (ns', pid', Some result')
    in

    fun (ns, pid) ->
      match timeout_duration with
      | None ->
          catch
            (fun () ->
              log_msg
                ~pid:(Process_id.get_id pid)
                ns
                Debug
                "receiving with no time out"
                (fun () ->
                  Format.fprintf ns.log_formatter "receiver process " ;
                  Process_id.print_string_of_pid pid ns.log_formatter)
              >>= fun () ->
              do_receive_blocking (ns, pid) >>= fun (ns', pid', res) ->
              log_msg
                ~pid:(Process_id.get_id pid)
                ns
                Debug
                "successfully received and processed message with no time out"
                (fun () ->
                  Format.fprintf ns.log_formatter "receiver process " ;
                  Process_id.print_string_of_pid pid ns.log_formatter)
              >>= fun () -> return (ns', pid', res))
            (fun e ->
              if not !mailbox_cleaned_up then restore_mailbox ns pid else () ;
              log_msg
                ~pid:(Process_id.get_id pid)
                ns
                ~exn:e
                Error
                "receiving with no time out failed"
                (fun () ->
                  Format.fprintf ns.log_formatter "receiver process " ;
                  Process_id.print_string_of_pid pid ns.log_formatter ;
                  Format.fprintf ns.log_formatter ", encountred exception")
              >>= fun () -> fail e)
      | Some timeout_duration' ->
          log_msg
            ~pid:(Process_id.get_id pid)
            ns
            Debug
            "receiving with time out"
            (fun () ->
              Format.fprintf ns.log_formatter "receiver process " ;
              Process_id.print_string_of_pid pid ns.log_formatter ;
              Format.fprintf ns.log_formatter ", time out %f" timeout_duration')
          >>= fun () ->
          catch
            (fun () ->
              pick [do_receive_blocking (ns, pid); timeout timeout_duration']
              >>= fun (ns', pid', res) ->
              log_msg
                ns
                ~pid:(Process_id.get_id pid)
                Debug
                "successfully received and processed a message with time out"
                (fun () ->
                  Format.fprintf ns.log_formatter "receiver process " ;
                  Process_id.print_string_of_pid pid ns.log_formatter ;
                  Format.fprintf
                    ns.log_formatter
                    ", time out %f"
                    timeout_duration')
              >>= fun () -> return (ns', pid', res))
            (fun e ->
              if not !mailbox_cleaned_up then restore_mailbox ns pid else () ;
              match e with
              | Timeout ->
                  log_msg
                    ~pid:(Process_id.get_id pid)
                    ns
                    Debug
                    "receive timed out"
                    (fun () ->
                      Format.fprintf ns.log_formatter "receiver process " ;
                      Process_id.print_string_of_pid pid ns.log_formatter ;
                      Format.fprintf
                        ns.log_formatter
                        ", time out %f"
                        timeout_duration')
                  >>= fun () -> return (ns, pid, None)
              | e ->
                  log_msg
                    ~pid:(Process_id.get_id pid)
                    ns
                    ~exn:e
                    Error
                    "receiving with time out failed"
                    (fun () ->
                      Format.fprintf ns.log_formatter "receiver process " ;
                      Process_id.print_string_of_pid pid ns.log_formatter ;
                      Format.fprintf
                        ns.log_formatter
                        ", time out %f"
                        timeout_duration')
                  >>= fun () -> fail e)

  let rec receive_loop ?timeout_duration (matchers : bool matcher_list) : unit t
      =
    let open I in
    fun (ns, pid) ->
      (receive ?timeout_duration matchers) (ns, pid) >>= fun (ns', pid', res) ->
      match res with
      | None | Some false -> return (ns', pid', ())
      | Some true -> (receive_loop ?timeout_duration matchers) (ns', pid')

  let send_to_remote_node_helper (pid : int) (ns : node_state)
      (node : Node_id.t) (sending_log_action : string)
      (print_sending_log_msg : unit -> unit)
      (print_unknown_node_msg : unit -> unit) (msg : message) : unit I.t =
    let open I in
    match
      Potpourri.of_option @@ fun () -> Node_id_hashtbl.find ns.remote_nodes node
    with
    | Some remote_output ->
        log_msg ns ~pid Debug sending_log_action (fun () ->
            print_sending_log_msg ())
        >>= fun () ->
        write_value
          ~flags:[Marshal.Closures]
          remote_output
          msg (* marshal because the message could be a function *)
    | None ->
        log_msg ns ~pid Error sending_log_action (fun () ->
            print_unknown_node_msg ())
        >>= fun () -> fail @@ InvalidNode node

  let send (remote_pid : Process_id.t) (msg : message_type) : unit t =
    let open I in
    fun (ns, pid) ->
      if Process_id.is_local remote_pid ns.local_node then
        match
          Potpourri.of_option @@ fun () ->
          Hashtbl.find ns.mailboxes (Process_id.get_id remote_pid)
        with
        | None ->
            log_msg
              ns
              ~pid:(Process_id.get_id pid)
              I.Warning
              "unable to send message to local process"
              (fun () ->
                Format.fprintf
                  ns.log_formatter
                  "message : %s, to unknown local process: "
                  (M.string_of_message msg) ;
                Process_id.print_string_of_pid remote_pid ns.log_formatter ;
                Format.fprintf ns.log_formatter ", from local process: " ;
                Process_id.print_string_of_pid pid ns.log_formatter)
            >>= fun () -> return (ns, pid, ())
        | Some (_, push_fn) ->
            log_msg
              ns
              ~pid:(Process_id.get_id pid)
              I.Debug
              "successfully sent message to local process"
              (fun () ->
                Format.fprintf
                  ns.log_formatter
                  "message : %s, to local process: "
                  (M.string_of_message msg) ;
                Process_id.print_string_of_pid remote_pid ns.log_formatter ;
                Format.fprintf ns.log_formatter ", from local process: " ;
                Process_id.print_string_of_pid pid ns.log_formatter)
            >>= fun () ->
            return @@ (ns, pid, push_fn @@ Some (Data (pid, remote_pid, msg)))
      else
        let sending_msg () =
          Format.fprintf
            ns.log_formatter
            "message : %s, to remote process: "
            (M.string_of_message msg) ;
          Process_id.print_string_of_pid remote_pid ns.log_formatter ;
          Format.fprintf ns.log_formatter ", from local process: " ;
          Process_id.print_string_of_pid pid ns.log_formatter
        in
        let unknown_node_msg () =
          Format.fprintf
            ns.log_formatter
            "message : %s, to unknown remote process: "
            (M.string_of_message msg) ;
          Process_id.print_string_of_pid remote_pid ns.log_formatter ;
          Format.fprintf ns.log_formatter ", from local process: " ;
          Process_id.print_string_of_pid pid ns.log_formatter
        in
        send_to_remote_node_helper
          (Process_id.get_id pid)
          ns
          (Process_id.get_node remote_pid)
          "sending message to remote process"
          sending_msg
          unknown_node_msg
          (Data (pid, remote_pid, msg))
        >>= fun () ->
        log_msg
          ns
          ~pid:(Process_id.get_id pid)
          I.Debug
          "successfully sent message to remote process"
          (fun () ->
            Format.fprintf
              ns.log_formatter
              "message : %s, to remote process: "
              (M.string_of_message msg) ;
            Process_id.print_string_of_pid remote_pid ns.log_formatter ;
            Format.fprintf ns.log_formatter ", from local process: " ;
            Process_id.print_string_of_pid pid ns.log_formatter)
        >>= fun () -> return (ns, pid, ())

  let ( >! ) (pid : Process_id.t) (msg : message_type) : unit t = send pid msg

  let broadcast_local ?pid (ns : node_state) (sending_pid : Process_id.t)
      (m : message_type) : unit io =
    let open I in
    Hashtbl.fold
      (fun recev_pid (_, push_fn) _ ->
        let recev_pid' = Process_id.make ns.local_node recev_pid in
        if recev_pid' = sending_pid then return ()
        else
          log_msg ?pid ns I.Debug "broadcast" (fun () ->
              Format.fprintf
                ns.log_formatter
                "sending message %s to local process "
                (M.string_of_message m) ;
              Process_id.print_string_of_pid recev_pid' ns.log_formatter ;
              Format.fprintf ns.log_formatter " from process " ;
              Process_id.print_string_of_pid sending_pid ns.log_formatter ;
              Format.fprintf ns.log_formatter " as result of broadcast request")
          >>= fun () ->
          return @@ push_fn @@ Some (Data (sending_pid, recev_pid', m)))
      ns.mailboxes
      (return ())

  let broadcast (node : Node_id.t) (m : message_type) : unit t =
    let open I in
    fun (ns, pid) ->
      if Node_id.is_local node ns.local_node then
        log_msg ~pid:(Process_id.get_id pid) ns I.Debug "broadcast" (fun () ->
            Format.fprintf
              ns.log_formatter
              "sending broadcast message %s to local processes running on \
               local node "
              (M.string_of_message m) ;
            Node_id.print_string_of_node node ns.log_formatter ;
            Format.fprintf ns.log_formatter " from local process " ;
            Process_id.print_string_of_pid pid ns.log_formatter)
        >>= fun () ->
        broadcast_local ns pid m >>= fun () -> return (ns, pid, ())
      else
        let sending_msg () =
          Format.fprintf ns.log_formatter "Process " ;
          Process_id.print_string_of_pid pid ns.log_formatter ;
          Format.fprintf
            ns.log_formatter
            " is sending broadcast message %s to remote node "
            (M.string_of_message m) ;
          Node_id.print_string_of_node node ns.log_formatter
        in
        let unknwon_node_msg () =
          Format.fprintf ns.log_formatter "Process " ;
          Process_id.print_string_of_pid pid ns.log_formatter ;
          Format.fprintf
            ns.log_formatter
            " failed to send broadcast message %s to remote node "
            (M.string_of_message m) ;
          Node_id.print_string_of_node node ns.log_formatter ;
          Format.fprintf ns.log_formatter ", remote node is unknown"
        in
        send_to_remote_node_helper
          (Process_id.get_id pid)
          ns
          node
          "broadcasting to remote node"
          sending_msg
          unknwon_node_msg
          (Broadcast (pid, node, m))
        >>= fun () ->
        log_msg
          ns
          ~pid:(Process_id.get_id pid)
          I.Debug
          "successfully sent broadcast message to remote node"
          (fun () ->
            Format.fprintf
              ns.log_formatter
              "message : %s, to remote node: "
              (M.string_of_message m) ;
            Node_id.print_string_of_node node ns.log_formatter)
        >>= fun () -> return (ns, pid, ())

  let lookup_node_and_send (pid : int) (ns : node_state)
      (receiver_process : Process_id.t) (action : string)
      (unknown_node_msg : unit -> unit)
      (node_found_fn : I.output_channel -> 'a I.t) : 'a I.t =
    let open I in
    match
      Potpourri.of_option @@ fun () ->
      Node_id_hashtbl.find
        ns.remote_nodes
        (Process_id.get_node @@ receiver_process)
    with
    | None ->
        log_msg ~pid ns Error action (fun () -> unknown_node_msg ())
        >>= fun () -> fail @@ InvalidNode (Process_id.get_node receiver_process)
    | Some out_ch -> node_found_fn out_ch

  let monitor (pid_to_monitor : Process_id.t) : monitor_ref t =
   fun (ns, pid) ->
    let open I in
    if Process_id.is_local pid_to_monitor ns.local_node then
      log_msg ~pid:(Process_id.get_id pid) ns Debug "monitored" (fun () ->
          Format.fprintf ns.log_formatter "Creating monitor for local process " ;
          Process_id.print_string_of_pid pid_to_monitor ns.log_formatter ;
          Format.fprintf ns.log_formatter " to be monitored by local process " ;
          Process_id.print_string_of_pid pid ns.log_formatter)
      >>= fun () -> return (ns, pid, monitor_local ns pid pid_to_monitor)
    else
      log_msg ~pid:(Process_id.get_id pid) ns Debug "monitoring" (fun () ->
          Format.fprintf ns.log_formatter "Creating monitor for remote process " ;
          Process_id.print_string_of_pid pid_to_monitor ns.log_formatter ;
          Format.fprintf ns.log_formatter " to be monitored by local process " ;
          Process_id.print_string_of_pid pid ns.log_formatter)
      >>= fun () ->
      let unknown_mode_msg () =
        Format.fprintf ns.log_formatter "Process " ;
        Process_id.print_string_of_pid pid ns.log_formatter ;
        Format.fprintf ns.log_formatter " failed to monitor remote process " ;
        Process_id.print_string_of_pid pid_to_monitor ns.log_formatter ;
        Format.fprintf ns.log_formatter " on remote node " ;
        Node_id.print_string_of_node
          (Process_id.get_node pid_to_monitor)
          ns.log_formatter ;
        Format.fprintf ns.log_formatter ", remote node is unknown"
      in
      let node_found_fn out_ch =
        sync_send
          (Process_id.get_id pid)
          ns
          out_ch
          (fun receiver_pid -> Monitor (pid, pid_to_monitor, receiver_pid))
          (fun res ->
            let res' =
              match res with
              | Monitor_result (mon_msg, mon_res, _) -> (mon_msg, mon_res)
              | _ -> assert false
            in
            (*BISECT-IGNORE*)
            log_msg
              ~pid:(Process_id.get_id pid)
              ns
              I.Debug
              "successfully monitored remote process"
              (fun () ->
                Format.fprintf ns.log_formatter "result: " ;
                print_string_of_message res ns.log_formatter)
            >>= fun () -> return (ns, pid, monitor_response_handler ns res'))
      in
      lookup_node_and_send
        (Process_id.get_id pid)
        ns
        pid_to_monitor
        "monitoring"
        unknown_mode_msg
        node_found_fn

  let unmonitor_local (ns : node_state)
      (Monitor_Ref (_, _, process_to_unmonitor) as mref) : unit =
    match
      Potpourri.of_option @@ fun () ->
      Process_id_hashtbl.find ns.monitor_table process_to_unmonitor
    with
    | None -> ()
    | Some curr_set ->
        let curr_set' = Monitor_ref_set.remove mref curr_set in
        if Monitor_ref_set.is_empty curr_set' then
          Process_id_hashtbl.remove ns.monitor_table process_to_unmonitor
        else
          Process_id_hashtbl.replace
            ns.monitor_table
            process_to_unmonitor
            curr_set'

  let unmonitor (Monitor_Ref (_, _, process_to_unmonitor) as mref) : unit t =
    let open I in
    fun (ns, pid) ->
      if Process_id.is_local process_to_unmonitor ns.local_node then
        log_msg ns ~pid:(Process_id.get_id pid) Debug "unmonitored" (fun () ->
            Format.fprintf ns.log_formatter "Unmonitor local : " ;
            print_string_of_monitor_ref mref ns.log_formatter)
        >>= fun () -> return (ns, pid, unmonitor_local ns mref)
      else
        log_msg ns ~pid:(Process_id.get_id pid) Debug "unmonitoring" (fun () ->
            Format.fprintf ns.log_formatter "Unmonitor remote : " ;
            print_string_of_monitor_ref mref ns.log_formatter)
        >>= fun () ->
        let unknown_node_msg () =
          Format.fprintf ns.log_formatter "Process " ;
          Process_id.print_string_of_pid pid ns.log_formatter ;
          Format.fprintf ns.log_formatter " failed to monitor remote process " ;
          Process_id.print_string_of_pid process_to_unmonitor ns.log_formatter ;
          Format.fprintf ns.log_formatter " on remote node " ;
          Node_id.print_string_of_node
            (Process_id.get_node process_to_unmonitor)
            ns.log_formatter ;
          Format.fprintf ns.log_formatter ", remote node is unknown"
        in
        let node_found_fn out_ch =
          sync_send
            (Process_id.get_id pid)
            ns
            out_ch
            (fun recv_pid -> Unmonitor (mref, recv_pid))
            (fun _ ->
              log_msg
                ~pid:(Process_id.get_id pid)
                ns
                Debug
                "successfully unmonitored"
                (fun () ->
                  Format.fprintf ns.log_formatter "monitor ref : " ;
                  print_string_of_monitor_ref mref ns.log_formatter)
              >>= fun () -> return (ns, pid, ()))
        in
        lookup_node_and_send
          (Process_id.get_id pid)
          ns
          process_to_unmonitor
          "unmonitoring"
          unknown_node_msg
          node_found_fn

  let get_self_pid : Process_id.t t =
   fun (ns, proc_id) -> I.return (ns, proc_id, proc_id)

  let get_self_node : Node_id.t t =
   fun (ns, pid) -> I.return (ns, pid, Process_id.get_node pid)

  let get_remote_node node_name (ns, pid) =
    let res : Node_id.t option ref = ref None in
    let iter_fn node _ =
      if Node_id.get_name node = node_name && !res = None then res := Some node
      else ()
    in
    Node_id_hashtbl.iter iter_fn ns.remote_nodes ;
    I.return (ns, pid, !res)

  let get_remote_nodes : Node_id.t list t =
   fun (ns, pid) ->
    let res : Node_id.t list =
      Node_id_hashtbl.fold (fun n _ acc -> n :: acc) ns.remote_nodes []
    in
    I.return (ns, pid, res)

  let clean_up_node_connection ns node in_ch out_ch =
    let open I in
    let out_details () =
      if node <> None then (
        Format.fprintf
          ns.log_formatter
          "encountered error while closing output channel for remote node " ;
        Node_id.print_string_of_node
          (Potpourri.get_option node)
          ns.log_formatter)
      else
        Format.fprintf
          ns.log_formatter
          "encountered error while closing output channel"
    in
    let in_details () =
      if node <> None then (
        Format.fprintf
          ns.log_formatter
          "encountered error while closing input channel for remote node " ;
        Node_id.print_string_of_node
          (Potpourri.get_option node)
          ns.log_formatter)
      else
        Format.fprintf
          ns.log_formatter
          "encountered error while closing input channel"
    in
    safe_close_channel ns (`In in_ch) "node connection clean up" out_details
    >>= fun () ->
    safe_close_channel ns (`Out out_ch) "node connection clean up" in_details
    >>= fun () ->
    if node = None then return ()
    else
      return
      @@ Node_id_hashtbl.remove ns.remote_nodes (Potpourri.get_option node)

  let rec wait_for_node_msg ns in_ch out_ch client_addr node_ref =
    let open I in
    let print_string_of_client_addr () =
      match client_addr with
      | Unix.ADDR_UNIX s -> Format.fprintf ns.log_formatter "%s" s
      | Unix.ADDR_INET (ip, port) ->
          Format.fprintf
            ns.log_formatter
            "%s:%d"
            (Unix.string_of_inet_addr ip)
            port
    in

    read_value in_ch >>= fun (msg : message) ->
    log_msg ns Debug "node process message" (fun () ->
        Format.fprintf ns.log_formatter "received message " ;
        print_string_of_message msg ns.log_formatter ;
        Format.fprintf ns.log_formatter " from " ;
        print_string_of_client_addr ())
    >>= fun () ->
    match msg with
    | Node node ->
        node_ref := Some node ;
        Node_id_hashtbl.replace ns.remote_nodes node out_ch ;
        return node
    | _ ->
        log_msg ns Debug "node process message" (fun () ->
            Format.fprintf ns.log_formatter "ignore message " ;
            print_string_of_message msg ns.log_formatter ;
            Format.fprintf ns.log_formatter ", waiting for handshake")
        >>= fun () -> wait_for_node_msg ns in_ch out_ch client_addr node_ref

  let server_handler (ns : node_state)
      ((in_ch, out_ch) : I.input_channel * I.output_channel) (node : Node_id.t)
      : unit I.t =
    let open I in
    let remote_config = Potpourri.get_option !(ns.config) in

    let spawn_preamble () =
      let new_pid =
        Process_id.make_remote
          remote_config.Remote_config.node_ip
          remote_config.Remote_config.local_port
          remote_config.Remote_config.node_name
          ns.next_process_id
      in
      Hashtbl.replace
        ns.mailboxes
        (Process_id.get_id new_pid)
        (I.create_stream ()) ;
      new_pid
    in

    let put_in_mailbox receiver_pid msg =
      match
        Potpourri.of_option @@ fun () ->
        Hashtbl.find ns.mailboxes (Process_id.get_id receiver_pid)
      with
      | None ->
          let receiver_not_found_err_msg () =
            Format.fprintf ns.log_formatter "remote node " ;
            Node_id.print_string_of_node node ns.log_formatter ;
            Format.fprintf ns.log_formatter ", processed message " ;
            print_string_of_message msg ns.log_formatter ;
            Format.fprintf ns.log_formatter ", recipient unknown local process " ;
            Process_id.print_string_of_pid receiver_pid ns.log_formatter
          in
          log_msg ns I.Warning "node process message" receiver_not_found_err_msg
      | Some (_, push_fn) -> return @@ push_fn (Some msg)
    in

    let rec handler () =
      let log_handler_stop msg =
        let node_str () = Node_id.print_string_of_node node ns.log_formatter in
        log_msg ns Error "node process message" (fun () ->
            Format.fprintf ns.log_formatter "remote node " ;
            node_str () ;
            Format.fprintf
              ns.log_formatter
              " %s, stopping handler for remote node "
              msg ;
            node_str ())
      in
      if
        ( Potpourri.of_option @@ fun () ->
          Node_id_hashtbl.find ns.remote_nodes node )
        = None
      then log_handler_stop "has been previously removed"
      else
        read_value in_ch >>= fun (msg : message) ->
        log_msg ns Debug "node process message" (fun () ->
            Format.fprintf ns.log_formatter "remote node " ;
            Node_id.print_string_of_node node ns.log_formatter ;
            Format.fprintf ns.log_formatter ", message " ;
            print_string_of_message msg ns.log_formatter)
        >>= fun () ->
        match msg with
        | Node _ -> handler ()
        | Proc (p, sender_pid, pid_to_send) ->
            let result_pid = spawn_preamble () in
            write_value out_ch (Proc_result (result_pid, sender_pid))
            >>= fun () ->
            let p =
              match p with
              | Fun p -> fun _ -> p
              | Registered n -> Hashtbl.find ns.registered_funs n
            in
            async (fun () -> run_process' ns result_pid (p pid_to_send ())) ;
            handler ()
        | Spawn_monitor (p, monitor_pid, sender, pid_to_send) ->
            let new_pid = spawn_preamble () in
            let monitor_msg, monitor_res =
              monitor_helper ns monitor_pid new_pid
            in
            write_value
              out_ch
              (Spawn_monitor_result (monitor_msg, monitor_res, sender))
            >>= fun () ->
            let p =
              match p with
              | Fun p -> fun _ -> p
              | Registered n -> Hashtbl.find ns.registered_funs n
            in
            async (fun () -> run_process' ns new_pid (p pid_to_send ())) ;
            handler ()
        | Monitor (monitor_pid, to_be_monitored, sender) ->
            let mon_msg, mon_res =
              monitor_helper ns monitor_pid to_be_monitored
            in
            write_value out_ch (Monitor_result (mon_msg, mon_res, sender))
            >>= fun () -> handler ()
        | Unmonitor (mref, sender) ->
            unmonitor_local ns mref ;
            write_value out_ch (Unmonitor_result (mref, sender)) >>= fun () ->
            handler ()
        | Broadcast (sender_pid, _, msg) ->
            broadcast_local ns sender_pid msg >>= fun () -> handler ()
        | Data (_, r, _) as data ->
            put_in_mailbox r data >>= fun () -> handler ()
        | Exit (s, m) -> (
            match
              Potpourri.of_option @@ fun () ->
              Process_id_hashtbl.find ns.monitor_table s
            with
            | None ->
                log_msg ns Error "node process message" (fun () ->
                    Format.fprintf ns.log_formatter "no entry for " ;
                    Process_id.print_string_of_pid s ns.log_formatter ;
                    Format.fprintf
                      ns.log_formatter
                      " in monitor table when processing " ;
                    print_string_of_message msg ns.log_formatter)
                >>= fun () -> handler ()
            | Some pids ->
                Monitor_ref_set.fold
                  (fun (Monitor_Ref (_, pid, _)) _ ->
                    put_in_mailbox pid (Exit (s, m)))
                  pids
                  (return ())
                >>= fun () -> handler ())
        | Proc_result (_, receiver_pid) as pres ->
            put_in_mailbox receiver_pid pres >>= fun () -> handler ()
        | Spawn_monitor_result (monitor_msg, mref, receiver) as sres ->
            ignore (monitor_response_handler ns (monitor_msg, mref)) ;
            put_in_mailbox receiver sres >>= fun () -> handler ()
        | Monitor_result (mon_msg, mref, receiver) as mres ->
            ignore (monitor_response_handler ns (mon_msg, mref)) ;
            put_in_mailbox receiver mres >>= fun () -> handler ()
        | Unmonitor_result (mref, receiver_pid) as unmonres ->
            unmonitor_local ns mref ;
            put_in_mailbox receiver_pid unmonres >>= fun () -> handler ()
    in
    handler ()

  let node_server_fn (ns : node_state) (client_addr : Unix.sockaddr)
      ((in_ch, out_ch) : I.input_channel * I.output_channel) : unit I.t =
    let open I in
    let node_ref = ref None in
    catch
      (fun () ->
        ns.est_in_ch := Some in_ch ;
        ns.est_out_ch := Some out_ch ;
        wait_for_node_msg ns in_ch out_ch client_addr node_ref >>= fun node ->
        write_value out_ch @@ Node ns.local_node >>= fun () ->
        log_msg ns Debug "node server loop" (fun () ->
            Format.fprintf ns.log_formatter "starting server handler")
        >>= fun _ -> server_handler ns (in_ch, out_ch) node)
      (fun e ->
        log_msg ns ~exn:e Error "node process message" (fun () ->
            Format.fprintf ns.log_formatter "unexpected exception")
        >>= fun () -> clean_up_node_connection ns !node_ref in_ch out_ch)

  let connect_to_remote_node ?pid (ns : node_state) (remote_node : Node_id.t)
      (ip : string) (port : int) (name : string)
      (remote_sock_addr : Unix.sockaddr) : unit I.t =
    let open I in
    let node_ref = ref None in
    let server_handler_safe server_fn (in_ch, out_ch) =
      catch
        (fun () -> server_fn ns (in_ch, out_ch) remote_node)
        (fun e ->
          clean_up_node_connection ns (Some remote_node) in_ch out_ch
          >>= fun () ->
          log_msg ns ~exn:e Error "node client loop" @@ fun () ->
          Format.fprintf
            ns.log_formatter
            "unexpected exception while processing messages for remote node " ;
          Node_id.print_string_of_node remote_node ns.log_formatter)
    in
    log_msg ns ?pid Debug "connecting to remote node" (fun () ->
        Format.fprintf
          ns.log_formatter
          "remote node %s:%d, name %s"
          ip
          port
          name)
    >>= fun () ->
    open_connection remote_sock_addr >>= fun (in_ch, out_ch) ->
    write_value out_ch @@ Node ns.local_node >>= fun () ->
    log_msg ns ?pid Debug "connecting to remote node" (fun () ->
        Format.fprintf ns.log_formatter "sent message " ;
        print_string_of_message (Node ns.local_node) ns.log_formatter ;
        Format.fprintf
          ns.log_formatter
          " remote node %s:%d, name %s"
          ip
          port
          name)
    >>= fun () ->
    wait_for_node_msg ns in_ch out_ch remote_sock_addr node_ref >>= fun _ ->
    log_msg ns Debug "connected to remote node" (fun () ->
        Format.fprintf
          ns.log_formatter
          "remote node %s:%d, name %s"
          ip
          port
          name)
    >>= fun () ->
    return
    @@ async (fun () -> server_handler_safe server_handler (in_ch, out_ch))

  let add_remote_node (ip : string) (port : int) (name : string) : Node_id.t t =
    let open I in
    fun (ns, pid) ->
      if !(ns.config) = None then
        log_msg
          ~pid:(Process_id.get_id pid)
          ns
          Error
          "add remote node"
          (fun () ->
            Format.fprintf
              ns.log_formatter
              "called add remote node when node is running with local only \
               configuration")
        >>= fun () -> fail Local_only_mode
      else
        log_msg
          ~pid:(Process_id.get_id pid)
          ns
          Debug
          "adding remote node"
          (fun () ->
            Format.fprintf ns.log_formatter "%s:%d, name %s" ip port name)
        >>= fun () ->
        let remote_sock_addr =
          Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
        in
        let remote_node = Node_id.make_remote_node ip port name in
        if Node_id_hashtbl.mem ns.remote_nodes remote_node then
          log_msg
            ~pid:(Process_id.get_id pid)
            ns
            Warning
            "remote node already exists"
            (fun () ->
              Format.fprintf ns.log_formatter "%s:%d, name %s" ip port name)
          >>= fun () -> return (ns, pid, remote_node)
        else
          connect_to_remote_node ns remote_node ip port name remote_sock_addr
          >>= fun () -> return (ns, pid, remote_node)

  let remove_remote_node (node : Node_id.t) : unit t =
    let open I in
    fun (ns, pid) ->
      if !(ns.config) = None then
        log_msg
          ~pid:(Process_id.get_id pid)
          ns
          Error
          "remote remote node"
          (fun () ->
            Format.fprintf
              ns.log_formatter
              "called remove remote node when node is running with local only \
               configuration")
        >>= fun () -> fail Local_only_mode
      else
        log_msg
          ~pid:(Process_id.get_id pid)
          ns
          Debug
          "removing remote node"
          (fun () ->
            Format.fprintf ns.log_formatter "remote node : " ;
            Node_id.print_string_of_node node ns.log_formatter)
        >>= fun () ->
        Node_id_hashtbl.remove ns.remote_nodes node ;
        return (ns, pid, ())

  let rec connect_to_remote_nodes (ns : node_state)
      (nodes : (string * int * string) list) : unit io =
    let open I in
    match nodes with
    | [] -> return ()
    | (ip, port, name) :: rest ->
        let remote_sock_addr =
          Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
        in
        let remote_node = Node_id.make_remote_node ip port name in
        connect_to_remote_node ns remote_node ip port name remote_sock_addr
        >>= fun () -> connect_to_remote_nodes ns rest

  let run_node ?process (node_config : node_config) : unit io =
    let open I in
    if !initalised then fail Init_more_than_once
    else (
      initalised := true ;
      let buff = Buffer.create 1024 in
      match node_config with
      | Local local_config ->
          let ns =
            {
              mailboxes = Hashtbl.create 1000;
              remote_nodes = Node_id_hashtbl.create ~random:true 10;
              monitor_table = Process_id_hashtbl.create ~random:true 1000;
              local_node =
                Node_id.make_local_node local_config.Local_config.node_name;
              monitor_ref_id = ref 0;
              config = ref None;
              log_buffer = buff;
              log_formatter = Format.formatter_of_buffer buff;
              est_in_ch = ref None;
              est_out_ch = ref None;
              node_server = ref None;
              next_process_id = ref 0;
              registered_funs = Hashtbl.create 10;
            }
          in
          log_msg ns Info "node start up" (fun () ->
              Format.fprintf
                ns.log_formatter
                "{Distributed library version : %s ; Threading implementation \
                 : [name : %s ; version : %s ; description : %s]}"
                dist_lib_version
                lib_name
                lib_version
                lib_description)
          >>= fun () ->
          log_msg ns Info "node start up" (fun () ->
              Format.fprintf
                ns.log_formatter
                "local only mode with configuration of " ;
              print_string_of_config node_config ns.log_formatter)
          >>= fun () ->
          if process = None then return ()
          else
            let new_pid =
              Process_id.make_local
                local_config.Local_config.node_name
                ns.next_process_id
            in
            Hashtbl.replace
              ns.mailboxes
              (Process_id.get_id new_pid)
              (I.create_stream ()) ;
            run_process' ns new_pid ((Potpourri.get_option process) ())
      | Remote remote_config ->
          let ns =
            {
              mailboxes = Hashtbl.create 1000;
              remote_nodes = Node_id_hashtbl.create ~random:true 10;
              monitor_table = Process_id_hashtbl.create ~random:true 1000;
              local_node =
                Node_id.make_remote_node
                  remote_config.Remote_config.node_ip
                  remote_config.Remote_config.local_port
                  remote_config.Remote_config.node_name;
              monitor_ref_id = ref 0;
              config = ref (Some remote_config);
              log_buffer = buff;
              log_formatter = Format.formatter_of_buffer buff;
              est_in_ch = ref None;
              est_out_ch = ref None;
              node_server = ref None;
              (* fill in below *)
              next_process_id = ref 0;
              registered_funs = Hashtbl.create 10;
            }
          in
          log_msg ns Info "node start up" (fun () ->
              Format.fprintf
                ns.log_formatter
                "{Distributed library version : %s ; Threading implementation \
                 : [name : %s ; version : %s ; description : %s]}"
                dist_lib_version
                lib_name
                lib_version
                lib_description)
          >>= fun () ->
          log_msg ns Info "node start up" (fun () ->
              Format.fprintf
                ns.log_formatter
                "remote mode with configuration of " ;
              print_string_of_config node_config ns.log_formatter)
          >>= fun () ->
          I.catch
            (fun () ->
              connect_to_remote_nodes
                ns
                remote_config.Remote_config.remote_nodes
              >>= fun () ->
              let local_sock_addr =
                Unix.ADDR_INET
                  (Unix.inet_addr_any, remote_config.Remote_config.local_port)
              in
              I.establish_server
                ~backlog:remote_config.Remote_config.connection_backlog
                local_sock_addr
                (node_server_fn ns)
              >>= fun command_process_server ->
              ns.node_server := Some command_process_server ;
              at_exit (fun () ->
                  log_msg ns Info "node shutting down" (fun () ->
                      Format.fprintf
                        ns.log_formatter
                        "start clean up actions for remote mode with \
                         configuration of " ;
                      print_string_of_config node_config ns.log_formatter)
                  >>= fun () ->
                  at_exit_handler ns () >>= fun () ->
                  log_msg ns Info "node shutting down" (fun () ->
                      Format.fprintf
                        ns.log_formatter
                        "finished clean up actions for remote mode with \
                         configuration of " ;
                      print_string_of_config node_config ns.log_formatter)) ;
              if process = None then return ()
              else
                let new_pid =
                  Process_id.make_remote
                    remote_config.Remote_config.node_ip
                    remote_config.Remote_config.local_port
                    remote_config.Remote_config.node_name
                    ns.next_process_id
                in
                Hashtbl.replace
                  ns.mailboxes
                  (Process_id.get_id new_pid)
                  (I.create_stream ()) ;
                run_process' ns new_pid ((Potpourri.get_option process) ()))
            (fun e ->
              log_msg ns Error ~exn:e "node start up" (fun () ->
                  Format.fprintf
                    ns.log_formatter
                    "encountered exception during node startup, shutting down \
                     server")
              >>= fun () ->
              at_exit_handler ns () >>= fun () ->
              (* reraise exception so os level process stops*)
              fail e))
end
