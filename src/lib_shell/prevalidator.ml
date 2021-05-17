(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Prevalidator_worker_state

type limits = {
  max_refused_operations : int;
  operation_timeout : Time.System.Span.t;
  worker_limits : Worker_types.limits;
  operations_batch_size : int;
}

type name_t = Chain_id.t * Protocol_hash.t

module Logger =
  Worker_logger.Make (Event) (Request)
    (struct
      let worker_name = "node_prevalidator"
    end)

module type T = sig
  module Proto : Registered_protocol.T

  module Filter : Prevalidator_filters.FILTER with module Proto = Proto

  val name : name_t

  val parameters : limits * Distributed_db.chain_db

  module Prevalidation : Prevalidation.T with module Proto = Proto

  type types_state = {
    chain_db : Distributed_db.chain_db;
    limits : limits;
    mutable predecessor : Store.Block.t;
    mutable timestamp : Time.System.t;
    mutable live_blocks : Block_hash.Set.t;
    mutable live_operations : Operation_hash.Set.t;
    refused : Operation_hash.t Ringo.Ring.t;
    mutable refusals : (Operation.t * error list) Operation_hash.Map.t;
    branch_refused : Operation_hash.t Ringo.Ring.t;
    mutable branch_refusals : (Operation.t * error list) Operation_hash.Map.t;
    branch_delayed : Operation_hash.t Ringo.Ring.t;
    mutable branch_delays : (Operation.t * error list) Operation_hash.Map.t;
    mutable fetching : Operation_hash.Set.t;
    mutable pending : Operation.t Operation_hash.Map.t;
    mutable mempool : Mempool.t;
    mutable in_mempool : Operation_hash.Set.t;
    mutable applied : (Operation_hash.t * Operation.t) list;
    mutable validation_state : Prevalidation.t tzresult;
    mutable operation_stream :
      ( [`Applied | `Refused | `Branch_refused | `Branch_delayed]
      * Operation.shell_header
      * Proto.operation_data )
      Lwt_watcher.input;
    mutable advertisement : [`Pending of Mempool.t | `None];
    mutable rpc_directory : types_state RPC_directory.t lazy_t;
    mutable filter_config : Data_encoding.json Protocol_hash.Map.t;
  }

  module Name : Worker_intf.NAME with type t = name_t

  module Types : Worker_intf.TYPES with type state = types_state

  module Worker :
    Worker.T
      with type Event.t = Event.t
       and type 'a Request.t = 'a Request.t
       and type Request.view = Request.view
       and type Types.state = types_state

  type worker = Worker.infinite Worker.queue Worker.t

  val list_pendings :
    Distributed_db.chain_db ->
    from_block:Store.Block.t ->
    to_block:Store.Block.t ->
    live_blocks:Block_hash.Set.t ->
    Operation.t Operation_hash.Map.t ->
    Operation.t Operation_hash.Map.t Lwt.t

  val validation_result : types_state -> error Preapply_result.t

  val fitness : unit -> Fitness.t Lwt.t

  val initialization_errors : unit tzresult Lwt.t

  val worker : worker Lazy.t
end

module type ARG = sig
  val limits : limits

  val chain_db : Distributed_db.chain_db

  val chain_id : Chain_id.t
end

type t = (module T)

module Make (Filter : Prevalidator_filters.FILTER) (Arg : ARG) : T = struct
  module Filter = Filter
  module Proto = Filter.Proto

  let name = (Arg.chain_id, Proto.hash)

  let parameters = (Arg.limits, Arg.chain_db)

  module Prevalidation = Prevalidation.Make (Proto)

  type types_state = {
    chain_db : Distributed_db.chain_db;
    limits : limits;
    mutable predecessor : Store.Block.t;
    mutable timestamp : Time.System.t;
    mutable live_blocks : Block_hash.Set.t;
    (* just a cache *)
    mutable live_operations : Operation_hash.Set.t;
    (* just a cache *)
    refused : Operation_hash.t Ringo.Ring.t;
    mutable refusals : (Operation.t * error list) Operation_hash.Map.t;
    branch_refused : Operation_hash.t Ringo.Ring.t;
    mutable branch_refusals : (Operation.t * error list) Operation_hash.Map.t;
    branch_delayed : Operation_hash.t Ringo.Ring.t;
    mutable branch_delays : (Operation.t * error list) Operation_hash.Map.t;
    mutable fetching : Operation_hash.Set.t;
    mutable pending : Operation.t Operation_hash.Map.t;
    mutable mempool : Mempool.t;
    mutable in_mempool : Operation_hash.Set.t;
    mutable applied : (Operation_hash.t * Operation.t) list;
    mutable validation_state : Prevalidation.t tzresult;
    mutable operation_stream :
      ( [`Applied | `Refused | `Branch_refused | `Branch_delayed]
      * Operation.shell_header
      * Proto.operation_data )
      Lwt_watcher.input;
    mutable advertisement : [`Pending of Mempool.t | `None];
    mutable rpc_directory : types_state RPC_directory.t lazy_t;
    mutable filter_config : Data_encoding.json Protocol_hash.Map.t;
  }

  module Name = struct
    type t = name_t

    let encoding = Data_encoding.tup2 Chain_id.encoding Protocol_hash.encoding

    let chain_id_string =
      let (_ : string) = Format.flush_str_formatter () in
      Chain_id.pp_short Format.str_formatter Arg.chain_id ;
      Format.flush_str_formatter ()

    let proto_hash_string =
      let (_ : string) = Format.flush_str_formatter () in
      Protocol_hash.pp_short Format.str_formatter Proto.hash ;
      Format.flush_str_formatter ()

    let base = ["prevalidator"; chain_id_string; proto_hash_string]

    let pp fmt (chain_id, proto_hash) =
      Chain_id.pp_short fmt chain_id ;
      Format.pp_print_string fmt "." ;
      Protocol_hash.pp_short fmt proto_hash

    let equal (c1, p1) (c2, p2) =
      Chain_id.equal c1 c2 && Protocol_hash.equal p1 p2
  end

  module Types = struct
    (* Invariants:
       - an operation is in only one of these sets (map domains):
         pv.refusals pv.pending pv.fetching pv.live_operations pv.in_mempool
       - pv.in_mempool is the domain of all fields of pv.prevalidation_result
       - pv.prevalidation_result.refused = Ø, refused ops are in pv.refused
       - the 'applied' operations in pv.validation_result are in reverse order. *)
    type state = types_state

    type parameters = limits * Distributed_db.chain_db

    include Worker_state

    let view (state : state) _ : view =
      let domain map =
        Operation_hash.Map.fold
          (fun elt _ acc -> Operation_hash.Set.add elt acc)
          map
          Operation_hash.Set.empty
      in
      {
        head = Store.Block.hash state.predecessor;
        timestamp = state.timestamp;
        fetching = state.fetching;
        pending = domain state.pending;
        applied = List.rev_map (fun (h, _) -> h) state.applied;
        delayed =
          Operation_hash.Set.union
            (domain state.branch_delays)
            (domain state.branch_refusals);
      }
  end

  module Worker :
    Worker.T
      with type Name.t = Name.t
       and type Event.t = Event.t
       and type 'a Request.t = 'a Request.t
       and type Request.view = Request.view
       and type Types.state = Types.state
       and type Types.parameters = Types.parameters =
    Worker.Make (Name) (Prevalidator_worker_state.Event)
      (Prevalidator_worker_state.Request)
      (Types)
      (Logger)

  let decode_operation_data proto_bytes =
    try
      Some
        (Data_encoding.Binary.of_bytes_exn
           Proto.operation_data_encoding
           proto_bytes)
    with _ -> None

  (** Centralised operation stream for the RPCs *)
  let notify_operation {operation_stream; _} result {Operation.shell; proto} =
    match decode_operation_data proto with
    | Some protocol_data ->
        Lwt_watcher.notify operation_stream (result, shell, protocol_data)
    | None ->
        ()

  open Types

  type worker = Worker.infinite Worker.queue Worker.t

  let list_pendings chain_db ~from_block ~to_block ~live_blocks old_mempool =
    let chain_store = Distributed_db.chain_store chain_db in
    let rec pop_blocks ancestor block mempool =
      let hash = Store.Block.hash block in
      if Block_hash.equal hash ancestor then Lwt.return mempool
      else
        let operations = Store.Block.operations block in
        List.fold_left_s
          (List.fold_left_s (fun mempool op ->
               let h = Operation.hash op in
               Distributed_db.inject_operation chain_db h op
               >>= fun (_ : bool) ->
               Lwt.return (Operation_hash.Map.add h op mempool)))
          mempool
          operations
        >>= fun mempool ->
        Store.Block.read_predecessor_opt chain_store block
        >>= function
        | None ->
            assert false
        | Some predecessor ->
            pop_blocks ancestor predecessor mempool
    in
    let push_block mempool block =
      let operations = Store.Block.all_operation_hashes block in
      List.iter
        (List.iter (Distributed_db.Operation.clear_or_cancel chain_db))
        operations ;
      List.fold_left
        (List.fold_left (fun mempool h -> Operation_hash.Map.remove h mempool))
        mempool
        operations
    in
    Store.Chain_traversal.new_blocks chain_store ~from_block ~to_block
    >>= fun (ancestor, path) ->
    pop_blocks (Store.Block.hash ancestor) from_block old_mempool
    >>= fun mempool ->
    let new_mempool = List.fold_left push_block mempool path in
    let (new_mempool, outdated) =
      Operation_hash.Map.partition
        (fun _oph op ->
          Block_hash.Set.mem op.Operation.shell.branch live_blocks)
        new_mempool
    in
    Operation_hash.Map.iter
      (fun oph _op -> Distributed_db.Operation.clear_or_cancel chain_db oph)
      outdated ;
    Lwt.return new_mempool

  let already_handled pv oph =
    Operation_hash.Map.mem oph pv.refusals
    || Operation_hash.Map.mem oph pv.pending
    || Operation_hash.Set.mem oph pv.fetching
    || Operation_hash.Set.mem oph pv.live_operations
    || Operation_hash.Set.mem oph pv.in_mempool

  let validation_result (state : types_state) =
    {
      Preapply_result.applied = List.rev state.applied;
      branch_delayed = state.branch_delays;
      branch_refused = state.branch_refusals;
      refused = Operation_hash.Map.empty;
    }

  let advertise (w : worker) pv mempool =
    match pv.advertisement with
    | `Pending {Mempool.known_valid; pending} ->
        pv.advertisement <-
          `Pending
            {
              known_valid = known_valid @ mempool.Mempool.known_valid;
              pending = Operation_hash.Set.union pending mempool.pending;
            }
    | `None ->
        pv.advertisement <- `Pending mempool ;
        Lwt_utils.dont_wait
          (fun exc ->
            Format.eprintf
              "Uncaught exception: %s\n%!"
              (Printexc.to_string exc))
          (fun () ->
            Lwt_unix.sleep 0.1
            >>= fun () ->
            Worker.Queue.push_request_now w Advertise ;
            Lwt.return_unit)

  let is_endorsement (op : Prevalidation.operation) =
    Proto.acceptable_passes
      {shell = op.raw.shell; protocol_data = op.protocol_data}
    = [0]

  let filter_config w pv =
    try
      match Protocol_hash.Map.find Proto.hash pv.filter_config with
      | Some config ->
          Lwt.return
            (Data_encoding.Json.destruct Filter.Mempool.config_encoding config)
      | None ->
          Lwt.return Filter.Mempool.default_config
    with _ ->
      Worker.log_event w Invalid_mempool_filter_configuration
      >>= fun () -> Lwt.return Filter.Mempool.default_config

  let pre_filter w pv oph op =
    match decode_operation_data op.Operation.proto with
    | None ->
        Worker.log_event w (Unparsable_operation oph)
        >>= fun () -> Lwt.return false
    | Some protocol_data ->
        let op = {Filter.Proto.shell = op.shell; protocol_data} in
        filter_config w pv
        >>= fun config ->
        Lwt.return
          (Filter.Mempool.pre_filter config op.Filter.Proto.protocol_data)

  let post_filter w pv ~validation_state_before ~validation_state_after op
      receipt =
    filter_config w pv
    >>= fun config ->
    Filter.Mempool.post_filter
      config
      ~validation_state_before
      ~validation_state_after
      (op, receipt)

  let handle_branch_refused pv op oph errors =
    notify_operation pv `Branch_refused op ;
    Option.iter
      (fun e ->
        pv.branch_refusals <- Operation_hash.Map.remove e pv.branch_refusals ;
        pv.in_mempool <- Operation_hash.Set.remove e pv.in_mempool)
      (Ringo.Ring.add_and_return_erased pv.branch_refused oph) ;
    pv.in_mempool <- Operation_hash.Set.add oph pv.in_mempool ;
    pv.branch_refusals <-
      Operation_hash.Map.add oph (op, errors) pv.branch_refusals

  let handle_unprocessed w pv =
    match pv.validation_state with
    | Error err ->
        Operation_hash.Map.iter
          (fun h op ->
            Option.iter
              (fun e ->
                pv.branch_delays <-
                  Operation_hash.Map.remove e pv.branch_delays ;
                pv.in_mempool <- Operation_hash.Set.remove e pv.in_mempool)
              (Ringo.Ring.add_and_return_erased pv.branch_delayed h) ;
            pv.in_mempool <- Operation_hash.Set.add h pv.in_mempool ;
            pv.branch_delays <-
              Operation_hash.Map.add h (op, err) pv.branch_delays)
          pv.pending ;
        pv.pending <- Operation_hash.Map.empty ;
        Lwt.return_unit
    | Ok state -> (
      match Operation_hash.Map.cardinal pv.pending with
      | 0 ->
          Lwt.return_unit
      | n ->
          Worker.log_event w (Processing_n_operations n)
          >>= fun () ->
          Operation_hash.Map.fold_es
            (fun oph op (acc_validation_state, acc_mempool, limit) ->
              if limit <= 0 then
                (* Using Error as an early-return mechanism *)
                Lwt.return_error (acc_validation_state, acc_mempool)
              else (
                (* Do not forget to remove the treated operation
                         from the pendings *)
                pv.pending <- Operation_hash.Map.remove oph pv.pending ;
                let limit = limit - 1 in
                let refused errors =
                  notify_operation pv `Refused op ;
                  Option.iter
                    (fun e ->
                      pv.refusals <- Operation_hash.Map.remove e pv.refusals)
                    (Ringo.Ring.add_and_return_erased pv.refused oph) ;
                  pv.refusals <-
                    Operation_hash.Map.add oph (op, errors) pv.refusals ;
                  Distributed_db.Operation.clear_or_cancel pv.chain_db oph ;
                  pv.in_mempool <- Operation_hash.Set.add oph pv.in_mempool ;
                  Lwt.return_ok (acc_validation_state, acc_mempool, limit)
                in
                match Prevalidation.parse op with
                | Error errors ->
                    refused errors
                | Ok op -> (
                    Prevalidation.apply_operation state op
                    >>= function
                    | Applied (new_acc_validation_state, receipt) ->
                        post_filter
                          w
                          pv
                          ~validation_state_before:
                            (Prevalidation.validation_state
                               acc_validation_state)
                          ~validation_state_after:
                            (Prevalidation.validation_state
                               new_acc_validation_state)
                          op.protocol_data
                          receipt
                        >>= fun accept ->
                        if accept then (
                          notify_operation pv `Applied op.raw ;
                          let new_mempool =
                            Mempool.
                              {
                                acc_mempool with
                                known_valid =
                                  op.hash :: acc_mempool.known_valid;
                              }
                          in
                          pv.applied <- (op.hash, op.raw) :: pv.applied ;
                          pv.in_mempool <-
                            Operation_hash.Set.add op.hash pv.in_mempool ;
                          Lwt.return_ok
                            (new_acc_validation_state, new_mempool, limit) )
                        else
                          Lwt.return_ok
                            (acc_validation_state, acc_mempool, limit)
                    | Branch_delayed errors ->
                        notify_operation pv `Branch_delayed op.raw ;
                        Option.iter
                          (fun e ->
                            pv.branch_delays <-
                              Operation_hash.Map.remove e pv.branch_delays ;
                            pv.in_mempool <-
                              Operation_hash.Set.remove e pv.in_mempool)
                          (Ringo.Ring.add_and_return_erased
                             pv.branch_delayed
                             op.hash) ;
                        pv.in_mempool <-
                          Operation_hash.Set.add op.hash pv.in_mempool ;
                        pv.branch_delays <-
                          Operation_hash.Map.add
                            op.hash
                            (op.raw, errors)
                            pv.branch_delays ;
                        Lwt.return_ok (acc_validation_state, acc_mempool, limit)
                    | Branch_refused errors ->
                        handle_branch_refused pv op.raw op.hash errors ;
                        Lwt.return_ok (acc_validation_state, acc_mempool, limit)
                    | Refused errors ->
                        refused errors
                    | Duplicate | Outdated ->
                        Lwt.return_ok (acc_validation_state, acc_mempool, limit)
                    ) ))
            pv.pending
            (state, Mempool.empty, pv.limits.operations_batch_size)
          >>= (function
                | Error (state, advertised_mempool) ->
                    (* Early return after iteration limit was reached *)
                    Worker.Queue.push_request w Request.Leftover
                    >>= fun () -> Lwt.return (state, advertised_mempool)
                | Ok (state, advertised_mempool, _) ->
                    Lwt.return (state, advertised_mempool))
          >>= fun (state, advertised_mempool) ->
          let remaining_pendings =
            Operation_hash.Map.fold
              (fun k _ acc -> Operation_hash.Set.add k acc)
              pv.pending
              Operation_hash.Set.empty
          in
          pv.validation_state <- Ok state ;
          advertise
            w
            pv
            {
              advertised_mempool with
              known_valid = List.rev advertised_mempool.known_valid;
            } ;
          pv.mempool <-
            {
              Mempool.known_valid = List.rev_map fst pv.applied;
              pending = remaining_pendings;
            } ;
          let chain_store = Distributed_db.chain_store pv.chain_db in
          Store.Chain.set_mempool
            chain_store
            ~head:(Store.Block.hash pv.predecessor)
            pv.mempool
          >>= fun _res -> Lwt_main.yield () )

  let fetch_operation w pv ?peer oph =
    Worker.log_event w (Fetching_operation oph)
    >>= fun () ->
    Distributed_db.Operation.fetch
      ~timeout:pv.limits.operation_timeout
      pv.chain_db
      ?peer
      oph
      ()
    >>= function
    | Ok op ->
        Worker.Queue.push_request_now w (Arrived (oph, op)) ;
        Lwt.return_unit
    | Error (Distributed_db.Operation.Canceled _ :: _) ->
        Worker.log_event w (Operation_included oph)
        >>= fun () -> Lwt.return_unit
    | Error _ ->
        (* should not happen *)
        Lwt.return_unit

  let rpc_directory =
    lazy
      (let dir : state RPC_directory.t ref = ref RPC_directory.empty in
       let module Proto_services = Block_services.Make (Proto) (Proto) in
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.get_filter RPC_path.open_root)
           (fun pv () () ->
             match Protocol_hash.Map.find Proto.hash pv.filter_config with
             | Some obj ->
                 return obj
             | None -> (
               match Prevalidator_filters.find Proto.hash with
               | None ->
                   return (`O [])
               | Some (module Filter) ->
                   let default =
                     Data_encoding.Json.construct
                       Filter.Mempool.config_encoding
                       Filter.Mempool.default_config
                   in
                   return default )) ;
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.set_filter RPC_path.open_root)
           (fun pv () obj ->
             pv.filter_config <-
               Protocol_hash.Map.add Proto.hash obj pv.filter_config ;
             return ()) ;
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.pending_operations RPC_path.open_root)
           (fun pv () () ->
             let map_op op =
               match decode_operation_data op.Operation.proto with
               | Some protocol_data ->
                   Some {Proto.shell = op.shell; protocol_data}
               | None ->
                   None
             in
             let map_op_error oph (op, error) acc =
               match map_op op with
               | None ->
                   acc
               | Some res ->
                   Operation_hash.Map.add oph (res, error) acc
             in
             let applied =
               List.rev_filter_map
                 (fun (hash, op) ->
                   match map_op op with
                   | Some op ->
                       Some (hash, op)
                   | None ->
                       None)
                 pv.applied
             in
             let filter f map =
               Operation_hash.Map.fold f map Operation_hash.Map.empty
             in
             let refused = filter map_op_error pv.refusals in
             let branch_refused = filter map_op_error pv.branch_refusals in
             let branch_delayed = filter map_op_error pv.branch_delays in
             let unprocessed =
               Operation_hash.Map.fold
                 (fun oph op acc ->
                   match map_op op with
                   | Some op ->
                       Operation_hash.Map.add oph op acc
                   | None ->
                       acc)
                 pv.pending
                 Operation_hash.Map.empty
             in
             return
               {
                 Proto_services.Mempool.applied;
                 refused;
                 branch_refused;
                 branch_delayed;
                 unprocessed;
               }) ;
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.request_operations RPC_path.open_root)
           (fun pv () () ->
             Distributed_db.Request.current_head pv.chain_db () ;
             return_unit) ;
       dir :=
         RPC_directory.gen_register
           !dir
           (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
           (fun { applied;
                  refusals = refused;
                  branch_refusals = branch_refused;
                  branch_delays = branch_delayed;
                  operation_stream;
                  _ }
                params
                ()
                ->
             let (op_stream, stopper) =
               Lwt_watcher.create_stream operation_stream
             in
             (* Convert ops *)
             let map_op op =
               match decode_operation_data op.Operation.proto with
               | None ->
                   None
               | Some protocol_data ->
                   Some Proto.{shell = op.shell; protocol_data}
             in
             let fold_op _k (op, _error) acc =
               match map_op op with Some op -> op :: acc | None -> acc
             in
             (* First call : retrieve the current set of op from the mempool *)
             let applied =
               if params#applied then
                 List.filter_map map_op (List.map snd applied)
               else []
             in
             let refused =
               if params#refused then
                 Operation_hash.Map.fold fold_op refused []
               else []
             in
             let branch_refused =
               if params#branch_refused then
                 Operation_hash.Map.fold fold_op branch_refused []
               else []
             in
             let branch_delayed =
               if params#branch_delayed then
                 Operation_hash.Map.fold fold_op branch_delayed []
               else []
             in
             let current_mempool =
               List.concat [applied; refused; branch_refused; branch_delayed]
             in
             let current_mempool = ref (Some current_mempool) in
             let filter_result = function
               | `Applied ->
                   params#applied
               | `Refused ->
                   params#refused
               | `Branch_refused ->
                   params#branch_refused
               | `Branch_delayed ->
                   params#branch_delayed
             in
             let rec next () =
               match !current_mempool with
               | Some mempool ->
                   current_mempool := None ;
                   Lwt.return_some mempool
               | None -> (
                   Lwt_stream.get op_stream
                   >>= function
                   | Some (kind, shell, protocol_data) when filter_result kind
                     -> (
                     (* NOTE: Should the protocol change, a new Prevalidation
                      * context would  be created. Thus, we use the same Proto. *)
                     match
                       Data_encoding.Binary.to_bytes_opt
                         Proto.operation_data_encoding
                         protocol_data
                     with
                     | None ->
                         Lwt.return_none
                     | Some proto_bytes -> (
                       match decode_operation_data proto_bytes with
                       | None ->
                           Lwt.return_none
                       | Some protocol_data ->
                           Lwt.return_some [{Proto.shell; protocol_data}] ) )
                   | Some _ ->
                       next ()
                   | None ->
                       Lwt.return_none )
             in
             let shutdown () = Lwt_watcher.shutdown stopper in
             RPC_answer.return_stream {next; shutdown}) ;
       !dir)

  module Handlers = struct
    type self = worker

    let may_propagate_unknown_branch_operation pv op =
      Prevalidation.parse op
      >>?= (fun op ->
             let is_alternative_endorsement () =
               Lwt.return pv.validation_state
               >>=? fun validation_state ->
               Prevalidation.apply_operation validation_state op
               >>= function
               | Applied _ | Branch_delayed _ ->
                   return_true
               | _ ->
                   return_false
             in
             if is_endorsement op then is_alternative_endorsement ()
             else return_false)
      >>= function Ok b -> Lwt.return b | Error _ -> Lwt.return_false

    let on_operation_arrived w (pv : state) oph op =
      pv.fetching <- Operation_hash.Set.remove oph pv.fetching ;
      if already_handled pv oph then return_unit
      else if not (Block_hash.Set.mem op.Operation.shell.branch pv.live_blocks)
      then (
        let error = [Exn (Failure "Unknown branch operation")] in
        handle_branch_refused pv op oph error ;
        may_propagate_unknown_branch_operation pv op
        >>= function
        | true ->
            let pending = Operation_hash.Set.singleton oph in
            advertise w pv {Mempool.empty with pending} ;
            return_unit
        | false ->
            Distributed_db.Operation.clear_or_cancel pv.chain_db oph ;
            return_unit )
      else
        pre_filter w pv oph op
        >>= function
        | true ->
            (* TODO: should this have an influence on the peer's score ? *)
            pv.pending <- Operation_hash.Map.add oph op pv.pending ;
            return_unit
        | false ->
            return_unit

    let on_inject _w pv op =
      let oph = Operation.hash op in
      if already_handled pv oph then return_unit
        (* FIXME : is this an error ? *)
      else
        Lwt.return pv.validation_state
        >>=? fun validation_state ->
        Lwt.return (Prevalidation.parse op)
        >>=? fun parsed_op ->
        Prevalidation.apply_operation validation_state parsed_op
        >>= function
        | Applied (_, _result) ->
            Distributed_db.inject_operation pv.chain_db oph op
            >>= fun (_ : bool) ->
            pv.pending <- Operation_hash.Map.add parsed_op.hash op pv.pending ;
            return_unit
        | res ->
            failwith
              "Error while applying operation %a:@ %a"
              Operation_hash.pp
              oph
              Prevalidation.pp_result
              res

    let on_notify w pv peer mempool =
      let all_ophs =
        List.fold_left
          (fun s oph -> Operation_hash.Set.add oph s)
          mempool.Mempool.pending
          mempool.known_valid
      in
      let to_fetch =
        Operation_hash.Set.filter
          (fun oph -> not (already_handled pv oph))
          all_ophs
      in
      pv.fetching <- Operation_hash.Set.union to_fetch pv.fetching ;
      Operation_hash.Set.iter
        (fun oph -> Lwt.ignore_result (fetch_operation w pv ~peer oph))
        to_fetch

    let on_flush w pv predecessor live_blocks live_operations =
      Lwt_watcher.shutdown_input pv.operation_stream ;
      let chain_store = Distributed_db.chain_store pv.chain_db in
      list_pendings
        pv.chain_db
        ~from_block:pv.predecessor
        ~to_block:predecessor
        ~live_blocks
        (Operation_hash.Map.union
           (fun _key v _ -> Some v)
           (Preapply_result.operations (validation_result pv))
           pv.pending)
      >>= fun pending ->
      let timestamp_system = Tezos_stdlib_unix.Systime_os.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      Prevalidation.create
        chain_store
        ~predecessor
        ~live_blocks
        ~live_operations
        ~timestamp
        ()
      >>= fun validation_state ->
      Worker.log_event
        w
        (Operations_not_flushed (Operation_hash.Map.cardinal pending))
      >>= fun () ->
      pv.predecessor <- predecessor ;
      pv.live_blocks <- live_blocks ;
      pv.live_operations <- live_operations ;
      pv.timestamp <- timestamp_system ;
      pv.mempool <- {known_valid = []; pending = Operation_hash.Set.empty} ;
      pv.pending <- pending ;
      pv.in_mempool <- Operation_hash.Set.empty ;
      Ringo.Ring.clear pv.branch_delayed ;
      pv.branch_delays <- Operation_hash.Map.empty ;
      Ringo.Ring.clear pv.branch_refused ;
      pv.branch_refusals <- Operation_hash.Map.empty ;
      pv.applied <- [] ;
      pv.validation_state <- validation_state ;
      pv.operation_stream <- Lwt_watcher.create_input () ;
      return_unit

    let on_advertise pv =
      match pv.advertisement with
      | `None ->
          () (* should not happen *)
      | `Pending mempool ->
          pv.advertisement <- `None ;
          Distributed_db.Advertise.current_head
            pv.chain_db
            ~mempool
            pv.predecessor

    let on_request : type r. worker -> r Request.t -> r tzresult Lwt.t =
     fun w request ->
      let pv = Worker.state w in
      ( match request with
      | Request.Flush (hash, live_blocks, live_operations) ->
          on_advertise pv ;
          (* TODO: rebase the advertisement instead *)
          let chain_store = Distributed_db.chain_store pv.chain_db in
          Store.Block.read_block chain_store hash
          >>=? fun block ->
          on_flush w pv block live_blocks live_operations
          >>=? fun () -> return (() : r)
      | Request.Notify (peer, mempool) ->
          on_notify w pv peer mempool ;
          return_unit
      | Request.Leftover ->
          (* unprocessed ops are handled just below *)
          return_unit
      | Request.Inject op ->
          on_inject w pv op
      | Request.Arrived (oph, op) ->
          on_operation_arrived w pv oph op
      | Request.Advertise ->
          on_advertise pv ; return_unit )
      >>=? fun r -> handle_unprocessed w pv >>= fun () -> return r

    let on_close w =
      let pv = Worker.state w in
      Operation_hash.Set.iter
        (Distributed_db.Operation.clear_or_cancel pv.chain_db)
        pv.fetching ;
      Lwt.return_unit

    let on_launch w _ (limits, chain_db) =
      let chain_store = Distributed_db.chain_store chain_db in
      Store.Chain.current_head chain_store
      >>= fun predecessor ->
      Store.Chain.mempool chain_store
      >>= fun mempool ->
      Store.Chain.live_blocks chain_store
      >>= fun (live_blocks, live_operations) ->
      let timestamp_system = Tezos_stdlib_unix.Systime_os.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      Prevalidation.create
        chain_store
        ~predecessor
        ~timestamp
        ~live_blocks
        ~live_operations
        ()
      >>= fun validation_state ->
      let fetching =
        List.fold_left
          (fun s h -> Operation_hash.Set.add h s)
          Operation_hash.Set.empty
          mempool.known_valid
      in
      let pv =
        {
          limits;
          chain_db;
          predecessor;
          timestamp = timestamp_system;
          live_blocks;
          live_operations;
          mempool = {known_valid = []; pending = Operation_hash.Set.empty};
          refused = Ringo.Ring.create limits.max_refused_operations;
          refusals = Operation_hash.Map.empty;
          fetching;
          pending = Operation_hash.Map.empty;
          in_mempool = Operation_hash.Set.empty;
          applied = [];
          branch_refused = Ringo.Ring.create limits.max_refused_operations;
          branch_refusals = Operation_hash.Map.empty;
          branch_delayed = Ringo.Ring.create limits.max_refused_operations;
          branch_delays = Operation_hash.Map.empty;
          validation_state;
          operation_stream = Lwt_watcher.create_input ();
          advertisement = `None;
          rpc_directory;
          filter_config =
            Protocol_hash.Map.empty (* TODO: initialize from config file *);
        }
      in
      List.iter
        (fun oph -> Lwt.ignore_result (fetch_operation w pv oph))
        mempool.known_valid ;
      return pv

    let on_error w r st errs =
      Worker.record_event w (Event.Request (r, st, Some errs)) ;
      match r with
      | Request.(View (Inject _)) ->
          return_unit
      | _ ->
          Lwt.return_error errs

    let on_completion w r _ st =
      Worker.record_event w (Event.Request (Request.view r, st, None)) ;
      Lwt.return_unit

    let on_no_request _ = return_unit
  end

  let table = Worker.create_table Queue

  (* NOTE: we register a single worker for each instantiation of this Make
   * functor (and thus a single worker for the single instantiation of Worker).
   * Whilst this is somewhat abusing the intended purpose of worker, it is part
   * of a transition plan to a one-worker-per-peer architecture. *)
  let worker_promise =
    Worker.launch
      table
      Arg.limits.worker_limits
      name
      (Arg.limits, Arg.chain_db)
      (module Handlers)

  let initialization_errors = worker_promise >>=? fun _ -> return_unit

  let worker =
    lazy
      ( match Lwt.state worker_promise with
      | Lwt.Return (Ok worker) ->
          worker
      | Lwt.Return (Error _) | Lwt.Fail _ | Lwt.Sleep ->
          assert false )

  let fitness () =
    let w = Lazy.force worker in
    let pv = Worker.state w in
    Lwt.return pv.validation_state
    >>=? (fun state ->
           Prevalidation.status state
           >>=? fun status -> return status.block_result.fitness)
    >>= function
    | Ok fitness ->
        Lwt.return fitness
    | Error _ ->
        Lwt.return (Store.Block.fitness pv.predecessor)
end

module ChainProto_registry = Map.Make (struct
  type t = Chain_id.t * Protocol_hash.t

  let compare (c1, p1) (c2, p2) =
    let pc = Protocol_hash.compare p1 p2 in
    if pc = 0 then Chain_id.compare c1 c2 else pc
end)

let chain_proto_registry : t ChainProto_registry.t ref =
  ref ChainProto_registry.empty

let create limits (module Filter : Prevalidator_filters.FILTER) chain_db =
  let chain_store = Distributed_db.chain_store chain_db in
  let chain_id = Store.Chain.chain_id chain_store in
  match
    ChainProto_registry.find
      (chain_id, Filter.Proto.hash)
      !chain_proto_registry
  with
  | None ->
      let module Prevalidator =
        Make
          (Filter)
          (struct
            let limits = limits

            let chain_db = chain_db

            let chain_id = chain_id
          end)
      in
      (* Checking initialization errors before giving a reference to dangerous
       * `worker` value to caller. *)
      Prevalidator.initialization_errors
      >>=? fun () ->
      chain_proto_registry :=
        ChainProto_registry.add
          Prevalidator.name
          (module Prevalidator : T)
          !chain_proto_registry ;
      return (module Prevalidator : T)
  | Some p ->
      return p

let shutdown (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  chain_proto_registry :=
    ChainProto_registry.remove Prevalidator.name !chain_proto_registry ;
  Prevalidator.Worker.shutdown w

let flush (t : t) head live_blocks live_operations =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request_and_wait
    w
    (Request.Flush (head, live_blocks, live_operations))

let notify_operations (t : t) peer mempool =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request w (Request.Notify (peer, mempool))

let operations (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let pv = Prevalidator.Worker.state w in
  ( {(Prevalidator.validation_result pv) with applied = List.rev pv.applied},
    pv.pending )

let pending (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let pv = Prevalidator.Worker.state w in
  let ops = Preapply_result.operations (Prevalidator.validation_result pv) in
  Lwt.return ops

let timestamp (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let pv = Prevalidator.Worker.state w in
  pv.timestamp

let fitness (t : t) =
  let module Prevalidator : T = (val t) in
  Prevalidator.fitness ()

let inject_operation (t : t) op =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request_and_wait w (Inject op)

let status (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.status w

let running_workers () =
  ChainProto_registry.fold
    (fun (id, proto) t acc -> (id, proto, t) :: acc)
    !chain_proto_registry
    []

let pending_requests (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests w

let current_request (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.current_request w

let last_events (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.last_events w

let protocol_hash (t : t) =
  let module Prevalidator : T = (val t) in
  Prevalidator.Proto.hash

let parameters (t : t) =
  let module Prevalidator : T = (val t) in
  Prevalidator.parameters

let information (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.information w

let pipeline_length (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests_length w

let empty_rpc_directory : unit RPC_directory.t =
  RPC_directory.register
    RPC_directory.empty
    (Block_services.Empty.S.Mempool.pending_operations RPC_path.open_root)
    (fun _pv () () ->
      return
        {
          Block_services.Empty.Mempool.applied = [];
          refused = Operation_hash.Map.empty;
          branch_refused = Operation_hash.Map.empty;
          branch_delayed = Operation_hash.Map.empty;
          unprocessed = Operation_hash.Map.empty;
        })

let rpc_directory : t option RPC_directory.t =
  RPC_directory.register_dynamic_directory
    RPC_directory.empty
    (Block_services.mempool_path RPC_path.open_root)
    (function
      | None ->
          Lwt.return
            (RPC_directory.map (fun _ -> Lwt.return_unit) empty_rpc_directory)
      | Some t -> (
          let module Prevalidator : T = (val t : T) in
          Prevalidator.initialization_errors
          >>= function
          | Error _ ->
              Lwt.return
                (RPC_directory.map
                   (fun _ -> Lwt.return_unit)
                   empty_rpc_directory)
          | Ok () ->
              let w = Lazy.force Prevalidator.worker in
              let pv = Prevalidator.Worker.state w in
              let pv_rpc_dir = Lazy.force pv.rpc_directory in
              Lwt.return
                (RPC_directory.map (fun _ -> Lwt.return pv) pv_rpc_dir) ))
