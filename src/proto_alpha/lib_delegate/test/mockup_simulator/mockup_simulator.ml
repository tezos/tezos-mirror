(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type block = {
  rpc_context : Tezos_protocol_environment.rpc_context;
  protocol_data : Protocol.Alpha_context.Block_header.protocol_data;
  raw_protocol_data : Bytes.t;
  operations : Mockup.M.Block_services.operation list list;
  resulting_context_hash : Context_hash.t;
}

type chain = block list

(** As new blocks and operations are received they are pushed to an Lwt_pipe
   wrapped into this type. *)
type broadcast =
  | Broadcast_block of Block_hash.t * Block_header.t * Operation.t list list
  | Broadcast_op of Operation_hash.t * Alpha_context.packed_operation

(** The state of a mockup node. *)
type state = {
  instance_index : int;
      (** Index of this node. Indices go from 0 to N-1 where N is the total
     number of bakers in the simulation. *)
  live_depth : int;
      (** How many blocks (counting from the head into the past) are considered live? *)
  mutable chain : chain;  (** The chain as seen by this fake "node". *)
  mutable mempool : (Operation_hash.t * Mockup.M.Protocol.operation) list;
      (** Mempool of this fake "node". *)
  chain_table : chain Block_hash.Table.t;
      (** The chain table of this fake "node". It maps from block hashes to
     blocks. *)
  global_chain_table : block Block_hash.Table.t;
      (** The global chain table that allows us to look up blocks that may be
     missing in [chain_table], i.e. not known to this particular node. This
     is used to find unknown predecessors. The real node can ask about an
     unknown block and receive it on request, this is supposed to emulate
     that functionality. *)
  ctxt_table : Tezos_protocol_environment.rpc_context Context_hash.Table.t;
      (** The context table allows us to look up rpc_context by its hash. *)
  validated_blocks_pipe :
    (Block_hash.t * Block_header.t * Operation.t list list) Lwt_pipe.Unbounded.t;
      (** [validated_blocks_pipe] is used to implement the
          [monitor_validated_blocks] RPC. *)
  heads_pipe : (Block_hash.t * Block_header.t) Lwt_pipe.Unbounded.t;
      (** [heads_pipe] is used to implement the [monitor_heads]
          RPC. *)
  mutable operations_stream :
    (Operation_hash.t * Mockup.M.Protocol.operation) list Lwt_stream.t;
  mutable operations_stream_push :
    (Operation_hash.t * Mockup.M.Protocol.operation) list option -> unit;
      (** [operations_pipe] is used to implement the [operations_pipe] RPC. *)
  mutable streaming_operations : bool;
      (** A helper flag used to implement the monitor operations RPC. *)
  broadcast_pipes : broadcast Lwt_pipe.Unbounded.t list;
      (** Broadcast pipes per node. *)
  genesis_block_true_hash : Block_hash.t;
      (** True hash of the genesis
                                 block as calculated by the
                                 [Block_header.hash] function. *)
}

let accounts = Mockup.Protocol_parameters.default_value.bootstrap_accounts

let chain_id = Chain_id.of_string_exn "main"

let genesis_block_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"

let genesis_predecessor_block_hash = Block_hash.zero

type propagation = Block | Pass | Delay of float

type propagation_vector = propagation list

module type Hooks = sig
  val on_inject_block :
    level:int32 ->
    round:int32 ->
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    operations:Operation.t list list ->
    protocol_data:Alpha_context.Block_header.protocol_data ->
    (Block_hash.t * Block_header.t * Operation.t list list * propagation_vector)
    tzresult
    Lwt.t

  val on_inject_operation :
    op_hash:Operation_hash.t ->
    op:Alpha_context.packed_operation ->
    (Operation_hash.t * Alpha_context.packed_operation * propagation_vector)
    tzresult
    Lwt.t

  val on_new_validated_block :
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    operations:Operation.t list list ->
    (Block_hash.t * Block_header.t * Operation.t list list) option Lwt.t

  val on_new_head :
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    (Block_hash.t * Block_header.t) option Lwt.t

  val on_new_operation :
    Operation_hash.t * Alpha_context.packed_operation ->
    (Operation_hash.t * Alpha_context.packed_operation) option Lwt.t

  val check_block_before_processing :
    level:int32 ->
    round:int32 ->
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    protocol_data:Alpha_context.Block_header.protocol_data ->
    unit tzresult Lwt.t

  val check_chain_after_processing :
    level:int32 -> round:int32 -> chain:chain -> unit tzresult Lwt.t

  val check_mempool_after_processing :
    mempool:(Operation_hash.t * Mockup.M.Protocol.operation) list ->
    unit tzresult Lwt.t

  val stop_on_event : Baking_state.event -> bool

  val on_start_baker :
    baker_position:int ->
    delegates:Baking_state.consensus_key list ->
    cctxt:Protocol_client_context.full ->
    unit Lwt.t

  val check_chain_on_success : chain:chain -> unit tzresult Lwt.t
end

(** Return a series of blocks starting from the block with the given
    identifier. *)
let locate_blocks (state : state)
    (block : Tezos_shell_services.Block_services.block) :
    block list tzresult Lwt.t =
  let open Lwt_result_syntax in
  match block with
  | `Hash (hash, rel) -> (
      match Block_hash.Table.find state.chain_table hash with
      | None ->
          failwith "locate_blocks: can't find the block %a" Block_hash.pp hash
      | Some chain0 ->
          let _, chain = List.split_n rel chain0 in
          return chain)
  | `Head rel ->
      let _, chain = List.split_n rel state.chain in
      return chain
  | `Level _ -> failwith "locate_blocks: `Level block spec not handled"
  | `Genesis -> failwith "locate_blocks: `Genesis block spec net handled"
  | `Alias _ -> failwith "locate_blocks: `Alias block spec not handled"

(** Similar to [locate_blocks], but only returns the first block. *)
let locate_block (state : state)
    (block : Tezos_shell_services.Block_services.block) : block tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* blocks = locate_blocks state block in
  match blocks with
  | [] -> failwith "locate_block: can't find the block"
  | x :: _ -> return x

(** Return the collection of live blocks for a given block identifier. *)
let live_blocks (state : state) block =
  let open Lwt_result_syntax in
  let* chain = locate_blocks state block in
  let segment, _ = List.split_n state.live_depth chain in
  return
    (List.fold_left
       (fun set ({rpc_context; _} : block) ->
         let hash = rpc_context.Tezos_protocol_environment.block_hash in
         Block_hash.Set.add hash set)
       (Block_hash.Set.of_list
          [state.genesis_block_true_hash; genesis_predecessor_block_hash])
       segment)

(** Extract the round number from raw fitness. *)
let round_from_raw_fitness raw_fitness =
  let open Lwt_result_syntax in
  match Protocol.Alpha_context.Fitness.from_raw raw_fitness with
  | Ok fitness ->
      return
        (Alpha_context.Round.to_int32
           (Protocol.Alpha_context.Fitness.round fitness))
  | Error _ -> failwith "round_from_raw_fitness: cannot parse fitness"

(** Extract level from a block header. *)
let get_block_level (block_header : Block_header.t) =
  let open Lwt_result_syntax in
  return block_header.shell.level

(** Extract round from a block header. *)
let get_block_round (block_header : Block_header.t) =
  round_from_raw_fitness block_header.shell.fitness

(** Parse protocol data. *)
let parse_protocol_data (protocol_data : Bytes.t) =
  let open Lwt_result_syntax in
  match
    Data_encoding.Binary.of_bytes_opt
      Protocol.Alpha_context.Block_header.protocol_data_encoding
      protocol_data
  with
  | None -> failwith "can't parse protocol data of a block"
  | Some parsed_protocol_data -> return parsed_protocol_data

(** Broadcast an operation or block according to the given propagation
    vector. *)
let handle_propagation msg propagation_vector broadcast_pipes =
  let open Lwt_result_syntax in
  let*! () =
    List.iter_s
      (fun (propagation, pipe) ->
        match propagation with
        | Block -> Lwt.return_unit
        | Pass ->
            Lwt_pipe.Unbounded.push pipe msg ;
            Lwt.return_unit
        | Delay s ->
            Lwt.dont_wait
              (fun () ->
                let*! () = Lwt_unix.sleep s in
                Lwt_pipe.Unbounded.push pipe msg ;
                Lwt.return_unit)
              (fun _exn -> ()) ;
            Lwt.return_unit)
      (List.combine_drop propagation_vector broadcast_pipes)
  in
  return_unit

(** Use the [user_hooks] to produce a module of functions that will perform
    the heavy lifting for the RPC implementations. *)
let make_mocked_services_hooks (state : state) (user_hooks : (module Hooks)) :
    Faked_services.hooks =
  let open Lwt_result_syntax in
  let module User_hooks = (val user_hooks : Hooks) in
  let module Impl : Faked_services.Mocked_services_hooks = struct
    type mempool = Mockup.M.Block_services.Mempool.t

    let monitor_validated_blocks () =
      let next () =
        let rec pop_until_ok () =
          let*! block_hash, block_header, operations =
            Lwt_pipe.Unbounded.pop state.validated_blocks_pipe
          in
          let*! result =
            User_hooks.on_new_validated_block
              ~block_hash
              ~block_header
              ~operations
          in
          match result with
          | None -> pop_until_ok ()
          | Some (hash, head, operations) ->
              Lwt.return_some (chain_id, hash, head, operations)
        in
        pop_until_ok ()
      in
      let shutdown () = () in
      Tezos_rpc.Answer.{next; shutdown}

    let monitor_heads () =
      let next () =
        let rec pop_until_ok () =
          let*! block_hash, block_header =
            Lwt_pipe.Unbounded.pop state.heads_pipe
          in
          (* Sleep a 0.1s to simulate a block application delay *)
          let*! () = Lwt_unix.sleep 0.1 in
          let*! head_opt = User_hooks.on_new_head ~block_hash ~block_header in
          match head_opt with
          | None -> pop_until_ok ()
          | Some head -> Lwt.return_some head
        in
        pop_until_ok ()
      in
      let shutdown () = () in
      Tezos_rpc.Answer.{next; shutdown}

    let monitor_bootstrapped () =
      let first_run = ref true in
      let next () =
        if !first_run then (
          first_run := false ;
          let b = match state.chain with [] -> assert false | b :: _ -> b in
          let head_hash = b.rpc_context.block_hash in
          let timestamp = b.rpc_context.block_header.timestamp in
          Lwt.return_some (head_hash, timestamp))
        else Lwt.return_none
      in
      let shutdown () = () in
      Tezos_rpc.Answer.{next; shutdown}

    let protocols (block : Tezos_shell_services.Block_services.block) =
      let* x = locate_block state block in
      let hash = x.rpc_context.block_hash in
      let is_predecessor_of_genesis =
        match block with
        | `Hash (requested_hash, rel) ->
            Int.equal rel 0
            && Block_hash.equal requested_hash genesis_predecessor_block_hash
        | _ -> false
      in
      (* It is important to tell the baker that the genesis block is not in
         the alpha protocol (we use Protocol_hash.zero). This will make the
         baker not try to propose alternatives to that block and just accept
         it as final in that Protocol_hash.zero protocol. The same for
         predecessor of genesis, it should be in Protocol_hash.zero. *)
      return
        Tezos_shell_services.Block_services.
          {
            current_protocol =
              (if
               Block_hash.equal hash genesis_block_hash
               || is_predecessor_of_genesis
              then Protocol_hash.zero
              else Protocol.hash);
            next_protocol =
              (if is_predecessor_of_genesis then Protocol_hash.zero
              else Protocol.hash);
          }

    let may_lie_on_proto_level block x =
      (* As for ../protocols, the baker distinguishes activation
         blocks from "normal" blocks by comparing the [proto_level] of
         the shell header and its predecessor. If the predecessor's
         one is different, it must mean that we are considering an
         activation block and must not attest. Here, we do a bit of
         hacking in order to return a different proto_level for the
         predecessor of the genesis block which is considered as the
         current protocol activation block. To perfectly mimic what is
         supposed to happen, the first mocked up block created should
         be made in the genesis protocol, however, it is not what's
         done in the mockup mode. *)
      let is_predecessor_of_genesis =
        match block with
        | `Hash (requested_hash, rel) ->
            Int.equal rel 0
            && Block_hash.equal requested_hash genesis_predecessor_block_hash
        | _ -> false
      in
      if is_predecessor_of_genesis then
        {
          x.rpc_context.block_header with
          proto_level = pred x.rpc_context.block_header.proto_level;
        }
      else x.rpc_context.block_header

    let raw_header (block : Tezos_shell_services.Block_services.block) :
        bytes tzresult Lwt.t =
      let* x = locate_block state block in
      let shell = may_lie_on_proto_level block x in
      let protocol_data =
        Data_encoding.Binary.to_bytes_exn
          Protocol.block_header_data_encoding
          x.protocol_data
      in
      return
        (Data_encoding.Binary.to_bytes_exn
           Tezos_base.Block_header.encoding
           {shell; protocol_data})

    let header (block : Tezos_shell_services.Block_services.block) :
        Mockup.M.Block_services.block_header tzresult Lwt.t =
      let* x = locate_block state block in
      let shell = may_lie_on_proto_level block x in
      return
        {
          Mockup.M.Block_services.hash = x.rpc_context.block_hash;
          chain_id;
          shell;
          protocol_data = x.protocol_data;
        }

    let resulting_context_hash
        (block : Tezos_shell_services.Block_services.block) :
        Context_hash.t tzresult Lwt.t =
      let* x = locate_block state block in
      return x.resulting_context_hash

    let operations block =
      let* x = locate_block state block in
      return x.operations

    let inject_block block_hash (block_header : Block_header.t) operations =
      let* protocol_data = parse_protocol_data block_header.protocol_data in
      let* level = get_block_level block_header in
      let* round = get_block_round block_header in
      let* block_hash1, block_header1, operations1, propagation_vector =
        User_hooks.on_inject_block
          ~level
          ~round
          ~block_hash
          ~block_header
          ~operations
          ~protocol_data
      in
      handle_propagation
        (Broadcast_block (block_hash1, block_header1, operations1))
        propagation_vector
        state.broadcast_pipes

    let all_pipes_or_select = function
      | None -> return state.broadcast_pipes
      | Some l ->
          List.map_es
            (fun n ->
              match List.nth_opt state.broadcast_pipes n with
              | None ->
                  failwith
                    "Node number %d is out of range (max is %d)"
                    n
                    (List.length state.broadcast_pipes - 1)
              | Some pipe -> return pipe)
            l

    let broadcast_block ?dests block_hash (block_header : Block_header.t)
        operations =
      let* pipes = all_pipes_or_select dests in
      let*! () =
        List.iter_s
          (fun pipe ->
            Lwt_pipe.Unbounded.push
              pipe
              (Broadcast_block (block_hash, block_header, operations)) ;
            Lwt.return_unit)
          pipes
      in
      return_unit

    let inject_operation (Operation.{shell; proto} as op) =
      let op_hash = Operation.hash op in
      let proto_op_opt =
        Data_encoding.Binary.of_bytes Protocol.operation_data_encoding proto
      in
      match proto_op_opt with
      | Error _ -> failwith "inject_operation: cannot parse operation"
      | Ok protocol_data ->
          let op : Protocol.Alpha_context.packed_operation =
            {shell; protocol_data}
          in
          let* op_hash1, op1, propagation_vector =
            User_hooks.on_inject_operation ~op_hash ~op
          in
          let* () =
            handle_propagation
              (Broadcast_op (op_hash1, op1))
              propagation_vector
              state.broadcast_pipes
          in
          return op_hash1

    let broadcast_operation ?dests
        (op : Protocol.Alpha_context.packed_operation) =
      let* pipes = all_pipes_or_select dests in
      let op_hash = Alpha_context.Operation.hash_packed op in
      let*! () =
        List.iter_s
          (fun pipe ->
            Lwt_pipe.Unbounded.push pipe (Broadcast_op (op_hash, op)) ;
            Lwt.return_unit)
          pipes
      in
      return_unit

    let pending_operations () =
      let ops = state.mempool in
      Lwt.return
        Mockup.M.Block_services.Mempool.
          {
            validated = ops;
            refused = Operation_hash.Map.empty;
            outdated = Operation_hash.Map.empty;
            branch_refused = Operation_hash.Map.empty;
            branch_delayed = Operation_hash.Map.empty;
            unprocessed = Operation_hash.Map.empty;
          }

    let monitor_operations ~version ~validated ~branch_delayed ~branch_refused
        ~refused =
      ignore validated ;
      ignore branch_delayed ;
      ignore branch_refused ;
      ignore refused ;
      let streamed = ref false in
      state.streaming_operations <- true ;
      let next () =
        let rec loop () =
          let*! ops_opt = Lwt_stream.get state.operations_stream in
          match ops_opt with
          | None when !streamed -> Lwt.return_none
          | None ->
              streamed := true ;
              Lwt.return_some (version, [])
          | Some ops -> (
              let*! result =
                List.filter_map_s User_hooks.on_new_operation ops
              in
              match result with
              | [] -> loop ()
              | l -> Lwt.return_some (version, List.map (fun x -> (x, None)) l))
        in
        loop ()
      in
      let shutdown () = () in
      Tezos_rpc.Answer.{next; shutdown}

    let rpc_context_callback block =
      let* x = locate_block state block in
      return x.rpc_context

    let list_blocks ~heads ~length ~min_date:_ =
      let compare_block_fitnesses block0 block1 =
        Fitness.compare
          block0.rpc_context.block_header.fitness
          block1.rpc_context.block_header.fitness
      in
      let hash_of_block block = block.rpc_context.block_hash in
      let lookup_head head =
        let* xs = locate_blocks state (`Hash (head, 0)) in
        let segment =
          match length with None -> xs | Some n -> List.take_n n xs
        in
        return
          (List.map hash_of_block (List.sort compare_block_fitnesses segment))
      in
      List.map_es lookup_head heads

    let live_blocks block = live_blocks state block

    let raw_protocol_data block =
      let* x = locate_block state block in
      return x.raw_protocol_data
  end in
  (module Impl)

(** Return the current head. *)
let head {chain; _} =
  let open Lwt_result_syntax in
  match List.hd chain with
  | None -> failwith "mockup_simulator.ml: empty chain"
  | Some hd -> return hd

(** Clear from the mempool operations whose branch does not point to
    a live block with respect to the current head. *)
let clear_mempool state =
  let open Lwt_result_syntax in
  let* head = head state in
  let included_ops_hashes =
    List.map
      (fun (op : Mockup.M.Block_services.operation) -> op.hash)
      (List.flatten head.operations)
  in
  let* live_set = live_blocks state (`Head 0) in
  let mempool =
    List.filter
      (fun (_oph, (op : Mockup.M.Protocol.operation)) ->
        let included_in_head =
          List.mem
            ~equal:Operation_hash.equal
            (Alpha_context.Operation.hash_packed op)
            included_ops_hashes
        in
        Block_hash.Set.mem op.shell.branch live_set && not included_in_head)
      state.mempool
  in
  state.mempool <- mempool ;
  return_unit

let begin_validation_and_application ctxt chain_id mode ~predecessor ~cache =
  let open Lwt_result_syntax in
  let* validation_state =
    Mockup.M.Protocol.begin_validation ctxt chain_id mode ~predecessor ~cache
  in
  let* application_state =
    Mockup.M.Protocol.begin_application ctxt chain_id mode ~predecessor ~cache
  in
  return (validation_state, application_state)

let validate_and_apply_operation (validation_state, application_state) oph op =
  let open Lwt_result_syntax in
  let* validation_state =
    Mockup.M.Protocol.validate_operation validation_state oph op
  in
  let* application_state, receipt =
    Mockup.M.Protocol.apply_operation application_state oph op
  in
  return ((validation_state, application_state), receipt)

let finalize_validation_and_application (validation_state, application_state)
    shell_header =
  let open Lwt_result_syntax in
  let* () = Mockup.M.Protocol.finalize_validation validation_state in
  Mockup.M.Protocol.finalize_application application_state shell_header

(** Apply a block to the given [rpc_context]. *)
let reconstruct_context (rpc_context : Tezos_protocol_environment.rpc_context)
    (operations : Operation.t list list) (block_header : Block_header.t) =
  let open Lwt_result_syntax in
  let predecessor = rpc_context.block_header in
  let predecessor_context = rpc_context.context in
  let* protocol_data = parse_protocol_data block_header.protocol_data in
  let* state =
    begin_validation_and_application
      predecessor_context
      chain_id
      (Application {shell = block_header.shell; protocol_data})
      ~predecessor
      ~cache:`Lazy
  in
  let i = ref 0 in
  let* state, _ =
    List.fold_left_es
      (List.fold_left_es (fun (state, results) op ->
           incr i ;
           let oph = Operation.hash op in
           let operation_data =
             Data_encoding.Binary.of_bytes_exn
               Mockup.M.Protocol.operation_data_encoding
               op.Operation.proto
           in
           let op =
             {
               Mockup.M.Protocol.shell = op.shell;
               protocol_data = operation_data;
             }
           in
           let* state, receipt = validate_and_apply_operation state oph op in
           return (state, receipt :: results)))
      (state, [])
      operations
  in
  finalize_validation_and_application state None

(** Process an incoming block. If validation succeeds:
    - update the current head to this new block
    - cleanup outdated operations
    - cleanup listener table
    Note that this implementation does not handle concurrent branches. *)
let rec process_block state block_hash (block_header : Block_header.t)
    operations =
  let open Lwt_result_syntax in
  let get_predecessor () =
    let predecessor_hash = block_header.Block_header.shell.predecessor in
    let* head = head state in
    match Block_hash.Table.find state.chain_table predecessor_hash with
    | None | Some [] -> (
        (* Even if the predecessor is not known locally, it might be known by
           some node in the network. The code below "requests" information
           about the block by its hash. *)
        match
          Block_hash.Table.find state.global_chain_table predecessor_hash
        with
        | None -> failwith "get_predecessor: unknown predecessor block"
        | Some predecessor ->
            let predecessor_block_header =
              Block_header.
                {
                  shell = predecessor.rpc_context.block_header;
                  protocol_data = predecessor.raw_protocol_data;
                }
            in
            let predecessor_ops =
              List.map
                (fun xs ->
                  List.map
                    (fun (op : Mockup.M.Block_services.operation) ->
                      Operation.
                        {
                          shell = op.shell;
                          proto =
                            Data_encoding.Binary.to_bytes_exn
                              Protocol.operation_data_encoding
                              op.protocol_data;
                        })
                    xs)
                predecessor.operations
            in
            (* If the block is found, apply it before proceeding. *)
            let* () =
              process_block
                state
                predecessor.rpc_context.block_hash
                predecessor_block_header
                predecessor_ops
            in
            return predecessor)
    | Some (predecessor :: _) ->
        if
          Int32.sub
            head.rpc_context.block_header.level
            predecessor.rpc_context.block_header.level
          <= 2l
        then return predecessor
        else failwith "get_predecessor: the predecessor block is too old"
  in
  match Block_hash.Table.find state.chain_table block_hash with
  | Some _ ->
      (* The block is already known. *)
      return_unit
  | None ->
      let* predecessor = get_predecessor () in
      let* head = head state in
      let* {context; message; _}, _ =
        reconstruct_context predecessor.rpc_context operations block_header
      in
      let resulting_context_hash =
        Tezos_context_ops.Context_ops.hash
          ~time:block_header.shell.timestamp
          ?message
          context
      in
      let rpc_context =
        Tezos_protocol_environment.
          {context; block_hash; block_header = block_header.shell}
      in
      let operations =
        List.map
          (fun pass ->
            List.map
              (fun (Operation.{shell; proto} as op) ->
                let hash : Operation_hash.t = Operation.hash op in
                let protocol_data : Alpha_context.packed_protocol_data =
                  Data_encoding.Binary.of_bytes_exn
                    Protocol.operation_data_encoding
                    proto
                in
                {
                  Mockup.M.Block_services.chain_id;
                  hash;
                  shell;
                  protocol_data;
                  receipt = Empty;
                })
              pass)
          operations
      in
      let* protocol_data = parse_protocol_data block_header.protocol_data in
      let new_block =
        {
          rpc_context;
          protocol_data;
          raw_protocol_data = block_header.protocol_data;
          operations;
          resulting_context_hash;
        }
      in
      let predecessor_hash = block_header.Block_header.shell.predecessor in
      let tail =
        Block_hash.Table.find state.chain_table predecessor_hash
        |> WithExceptions.Option.get ~loc:__LOC__
      in
      let new_chain = new_block :: tail in
      Block_hash.Table.replace state.chain_table block_hash new_chain ;
      Block_hash.Table.replace state.global_chain_table block_hash new_block ;
      Context_hash.Table.replace
        state.ctxt_table
        resulting_context_hash
        rpc_context ;
      if
        Fitness.(
          block_header.shell.fitness > head.rpc_context.block_header.fitness)
      then (
        state.chain <- new_chain ;
        let* () = clear_mempool state in
        (* The head changed: notify that the stream ended. *)
        state.operations_stream_push None ;
        state.streaming_operations <- false ;
        (* Instanciate a new stream *)
        let operations_stream, operations_stream_push = Lwt_stream.create () in
        state.operations_stream <- operations_stream ;
        state.operations_stream_push <- operations_stream_push ;
        state.operations_stream_push (Some state.mempool) ;
        return_unit)
      else return_unit

(** This process listens to broadcast block and operations and incorporates
    them in the context of the fake node. *)
let rec listener ~(user_hooks : (module Hooks)) ~state ~broadcast_pipe =
  let open Lwt_result_syntax in
  let module User_hooks = (val user_hooks : Hooks) in
  let*! result = Lwt_pipe.Unbounded.pop broadcast_pipe in
  match result with
  | Broadcast_op (operation_hash, packed_operation) ->
      let* () =
        if
          List.mem_assoc
            ~equal:Operation_hash.equal
            operation_hash
            state.mempool
        then return_unit
        else (
          state.mempool <- (operation_hash, packed_operation) :: state.mempool ;
          state.operations_stream_push
            (Some [(operation_hash, packed_operation)]) ;
          User_hooks.check_mempool_after_processing ~mempool:state.mempool)
      in
      listener ~user_hooks ~state ~broadcast_pipe
  | Broadcast_block (block_hash, block_header, operations) ->
      let* level = get_block_level block_header in
      let* round = get_block_round block_header in
      let* protocol_data = parse_protocol_data block_header.protocol_data in
      let* () =
        User_hooks.check_block_before_processing
          ~level
          ~round
          ~block_hash
          ~block_header
          ~protocol_data
      in
      let* () = process_block state block_hash block_header operations in
      let* () =
        User_hooks.check_chain_after_processing ~level ~round ~chain:state.chain
      in
      Lwt_pipe.Unbounded.push
        state.validated_blocks_pipe
        (block_hash, block_header, operations) ;
      Lwt_pipe.Unbounded.push state.heads_pipe (block_hash, block_header) ;
      listener ~user_hooks ~state ~broadcast_pipe

(** Create a fake node state. *)
let create_fake_node_state ~i ~live_depth
    ~(genesis_block : Block_header.t * Tezos_protocol_environment.rpc_context)
    ~global_chain_table ~broadcast_pipes =
  let open Lwt_result_syntax in
  let block_header0, rpc_context0 = genesis_block in
  let* protocol_data = parse_protocol_data block_header0.protocol_data in
  let genesis0 =
    {
      rpc_context = rpc_context0;
      protocol_data;
      raw_protocol_data = block_header0.protocol_data;
      operations = [[]; []; []; []];
      resulting_context_hash = block_header0.shell.context;
    }
  in
  let chain0 = [genesis0] in
  let validated_blocks_pipe = Lwt_pipe.Unbounded.create () in
  let heads_pipe = Lwt_pipe.Unbounded.create () in
  let operations_stream, operations_stream_push = Lwt_stream.create () in
  let genesis_block_true_hash =
    Block_header.hash
      {
        shell = rpc_context0.block_header;
        protocol_data = block_header0.protocol_data;
      }
  in
  (* Only push genesis block as a new head, not a valid block: it is
     the shell's semantics to not advertise "transition" blocks. *)
  Lwt_pipe.Unbounded.push heads_pipe (rpc_context0.block_hash, block_header0) ;
  return
    {
      instance_index = i;
      live_depth;
      mempool = [];
      chain = chain0;
      chain_table =
        Block_hash.Table.of_seq
          (List.to_seq
             [
               (rpc_context0.block_hash, chain0);
               (genesis_block_true_hash, chain0);
               (genesis_predecessor_block_hash, chain0);
             ]);
      global_chain_table;
      ctxt_table =
        Context_hash.Table.of_seq
          (List.to_seq
             [
               ( rpc_context0.Tezos_protocol_environment.block_header
                   .Block_header.context,
                 rpc_context0 );
             ]);
      validated_blocks_pipe;
      heads_pipe;
      operations_stream;
      operations_stream_push;
      streaming_operations = false;
      broadcast_pipes;
      genesis_block_true_hash;
    }

class tezt_printer : Tezos_client_base.Client_context.printer =
  let open Tezos_client_base in
  let open Client_context in
  let wrap_tezt_log : (_ format4 -> _) -> _ format4 -> _ =
   fun f x ->
    Format.kasprintf
      (fun msg ->
        f "%s" msg ;
        Lwt.return_unit)
      x
  in
  object
    method error : type a b. (a, b) lwt_format -> a =
      Format.kasprintf (fun msg -> Lwt.fail (Failure msg))

    method warning : type a. (a, unit) lwt_format -> a =
      wrap_tezt_log Tezt_core.Log.warn

    method message : type a. (a, unit) lwt_format -> a =
      wrap_tezt_log (fun x -> Tezt_core.Log.info x)

    method answer : type a. (a, unit) lwt_format -> a =
      wrap_tezt_log (fun x -> Tezt_core.Log.info x)

    method log : type a. string -> (a, unit) lwt_format -> a =
      fun _log_output -> wrap_tezt_log (fun x -> Tezt_core.Log.info x)
  end

(** Start baker process. *)
let baker_process ~(delegates : Baking_state.consensus_key list) ~base_dir
    ~(genesis_block : Block_header.t * Tezos_protocol_environment.rpc_context)
    ~i ~global_chain_table ~broadcast_pipes ~(user_hooks : (module Hooks)) =
  let open Lwt_result_syntax in
  let broadcast_pipe =
    List.nth broadcast_pipes i |> WithExceptions.Option.get ~loc:__LOC__
  in
  let* state =
    create_fake_node_state
      ~i
      ~live_depth:60
      ~genesis_block
      ~global_chain_table
      ~broadcast_pipes
  in
  let filesystem = String.Hashtbl.create 10 in
  let wallet = new Faked_client_context.faked_io_wallet ~base_dir ~filesystem in
  let cctxt =
    let hooks = make_mocked_services_hooks state user_hooks in
    new Protocol_client_context.wrap_full
      (new Faked_client_context.unix_faked
         ~base_dir
         ~filesystem
         ~chain_id
         ~hooks)
  in
  let module User_hooks = (val user_hooks : Hooks) in
  let*! () = User_hooks.on_start_baker ~baker_position:i ~delegates ~cctxt in
  let* () =
    List.iter_es
      (fun ({alias; public_key; public_key_hash; secret_key_uri} :
             Baking_state.consensus_key) ->
        let open Tezos_client_base in
        let name = alias |> WithExceptions.Option.get ~loc:__LOC__ in
        let* public_key_uri = Client_keys.neuterize secret_key_uri in
        Client_keys.register_key
          wallet
          ~force:false
          (public_key_hash, public_key_uri, secret_key_uri)
          ~public_key
          name)
      delegates
  in
  let context_index =
    let open Abstract_context_index in
    {
      sync_fun = Lwt.return;
      checkout_fun =
        (fun hash ->
          Context_hash.Table.find state.ctxt_table hash
          |> Option.map (fun Tezos_protocol_environment.{context; _} -> context)
          |> Lwt.return);
      finalize_fun = Lwt.return;
    }
  in
  let module User_hooks = (val user_hooks : Hooks) in
  let listener_process () = listener ~user_hooks ~state ~broadcast_pipe in
  let stop_on_event event = User_hooks.stop_on_event event in
  let baker_process () =
    Faked_daemon.Baker.run
      ~cctxt
      ~stop_on_event
      ~chain_id
      ~context_index
      ~delegates
  in
  let* () = Lwt.pick [listener_process (); baker_process ()] in
  User_hooks.check_chain_on_success ~chain:state.chain

let genesis_protocol_data (baker_sk : Signature.secret_key)
    (predecessor_hash : Block_hash.t) (block_header : Block_header.shell_header)
    : Bytes.t =
  let proof_of_work_nonce =
    Bytes.create Protocol.Alpha_context.Constants.proof_of_work_nonce_size
  in
  let payload_hash =
    Protocol.Alpha_context.Block_payload.hash
      ~predecessor_hash
      ~payload_round:Alpha_context.Round.zero
      []
  in
  let per_block_votes =
    {
      Protocol.Per_block_votes_repr.liquidity_baking_vote =
        Baking_configuration.default_votes_config
          .Baking_configuration.liquidity_baking_vote;
      adaptive_issuance_vote =
        Baking_configuration.default_votes_config
          .Baking_configuration.adaptive_issuance_vote;
    }
  in
  let contents =
    Protocol.Alpha_context.Block_header.
      {
        payload_hash;
        payload_round = Alpha_context.Round.zero;
        proof_of_work_nonce;
        seed_nonce_hash = None;
        per_block_votes;
      }
  in
  let unsigned_header =
    Data_encoding.Binary.to_bytes_exn
      Protocol.Alpha_context.Block_header.unsigned_encoding
      (block_header, contents)
  in
  let signature =
    Signature.sign
      ~watermark:
        Alpha_context.Block_header.(to_watermark (Block_header chain_id))
      baker_sk
      unsigned_header
  in
  Data_encoding.Binary.to_bytes_exn
    Protocol.Alpha_context.Block_header.protocol_data_encoding
    {contents; signature}

(** Figure out who should be the signer for the genesis block. *)
let deduce_baker_sk
    (accounts_with_secrets :
      (Protocol.Alpha_context.Parameters.bootstrap_account
      * Tezos_mockup_commands.Mockup_wallet.bootstrap_secret)
      list) (total_accounts : int) (level : int) :
    Signature.secret_key tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* baker_index =
    match (total_accounts, level) with
    | _, 0 -> return 0 (* apparently this doesn't really matter *)
    | _ ->
        failwith
          "cannot deduce baker for a genesis block, total accounts = %d, level \
           = %d"
          total_accounts
          level
  in
  let _, secret =
    List.nth accounts_with_secrets baker_index
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  let secret_key =
    Signature.Secret_key.of_b58check_exn (Uri.path (secret.sk_uri :> Uri.t))
  in
  return secret_key

(** Generate the two initial genesis blocks. *)
let make_genesis_context ~delegate_selection ~initial_seed ~round0 ~round1
    ~consensus_committee_size ~consensus_threshold accounts_with_secrets
    (total_accounts : int) =
  let open Lwt_result_syntax in
  let default_constants = Mockup.Protocol_parameters.default_value.constants in
  let round_durations =
    let open Alpha_context in
    Stdlib.Option.get
      (Round.Durations.create_opt
         ~first_round_duration:(Period.of_seconds_exn round0)
         ~delay_increment_per_round:
           (Period.of_seconds_exn (Int64.sub round1 round0)))
  in
  let constants =
    {
      default_constants with
      initial_seed;
      consensus_committee_size;
      consensus_threshold;
      minimal_block_delay = Alpha_context.Period.of_seconds_exn (max 1L round0);
      delay_increment_per_round =
        Alpha_context.Period.of_seconds_exn Int64.(max 1L (sub round1 round0));
    }
  in
  let from_bootstrap_account i
      ( (account : Protocol.Alpha_context.Parameters.bootstrap_account),
        (secret : Tezos_mockup_commands.Mockup_wallet.bootstrap_secret) ) :
      Mockup.Parsed_account.t =
    {
      name = Format.sprintf "bootstrap%d" (i + 1);
      sk_uri = secret.sk_uri;
      amount = account.amount;
    }
  in
  let bootstrap_accounts =
    Data_encoding.Json.construct
      (Data_encoding.list Mockup.Parsed_account.encoding)
      (List.mapi from_bootstrap_account accounts_with_secrets)
  in
  let*? delegate_selection =
    let open Result_syntax in
    List.map_e
      (fun (level, round_delegates) ->
        let* level = Raw_level_repr.of_int32 level in
        let+ round_delegates =
          List.map_e
            (fun (round, delegate) ->
              let+ round = Round_repr.of_int32 round in
              (round, delegate))
            round_delegates
        in
        (level, round_delegates))
      delegate_selection
    |> Environment.wrap_tzresult
  in
  let cctxt = new tezt_printer in
  let* initial_seed =
    match (delegate_selection, constants.initial_seed) with
    | [], seed_opt -> return seed_opt
    | selection, (Some _ as seed) -> (
        let*! () = cctxt#message "Checking provided seed." in
        let* result =
          Tenderbrute.check_seed
            ~bootstrap_accounts_json:bootstrap_accounts
            ~parameters:
              Mockup.Protocol_parameters.{default_value with constants}
            ~seed
            selection
        in
        match result with
        | true -> return seed
        | false ->
            failwith "Provided initial seed does not match delegate selection")
    | _, None ->
        let*! () = cctxt#message "No initial seed provided, bruteforcing." in
        Tenderbrute.bruteforce
          ~max:100_000_000_000
          ~bootstrap_accounts_json:bootstrap_accounts
          ~parameters:Mockup.Protocol_parameters.{default_value with constants}
          delegate_selection
  in
  let*! () =
    match initial_seed with
    | None -> Lwt.return_unit
    | _ when initial_seed = constants.initial_seed -> Lwt.return_unit
    | Some seed ->
        cctxt#warning
          "Bruteforced seed is %a, please save into your test."
          State_hash.pp
          seed
  in
  let constants = {constants with initial_seed} in
  let common_parameters =
    Mockup.Protocol_parameters.{default_value with constants}
  in
  let make_block0 initial_timestamp =
    let parameters = {common_parameters with initial_timestamp} in
    let reencoded_parameters =
      Data_encoding.Binary.of_bytes_exn Mockup.M.parameters_encoding
      @@ Data_encoding.Binary.to_bytes_exn
           Mockup.Protocol_parameters.encoding
           parameters
    in
    let* {chain = _; rpc_context = rpc_context0; protocol_data = _} =
      Mockup.M.init
        ~cctxt
        ~parameters:reencoded_parameters
        ~constants_overrides_json:None
        ~bootstrap_accounts_json:(Some bootstrap_accounts)
    in
    let block_header0 =
      {
        rpc_context0.block_header with
        predecessor = genesis_predecessor_block_hash;
      }
    in
    let rpc_context = {rpc_context0 with block_header = block_header0} in
    let* baker_sk = deduce_baker_sk accounts_with_secrets total_accounts 0 in
    let protocol_data =
      genesis_protocol_data
        baker_sk
        genesis_predecessor_block_hash
        rpc_context.block_header
    in
    let block_header =
      Block_header.{shell = rpc_context.block_header; protocol_data}
    in
    return (block_header, rpc_context)
  in
  let level0_round0_duration =
    Protocol.Alpha_context.Round.round_duration
      round_durations
      Alpha_context.Round.zero
  in
  let timestamp0 =
    Time.Protocol.of_seconds
      Int64.(
        sub
          (of_float (Unix.time ()))
          (Alpha_context.Period.to_seconds level0_round0_duration))
  in
  make_block0 timestamp0

(** By default, propagate every message everywhere. *)
let default_propagation_vector = List.repeat 5 Pass

module Default_hooks : Hooks = struct
  let on_inject_block ~level:_ ~round:_ ~block_hash ~block_header ~operations
      ~protocol_data:_ =
    let open Lwt_result_syntax in
    return (block_hash, block_header, operations, default_propagation_vector)

  let on_inject_operation ~op_hash ~op =
    let open Lwt_result_syntax in
    return (op_hash, op, default_propagation_vector)

  let on_new_validated_block ~block_hash ~block_header ~operations =
    Lwt.return_some (block_hash, block_header, operations)

  let on_new_head ~block_hash ~block_header =
    Lwt.return_some (block_hash, block_header)

  let on_new_operation x = Lwt.return_some x

  let check_block_before_processing ~level:_ ~round:_ ~block_hash:_
      ~block_header:_ ~protocol_data:_ =
    Lwt_result_syntax.return_unit

  let check_chain_after_processing ~level:_ ~round:_ ~chain:_ =
    Lwt_result_syntax.return_unit

  let check_mempool_after_processing ~mempool:_ = Lwt_result_syntax.return_unit

  let stop_on_event _ = false

  let on_start_baker ~baker_position:_ ~delegates:_ ~cctxt:_ = Lwt.return_unit

  let check_chain_on_success ~chain:_ = Lwt_result_syntax.return_unit
end

type config = {
  round0 : int64;
  round1 : int64;
  timeout : int;
  delegate_selection : (int32 * (int32 * Signature.public_key_hash) list) list;
  initial_seed : State_hash.t option;
  consensus_committee_size : int;
  consensus_threshold : int;
}

let default_config =
  {
    round0 = 2L;
    (* Rounds should be long enough for the bakers to
       exchange all the necessary messages. *)
    round1 = 3L (* No real need to increase round durations. *);
    timeout = 30;
    delegate_selection = [];
    initial_seed = None;
    consensus_committee_size =
      Default_parameters.constants_mainnet.consensus_committee_size;
    consensus_threshold =
      Default_parameters.constants_mainnet.consensus_threshold;
  }

let make_baking_delegate
    ( (account : Alpha_context.Parameters.bootstrap_account),
      (secret : Tezos_mockup_commands.Mockup_wallet.bootstrap_secret) ) :
    Baking_state.consensus_key =
  Baking_state.
    {
      alias = Some secret.name;
      public_key = account.public_key |> WithExceptions.Option.get ~loc:__LOC__;
      public_key_hash = account.public_key_hash;
      secret_key_uri = secret.sk_uri;
    }

let run ?(config = default_config) bakers_spec =
  let open Lwt_result_syntax in
  Tezos_client_base.Client_keys.register_signer
    (module Tezos_signer_backends.Unencrypted) ;
  let total_accounts =
    List.fold_left (fun acc (n, _) -> acc + n) 0 bakers_spec
  in
  if total_accounts = 0 then
    failwith "the simulation should use at least one delegate"
  else if total_accounts > 5 then
    failwith "only up to 5 bootstrap accounts are available"
  else
    let total_bakers = List.length bakers_spec in
    let* broadcast_pipes =
      List.init ~when_negative_length:() total_bakers (fun _ ->
          Lwt_pipe.Unbounded.create ())
      |> function
      | Error () -> failwith "impossible: negative length of the baker spec"
      | Ok xs -> return xs
    in
    let global_chain_table = Block_hash.Table.create 10 in
    let* bootstrap_secrets =
      Tezos_mockup_commands.Mockup_wallet.default_bootstrap_accounts
    in
    let accounts_with_secrets =
      List.combine_drop (List.take_n total_accounts accounts) bootstrap_secrets
    in
    let all_delegates = List.map make_baking_delegate accounts_with_secrets in
    let* genesis_block =
      make_genesis_context
        ~delegate_selection:config.delegate_selection
        ~initial_seed:config.initial_seed
        ~round0:config.round0
        ~round1:config.round1
        ~consensus_committee_size:config.consensus_committee_size
        ~consensus_threshold:config.consensus_threshold
        accounts_with_secrets
        total_accounts
    in
    let take_third (_, _, x) = x in
    let timeout_process () =
      let*! () = Lwt_unix.sleep (Float.of_int config.timeout) in
      failwith "the test is taking longer than %d seconds@." config.timeout
    in
    Lwt.pick
      [
        timeout_process ();
        Lwt_result_syntax.tzjoin
          (take_third
             (List.fold_left
                (fun (i, delegates_acc, ms) (n, user_hooks) ->
                  let delegates, leftover_delegates =
                    List.split_n n delegates_acc
                  in
                  let m =
                    baker_process
                      ~delegates
                      ~base_dir:"dummy"
                      ~genesis_block
                      ~i
                      ~global_chain_table
                      ~broadcast_pipes
                      ~user_hooks
                  in
                  (i + 1, leftover_delegates, m :: ms))
                (0, all_delegates, [])
                bakers_spec));
      ]

let get_account_pk i =
  match List.nth accounts i with
  | None -> assert false
  | Some acc -> acc.public_key |> WithExceptions.Option.get ~loc:__LOC__

let bootstrap1 = get_account_pk 0

let bootstrap2 = get_account_pk 1

let bootstrap3 = get_account_pk 2

let bootstrap4 = get_account_pk 3

let bootstrap5 = get_account_pk 4

let check_block_signature ~block_hash ~(block_header : Block_header.t)
    ~public_key =
  let open Lwt_result_syntax in
  let (protocol_data : Protocol.Alpha_context.Block_header.protocol_data) =
    Data_encoding.Binary.of_bytes_exn
      Protocol.Alpha_context.Block_header.protocol_data_encoding
      block_header.protocol_data
  in
  let unsigned_header =
    Data_encoding.Binary.to_bytes_exn
      Protocol.Alpha_context.Block_header.unsigned_encoding
      (block_header.shell, protocol_data.contents)
  in
  if
    Signature.check
      ~watermark:
        Alpha_context.Block_header.(to_watermark (Block_header chain_id))
      public_key
      protocol_data.signature
      unsigned_header
  then return_unit
  else
    failwith
      "unexpected signature for %a; tried with %a@."
      Block_hash.pp
      block_hash
      Signature.Public_key.pp
      public_key

type op_predicate =
  Operation_hash.t -> Alpha_context.packed_operation -> bool tzresult Lwt.t

let mempool_count_ops ~mempool ~predicate =
  let open Lwt_result_syntax in
  let* results =
    List.map_es (fun (op_hash, op) -> predicate op_hash op) mempool
  in
  return
    (List.fold_left
       (fun acc result -> if result then acc + 1 else acc)
       0
       results)

let mempool_has_op ~mempool ~predicate =
  let open Lwt_result_syntax in
  let* n = mempool_count_ops ~mempool ~predicate in
  return (n > 0)

let mempool_has_op_ref ~mempool ~predicate ~var =
  let open Lwt_result_syntax in
  let* result = mempool_has_op ~mempool ~predicate in
  if result then var := true ;
  return_unit

let op_is_signed_by ~public_key (op_hash : Operation_hash.t)
    (op : Alpha_context.packed_operation) =
  let open Lwt_result_syntax in
  match op.protocol_data with
  | Operation_data d -> (
      let* watermark =
        match d.contents with
        | Single op_contents ->
            return
              (match op_contents with
              | Attestation _ ->
                  Alpha_context.Operation.to_watermark (Attestation chain_id)
              | Preattestation _ ->
                  Alpha_context.Operation.to_watermark (Preattestation chain_id)
              | _ -> Signature.Generic_operation)
        | _ -> failwith "unexpected contents in %a@." Operation_hash.pp op_hash
      in
      match d.signature with
      | None ->
          failwith
            "did not find a signature for op %a@."
            Operation_hash.pp
            op_hash
      | Some signature ->
          let unsigned_operation_bytes =
            Data_encoding.Binary.to_bytes_exn
              Protocol.Alpha_context.Operation.unsigned_encoding
              (op.shell, Contents_list d.contents)
          in
          return
            (Signature.check
               ~watermark
               public_key
               signature
               unsigned_operation_bytes))

let op_is_preattestation ?level ?round (op_hash : Operation_hash.t)
    (op : Alpha_context.packed_operation) =
  let open Lwt_result_syntax in
  match op.protocol_data with
  | Operation_data d -> (
      match d.contents with
      | Single op_contents -> (
          match op_contents with
          | Preattestation consensus_content ->
              let right_level =
                match level with
                | None -> true
                | Some expected_level ->
                    Int32.equal
                      (Alpha_context.Raw_level.to_int32 consensus_content.level)
                      expected_level
              in
              let right_round =
                match round with
                | None -> true
                | Some expected_round ->
                    Int32.equal
                      (Alpha_context.Round.to_int32 consensus_content.round)
                      expected_round
              in
              return (right_level && right_round)
          | _ -> return_false)
      | _ -> failwith "unexpected contents in %a@." Operation_hash.pp op_hash)

let op_is_attestation ?level ?round (op_hash : Operation_hash.t)
    (op : Alpha_context.packed_operation) =
  let open Lwt_result_syntax in
  match op.protocol_data with
  | Operation_data d -> (
      match d.contents with
      | Single op_contents -> (
          match op_contents with
          | Attestation consensus_content ->
              let right_level =
                match level with
                | None -> true
                | Some expected_level ->
                    Int32.equal
                      (Alpha_context.Raw_level.to_int32 consensus_content.level)
                      expected_level
              in
              let right_round =
                match round with
                | None -> true
                | Some expected_round ->
                    Int32.equal
                      (Alpha_context.Round.to_int32 consensus_content.round)
                      expected_round
              in
              return (right_level && right_round)
          | _ -> return_false)
      | _ -> failwith "unexpected contents in %a@." Operation_hash.pp op_hash)

let op_is_both f g op_hash op =
  let open Lwt_result_syntax in
  let* f_result = f op_hash op in
  if f_result then g op_hash op else return_false

let save_proposal_payload
    ~(protocol_data : Alpha_context.Block_header.protocol_data) ~var =
  let open Lwt_result_syntax in
  var :=
    Some
      (protocol_data.contents.payload_hash, protocol_data.contents.payload_round) ;
  return_unit

let verify_payload_hash
    ~(protocol_data : Alpha_context.Block_header.protocol_data)
    ~original_proposal ~message =
  let open Lwt_result_syntax in
  match !original_proposal with
  | None ->
      failwith
        "verify_payload_hash: expected to have observed a proposal by now"
  | Some (original_hash, original_round) ->
      if
        Protocol.Block_payload_hash.equal
          original_hash
          protocol_data.contents.payload_hash
        && Protocol.Alpha_context.Round.equal
             original_round
             protocol_data.contents.payload_round
      then return_unit
      else failwith "verify_payload_hash: %s" message

let get_block_round block =
  round_from_raw_fitness block.rpc_context.block_header.fitness
