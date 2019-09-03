(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Logging = Internal_event.Legacy_logging.Make (struct
  let name = "node.distributed_db_cache"
end)

module Message = Distributed_db_message

type 'a request_param = {
  p2p : (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.t;
  data : 'a;
  active : unit -> P2p_peer.Set.t;
  send : P2p_peer.Id.t -> Message.t -> unit;
}

module type EXTENDED_REQUESTER = sig
  include Requester.FULL_REQUESTER

  val state_of_t :
    t -> Chain_validator_worker_state.Distributed_db_state.table_scheduler
end

module type EXTENDED_REQUESTER_2 = sig
  include EXTENDED_REQUESTER

  val clear_all : t -> Block_hash.t -> int -> unit
end

module Make_raw (Hash : sig
  type t

  val name : string

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Logging : sig
    val tag : t Tag.def
  end
end)
(Disk_table : Requester.DISK_TABLE with type key := Hash.t)
(Memory_table : Requester.MEMORY_TABLE with type key := Hash.t)
(Request_message : sig
  type param

  val max_length : int

  val initial_delay : Time.System.Span.t

  val forge : param -> Hash.t list -> Message.t
end)
(Precheck : Requester.PRECHECK
              with type key := Hash.t
               and type value := Disk_table.value) :
  EXTENDED_REQUESTER
    with type key = Hash.t
     and type value = Disk_table.value
     and type request_param = Request_message.param request_param
     and type store = Disk_table.store
     and type param = Precheck.param
     and type notified_value = Precheck.notified_value = struct
  module Request = struct
    type param = Request_message.param request_param

    let active {active; _} = active ()

    let initial_delay = Request_message.initial_delay

    let rec send state gid keys =
      let (first_keys, keys) = List.split_n Request_message.max_length keys in
      let msg = Request_message.forge state.data first_keys in
      state.send gid msg ;
      let open Peer_metadata in
      let (req : requests_kind) =
        match msg with
        | Get_current_branch _ ->
            Branch
        | Get_current_head _ ->
            Head
        | Get_block_headers _ ->
            Block_header
        | Get_operations _ ->
            Operations
        | Get_protocols _ ->
            Protocols
        | Get_operation_hashes_for_blocks _ ->
            Operation_hashes_for_block
        | Get_operations_for_blocks _ ->
            Operations_for_block
        | _ ->
            Other
      in
      let meta = P2p.get_peer_metadata state.p2p gid in
      Peer_metadata.incr meta @@ Scheduled_request req ;
      if keys <> [] then send state gid keys
  end

  module Table =
    Requester.Make (Hash) (Disk_table) (Memory_table) (Request) (Precheck)
  include Table

  let state_of_t t =
    let table_length = Table.memory_table_length t in
    let scheduler_length = Table.pending_requests t in
    {
      Chain_validator_worker_state.Distributed_db_state.table_length;
      scheduler_length;
    }

  let create ?global_input request_param disk =
    Table.create ?global_input request_param disk

  let shutdown t =
    Logging.lwt_log_notice "Shutting down the cache..."
    >>= fun () -> Table.shutdown t
end

module Fake_operation_storage = struct
  type store = State.Chain.t

  type value = Operation.t

  let known _ _ = Lwt.return_false

  let read _ _ = Lwt.return (Error_monad.error_exn Not_found)

  let read_opt _ _ = Lwt.return_none
end

module Raw_operation =
  Make_raw (Operation_hash) (Fake_operation_storage) (Operation_hash.Table)
    (struct
      type param = unit

      let max_length = 10

      let initial_delay = Time.System.Span.of_seconds_exn 0.5

      let forge () keys = Message.Get_operations keys
    end)
    (struct
      type param = unit

      type notified_value = Operation.t

      let precheck _ _ v = Some v
    end)

module Block_header_storage = struct
  type store = State.Chain.t

  type value = Block_header.t

  let known = State.Block.known_valid

  let read chain_state h =
    State.Block.read chain_state h >>=? fun b -> return (State.Block.header b)

  let read_opt chain_state h =
    State.Block.read_opt chain_state h
    >>= fun b -> Lwt.return (Option.map ~f:State.Block.header b)
end

module Raw_block_header =
  Make_raw (Block_hash) (Block_header_storage) (Block_hash.Table)
    (struct
      type param = unit

      let max_length = 10

      let initial_delay = Time.System.Span.of_seconds_exn 0.5

      let forge () keys = Message.Get_block_headers keys
    end)
    (struct
      type param = unit

      type notified_value = Block_header.t

      let precheck _ _ v = Some v
    end)

module Operation_hashes_storage = struct
  type store = State.Chain.t

  type value = Operation_hash.t list

  let known chain_state (h, _) = State.Block.known_valid chain_state h

  let read chain_state (h, i) =
    State.Block.read chain_state h
    >>=? fun b ->
    State.Block.operation_hashes b i >>= fun (ops, _) -> return ops

  let read_opt chain_state (h, i) =
    State.Block.read_opt chain_state h
    >>= function
    | None ->
        Lwt.return_none
    | Some b ->
        State.Block.operation_hashes b i
        >>= fun (ops, _) -> Lwt.return_some ops
end

module Operations_table = Hashtbl.Make (struct
  type t = Block_hash.t * int

  let hash = Hashtbl.hash

  let equal (b1, i1) (b2, i2) = Block_hash.equal b1 b2 && i1 = i2
end)

module Raw_operation_hashes = struct
  include Make_raw
            (struct
              type t = Block_hash.t * int

              let name = "operation_hashes"

              let pp ppf (h, n) = Format.fprintf ppf "%a:%d" Block_hash.pp h n

              let encoding =
                let open Data_encoding in
                obj2 (req "block" Block_hash.encoding) (req "index" uint16)

              module Logging = struct
                let tag = Tag.def ~doc:"Operation hashes" "operation_hashes" pp
              end
            end)
            (Operation_hashes_storage)
            (Operations_table)
            (struct
              type param = unit

              let max_length = 10

              let initial_delay = Time.System.Span.of_seconds_exn 1.

              let forge () keys = Message.Get_operation_hashes_for_blocks keys
            end)
            (struct
              type param = Operation_list_list_hash.t

              type notified_value =
                Operation_hash.t list * Operation_list_list_hash.path

              let precheck (_block, expected_ofs) expected_hash (ops, path) =
                let (received_hash, received_ofs) =
                  Operation_list_list_hash.check_path
                    path
                    (Operation_list_hash.compute ops)
                in
                if
                  received_ofs = expected_ofs
                  && Operation_list_list_hash.compare
                       expected_hash
                       received_hash
                     = 0
                then Some ops
                else None
            end)

  let clear_all table hash n =
    List.iter (fun i -> clear_or_cancel table (hash, i)) (0 -- (n - 1))
end

module Operations_storage = struct
  type store = State.Chain.t

  type value = Operation.t list

  let known chain_state (h, _) = State.Block.known_valid chain_state h

  let read chain_state (h, i) =
    State.Block.read chain_state h
    >>=? fun b -> State.Block.operations b i >>= fun (ops, _) -> return ops

  let read_opt chain_state (h, i) =
    State.Block.read_opt chain_state h
    >>= function
    | None ->
        Lwt.return_none
    | Some b ->
        State.Block.operations b i >>= fun (ops, _) -> Lwt.return_some ops
end

module Raw_operations = struct
  include Make_raw
            (struct
              type t = Block_hash.t * int

              let name = "operations"

              let pp ppf (h, n) = Format.fprintf ppf "%a:%d" Block_hash.pp h n

              let encoding =
                let open Data_encoding in
                obj2 (req "block" Block_hash.encoding) (req "index" uint16)

              module Logging = struct
                let tag = Tag.def ~doc:"Operations" "operations" pp
              end
            end)
            (Operations_storage)
            (Operations_table)
            (struct
              type param = unit

              let max_length = 10

              let initial_delay = Time.System.Span.of_seconds_exn 1.

              let forge () keys = Message.Get_operations_for_blocks keys
            end)
            (struct
              type param = Operation_list_list_hash.t

              type notified_value =
                Operation.t list * Operation_list_list_hash.path

              let precheck (_block, expected_ofs) expected_hash (ops, path) =
                let (received_hash, received_ofs) =
                  Operation_list_list_hash.check_path
                    path
                    (Operation_list_hash.compute (List.map Operation.hash ops))
                in
                if
                  received_ofs = expected_ofs
                  && Operation_list_list_hash.compare
                       expected_hash
                       received_hash
                     = 0
                then Some ops
                else None
            end)

  let clear_all table hash n =
    List.iter (fun i -> clear_or_cancel table (hash, i)) (0 -- (n - 1))
end

module Protocol_storage = struct
  type store = State.t

  type value = Protocol.t

  let known = State.Protocol.known

  let read = State.Protocol.read

  let read_opt = State.Protocol.read_opt
end

module Raw_protocol =
  Make_raw (Protocol_hash) (Protocol_storage) (Protocol_hash.Table)
    (struct
      type param = unit

      let initial_delay = Time.System.Span.of_seconds_exn 10.

      let max_length = 10

      let forge () keys = Message.Get_protocols keys
    end)
    (struct
      type param = unit

      type notified_value = Protocol.t

      let precheck _ _ v = Some v
    end)
