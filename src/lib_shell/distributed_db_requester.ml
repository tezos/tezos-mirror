(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
module Message = Distributed_db_message
module Requester_event = Distributed_db_event.Requester_event

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

module type REQUEST_MESSAGE = sig
  type param

  type hash

  val max_length : int

  val initial_delay : Time.System.Span.t

  val forge : param -> hash list -> Message.t
end

module Make_raw
    (Hash : Requester.HASH)
    (Disk_table : Requester.DISK_TABLE with type key := Hash.t)
    (Memory_table : Hashtbl.SeededS with type key := Hash.t)
    (Request_message : REQUEST_MESSAGE with type hash := Hash.t)
    (Probe :
      Requester.PROBE with type key := Hash.t and type value := Disk_table.value) :
  EXTENDED_REQUESTER
    with type key = Hash.t
     and type value = Disk_table.value
     and type request_param = Request_message.param request_param
     and type store = Disk_table.store
     and type param = Probe.param
     and type notified_value = Probe.notified_value = struct
  module Request = struct
    type param = Request_message.param request_param

    let active {active; _} = active ()

    let initial_delay = Request_message.initial_delay

    let rec send state gid keys =
      let first_keys, keys = List.split_n Request_message.max_length keys in
      let msg = Request_message.forge state.data first_keys in
      state.send gid msg ;
      let open Peer_metadata in
      let (req : requests_kind) =
        match msg with
        | Get_current_branch _ -> Branch
        | Get_current_head _ -> Head
        | Get_block_headers _ -> Block_header
        | Get_operations _ -> Operations
        | Get_protocols _ -> Protocols
        | Get_operations_for_blocks _ -> Operations_for_block
        | _ -> Other
      in
      let meta = P2p.get_peer_metadata state.p2p gid in
      Peer_metadata.incr meta @@ Scheduled_request req ;
      if keys <> [] then send state gid keys
  end

  module Monitored_memory_table = struct
    type 'a t = {
      table : 'a Memory_table.t;
      metrics : Shell_metrics.Distributed_db.t;
    }

    let create ~entry_type ?random s =
      {
        table = Memory_table.create ?random s;
        metrics = Shell_metrics.Distributed_db.init ~kind:Hash.name ~entry_type;
      }

    let find t x = Memory_table.find t.table x

    let add t k x =
      Memory_table.add t.table k x ;
      Shell_metrics.Distributed_db.update
        t.metrics
        ~length:(Memory_table.length t.table)

    let replace t k x =
      Memory_table.replace t.table k x ;
      Shell_metrics.Distributed_db.update
        t.metrics
        ~length:(Memory_table.length t.table)

    let remove t k =
      Memory_table.remove t.table k ;
      Shell_metrics.Distributed_db.update
        t.metrics
        ~length:(Memory_table.length t.table)

    let length t = Memory_table.length t.table

    let fold f t x = Memory_table.fold f t.table x
  end

  module Table =
    Requester.Make (Hash) (Disk_table) (Monitored_memory_table) (Request)
      (Probe)
  include Table

  let state_of_t t =
    let table_length = Table.memory_table_length t in
    let scheduler_length = Table.pending_requests t in
    {
      Chain_validator_worker_state.Distributed_db_state.table_length;
      scheduler_length;
    }

  let create ?random_table ?global_input request_param disk =
    Table.create ?random_table ?global_input request_param disk

  let shutdown t =
    let open Lwt_syntax in
    let* () = Requester_event.(emit shutting_down_requester) () in
    Table.shutdown t
end

module Fake_operation_storage = struct
  type store = Store.chain_store

  type value = Operation.t

  let known _ _ = Lwt.return_false

  let read _ _ = fail_with_exn Not_found

  let read_opt _ _ = Lwt.return_none
end

module Raw_operation =
  Make_raw
    (struct
      include Operation_hash

      let name = "operation"
    end)
    (Fake_operation_storage)
    (Operation_hash.Table)
    (struct
      type param = unit

      let max_length = 10

      let initial_delay = Time.System.Span.of_seconds_exn 0.5

      let forge () keys = Message.Get_operations keys
    end)
    (struct
      type param = unit

      type notified_value = Operation.t

      let probe _ _ v = Some v
    end)

module Block_header_storage = struct
  type store = Store.chain_store

  type value = Block_header.t

  let known chain_store hash =
    let open Lwt_syntax in
    let* b = Store.Block.is_known_valid chain_store hash in
    match b with
    | true -> Lwt.return_true
    | false -> Store.Block.is_known_validated chain_store hash

  let read chain_store h =
    let open Lwt_result_syntax in
    let* b =
      let*! r = Store.Block.read_block chain_store h in
      match r with
      | Ok b -> return b
      | Error _ -> Store.Block.read_validated_block chain_store h
    in
    return (Store.Block.header b)

  let read_opt chain_store h =
    let open Lwt_syntax in
    let* b =
      let* o = Store.Block.read_block_opt chain_store h in
      match o with
      | Some b -> Lwt.return_some b
      | None -> Store.Block.read_validated_block_opt chain_store h
    in
    Lwt.return (Option.map Store.Block.header b)
end

module Raw_block_header =
  Make_raw
    (struct
      include Block_hash

      let name = "block_header"
    end)
    (Block_header_storage)
    (Block_hash.Table)
    (struct
      type param = unit

      let max_length = 10

      let initial_delay = Time.System.Span.of_seconds_exn 0.5

      let forge () keys = Message.Get_block_headers keys
    end)
    (struct
      type param = unit

      type notified_value = Block_header.t

      let probe _ _ v = Some v
    end)

module Operations_table = Hashtbl.MakeSeeded (struct
  type t = Block_hash.t * int

  (* See [src/lib_base/tzPervasives.ml] for an explanation *)
  [@@@ocaml.warning "-32"]

  let hash = Hashtbl.seeded_hash

  let seeded_hash = Hashtbl.seeded_hash

  [@@@ocaml.warning "+32"]

  let equal (b1, i1) (b2, i2) = Block_hash.equal b1 b2 && i1 = i2
end)

module Operations_storage = struct
  type store = Store.chain_store

  type value = Operation.t list

  let known chain_store (h, _) = Store.Block.is_known_valid chain_store h

  let read chain_store (h, i) =
    let open Lwt_result_syntax in
    let* b = Store.Block.read_block chain_store h in
    let ops =
      List.nth (Store.Block.operations b) i
      |> WithExceptions.Option.to_exn ~none:Not_found
    in
    return ops

  let read_opt chain_store (h, i) =
    let open Lwt_syntax in
    let* o = Store.Block.read_block_opt chain_store h in
    match o with
    | None -> Lwt.return_none
    | Some b -> Lwt.return (List.nth (Store.Block.operations b) i)
end

module Raw_operations = struct
  include
    Make_raw
      (struct
        type t = Block_hash.t * int

        let name = "operations"

        let pp ppf (h, n) = Format.fprintf ppf "%a:%d" Block_hash.pp h n

        let encoding =
          let open Data_encoding in
          obj2 (req "block" Block_hash.encoding) (req "index" uint16)
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

        type notified_value = Operation.t list * Operation_list_list_hash.path

        let probe (_block, expected_ofs) expected_hash (ops, path) =
          let received_hash, received_ofs =
            Operation_list_list_hash.check_path
              path
              (Operation_list_hash.compute (List.map Operation.hash ops))
          in
          if
            received_ofs = expected_ofs
            && Operation_list_list_hash.compare expected_hash received_hash = 0
          then Some ops
          else None
      end)

  let clear_all table hash n =
    List.iter (fun i -> clear_or_cancel table (hash, i)) (0 -- (n - 1))
end

module Protocol_storage = struct
  type store = Store.store

  type value = Protocol.t

  let known store ph = Lwt.return (Store.Protocol.mem store ph)

  let read_opt store ph = Store.Protocol.read store ph

  let read store ph =
    let open Lwt_syntax in
    let* o = read_opt store ph in
    match o with None -> fail_with_exn Not_found | Some p -> return_ok p
end

module Raw_protocol =
  Make_raw
    (struct
      include Protocol_hash

      let name = "protocol"
    end)
    (Protocol_storage)
    (Protocol_hash.Table)
    (struct
      type param = unit

      let initial_delay = Time.System.Span.of_seconds_exn 10.

      let max_length = 10

      let forge () keys = Message.Get_protocols keys
    end)
    (struct
      type param = unit

      type notified_value = Protocol.t

      let probe _ _ v = Some v
    end)
