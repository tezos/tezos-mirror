(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
open Protocol

type block_id =
  [ `Head
  | `L2_block of L2block.hash
  | `Tezos_block of Block_hash.t
  | `Level of L2block.level ]

type context_id = [block_id | `Context of Tx_rollup_l2_context_hash.t]

module Arg = struct
  let indexable ~kind ~construct ~destruct =
    let construct i =
      match Indexable.destruct i with
      | Left i -> Int32.to_string @@ Indexable.to_int32 i
      | Right x -> construct x
    in
    let destruct s =
      match destruct s with
      | Some a -> Ok Indexable.(forget @@ from_value a)
      | None -> (
          match Int32.of_string_opt s with
          | Some i ->
              Indexable.from_index i
              |> Result.map_error (fun _ -> "Invalid index")
              |> Result.map Indexable.forget
          | None -> Error ("Cannot parse index or " ^ kind))
    in
    RPC_arg.make
      ~descr:
        (Format.sprintf "An index or an L2 %s in the rollup in b58check." kind)
      ~name:(kind ^ "_indexable")
      ~construct
      ~destruct
      ()

  let address_indexable =
    indexable
      ~kind:"address"
      ~construct:Tx_rollup_l2_address.to_b58check
      ~destruct:Tx_rollup_l2_address.of_b58check_opt

  let ticket_indexable =
    let open Alpha_context in
    indexable
      ~kind:"ticket_hash"
      ~construct:Ticket_hash.to_b58check
      ~destruct:Ticket_hash.of_b58check_opt

  let construct_block_id = function
    | `Head -> "head"
    | `L2_block h -> L2block.Hash.to_b58check h
    | `Tezos_block h -> Block_hash.to_b58check h
    | `Level l -> L2block.level_to_string l

  let destruct_block_id h =
    match h with
    | "head" -> Ok `Head
    | "genesis" -> Ok (`Level L2block.Genesis)
    | _ -> (
        match Int32.of_string_opt h with
        | Some l -> (
            match Alpha_context.Tx_rollup_level.of_int32 l with
            | Error _ -> Error "Invalid rollup level"
            | Ok l -> Ok (`Level (L2block.Rollup_level l)))
        | None -> (
            match Block_hash.of_b58check_opt h with
            | Some b -> Ok (`Tezos_block b)
            | None -> (
                match L2block.Hash.of_b58check_opt h with
                | Some b -> Ok (`L2_block b)
                | None -> Error "Cannot parse block id")))

  let construct_context_id = function
    | #block_id as id -> construct_block_id id
    | `Context h -> Tx_rollup_l2_context_hash.to_b58check h

  let destruct_context_id h =
    match destruct_block_id h with
    | Ok b -> Ok b
    | Error _ -> (
        match Tx_rollup_l2_context_hash.of_b58check_opt h with
        | Some c -> Ok (`Context c)
        | None -> Error "Cannot parse block or context hash")

  let block_id : block_id RPC_arg.t =
    RPC_arg.make
      ~descr:"An L2 block identifier."
      ~name:"block_id"
      ~construct:construct_block_id
      ~destruct:destruct_block_id
      ()

  let context_id : context_id RPC_arg.t =
    RPC_arg.make
      ~descr:"An L2 block or context identifier."
      ~name:"context_id"
      ~construct:construct_context_id
      ~destruct:destruct_context_id
      ()

  let l2_transaction : L2_transaction.hash RPC_arg.t =
    RPC_arg.make
      ~descr:"An L2 transaction identifier."
      ~name:"l2_transaction_hash"
      ~construct:L2_transaction.Hash.to_b58check
      ~destruct:(fun s ->
        match L2_transaction.Hash.of_b58check_opt s with
        | None -> Error "Cannot parse L2 transaction hash"
        | Some h -> Ok h)
      ()
end

module Block = struct
  open Lwt_tzresult_syntax

  let path = RPC_path.(open_root)

  let directory : (State.t * block_id) RPC_directory.t ref =
    ref RPC_directory.empty

  let register service f =
    directory := RPC_directory.register !directory service f

  let block =
    RPC_service.get_service
      ~description:"Get the L2 block in the tx-rollup-node"
      ~query:RPC_query.empty
      ~output:
        Data_encoding.(
          option
          @@ merge_objs
               (obj1 (req "hash" L2block.Hash.encoding))
               L2block.encoding)
      path

  let header =
    RPC_service.get_service
      ~description:"Get the L2 block header in the tx-rollup-node"
      ~query:RPC_query.empty
      ~output:
        Data_encoding.(
          option
          @@ merge_objs
               (obj1 (req "hash" L2block.Hash.encoding))
               L2block.header_encoding)
      RPC_path.(path / "header")

  let inbox =
    RPC_service.get_service
      ~description:"Get the tx-rollup-node inbox for a given block"
      ~query:RPC_query.empty
      ~output:Data_encoding.(option Inbox.encoding)
      RPC_path.(path / "inbox")

  let block_header_of_id state block_id =
    let open Lwt_syntax in
    match block_id with
    | `L2_block b -> State.get_header state b
    | `Tezos_block b ->
        let+ b = State.get_tezos_l2_block state b in
        Option.map (fun b -> b.L2block.header) b
    | `Head -> return_some (State.get_head state).header
    | `Level l -> State.get_level_l2_block_header state l

  let block_of_id state block_id =
    let open Lwt_syntax in
    match block_id with
    | `L2_block b -> State.get_block state b
    | `Tezos_block b -> State.get_tezos_l2_block state b
    | `Head -> return_some (State.get_head state)
    | `Level l -> State.get_level_l2_block state l

  let () =
    register block @@ fun (state, block_id) () () ->
    let*! block = block_of_id state block_id in
    match block with
    | None -> return None
    | Some block -> return (Some (L2block.hash_header block.header, block))

  let () =
    register header @@ fun (state, block_id) () () ->
    let*! header = block_header_of_id state block_id in
    match header with
    | None -> return None
    | Some header -> return (Some (L2block.hash_header header, header))

  let () =
    register inbox @@ fun (state, block_id) () () ->
    let*! block = block_of_id state block_id in
    match block with
    | None -> return None
    | Some block -> (
        match block_id with
        | `Tezos_block b when Block_hash.(block.header.tezos_block <> b) ->
            (* Tezos block has no l2 inbox *)
            return None
        | _ -> return (Some block.inbox))

  let build_directory state =
    !directory
    |> RPC_directory.map (fun ((), block_id) -> Lwt.return (state, block_id))
    |> RPC_directory.prefix RPC_path.(open_root / "block" /: Arg.block_id)
end

module Context_RPC = struct
  open Lwt_tzresult_syntax

  let path = RPC_path.open_root

  let directory : Context.t RPC_directory.t ref = ref RPC_directory.empty

  let register service f =
    directory := RPC_directory.register !directory service f

  type address_metadata = {
    index : Tx_rollup_l2_context_sig.address_index;
    counter : int64;
    public_key : Environment.Bls_signature.pk;
  }

  let bls_pk_encoding =
    Data_encoding.(
      conv_with_guard
        Environment.Bls_signature.pk_to_bytes
        (fun x ->
          Option.to_result
            ~none:"not a valid bls public key"
            (Environment.Bls_signature.pk_of_bytes_opt x))
        bytes)

  let address_metadata_encoding =
    Data_encoding.(
      conv
        (fun {index; counter; public_key} -> (index, counter, public_key))
        (fun (index, counter, public_key) -> {index; counter; public_key})
      @@ obj3
           (req "index" Tx_rollup_l2_address.Indexable.index_encoding)
           (req "counter" int64)
           (req "public_key" bls_pk_encoding))

  let balance =
    RPC_service.get_service
      ~description:"Get the balance for an l2-address and a ticket"
      ~query:RPC_query.empty
      ~output:Tx_rollup_l2_qty.encoding
      RPC_path.(
        path / "tickets" /: Arg.ticket_indexable / "balance"
        /: Arg.address_indexable)

  let tickets_count =
    RPC_service.get_service
      ~description:
        "Get the number of tickets that have been involved in the transaction \
         rollup."
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "count" / "tickets")

  let addresses_count =
    RPC_service.get_service
      ~description:
        "Get the number of addresses that have been involved in the \
         transaction rollup."
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "count" / "addresses")

  let ticket_index =
    RPC_service.get_service
      ~description:
        "Get the index for the given ticket hash, or null if the ticket is not \
         known by the rollup."
      ~query:RPC_query.empty
      ~output:
        (Data_encoding.option
           Tx_rollup_l2_context_sig.Ticket_indexable.index_encoding)
      RPC_path.(path / "tickets" /: Arg.ticket_indexable / "index")

  let address_metadata =
    RPC_service.get_service
      ~description:
        "Get the metadata associated to a given address, or null if the \
         address has not performed any transfer or withdraw on the rollup."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option address_metadata_encoding)
      RPC_path.(path / "addresses" /: Arg.address_indexable / "metadata")

  let address_index =
    RPC_service.get_service
      ~description:
        "Get the index for the given address, or null if the address is not \
         known by the rollup."
      ~query:RPC_query.empty
      ~output:
        (Data_encoding.option Tx_rollup_l2_address.Indexable.index_encoding)
      RPC_path.(path / "addresses" /: Arg.address_indexable / "index")

  let address_counter =
    RPC_service.get_service
      ~description:"Get the current counter for the given address."
      ~query:RPC_query.empty
      ~output:Data_encoding.int64
      RPC_path.(path / "addresses" /: Arg.address_indexable / "counter")

  let address_public_key =
    RPC_service.get_service
      ~description:
        "Get the BLS public key associated to the given address, or null if \
         the address has not performed any transfer or withdraw on the rollup."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option bls_pk_encoding)
      RPC_path.(path / "addresses" /: Arg.address_indexable / "public_key")

  let get_index ?(check_index = false) (context : Context.t)
      (i : (_, _) Indexable.t) get count =
    match Indexable.destruct i with
    | Left i ->
        if check_index then
          let* number_indexes = count context in
          if Indexable.to_int32 i >= number_indexes then return None
          else return (Some i)
        else return (Some i)
    | Right v -> get context v

  let get_address_index ?check_index context address =
    get_index
      ?check_index
      context
      address
      Context.Address_index.get
      Context.Address_index.count

  let get_ticket_index ?check_index context ticket =
    get_index
      ?check_index
      context
      ticket
      Context.Ticket_index.get
      Context.Ticket_index.count

  let () =
    register balance @@ fun ((c, ticket), address) () () ->
    let* ticket_id = get_ticket_index c ticket in
    let* address_id = get_address_index c address in
    match (ticket_id, address_id) with
    | (None, _) | (_, None) -> return Tx_rollup_l2_qty.zero
    | (Some ticket_id, Some address_id) ->
        Context.Ticket_ledger.get c ticket_id address_id

  let () = register tickets_count @@ fun c () () -> Context.Ticket_index.count c

  let () =
    register addresses_count @@ fun c () () -> Context.Address_index.count c

  let () =
    register ticket_index @@ fun (c, ticket) () () ->
    get_ticket_index ~check_index:true c ticket

  let () =
    register address_index @@ fun (c, address) () () ->
    get_address_index ~check_index:true c address

  let () =
    register address_metadata @@ fun (c, address) () () ->
    let* address_index = get_address_index c address in
    match address_index with
    | None -> return None
    | Some address_index -> (
        let* metadata = Context.Address_metadata.get c address_index in
        match metadata with
        | None -> return None
        | Some {counter; public_key} ->
            return (Some {index = address_index; counter; public_key}))

  let () =
    register address_counter @@ fun (c, address) () () ->
    let* address_index = get_address_index c address in
    match address_index with
    | None -> return 0L
    | Some address_index -> (
        let* metadata = Context.Address_metadata.get c address_index in
        match metadata with
        | None -> return 0L
        | Some {counter; _} -> return counter)

  let () =
    register address_public_key @@ fun (c, address) () () ->
    let* address_index = get_address_index c address in
    match address_index with
    | None -> return None
    | Some address_index -> (
        let* metadata = Context.Address_metadata.get c address_index in
        match metadata with
        | None -> return None
        | Some {public_key; _} -> return (Some public_key))

  let context_of_block_id state block_id =
    let open Lwt_syntax in
    let*! header = Block.block_header_of_id state block_id in
    match header with
    | None -> Stdlib.failwith "Unknown block id"
    | Some b -> return b.context

  let context_of_id state context_id =
    match context_id with
    | #block_id as block_id -> context_of_block_id state block_id
    | `Context c -> Lwt.return c

  let build_directory state =
    !directory
    |> RPC_directory.map (fun ((), context_id) ->
           let open Lwt_syntax in
           let* context_hash = context_of_id state context_id in
           Context.checkout_exn state.State.context_index context_hash)
    |> RPC_directory.prefix RPC_path.(open_root / "context" /: Arg.context_id)
end

module Injection = struct
  let path = RPC_path.(open_root / "queue")

  let directory : Batcher.state RPC_directory.t ref = ref RPC_directory.empty

  let register service f =
    directory := RPC_directory.register !directory service f

  let build_directory state =
    match state.State.batcher_state with
    | None -> RPC_directory.empty
    | Some batcher_state ->
        !directory |> RPC_directory.map (fun () -> Lwt.return batcher_state)

  let inject_query =
    let open RPC_query in
    query (fun eager_batch ->
        object
          method eager_batch = eager_batch
        end)
    |+ flag "eager_batch" (fun t -> t#eager_batch)
    |> seal

  let inject_transaction =
    RPC_service.post_service
      ~description:"Inject an L2 transaction in the queue of the rollup node."
      ~query:inject_query
      ~input:L2_transaction.encoding
      ~output:L2_transaction.Hash.encoding
      RPC_path.(path / "injection" / "transaction")

  let get_transaction =
    RPC_service.get_service
      ~description:"Retrieve an L2 transaction in the queue."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option L2_transaction.encoding)
      RPC_path.(path / "transaction" /: Arg.l2_transaction)

  let get_queue =
    RPC_service.get_service
      ~description:"Get the whole queue of L2 transactions."
      ~query:RPC_query.empty
      ~output:(Data_encoding.list L2_transaction.encoding)
      path

  let () =
    register inject_transaction (fun state q transaction ->
        Batcher.register_transaction
          ~eager_batch:q#eager_batch
          state
          transaction)

  let () =
    register get_transaction (fun (state, tr_hash) () () ->
        return @@ Batcher.find_transaction state tr_hash)

  let () =
    register get_queue (fun state () () -> return @@ Batcher.get_queue state)
end

let register state =
  List.fold_left
    (fun dir f -> RPC_directory.merge dir (f state))
    RPC_directory.empty
    [
      Block.build_directory;
      Context_RPC.build_directory;
      Injection.build_directory;
    ]

let launch ~host ~acl ~node ~dir () =
  RPC_server.launch ~media_types:Media_type.all_media_types ~host ~acl node dir

let start configuration state =
  let open Lwt_result_syntax in
  let Configuration.{rpc_addr; _} = configuration in
  let (host, rpc_port) = rpc_addr in
  let host = P2p_addr.to_string host in
  let dir = register state in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  Lwt.catch
    (fun () ->
      let*! rpc_server = launch ~host ~acl ~node ~dir () in
      let*! () = Event.(emit rpc_server_is_ready) rpc_addr in
      return rpc_server)
    fail_with_exn
