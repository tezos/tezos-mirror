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
open Tezos_rpc
open Tezos_rpc_http
open Tezos_rpc_http_server

type block_id = [`Head | `Block of Block_hash.t]

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
    | `Block h -> Block_hash.to_b58check h

  let destruct_block_id h =
    if h = "head" then Ok `Head
    else
      match Block_hash.of_b58check_opt h with
      | Some b -> Ok (`Block b)
      | None -> Error "Cannot parse block id"

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
      ~descr:"A Tezos block identifier."
      ~name:"block_id"
      ~construct:construct_block_id
      ~destruct:destruct_block_id
      ()

  let context_id : context_id RPC_arg.t =
    RPC_arg.make
      ~descr:"A Tezos block or context identifier."
      ~name:"context_id"
      ~construct:construct_context_id
      ~destruct:destruct_context_id
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
      ~description:"Get the block hash handled in the tx-rollup-node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Block_hash.encoding)
      path

  let inbox =
    RPC_service.get_service
      ~description:"Get the tx-rollup-node inbox for a given Tezos block"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Alpha_context.Tx_rollup_inbox.encoding)
      RPC_path.(path / "inbox")

  let hash_of_block_id state block_id =
    match block_id with
    | `Block b -> Lwt.return (Some b)
    | `Head -> State.get_head state

  let () =
    register block @@ fun (state, block) () () ->
    let*! block = hash_of_block_id state block in
    match block with
    | None -> return None
    | Some block -> (
        let*! context_hash = State.context_hash state block in
        match context_hash with
        | Some _ -> return (Some block)
        | None -> return None)

  let () =
    register inbox @@ fun (state, block) () () ->
    let*! block = hash_of_block_id state block in
    match block with
    | None -> return None
    | Some block ->
        let*! inbox = State.find_inbox state block in
        return (Option.map Inbox.to_protocol_inbox inbox)

  let build_directory state =
    !directory
    |> RPC_directory.map (fun ((), block_id) -> Lwt.return (state, block_id))
    |> RPC_directory.prefix RPC_path.(open_root / "block" /: Arg.block_id)
end

module Context = struct
  open Lwt_tzresult_syntax

  let path = RPC_path.open_root

  let directory : Context.t RPC_directory.t ref = ref RPC_directory.empty

  let register service f =
    directory := RPC_directory.register !directory service f

  let balance =
    RPC_service.get_service
      ~description:"Get the balance for an l2-address and a ticket"
      ~query:RPC_query.empty
      ~output:Tx_rollup_l2_qty.encoding
      RPC_path.(
        path / "tickets" /: Arg.ticket_indexable / "balance"
        /: Arg.address_indexable)

  let get_index (context : Context.t) (i : (_, _) Indexable.t) get =
    match Indexable.destruct i with
    | Left i -> return (Some i)
    | Right v -> get context v

  let get_address_index context address =
    get_index context address Context.Address_index.get

  let get_ticket_index context ticket =
    get_index context ticket Context.Ticket_index.get

  let () =
    register balance @@ fun ((c, ticket), address) () () ->
    let* ticket_id = get_ticket_index c ticket in
    let* address_id = get_address_index c address in
    match (ticket_id, address_id) with
    | (None, _) | (_, None) -> return Tx_rollup_l2_qty.zero
    | (Some ticket_id, Some address_id) ->
        Context.Ticket_ledger.get c ticket_id address_id

  let hash_of_block_id state block_id =
    let open Lwt_syntax in
    let+ block = Block.hash_of_block_id state block_id in
    match block with
    | None -> Stdlib.failwith "Unknwon Tezos block"
    | Some b -> b

  let hash_of_context_id state context_id =
    let open Lwt_syntax in
    match context_id with
    | #block_id as block -> (
        let* block = hash_of_block_id state block in
        let+ ch = State.context_hash state block in
        match ch with
        | None ->
            Format.kasprintf
              Stdlib.failwith
              "No rollup context for block %a"
              Block_hash.pp
              block
        | Some ch -> ch)
    | `Context c -> Lwt.return c

  let build_directory state =
    !directory
    |> RPC_directory.map (fun ((), context_id) ->
           let open Lwt_syntax in
           let* context_hash = hash_of_context_id state context_id in
           Context.checkout_exn state.State.context_index context_hash)
    |> RPC_directory.prefix RPC_path.(open_root / "context" /: Arg.context_id)
end

let register state =
  List.fold_left
    (fun dir f -> RPC_directory.merge dir (f state))
    RPC_directory.empty
    [Block.build_directory; Context.build_directory]

let launch ~host ~acl ~node ~dir () =
  let open Lwt_tzresult_syntax in
  let*! r =
    RPC_server.launch
      ~media_types:Media_type.all_media_types
      ~host
      ~acl
      node
      dir
  in
  return r

let start configuration state =
  let open Lwt_syntax in
  let Configuration.{rpc_addr; rpc_port; _} = configuration in
  let addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string addr in
  let dir = register state in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default addr in
  Lwt.catch
    (fun () ->
      let* rpc_server = launch ~host ~acl ~node ~dir () in
      let* () = Event.(emit node_is_ready) (rpc_addr, rpc_port) in
      Lwt.return rpc_server)
    fail_with_exn
