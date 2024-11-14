(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

open Data_encoding

type chain = [`Main | `Test | `Hash of Chain_id.t]

let chain_arg = Block_services.chain_arg

let to_string = Block_services.chain_to_string

let parse_chain = Block_services.parse_chain

type invalid_block = {hash : Block_hash.t; level : Int32.t; errors : error list}

type prefix = Block_services.chain_prefix

let path = Block_services.chain_path

let block_descriptor_encoding =
  obj2 (req "block_hash" Block_hash.encoding) (req "level" int32)

let invalid_block_encoding =
  conv
    (fun {hash; level; errors} -> (hash, level, errors))
    (fun (hash, level, errors) -> {hash; level; errors})
    (obj3
       (req "block" Block_hash.encoding)
       (req "level" int32)
       (req "errors" Tezos_rpc.Error.encoding))

let bootstrap_encoding =
  obj2
    (req "bootstrapped" Encoding.bool)
    (req "sync_state" Chain_validator_worker_state.sync_status_encoding)

module S = struct
  let path : prefix Tezos_rpc.Path.context = Tezos_rpc.Path.open_root

  let chain_id =
    Tezos_rpc.Service.get_service
      ~description:"The chain unique identifier."
      ~query:Tezos_rpc.Query.empty
      ~output:Chain_id.encoding
      Tezos_rpc.Path.(path / "chain_id")

  let is_bootstrapped =
    Tezos_rpc.Service.get_service
      ~description:"The bootstrap status of a chain"
      ~query:Tezos_rpc.Query.empty
      ~output:bootstrap_encoding
      Tezos_rpc.Path.(path / "is_bootstrapped")

  let bootstrapped_flag_encoding =
    let open Data_encoding in
    obj1 (req "bootstrapped" bool)

  let force_bootstrapped =
    Tezos_rpc.Service.patch_service
      ~description:"Forcefully set the bootstrapped flag of the node"
      ~query:Tezos_rpc.Query.empty
      ~input:bootstrapped_flag_encoding
      ~output:unit
      path

  module Levels = struct
    let path = Tezos_rpc.Path.(path / "levels")

    let checkpoint =
      Tezos_rpc.Service.get_service
        ~description:"The current checkpoint for this chain."
        ~query:Tezos_rpc.Query.empty
        ~output:block_descriptor_encoding
        Tezos_rpc.Path.(path / "checkpoint")

    let savepoint =
      Tezos_rpc.Service.get_service
        ~description:"The current savepoint for this chain."
        ~query:Tezos_rpc.Query.empty
        ~output:block_descriptor_encoding
        Tezos_rpc.Path.(path / "savepoint")

    let caboose =
      Tezos_rpc.Service.get_service
        ~description:"The current caboose for this chain."
        ~query:Tezos_rpc.Query.empty
        ~output:block_descriptor_encoding
        Tezos_rpc.Path.(path / "caboose")
  end

  module Blocks = struct
    let list_query =
      let open Tezos_rpc.Query in
      query (fun length heads min_date ->
          object
            method length = length

            method heads = heads

            method min_date = min_date
          end)
      |+ opt_field
           "length"
           ~descr:
             "The requested number of predecessors to return (per request; see \
              next argument)."
           Tezos_rpc.Arg.uint
           (fun x -> x#length)
      |+ multi_field
           "head"
           ~descr:
             "An empty argument requests blocks starting with the current \
              head. A non empty list allows to request one or more specific \
              fragments of the chain."
           Block_hash.rpc_arg
           (fun x -> x#heads)
      |+ opt_field
           "min_date"
           ~descr:
             "When `min_date` is provided, blocks with a timestamp before \
              `min_date` are filtered out. However, if the `length` parameter \
              is also provided, then up to that number of predecessors will be \
              returned regardless of their date."
           Time.Protocol.rpc_arg
           (fun x -> x#min_date)
      |> seal

    let path = Tezos_rpc.Path.(path / "blocks")

    let list =
      let open Data_encoding in
      Tezos_rpc.Service.get_service
        ~description:
          "Lists block hashes from '<chain>', up to the last checkpoint, \
           sorted with decreasing fitness. Without arguments it returns the \
           head of the chain. Optional arguments allow to return the list of \
           predecessors of a given block or of a set of blocks."
        ~query:list_query
        ~output:(list (list Block_hash.encoding))
        path
  end

  module Invalid_blocks = struct
    let path = Tezos_rpc.Path.(path / "invalid_blocks")

    let list =
      Tezos_rpc.Service.get_service
        ~description:
          "Lists blocks that have been declared invalid along with the errors \
           that led to them being declared invalid."
        ~query:Tezos_rpc.Query.empty
        ~output:(list invalid_block_encoding)
        path

    let get =
      Tezos_rpc.Service.get_service
        ~description:"The errors that appears during the block (in)validation."
        ~query:Tezos_rpc.Query.empty
        ~output:invalid_block_encoding
        Tezos_rpc.Path.(path /: Block_hash.rpc_arg)

    let delete =
      Tezos_rpc.Service.delete_service
        ~description:"Remove an invalid block for the tezos storage"
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.empty
        Tezos_rpc.Path.(path /: Block_hash.rpc_arg)
  end
end

let make_call0 s ctxt chain q p =
  let s = Tezos_rpc.Service.prefix path s in
  Tezos_rpc.Context.make_call1 s ctxt chain q p

let make_call1 s ctxt chain a q p =
  let s = Tezos_rpc.Service.prefix path s in
  Tezos_rpc.Context.make_call2 s ctxt chain a q p

let chain_id ctxt =
  let f = make_call0 S.chain_id ctxt in
  fun ?(chain = `Main) () ->
    match chain with `Hash h -> Lwt.return_ok h | _ -> f chain () ()

module Levels = struct
  let checkpoint ctxt ?(chain = `Main) () =
    make_call0 S.Levels.checkpoint ctxt chain () ()

  let savepoint ctxt ?(chain = `Main) () =
    make_call0 S.Levels.savepoint ctxt chain () ()

  let caboose ctxt ?(chain = `Main) () =
    make_call0 S.Levels.caboose ctxt chain () ()
end

module Blocks = struct
  let list ctxt =
    let f = make_call0 S.Blocks.list ctxt in
    fun ?(chain = `Main) ?(heads = []) ?length ?min_date () ->
      f
        chain
        (object
           method heads = heads

           method length = length

           method min_date = min_date
        end)
        ()

  include Block_services.Empty

  type protocols = Block_services.protocols = {
    current_protocol : Protocol_hash.t;
    next_protocol : Protocol_hash.t;
  }

  let protocols = Block_services.protocols
end

module Mempool = Block_services.Empty.Mempool

module Invalid_blocks = struct
  let list ctxt =
    let f = make_call0 S.Invalid_blocks.list ctxt in
    fun ?(chain = `Main) () -> f chain () ()

  let get ctxt =
    let f = make_call1 S.Invalid_blocks.get ctxt in
    fun ?(chain = `Main) block -> f chain block () ()

  let delete ctxt =
    let f = make_call1 S.Invalid_blocks.delete ctxt in
    fun ?(chain = `Main) block -> f chain block () ()
end
