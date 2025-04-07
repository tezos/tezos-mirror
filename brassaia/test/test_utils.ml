module Tezos_context_encoding = Tezos_context_brassaia_encoding
module Tezos_context_disk = Tezos_context_brassaia_disk.Tezos_context_disk
module Conf = Tezos_context_encoding.Context_binary.Conf
module Schema = Tezos_context_encoding.Context.Schema
module Context = Tezos_context_disk.Context
module Path = Brassaia.Path
open Lwt_syntax

(* Same as [let* but handle errors using [Assert.fail_msg]. *)
let ( let*!! ) x f =
  let* result = x in
  match result with
  | Error trace ->
      let message = Format.asprintf "%a" Error_monad.pp_print_trace trace in
      Assert.fail_msg "%s" message
  | Ok x -> f x

let commit = Context.commit ~time:Time.Protocol.epoch ~message:""

type action = Add of Path.t * bytes | Remove of Path.t

let create_block idx com updates =
  let* o = Context.checkout idx com in
  match o with
  | None -> Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      let* ctxt =
        Lwt_list.fold_left_s
          (fun ctxt action ->
            match action with
            | Add (path, bytes) -> Context.add ctxt path bytes
            | Remove path -> Context.remove ctxt path)
          ctxt updates
      in
      commit ctxt

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L
let chain_id = Chain_id.of_block_hash genesis_block
