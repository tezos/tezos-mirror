(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt_result_syntax

let print_failures f =
  let*! o = f in
  match o with
  | Ok () -> Lwt.return_unit
  | Error e ->
      let () = Error_monad.pp_print_trace Format.err_formatter e in
      Lwt.return_unit

let split_endorsements_preendorsements operations =
  List.fold_left
    (fun (en, pre) (Consensus_ops.{op = {kind; _}; _} as x) ->
      match kind with
      | Consensus_ops.Preendorsement -> (en, x :: pre)
      | Consensus_ops.Endorsement -> (x :: en, pre))
    ([], [])
    operations

let wallet cctx =
  let*! wallet_res = Wallet.of_context cctx in
  match wallet_res with
  | Ok aliases -> Lwt.return aliases
  | Error err ->
      let () = Error_monad.pp_print_trace Format.err_formatter err in
      Lwt.return Wallet.empty

module Ring =
  (val Ringo.set_maker
         ~replacement:Ringo.LRU
         ~overflow:Ringo.Strong
         ~accounting:Ringo.Precise)
    (struct
      type t = Int32.t

      let hash = Hashtbl.hash

      let equal = Int32.equal
    end)

module type TABLES = sig
  val registered_rights_levels : Ring.t

  val rights_machine :
    (Client_context.full -> int32 -> Consensus_ops.rights tzresult Lwt.t)
    Protocol_hash.Table.t

  val block_machine :
    (Client_context.full ->
    int32 ->
    ( Data.Block.t
      * (Consensus_ops.block_op trace * Consensus_ops.block_op trace),
      tztrace )
    result
    Lwt.t)
    Protocol_hash.Table.t

  val endorsement_machine :
    (Client_context.full -> int32 -> (unit, tztrace) result Lwt.t)
    Protocol_hash.Table.t

  val live_block_machine :
    ((Client_context.full -> int32 -> Consensus_ops.rights tzresult Lwt.t)
    * (Client_context.full ->
      Block_hash.t ->
      Block_header.t ->
      Ptime.t ->
      ( Data.Block.t
        * (Consensus_ops.block_op trace * Consensus_ops.block_op trace),
        tztrace )
      result
      Lwt.t))
    Protocol_hash.Table.t
end

module Tables (Archiver : Archiver.S) = struct
  let registered_rights_levels = Ring.create 120

  let rights_machine = Protocol_hash.Table.create 10

  let block_machine = Protocol_hash.Table.create 10

  let endorsement_machine = Protocol_hash.Table.create 10

  let live_block_machine = Protocol_hash.Table.create 10
end

module Db_tables = Tables (Db_archiver)
module Json_tables = Tables (Json_archiver)
module Server_tables = Tables (Server_archiver)

let maybe_add_rights (module T : TABLES) (module A : Archiver.S) level rights
    wallet =
  if Ring.mem T.registered_rights_levels level then ()
  else
    let () = Ring.add T.registered_rights_levels level in
    A.add_rights ~level rights wallet

module Define
    (Tables : TABLES)
    (Services : Protocol_machinery.PROTOCOL_SERVICES)
    (Archiver : Archiver.S) =
struct
  let rights_of ctxt level =
    let cctx = Services.wrap_full ctxt in
    Services.endorsing_rights cctx ~reference_level:level level

  let () = Protocol_hash.Table.add Tables.rights_machine Services.hash rights_of

  let block_data cctx (delegate, timestamp, round, hash) reception_time =
    let* operations = Services.consensus_ops_info_of_block cctx hash in
    return
      ( Data.Block.
          {
            delegate;
            timestamp;
            round = Int32.of_int round;
            hash;
            nonce = None;
            delegate_alias = None;
            reception_time;
          },
        split_endorsements_preendorsements operations )

  let block_of ctxt level =
    let cctx = Services.wrap_full ctxt in
    let* block_info = Services.get_block_info cctx level in
    block_data cctx block_info None

  let () = Protocol_hash.Table.add Tables.block_machine Services.hash block_of

  let get_block ctxt hash header reception_time =
    let cctx = Services.wrap_full ctxt in
    let timestamp = header.Block_header.shell.Block_header.timestamp in
    let*? round = Services.block_round header in
    let* (delegate, _) = Services.baking_right cctx hash round in
    block_data cctx (delegate, timestamp, round, hash) (Some reception_time)

  let () =
    Protocol_hash.Table.add
      Tables.live_block_machine
      Services.hash
      (rights_of, get_block)

  let rec pack_by_slot i e = function
    | ((i', l) as x) :: t ->
        if Int.equal i i' then (i, e :: l) :: t else x :: pack_by_slot i e t
    | [] -> [(i, [e])]

  let dump_my_current_endorsements cctx' wallet ~full reference_level level ops
      =
    let* rights = Services.endorsing_rights cctx' ~reference_level level in
    let () =
      maybe_add_rights (module Tables) (module Archiver) level rights wallet
    in
    let (items, missing) = Services.couple_ops_to_rights ops rights in
    let endorsements =
      if full then
        List.fold_left (fun acc delegate -> (delegate, []) :: acc) items missing
      else items
    in
    let () = Archiver.add_mempool ~unaccurate:(not full) ~level endorsements in
    return_unit

  let endorsements_recorder cctx current_level =
    let*! wallet = wallet cctx in
    let cctx' = Services.wrap_full cctx in
    let* (op_stream, _stopper) = Services.consensus_operation_stream cctx' in
    let*! out =
      Lwt_stream.fold
        (fun ((hash, ((block, level, kind, round), slot)), errors) acc ->
          let reception_time = Time.System.now () in
          let op =
            Consensus_ops.{op = {hash; kind; round}; errors; reception_time}
          in
          Services.BlockIdMap.update
            block
            (function
              | Some (_, l) -> Some (level, pack_by_slot slot op l)
              | None -> Some (level, [(slot, [op])]))
            acc)
        op_stream
        Services.BlockIdMap.empty
    in
    Services.BlockIdMap.iter_ep
      (fun _ (level, endorsements) ->
        let full = Compare.Int32.(current_level = level) in
        dump_my_current_endorsements
          cctx'
          wallet
          ~full
          current_level
          level
          endorsements)
      out

  let () =
    Protocol_hash.Table.add
      Tables.endorsement_machine
      Services.hash
      endorsements_recorder
end

(* Db modules *)
module DM001 = Define (Db_tables) (PtCJ7pwo_machine.Services) (Db_archiver)
module DM002 = Define (Db_tables) (PsYLVpVv_machine.Services) (Db_archiver)
module DM003 = Define (Db_tables) (PsddFKi3_machine.Services) (Db_archiver)
module DM004 = Define (Db_tables) (Pt24m4xi_machine.Services) (Db_archiver)
module DM005 = Define (Db_tables) (PsBabyM1_machine.Services) (Db_archiver)
module DM006 = Define (Db_tables) (PsCARTHA_machine.Services) (Db_archiver)
module DM007 = Define (Db_tables) (PsDELPH1_machine.Services) (Db_archiver)
module DM008 = Define (Db_tables) (PtEdo2Zk_machine.Services) (Db_archiver)
module DM009 = Define (Db_tables) (PsFLoren_machine.Services) (Db_archiver)
module DM010 = Define (Db_tables) (PtGRANAD_machine.Services) (Db_archiver)
module DM011 = Define (Db_tables) (PtHangz2_machine.Services) (Db_archiver)
module DM012 = Define (Db_tables) (Psithaca_machine.Services) (Db_archiver)
module DM013 = Define (Db_tables) (PtJakart_machine.Services) (Db_archiver)
module DM014 = Define (Db_tables) (PtKathma_machine.Services) (Db_archiver)

(* Json modules *)
module JM001 = Define (Json_tables) (PtCJ7pwo_machine.Services) (Json_archiver)
module JM002 = Define (Json_tables) (PsYLVpVv_machine.Services) (Json_archiver)
module JM003 = Define (Json_tables) (PsddFKi3_machine.Services) (Json_archiver)
module JM004 = Define (Json_tables) (Pt24m4xi_machine.Services) (Json_archiver)
module JM005 = Define (Json_tables) (PsBabyM1_machine.Services) (Json_archiver)
module JM006 = Define (Json_tables) (PsCARTHA_machine.Services) (Json_archiver)
module JM007 = Define (Json_tables) (PsDELPH1_machine.Services) (Json_archiver)
module JM008 = Define (Json_tables) (PtEdo2Zk_machine.Services) (Json_archiver)
module JM009 = Define (Json_tables) (PsFLoren_machine.Services) (Json_archiver)
module JM010 = Define (Json_tables) (PtGRANAD_machine.Services) (Json_archiver)
module JM011 = Define (Json_tables) (PtHangz2_machine.Services) (Json_archiver)
module JM012 = Define (Json_tables) (Psithaca_machine.Services) (Json_archiver)
module JM013 = Define (Json_tables) (PtJakart_machine.Services) (Json_archiver)
module JM014 = Define (Json_tables) (PtKathma_machine.Services) (Json_archiver)

(* Server modules *)
module SM001 =
  Define (Server_tables) (PtCJ7pwo_machine.Services) (Server_archiver)
module SM002 =
  Define (Server_tables) (PsYLVpVv_machine.Services) (Server_archiver)
module SM003 =
  Define (Server_tables) (PsddFKi3_machine.Services) (Server_archiver)
module SM004 =
  Define (Server_tables) (Pt24m4xi_machine.Services) (Server_archiver)
module SM005 =
  Define (Server_tables) (PsBabyM1_machine.Services) (Server_archiver)
module SM006 =
  Define (Server_tables) (PsCARTHA_machine.Services) (Server_archiver)
module SM007 =
  Define (Server_tables) (PsDELPH1_machine.Services) (Server_archiver)
module SM008 =
  Define (Server_tables) (PtEdo2Zk_machine.Services) (Server_archiver)
module SM009 =
  Define (Server_tables) (PsFLoren_machine.Services) (Server_archiver)
module SM010 =
  Define (Server_tables) (PtGRANAD_machine.Services) (Server_archiver)
module SM011 =
  Define (Server_tables) (PtHangz2_machine.Services) (Server_archiver)
module SM012 =
  Define (Server_tables) (Psithaca_machine.Services) (Server_archiver)
module SM013 =
  Define (Server_tables) (PtJakart_machine.Services) (Server_archiver)
module SM014 =
  Define (Server_tables) (PtKathma_machine.Services) (Server_archiver)

module Loops (Tables : TABLES) (Archiver : Archiver.S) = struct
  let mecanism chain starting ctxt f =
    let rec loop current ending =
      if current > ending then
        let* {level; _} =
          Tezos_shell_services.Shell_services.Blocks.Header.shell_header
            ctxt
            ~chain
            ~block:(`Head 0)
            ()
        in
        if Int32.equal level ending then return_unit else loop current level
      else
        let* protos =
          Tezos_shell_services.Shell_services.Blocks.protocols
            ctxt
            ~chain
            ~block:(`Level current)
            ()
        in
        let* () = f protos current in
        loop (Int32.succ current) ending
    in
    let* {level = ending; _} =
      Tezos_shell_services.Shell_services.Blocks.Header.shell_header
        ctxt
        ~chain
        ~block:(`Head 0)
        ()
    in
    loop starting ending

  let rights chain starting cctx =
    let*! wallet = wallet cctx in
    mecanism chain starting cctx (fun {next_protocol; _} level ->
        match Protocol_hash.Table.find Tables.rights_machine next_protocol with
        | Some deal_with ->
            let* rights = deal_with cctx level in
            let () =
              maybe_add_rights
                (module Tables)
                (module Archiver)
                level
                rights
                wallet
            in
            return_unit
        | None -> return_unit)

  let blocks chain starting cctx =
    mecanism chain starting cctx (fun {current_protocol; next_protocol} level ->
        if Protocol_hash.equal current_protocol next_protocol then
          match
            Protocol_hash.Table.find Tables.block_machine current_protocol
          with
          | Some deal_with ->
              let* block_data = deal_with cctx level in
              let () = Archiver.add_block ~level block_data in
              return_unit
          | None -> return_unit
        else return_unit
          (* hack to ignore transition block because they need their own instantiation of Block_services *))

  let endorsements_loop cctx =
    let*! head_stream = Shell_services.Monitor.heads cctx cctx#chain in
    match head_stream with
    | Error e ->
        let () = Error_monad.pp_print_trace Format.err_formatter e in
        Lwt.return_unit
    | Ok (head_stream, _stopper) ->
        let*! _ =
          Lwt_stream.fold_s
            (fun (hash, header) acc ->
              let*! (endorsements_recorder, acc') =
                match acc with
                | Some (f, proto_level)
                  when proto_level
                       = header.Block_header.shell.Block_header.proto_level ->
                    Lwt.return (f, acc)
                | _ -> (
                    let*! proto_result =
                      Shell_services.Blocks.protocols
                        cctx
                        ~chain:cctx#chain
                        ~block:(`Hash (hash, 0))
                        ()
                    in
                    match proto_result with
                    | Error e -> Lwt.return ((fun _ _ -> fail e), None)
                    | Ok Shell_services.Blocks.{next_protocol; _} -> (
                        let recorder =
                          Protocol_hash.Table.find
                            Tables.endorsement_machine
                            next_protocol
                        in
                        match recorder with
                        | None ->
                            let*! () =
                              Lwt_fmt.eprintf
                                "no endorsement recorder found for protocol \
                                 %a@."
                                Protocol_hash.pp
                                next_protocol
                            in
                            Lwt.return ((fun _ _ -> return_unit), None)
                        | Some recorder ->
                            Lwt.return
                              ( recorder,
                                Some
                                  ( recorder,
                                    header.Block_header.shell
                                      .Block_header.proto_level ) )))
              in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! () =
                print_failures (endorsements_recorder cctx block_level)
              in
              Lwt.return acc')
            head_stream
            None
        in
        Lwt.return_unit

  let blocks_loop cctx =
    let*! block_stream =
      Shell_services.Monitor.valid_blocks cctx ~chains:[cctx#chain] ()
    in
    match block_stream with
    | Error e ->
        let () = Error_monad.pp_print_trace Format.err_formatter e in
        Lwt.return_unit
    | Ok (block_stream, _stopper) ->
        let*! wallet = wallet cctx in
        let*! _ =
          Lwt_stream.fold_s
            (fun ((_chain_id, hash), header) acc ->
              let*! (block_recorder, acc') =
                match acc with
                | Some (f, proto_level)
                  when proto_level
                       = header.Block_header.shell.Block_header.proto_level ->
                    Lwt.return (f, acc)
                | _ -> (
                    let*! proto_result =
                      Shell_services.Blocks.protocols
                        cctx
                        ~chain:cctx#chain
                        ~block:(`Hash (hash, 0))
                        ()
                    in
                    match proto_result with
                    | Error e -> Lwt.return ((fun _ _ _ _ _ -> fail e), None)
                    | Ok Shell_services.Blocks.{current_protocol; next_protocol}
                      ->
                        if Protocol_hash.equal current_protocol next_protocol
                        then
                          let recorder =
                            Protocol_hash.Table.find
                              Tables.live_block_machine
                              next_protocol
                          in
                          match recorder with
                          | None ->
                              let*! () =
                                Lwt_fmt.eprintf
                                  "no block recorder found for protocol %a@."
                                  Protocol_hash.pp
                                  current_protocol
                              in
                              Lwt.return ((fun _ _ _ _ _ -> return_unit), None)
                          | Some (rights_of, get_block) ->
                              let recorder cctx level hash header reception_time
                                  =
                                let* (( _block_info,
                                        (endorsements, preendorsements) ) as
                                     block_data) =
                                  get_block cctx hash header reception_time
                                in
                                let* () =
                                  if List.is_empty endorsements then return_unit
                                  else
                                    let* rights =
                                      rights_of cctx (Int32.pred level)
                                    in
                                    let () =
                                      maybe_add_rights
                                        (module Tables)
                                        (module Archiver)
                                        (Int32.pred level)
                                        rights
                                        wallet
                                    in
                                    return_unit
                                in
                                let* () =
                                  if List.is_empty preendorsements then
                                    return_unit
                                  else
                                    let* rights = rights_of cctx level in
                                    let () =
                                      maybe_add_rights
                                        (module Tables)
                                        (module Archiver)
                                        level
                                        rights
                                        wallet
                                    in
                                    return_unit
                                in
                                let () = Archiver.add_block ~level block_data in
                                return_unit
                              in
                              Lwt.return
                                ( recorder,
                                  Some
                                    ( recorder,
                                      header.Block_header.shell
                                        .Block_header.proto_level ) )
                        else
                          let*! () =
                            Lwt_fmt.eprintf
                              "skipping block %a, migrating from protocol %a \
                               to %a@."
                              Block_hash.pp
                              hash
                              Protocol_hash.pp
                              current_protocol
                              Protocol_hash.pp
                              next_protocol
                          in
                          Lwt.return ((fun _ _ _ _ _ -> return_unit), None))
              in
              let reception_time = Time.System.now () in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! () =
                print_failures
                  (block_recorder cctx block_level hash header reception_time)
              in
              Lwt.return acc')
            block_stream
            None
        in
        Lwt.return_unit
end

module Db_loops = Loops (Db_tables) (Db_archiver)
module Json_loops = Loops (Json_tables) (Json_archiver)
module Server_loops = Loops (Server_tables) (Server_archiver)
