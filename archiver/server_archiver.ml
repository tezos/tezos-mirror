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

let registered_rights_levels = Ring.create 120

let rights_machine = Protocol_hash.Table.create 10

let block_machine = Protocol_hash.Table.create 10

let endorsement_machine = Protocol_hash.Table.create 10

let live_block_machine = Protocol_hash.Table.create 10

let send_something cohttp_ctx auth endpoint path body =
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let headers = Cohttp.Header.add_authorization headers (`Basic auth) in
  let uri = Uri.with_path endpoint (Uri.path endpoint ^ "/" ^ path) in
  let*! (resp, out) =
    Cohttp_lwt_unix.Client.post ~ctx:cohttp_ctx ~body ~headers uri
  in
  let*! out = Cohttp_lwt.Body.to_string out in
  Lwt_io.printlf "%s: %s" (Cohttp.Code.string_of_status resp.status) out

let maybe_send_rights cohttp_ctx auth endpoint level rights =
  if Ring.mem registered_rights_levels level then Lwt.return_unit
  else
    let () = Ring.add registered_rights_levels level in
    let body =
      `String
        (Ezjsonm.value_to_string
           (Data_encoding.Json.construct
              Teztale_lib.Consensus_ops.rights_encoding
              rights))
    in
    let path = Int32.to_string level ^ "/rights" in
    let*! () = Lwt_io.printf "%li: " level in
    send_something cohttp_ctx auth endpoint path body

let send_block cohttp_ctx auth endpoint level block_data =
  let body =
    `String
      (Ezjsonm.value_to_string
         (Data_encoding.Json.construct Data.block_data_encoding block_data))
  in
  let path = Int32.to_string level ^ "/block" in
  let*! () = Lwt_io.printf "%li: " level in
  send_something cohttp_ctx auth endpoint path body

let send_mempool cohttp_ctx auth endpoint level ops =
  let body =
    `String
      (Ezjsonm.value_to_string
         (Data_encoding.Json.construct Consensus_ops.delegate_ops_encoding ops))
  in
  let path = Int32.to_string level ^ "/mempool" in
  let*! () = Lwt_io.printf "%li: " level in
  send_something cohttp_ctx auth endpoint path body

let split_endorsements_preendorsements operations =
  List.fold_left
    (fun (en, pre) (Consensus_ops.{op = {kind; _}; _} as x) ->
      match kind with
      | Consensus_ops.Preendorsement -> (en, x :: pre)
      | Consensus_ops.Endorsement -> (x :: en, pre))
    ([], [])
    operations

module Define (Services : Protocol_machinery.PROTOCOL_SERVICES) = struct
  let rights_of ctxt level =
    let cctxt = Services.wrap_full ctxt in
    Services.endorsing_rights cctxt ~reference_level:level level

  let () = Protocol_hash.Table.add rights_machine Services.hash rights_of

  let block_data cctxt (delegate, timestamp, round, hash) reception_time =
    let* operations = Services.consensus_ops_info_of_block cctxt hash in
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
    let cctxt = Services.wrap_full ctxt in
    let* block_info = Services.get_block_info cctxt level in
    block_data cctxt block_info None

  let () = Protocol_hash.Table.add block_machine Services.hash block_of

  let get_block ctxt hash header reception_time =
    let cctxt = Services.wrap_full ctxt in
    let timestamp = header.Block_header.shell.Block_header.timestamp in
    let*? round = Services.block_round header in
    let* (delegate, _) = Services.baking_right cctxt hash round in
    block_data cctxt (delegate, timestamp, round, hash) (Some reception_time)

  let () =
    Protocol_hash.Table.add
      live_block_machine
      Services.hash
      (rights_of, get_block)

  let rec pack_by_slot i e = function
    | ((i', l) as x) :: t ->
        if Int.equal i i' then (i, e :: l) :: t else x :: pack_by_slot i e t
    | [] -> [(i, [e])]

  let dump_my_current_endorsements cctxt' cohttp_ctx auth endpoint ~full
      reference_level level ops =
    let* rights = Services.endorsing_rights cctxt' ~reference_level level in
    let*! () = maybe_send_rights cohttp_ctx auth endpoint level rights in
    let (items, missing) = Services.couple_ops_to_rights ops rights in
    let endorsements =
      if full then
        List.fold_left (fun acc delegate -> (delegate, []) :: acc) items missing
      else items
    in
    let*! () = send_mempool cohttp_ctx auth endpoint level endorsements in
    return_unit

  let endorsements_recorder cohttp_ctx auth endpoint cctxt current_level =
    let cctxt' = Services.wrap_full cctxt in
    let* (op_stream, _stopper) = Services.consensus_operation_stream cctxt' in
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
          cctxt'
          cohttp_ctx
          auth
          endpoint
          ~full
          current_level
          level
          endorsements)
      out

  let () =
    Protocol_hash.Table.add
      endorsement_machine
      Services.hash
      endorsements_recorder
end

module M001 = Define (PtCJ7pwo_machine.Services)
module M002 = Define (PsYLVpVv_machine.Services)
module M003 = Define (PsddFKi3_machine.Services)
module M004 = Define (Pt24m4xi_machine.Services)
module M005 = Define (PsBabyM1_machine.Services)
module M006 = Define (PsCARTHA_machine.Services)
module M007 = Define (PsDELPH1_machine.Services)
module M008 = Define (PtEdo2Zk_machine.Services)
module M009 = Define (PsFLoren_machine.Services)
module M010 = Define (PtGRANAD_machine.Services)
module M011 = Define (PtHangz2_machine.Services)
module M012 = Define (Psithaca_machine.Services)
module M013 = Define (PtJakart_machine.Services)
module M014 = Define (PtKathma_machine.Services)

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

let rights cohttp_ctx auth endpoint chain starting ctxt =
  mecanism chain starting ctxt (fun {next_protocol; _} level ->
      match Protocol_hash.Table.find rights_machine next_protocol with
      | Some deal_with ->
          let* rights = deal_with ctxt level in
          let*! () = maybe_send_rights cohttp_ctx auth endpoint level rights in
          return_unit
      | None -> return_unit)

let blocks cohttp_ctx auth endpoint chain starting ctxt =
  mecanism chain starting ctxt (fun {current_protocol; next_protocol} level ->
      if Protocol_hash.equal current_protocol next_protocol then
        match Protocol_hash.Table.find block_machine current_protocol with
        | Some deal_with ->
            let* block_data = deal_with ctxt level in
            let*! () = send_block cohttp_ctx auth endpoint level block_data in
            return_unit
        | None -> return_unit
      else return_unit
        (* hack to ignore transition block because they need their own instantiation of Block_services *))

let print_failures f =
  let*! o = f in
  match o with
  | Ok () -> Lwt.return_unit
  | Error e ->
      let () = Error_monad.pp_print_trace Format.err_formatter e in
      Lwt.return_unit

let endorsements_loop cohttp_ctx auth endpoint cctxt =
  let*! head_stream = Shell_services.Monitor.heads cctxt cctxt#chain in
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
                      cctxt
                      ~chain:cctxt#chain
                      ~block:(`Hash (hash, 0))
                      ()
                  in
                  match proto_result with
                  | Error e -> Lwt.return ((fun _ _ _ _ _ -> fail e), None)
                  | Ok Shell_services.Blocks.{next_protocol; _} -> (
                      let recorder =
                        Protocol_hash.Table.find
                          endorsement_machine
                          next_protocol
                      in
                      match recorder with
                      | None -> Lwt.return ((fun _ _ _ _ _ -> return_unit), None)
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
              print_failures
                (endorsements_recorder
                   cohttp_ctx
                   auth
                   endpoint
                   cctxt
                   block_level)
            in
            Lwt.return acc')
          head_stream
          None
      in
      Lwt.return_unit

let blocks_loop cohttp_ctx auth endpoint cctxt =
  let*! block_stream =
    Shell_services.Monitor.valid_blocks cctxt ~chains:[cctxt#chain] ()
  in
  match block_stream with
  | Error e ->
      let () = Error_monad.pp_print_trace Format.err_formatter e in
      Lwt.return_unit
  | Ok (block_stream, _stopper) ->
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
                      cctxt
                      ~chain:cctxt#chain
                      ~block:(`Hash (hash, 0))
                      ()
                  in
                  match proto_result with
                  | Error e -> Lwt.return ((fun _ _ _ _ _ -> fail e), None)
                  | Ok Shell_services.Blocks.{current_protocol; next_protocol}
                    ->
                      if Protocol_hash.equal current_protocol next_protocol then
                        let recorder =
                          Protocol_hash.Table.find
                            live_block_machine
                            next_protocol
                        in
                        match recorder with
                        | None ->
                            Lwt.return ((fun _ _ _ _ _ -> return_unit), None)
                        | Some (rights_of, get_block) ->
                            let recorder cctxt level hash header reception_time
                                =
                              let* ((_, (endorsements, preendorsements)) as
                                   block_data) =
                                get_block cctxt hash header reception_time
                              in
                              let* () =
                                if List.is_empty endorsements then return_unit
                                else
                                  let* rights =
                                    rights_of cctxt (Int32.pred level)
                                  in
                                  let*! () =
                                    maybe_send_rights
                                      cohttp_ctx
                                      auth
                                      endpoint
                                      (Int32.pred level)
                                      rights
                                  in
                                  return_unit
                              in
                              let* () =
                                if List.is_empty preendorsements then
                                  return_unit
                                else
                                  let* rights = rights_of cctxt level in
                                  let*! () =
                                    maybe_send_rights
                                      cohttp_ctx
                                      auth
                                      endpoint
                                      level
                                      rights
                                  in
                                  return_unit
                              in
                              let*! () =
                                send_block
                                  cohttp_ctx
                                  auth
                                  endpoint
                                  level
                                  block_data
                              in
                              return_unit
                            in
                            Lwt.return
                              ( recorder,
                                Some
                                  ( recorder,
                                    header.Block_header.shell
                                      .Block_header.proto_level ) )
                      else Lwt.return ((fun _ _ _ _ _ -> return_unit), None))
            in
            let reception_time = Time.System.now () in
            let block_level = header.Block_header.shell.Block_header.level in
            let*! () =
              print_failures
                (block_recorder cctxt block_level hash header reception_time)
            in
            Lwt.return acc')
          block_stream
          None
      in
      Lwt.return_unit
