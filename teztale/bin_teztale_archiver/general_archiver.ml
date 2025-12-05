(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Lwt_result_syntax

let print_error logger e =
  let () =
    Log.error logger (fun () ->
        Format.asprintf "%a" Error_monad.pp_print_trace e)
  in
  Lwt.return_unit

let print_failures logger f =
  let*! o = f in
  match o with Ok () -> Lwt.return_unit | Error e -> print_error logger e

let split_attestations_preattestations operations =
  List.fold_left
    (fun (en, pre) (Consensus_ops.{op = {kind; _}; _} as x) ->
      match kind with
      | Consensus_ops.Preattestation -> (en, x :: pre)
      | Consensus_ops.Attestation -> (x :: en, pre))
    ([], [])
    operations

module Ring =
  Aches.Vache.Set (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      type t = Int32.t

      let hash = Hashtbl.hash

      let equal = Int32.equal
    end)

let supported_protocols = ref []

let registered_rights_levels = Ring.create 120

let registered_dal_shards_levels = Ring.create 120

let rights_machine = Protocol_hash.Table.create 10

let past_block_machine = Protocol_hash.Table.create 10

let attestation_machine = Protocol_hash.Table.create 10

let validated_block_machine = Protocol_hash.Table.create 10

let live_block_machine = Protocol_hash.Table.create 10

let maybe_add_rights (module A : Archiver.S) level rights =
  if Ring.mem registered_rights_levels level then ()
  else
    let () = Ring.add registered_rights_levels level in
    A.add_rights ~level rights

let maybe_add_dal_shards (module A : Archiver.S) level shard_assignments =
  if Ring.mem registered_dal_shards_levels level then ()
  else (
    Ring.add registered_dal_shards_levels level ;
    A.add_dal_shards ~level shard_assignments)

let dump_my_current_attestations (module A : Archiver.S) ~unaccurate ~level
    rights attestations =
  let () = maybe_add_rights (module A) level rights in
  let () = A.add_mempool ~unaccurate ~level attestations in
  return_unit

module Define (Services : Protocol_machinery.PROTOCOL_SERVICES) = struct
  let () = supported_protocols := Services.hash :: !supported_protocols

  let baking_rights ctxt level round =
    let cctx = Services.wrap_full ctxt in
    Services.baking_rights cctx level round

  let rights_of ctxt level =
    let cctx = Services.wrap_full ctxt in
    Services.attesting_rights cctx ~reference_level:level level

  let () = Protocol_hash.Table.add rights_machine Services.hash rights_of

  let block_info_data (delegate, timestamp, round, hash, predecessor)
      reception_times =
    Data.Block.
      {
        delegate;
        timestamp;
        round = Int32.of_int round;
        hash;
        predecessor;
        nonce = None;
        reception_times;
      }

  let block_data_no_baking_rights cctx ((_, _, _, hash, _) as info) cycle_info
      reception_times =
    let* operations = Services.consensus_ops_info_of_block cctx hash in
    return
      ( block_info_data info reception_times,
        cycle_info,
        split_attestations_preattestations operations,
        [] )

  let past_block ctxt level =
    let cctx = Services.wrap_full ctxt in
    let* block_info, cycle_info = Services.get_block_info cctx level in
    block_data_no_baking_rights cctx block_info (Some cycle_info) []

  let () = Protocol_hash.Table.add past_block_machine Services.hash past_block

  let get_validated_block ctxt level hash header reception_times =
    let cctx = Services.wrap_full ctxt in
    let timestamp = header.Block_header.shell.Block_header.timestamp in
    let*? round = Services.block_round header in
    let* delegate, baking_rights = Services.baking_rights cctx level round in
    let predecessor = Some header.Block_header.shell.Block_header.predecessor in
    return
      ( block_info_data
          (delegate, timestamp, round, hash, predecessor)
          reception_times,
        None,
        ([], []),
        baking_rights )

  let () =
    Protocol_hash.Table.add
      validated_block_machine
      Services.hash
      get_validated_block

  let get_applied_block ctxt hash header reception_times =
    let cctx = Services.wrap_full ctxt in
    let timestamp = header.Block_header.shell.Block_header.timestamp in
    let*? round = Services.block_round header in
    let* delegate, cycle_info = Services.baker_and_cycle cctx hash in
    let predecessor = Some header.Block_header.shell.Block_header.predecessor in
    block_data_no_baking_rights
      cctx
      (delegate, timestamp, round, hash, predecessor)
      (Some cycle_info)
      reception_times

  let dal_shards_of _ctxt _level =
    (* TODO: call a DAL RPC here and map to [Data.Dal.shard_assignment list]. *)
    return_nil

  let () =
    Protocol_hash.Table.add
      live_block_machine
      Services.hash
      (rights_of, get_applied_block, dal_shards_of)

  let rec pack_by_slot i e = function
    | ((i', l) as x) :: t ->
        if Int.equal i i' then (i, e :: l) :: t else x :: pack_by_slot i e t
    | [] -> [(i, [e])]

  (* [couple_ops_to_rights ops rights] returns [(participating,
     missing)], where [participating] is a list associating delegates
     with their operations in [ops], and [missing] is the list of
     delegates which do not have associated operations in [ops].

     TODO: it might be clearer to use a map instead of an association
     list for [participating]. *)
  let couple_ops_to_rights ops rights =
    let items, missing =
      List.fold_left
        (fun (acc, rights) (slot, ops) ->
          match
            List.partition
              (fun right -> Int.equal slot right.Consensus_ops.first_slot)
              rights
          with
          | ([] | _ :: _ :: _), _ -> assert false
          | [right], rights' -> ((right, ops) :: acc, rights'))
        ([], rights)
        ops
    in
    (items, missing)

  let attestations_recorder (module A : Archiver.S) cctx current_level =
    let cctx' = Services.wrap_full cctx in
    let* op_stream, _stopper = Services.consensus_operation_stream cctx' in
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
      (fun _ (level, attestations) ->
        let* rights =
          Services.attesting_rights cctx' ~reference_level:current_level level
        in
        let items, missing = couple_ops_to_rights attestations rights in
        let full = Compare.Int32.(current_level = level) in
        let attestations =
          if full then
            List.fold_left (fun acc right -> (right, []) :: acc) items missing
          else items
        in
        dump_my_current_attestations
          (module A)
          ~unaccurate:(not full)
          ~level
          rights
          attestations)
      out

  let () =
    Protocol_hash.Table.add
      attestation_machine
      Services.hash
      attestations_recorder
end

module Loops (Archiver : Archiver.S) = struct
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
    mecanism chain starting cctx (fun {next_protocol; _} level ->
        match Protocol_hash.Table.find rights_machine next_protocol with
        | Some deal_with ->
            let* rights = deal_with cctx level in
            let () = maybe_add_rights (module Archiver) level rights in
            return_unit
        | None -> return_unit)

  (** [past_blocks chain starting cctx]
      allows you to inject blocks from past level (in case of teztale shortage for instance) *)
  let past_blocks chain starting cctx =
    mecanism chain starting cctx (fun {current_protocol; next_protocol} level ->
        if Protocol_hash.equal current_protocol next_protocol then
          match
            Protocol_hash.Table.find past_block_machine current_protocol
          with
          | Some deal_with ->
              let* block_data = deal_with cctx level in
              let () = Archiver.add_block ~level block_data in
              return_unit
          | None -> return_unit
        else return_unit
          (* hack to ignore transition block because they need their own instantiation of Block_services *))

  let attestations_loop cctx =
    let logger = Log.logger () in
    let*! head_stream = Shell_services.Monitor.heads cctx cctx#chain in
    match head_stream with
    | Error e -> print_error logger e
    | Ok (head_stream, _stopper) ->
        let*! _ =
          Lwt_stream.fold_s
            (fun (hash, header) acc ->
              let*! attestations_recorder, acc' =
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
                            attestation_machine
                            next_protocol
                        in
                        match recorder with
                        | None ->
                            let*! () =
                              Lwt_fmt.eprintf
                                "no attestation recorder found for protocol \
                                 %a@."
                                Protocol_hash.pp
                                next_protocol
                            in
                            Lwt.return ((fun _ _ -> return_unit), None)
                        | Some recorder ->
                            Lwt.return
                              ( recorder (module Archiver),
                                Some
                                  ( recorder (module Archiver),
                                    header.Block_header.shell
                                      .Block_header.proto_level ) )))
              in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! () =
                print_failures logger (attestations_recorder cctx block_level)
              in
              Lwt.return acc')
            head_stream
            None
        in
        Lwt.return_unit

  let reception_blocks_loop cctx =
    let logger = Log.logger () in
    let*! block_stream =
      Shell_services.Monitor.applied_blocks cctx ~chains:[cctx#chain] ()
    in
    match block_stream with
    | Error e -> print_error logger e
    | Ok (block_stream, _stopper) ->
        let*! _ =
          Lwt_stream.fold_s
            (fun (_chain_id, hash, header, _operations) acc ->
              let now = Time.System.now () in
              let*! block_recorder, acc' =
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
                              live_block_machine
                              next_protocol
                          in
                          match recorder with
                          | None ->
                              let () =
                                Log.error logger (fun () ->
                                    Format.asprintf
                                      "no block recorder found for protocol \
                                       %a@."
                                      Protocol_hash.pp
                                      current_protocol)
                              in
                              Lwt.return ((fun _ _ _ _ _ -> return_unit), None)
                          | Some (rights_of, get_applied_block, dal_shards_of)
                            ->
                              let recorder cctx level hash header reception_time
                                  =
                                let* (( _block_info,
                                        _cycle_info,
                                        (attestations, preattestations),
                                        _baking_rights ) as block_data) =
                                  get_applied_block
                                    cctx
                                    hash
                                    header
                                    reception_time
                                in
                                let* () =
                                  if List.is_empty attestations then return_unit
                                  else
                                    let* rights =
                                      rights_of cctx (Int32.pred level)
                                    in
                                    let pred_level = Int32.pred level in
                                    let () =
                                      maybe_add_rights
                                        (module Archiver)
                                        pred_level
                                        rights
                                    in
                                    let* dal_shards =
                                      dal_shards_of cctx pred_level
                                    in
                                    let () =
                                      maybe_add_dal_shards
                                        (module Archiver)
                                        pred_level
                                        dal_shards
                                    in
                                    return_unit
                                in
                                let* () =
                                  if List.is_empty preattestations then
                                    return_unit
                                  else
                                    let* rights = rights_of cctx level in
                                    let () =
                                      maybe_add_rights
                                        (module Archiver)
                                        level
                                        rights
                                    in
                                    let* dal_shards =
                                      dal_shards_of cctx level
                                    in
                                    let () =
                                      maybe_add_dal_shards
                                        (module Archiver)
                                        level
                                        dal_shards
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
                          let () =
                            Log.error logger (fun () ->
                                Format.asprintf
                                  "skipping block %a, migrating from protocol \
                                   %a to %a@."
                                  Block_hash.pp
                                  hash
                                  Protocol_hash.pp
                                  current_protocol
                                  Protocol_hash.pp
                                  next_protocol)
                          in
                          Lwt.return ((fun _ _ _ _ _ -> return_unit), None))
              in
              let reception =
                {
                  Data.Block.source = "archiver";
                  application_time = Some now;
                  validation_time = None;
                }
              in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! () =
                print_failures
                  logger
                  (block_recorder cctx block_level hash header [reception])
              in
              Lwt.return acc')
            block_stream
            None
        in
        Lwt.return_unit

  let validation_blocks_loop cctx =
    let logger = Log.logger () in
    let*! block_stream =
      Shell_services.Monitor.validated_blocks cctx ~chains:[cctx#chain] ()
    in
    match block_stream with
    | Error e -> print_error logger e
    | Ok (block_stream, _stopper) ->
        let*! _ =
          Lwt_stream.fold_s
            (fun (_chain_id, hash, header, _operations) acc ->
              let now = Time.System.now () in
              let*! block_recorder, acc' =
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
                        ~block:
                          (`Level
                             (Int32.pred
                                header.Block_header.shell.Block_header.level))
                        ()
                    in
                    match proto_result with
                    | Error e -> Lwt.return ((fun _ _ _ _ _ -> fail e), None)
                    | Ok Shell_services.Blocks.{next_protocol; _} -> (
                        let recorder =
                          Protocol_hash.Table.find
                            validated_block_machine
                            next_protocol
                        in
                        match recorder with
                        | None ->
                            let*! () =
                              Lwt_fmt.eprintf
                                "no block recorder found for protocol %a@."
                                Protocol_hash.pp
                                next_protocol
                            in
                            Lwt.return ((fun _ _ _ _ _ -> return_unit), None)
                        | Some get_validated_block ->
                            let recorder cctx level hash header reception_time =
                              let* block_data =
                                get_validated_block
                                  cctx
                                  level
                                  hash
                                  header
                                  reception_time
                              in
                              let () = Archiver.add_block ~level block_data in
                              return_unit
                            in
                            Lwt.return
                              ( recorder,
                                Some
                                  ( recorder,
                                    header.Block_header.shell
                                      .Block_header.proto_level ) )))
              in
              let reception =
                {
                  Data.Block.source = "archiver";
                  application_time = None;
                  validation_time = Some now;
                }
              in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! () =
                print_failures
                  logger
                  (block_recorder cctx block_level hash header [reception])
              in
              Lwt.return acc')
            block_stream
            None
        in
        Lwt.return_unit

  let blocks_loop cctx =
    Lwt.join [reception_blocks_loop cctx; validation_blocks_loop cctx]
end

module Json_loops = Loops (Json_archiver)
module Server_loops = Loops (Server_archiver)
