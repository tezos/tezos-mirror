(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
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

let supported_protocols = ref []

let balance_update_machine = Protocol_hash.Table.create 10

let delegator_machine :
    (Tezos_client_base.Client_context.full ->
    Int32.t ->
    Signature.Public_key_hash.t ->
    (Signature.Public_key_hash.t * int64) list tzresult Lwt.t)
    Protocol_hash.Table.t =
  Protocol_hash.Table.create 10

let baking_rights_machine = Protocol_hash.Table.create 10

module Define (Services : Protocol_machinery.PROTOCOL_SERVICES) = struct
  let () = supported_protocols := Services.hash :: !supported_protocols

  let balance_update_recorder ctxt level =
    let cctx = Services.wrap_full ctxt in
    let* level, cycle, block_hash, time, proposer, balance_updates =
      Services.get_balance_updates cctx level
    in
    let logger = Log.logger () in
    Log.info logger (fun () ->
        Format.asprintf
          "@.  Level:%ld@.  Cycle:%ld@.  Hash:%a@.  Proposer:%a@.  \
           Balance_updates:%d"
          level
          cycle
          Block_hash.pp_short
          block_hash
          Signature.Public_key_hash.pp_short
          proposer
          (List.length balance_updates)) ;
    return (level, cycle, block_hash, time, proposer, balance_updates)

  let baking_rights_counter ctxt cycle level =
    let cctx = Services.wrap_full ctxt in
    Services.get_cycle_baking_rights_count cctx cycle level

  let () =
    Protocol_hash.Table.add
      balance_update_machine
      Services.hash
      balance_update_recorder

  let () =
    Protocol_hash.Table.add
      delegator_machine
      Services.hash
      Services.get_delegators

  let () =
    Protocol_hash.Table.add
      baking_rights_machine
      Services.hash
      baking_rights_counter
end

module Loops = struct
  let with_cache _mutex request mem add (module Db : Caqti_lwt.CONNECTION) list
      =
    let open Lwt_result.Syntax in
    (* Note: even if data is already in cache,
     we add it again at the end in order to mark it as recent. *)
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
      (fun x ->
        let+ () =
          if mem x then
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax
            .return_unit
          else Db.exec request x
        in
        add x)
      list

  let without_cache mutex request =
    (* We don't use the cache feature here,
     but we want to reuse the mutex and transactions handling from above.
     Typically used by functions when you need to process multiple requests
     before marking the key as done in an external cache,
     or when it is relevant to use a cache the whole list as single item.
     (e.g. use the level as cache key instead of one key for each list element) *)
    with_cache mutex request (fun _ -> false) (fun _ -> ())

  let maybe_add_balance_updates logger pool level cycle
      (balance_updates : Data.Balance_update.balance_update list) =
    let rows =
      List.map
        (fun (balance_update : Data.Balance_update.balance_update) ->
          ( (level, cycle, balance_update.address),
            ( Data.Balance_update.category_to_string balance_update.category,
              Data.Balance_update.result_to_string balance_update.result,
              balance_update.value ) ))
        balance_updates
    in
    let out =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          without_cache
            Sql_requests.Mutex.balance_updates
            Sql_requests.insert_block_balance_update
            (module Db)
            rows)
        pool
    in
    Lwt.bind out (function
      | Ok () -> Lwt.return_unit
      | Error e ->
          Log.error logger (fun () -> Caqti_error.show e) ;
          Lwt.return_unit)

  let maybe_add_delegators logger pool baker cycle delegators =
    let rows =
      List.map
        (fun (pkh, balance) ->
          let addr = Signature.Public_key_hash.to_b58check pkh in
          ((baker, cycle), (addr, balance)))
        delegators
    in
    let out =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          without_cache
            Sql_requests.Mutex.balance_updates
            Sql_requests.insert_cycle_delegator
            (module Db)
            rows)
        pool
    in
    Lwt.bind out (function
      | Ok () -> Lwt.return_unit
      | Error e ->
          Log.error logger (fun () -> Caqti_error.show e) ;
          Lwt.return_unit)

  let snapshot_delegators logger pool cctx level cycle next_protocol
      watched_addresses =
    match Protocol_hash.Table.find delegator_machine next_protocol with
    | None ->
        Log.info logger (fun () ->
            "No delegator recorder found for current protocol") ;
        Lwt.return_unit
    | Some get_delegators ->
        Log.info logger (fun () ->
            Format.asprintf
              "Snapshotting delegators for cycle %ld (%d watched bakers)"
              cycle
              (Signature.Public_key_hash.Set.cardinal watched_addresses)) ;
        Lwt_list.iter_s
          (fun baker ->
            Log.info logger (fun () ->
                Format.asprintf
                  "  Fetching delegators for %a"
                  Signature.Public_key_hash.pp_short
                  baker) ;
            let*! result = get_delegators cctx level baker in
            match result with
            | Ok delegators ->
                Log.info logger (fun () ->
                    Format.asprintf
                      "  Found %d delegators"
                      (List.length delegators)) ;
                maybe_add_delegators logger pool baker cycle delegators
            | Error e ->
                Log.error logger (fun () ->
                    Format.asprintf
                      "Error fetching delegators for %a: %a"
                      Signature.Public_key_hash.pp_short
                      baker
                      Error_monad.pp_print_trace
                      e) ;
                Lwt.return_unit)
          (Signature.Public_key_hash.Set.elements watched_addresses)

  let maybe_add_block_baker logger pool level cycle proposer =
    let out =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          Db.exec Sql_requests.insert_block_baker (level, cycle, proposer))
        pool
    in
    Lwt.bind out (function
      | Ok () -> Lwt.return_unit
      | Error e ->
          Log.error logger (fun () ->
              Format.sprintf "insert_block_baker: %s" (Caqti_error.show e)) ;
          Lwt.return_unit)

  let maybe_store_expected_blocks logger pool cycle counts =
    Log.info logger (fun () ->
        Format.sprintf
          "Storing expected_blocks for cycle %ld (%d bakers)"
          cycle
          (List.length counts)) ;
    let out =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun (baker, count) ->
              Db.exec
                Sql_requests.insert_expected_blocks
                (cycle, baker, Int32.of_int count))
            counts)
        pool
    in
    Lwt.bind out (function
      | Ok () -> Lwt.return_unit
      | Error e ->
          Log.error logger (fun () ->
              Format.sprintf "insert_expected_blocks: %s" (Caqti_error.show e)) ;
          Lwt.return_unit)

  let balance_update_loop cctx pool watched_addresses =
    let logger = Log.logger () in
    let*! head_stream = Shell_services.Monitor.heads cctx cctx#chain in
    match head_stream with
    | Error e -> print_error logger e
    | Ok (head_stream, _stopper) ->
        (* Accumulator:
           proto_acc = (recorder, baking_rights_fn, proto_level, next_protocol) option
           prev_cycle = int32 option *)
        let*! _ =
          Lwt_stream.fold_s
            (fun (hash, header) (proto_acc, prev_cycle) ->
              Log.info logger (fun () ->
                  Format.sprintf "Level %ld" header.Block_header.shell.level) ;
              let*! ( balance_updates_recorder,
                      baking_rights_fn,
                      proto_acc',
                      next_protocol ) =
                match proto_acc with
                | Some (f, baking_right_machine, proto_level, next_protocol)
                  when proto_level
                       = header.Block_header.shell.Block_header.proto_level ->
                    Lwt.return
                      (f, baking_right_machine, proto_acc, next_protocol)
                | _ -> (
                    let*! proto_result =
                      Shell_services.Blocks.protocols
                        cctx
                        ~chain:cctx#chain
                        ~block:(`Hash (hash, 0))
                        ()
                    in
                    match proto_result with
                    | Error e ->
                        Lwt.return
                          ( (fun _ _ -> fail e),
                            (fun _ _ _ -> fail e),
                            None,
                            Protocol_hash.zero )
                    | Ok Shell_services.Blocks.{next_protocol; _} -> (
                        let recorder =
                          Protocol_hash.Table.find
                            balance_update_machine
                            next_protocol
                        in
                        let baking_rights_machine =
                          Protocol_hash.Table.find
                            baking_rights_machine
                            next_protocol
                        in
                        match (recorder, baking_rights_machine) with
                        | None, _ | _, None ->
                            let*! () =
                              Lwt_fmt.eprintf
                                "no balance update recorder found for protocol \
                                 %a@."
                                Protocol_hash.pp
                                next_protocol
                            in
                            assert false (* TODO *)
                        | Some recorder, Some baking_rights_machine ->
                            Lwt.return
                              ( recorder,
                                baking_rights_machine,
                                Some
                                  ( recorder,
                                    baking_rights_machine,
                                    header.Block_header.shell
                                      .Block_header.proto_level,
                                    next_protocol ),
                                next_protocol )))
              in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! res = balance_updates_recorder cctx block_level in
              match res with
              | Error errs ->
                  Log.info logger (fun () ->
                      Format.asprintf "Error: %a" pp_print_trace errs) ;
                  assert false
              | Ok (level, cycle, _block_hash, _time, proposer, balance_updates)
                ->
                  let*! _ =
                    maybe_add_balance_updates
                      logger
                      pool
                      level
                      cycle
                      balance_updates
                  in
                  let*! _ =
                    maybe_add_block_baker logger pool level cycle proposer
                  in
                  (* On cycle transition, fetch baking rights and snapshot
                     delegators *)
                  let*! () =
                    match prev_cycle with
                    | Some prev when prev <> cycle ->
                        Log.info logger (fun () ->
                            Format.sprintf
                              "Cycle transition %ld -> %ld, fetching baking \
                               rights for cycle %ld"
                              prev
                              cycle
                              prev) ;
                        let*! baking_rights_machine =
                          baking_rights_fn cctx prev level
                        in
                        let*! () =
                          match baking_rights_machine with
                          | Ok counts ->
                              maybe_store_expected_blocks
                                logger
                                pool
                                prev
                                counts
                          | Error errs ->
                              Log.error logger (fun () ->
                                  Format.asprintf
                                    "Failed to fetch baking rights for cycle \
                                     %ld: %a"
                                    prev
                                    pp_print_trace
                                    errs) ;
                              Lwt.return_unit
                        in
                        snapshot_delegators
                          logger
                          pool
                          cctx
                          level
                          cycle
                          next_protocol
                          watched_addresses
                    | None ->
                        (* First block seen — snapshot if we have watched bakers *)
                        if
                          not
                            (Signature.Public_key_hash.Set.is_empty
                               watched_addresses)
                        then
                          snapshot_delegators
                            logger
                            pool
                            cctx
                            level
                            cycle
                            next_protocol
                            watched_addresses
                        else Lwt.return_unit
                    | _ -> Lwt.return_unit
                  in
                  Lwt.return (proto_acc', Some cycle))
            head_stream
            (None, None)
        in
        Lwt.return_unit

  let blocks_loop cctx pool watched_addresses =
    Lwt.join [balance_update_loop cctx pool watched_addresses]
end
