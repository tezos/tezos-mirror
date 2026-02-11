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

module Define (Services : Protocol_machinery.PROTOCOL_SERVICES) = struct
  let () = supported_protocols := Services.hash :: !supported_protocols

  let balance_update_recorder ctxt level =
    let cctx = Services.wrap_full ctxt in
    let* level, block_hash, time, balance_updates =
      Services.get_balance_updates cctx level
    in
    let logger = Log.logger () in
    Log.info logger (fun () ->
        Format.asprintf
          "@.  Level:%ld@.  Hash:%a@.  Balance_updates:%d"
          level
          Block_hash.pp_short
          block_hash
          (List.length balance_updates)) ;
    return (level, block_hash, time, balance_updates)

  let () =
    Protocol_hash.Table.add
      balance_update_machine
      Services.hash
      balance_update_recorder
end

module Loops = struct
  let balance_update_loop cctx =
    let logger = Log.logger () in
    let*! head_stream = Shell_services.Monitor.heads cctx cctx#chain in
    match head_stream with
    | Error e -> print_error logger e
    | Ok (head_stream, _stopper) ->
        let*! _ =
          Lwt_stream.fold_s
            (fun (hash, header) acc ->
              Log.info logger (fun () ->
                  Format.sprintf "Level %ld" header.Block_header.shell.level) ;
              let*! balance_updates_recorder, acc' =
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
                            balance_update_machine
                            next_protocol
                        in
                        match recorder with
                        | None ->
                            let*! () =
                              Lwt_fmt.eprintf
                                "no balance update recorder found for protocol \
                                 %a@."
                                Protocol_hash.pp
                                next_protocol
                            in
                            assert false (* TODO *)
                        | Some recorder ->
                            Lwt.return
                              ( recorder,
                                Some
                                  ( recorder,
                                    header.Block_header.shell
                                      .Block_header.proto_level ) )))
              in
              let block_level = header.Block_header.shell.Block_header.level in
              let*! res = balance_updates_recorder cctx block_level in
              match res with
              | Error _ -> assert false
              | Ok (_level, _block_hash, _time, _balance_updates) ->
                  Lwt.return acc')
            head_stream
            None
        in
        Lwt.return_unit

  let blocks_loop cctx = Lwt.join [balance_update_loop cctx]
end
