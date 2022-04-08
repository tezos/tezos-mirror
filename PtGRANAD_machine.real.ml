(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Block_services =
  Tezos_client_010_PtGRANAD.Protocol_client_context.Alpha_block_services

open Lwt_tzresult_syntax
open Tezos_protocol_010_PtGRANAD

let print_failures f =
  let*! o = f in
  match o with
  | Ok () -> Lwt.return_unit
  | Error e ->
      let () = Error_monad.pp_print_trace Format.err_formatter e in
      Lwt.return_unit

let dump_my_current_endorsements cctxt ~full block level ops =
  let* rights =
    Protocol.Delegate_services.Endorsing_rights.get
      cctxt
      (cctxt#chain, `Hash (block, 0))
  in
  let (items, missing) =
    List.fold_left
      Protocol.Delegate_services.Endorsing_rights.(
        fun (acc, rights) (errors, delay, slot) ->
          match
            List.partition
              (fun right -> List.mem ~equal:Int.equal slot right.slots)
              rights
          with
          | (([] | _ :: _ :: _), _) -> assert false
          | ([right], rights') ->
              ((right.delegate, errors, Some delay) :: acc, rights'))
      ([], rights)
      ops
  in
  let endorsements =
    if full then
      List.fold_left
        (fun acc right ->
          ( right.Protocol.Delegate_services.Endorsing_rights.delegate,
            None,
            None )
          :: acc)
        items
        missing
    else items
  in
  let unaccurate = if full then Some false else None in
  let () = Archiver.add_received ?unaccurate level endorsements in
  return_unit

let extract_endorsement
    (operation_content : Protocol.Alpha_context.packed_operation) =
  match operation_content with
  | {
   Protocol.Main.protocol_data =
     Protocol.Alpha_context.Operation_data
       {
         contents =
           Single
             (Endorsement_with_slot
               {
                 endorsement =
                   {
                     shell = _;
                     protocol_data =
                       {contents = Single (Endorsement {level}); signature = _};
                   };
                 slot;
               });
         signature = _;
       };
   shell = {branch};
  } ->
      Some ((branch, level), slot)
  | _ -> None

let endorsements_recorder cctxt current_level =
  let* (ops_stream, _stopper) =
    Block_services.Mempool.monitor_operations
      cctxt
      ~chain:cctxt#chain
      ~applied:true
      ~refused:false
      ~branch_delayed:true
      ~branch_refused:true
      ()
  in
  let op_stream = Lwt_stream.flatten ops_stream in
  let*! out =
    Lwt_stream.fold
      (fun ((_hash, op), errors) acc ->
        let delay = Time.System.now () in
        match extract_endorsement op with
        | None -> acc
        | Some ((block, level), news) ->
            Block_hash.Map.update
              block
              (function
                | Some (_, l) -> Some (level, (errors, delay, news) :: l)
                | None -> Some (level, [(errors, delay, news)]))
              acc)
      op_stream
      Block_hash.Map.empty
  in
  Block_hash.Map.iter_ep
    (fun block (level, endorsements) ->
      let level = Protocol.Alpha_context.Raw_level.to_int32 level in
      let full = Compare.Int32.(current_level = level) in
      dump_my_current_endorsements cctxt ~full block level endorsements)
    out

let blocks_loop cctxt =
  let*! block_stream =
    Shell_services.Monitor.valid_blocks cctxt ~chains:[cctxt#chain] ()
  in
  match block_stream with
  | Error e ->
      let () = Error_monad.pp_print_trace Format.err_formatter e in
      Lwt.return_unit
  | Ok (block_stream, _stopper) ->
      Lwt_stream.iter_p
        (fun ((chain_id, hash), header) ->
          let reception_time = Time.System.now () in
          let block_level = header.Block_header.shell.Block_header.level in
          match
            Data_encoding.Binary.of_bytes
              Protocol.block_header_data_encoding
              header.Block_header.protocol_data
          with
          | Error err ->
              let () =
                Format.eprintf "@[%a@]@." Data_encoding.Binary.pp_read_error err
              in
              Lwt.return_unit
          | Ok {contents = {priority; seed_nonce_hash = _; _}; signature = _}
            -> (
              let*! ops =
                Block_services.Operations.operations_in_pass
                  cctxt
                  ~chain:(`Hash chain_id)
                  ~block:(`Hash (hash, 0))
                  0
              in
              match ops with
              | Error e ->
                  Lwt.return (Error_monad.pp_print_trace Format.err_formatter e)
              | Ok ops -> (
                  match
                    Protocol.Alpha_context.Raw_level.of_int32 block_level
                  with
                  | Error x ->
                      Lwt.return
                        (Protocol.Environment.Error_monad.pp_trace
                           Format.err_formatter
                           x)
                  | Ok level -> (
                      let*! baking_rights =
                        Protocol.Delegate_services.Baking_rights.get
                          ~levels:[level]
                          ~max_priority:priority
                          cctxt
                          (cctxt#chain, `Hash (hash, 0))
                      in
                      match baking_rights with
                      | Error e ->
                          Error_monad.pp_print_trace Format.err_formatter e ;
                          Lwt.return_unit
                      | Ok baking_rights -> (
                          match List.last_opt baking_rights with
                          | None -> Lwt.return_unit
                          | Some {level = l; delegate; priority = p; _} ->
                              let () =
                                assert (
                                  Protocol.Alpha_context.Raw_level.equal level l
                                  && Compare.Int.equal priority p)
                              in
                              let timestamp =
                                header.Block_header.shell.Block_header.timestamp
                              in
                              let pks =
                                List.filter_map
                                  (fun Block_services.{receipt; _} ->
                                    match receipt with
                                    | Receipt
                                        (Protocol.Apply_results
                                         .Operation_metadata
                                          {
                                            contents =
                                              Single_result
                                                (Protocol.Apply_results
                                                 .Endorsement_with_slot_result
                                                  (Tezos_raw_protocol_010_PtGRANAD
                                                   .Apply_results
                                                   .Endorsement_result
                                                    {delegate; _}));
                                          }) ->
                                        Some delegate
                                    | _ -> None)
                                  ops
                              in
                              Archiver.add_block
                                block_level
                                hash
                                timestamp
                                reception_time
                                delegate
                                pks ;
                              Lwt.return_unit)))))
        block_stream

let endorsements_loop cctxt =
  let*! head_stream = Shell_services.Monitor.heads cctxt cctxt#chain in
  match head_stream with
  | Error e ->
      let () = Error_monad.pp_print_trace Format.err_formatter e in
      Lwt.return_unit
  | Ok (head_stream, _stopper) ->
      Lwt_stream.iter_p
        (fun (_hash, header) ->
          let block_level = header.Block_header.shell.Block_header.level in
          print_failures (endorsements_recorder cctxt block_level))
        head_stream

let main cctxt prefix =
  let dumper = Archiver.launch cctxt prefix in
  let cctxt =
    new Tezos_client_010_PtGRANAD.Protocol_client_context.wrap_full cctxt
  in
  let main =
    let*! () = Lwt.Infix.(blocks_loop cctxt <&> endorsements_loop cctxt) in
    let () = Archiver.stop () in
    Lwt.return_unit
  in
  let*! out = Lwt.join [dumper; main] in
  return out

let group = {Clic.name = "teztale"; Clic.title = "A delegate operation monitor"}

let directory_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let register_commands () =
  Tezos_client_commands.Client_commands.register Protocol.hash (fun _ ->
      [
        Clic.command
          ~group
          ~desc:"Go"
          Clic.no_options
          (Clic.prefixes ["run"; "in"]
          @@ Clic.param
               ~name:"archive_path"
               ~desc:"folder in which to dump files"
               directory_parameter
          @@ Clic.stop)
          (fun () prefix cctxt -> main cctxt prefix);
      ])
