(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Block_services = Protocol_client_context.Alpha_block_services
open Tezos_protocol_009_PsFLoren

let dump_my_current_endorsements cctxt ?unaccurate ~full block level errors ops
    =
  Protocol.Delegate_services.Endorsing_rights.get
    cctxt
    (cctxt#chain, `Hash (block, 0))
  >>=? fun rights ->
  let (items, missing) =
    List.fold_left
      Protocol.Delegate_services.Endorsing_rights.(
        fun (acc, rights) (delay, slot) ->
          match
            List.partition (fun right -> List.mem slot right.slots) rights
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
          (right.Protocol.Delegate_services.Endorsing_rights.delegate, [], None)
          :: acc)
        items
        missing
    else items
  in
  let () = Archiver.add_received ?unaccurate level endorsements in
  return_unit

let rec check_level_in_endorsements ref_level = function
  | Block_services.
      { protocol_data =
          Protocol.Alpha_context.Operation_data
            { contents = Single (Endorsement { level }); _ };
        _
      }
    :: tail ->
      Int32.equal ref_level (Protocol.Alpha_context.Raw_level.to_int32 level)
      && check_level_in_endorsements ref_level tail
  | _ :: tail -> check_level_in_endorsements ref_level tail
  | [] -> true

let extract_endorsement infos
    (operation_content : Protocol.Alpha_context.packed_operation) =
  match operation_content with
  | { Protocol.Main.protocol_data =
        Protocol.Alpha_context.Operation_data
          { contents =
              Single
                (Endorsement_with_slot
                  { endorsement =
                      { shell = _;
                        protocol_data =
                          { contents = Single (Endorsement { level });
                            signature = _
                          }
                      };
                    slot
                  });
            signature = _
          };
      shell = { branch }
    } ->
      let () =
        assert (
          match infos with
          | None -> true
          | Some (b', l') ->
              Block_hash.equal branch b'
              && Protocol.Alpha_context.Raw_level.equal level l' )
      in
      (Some (branch, level), Some slot)
  | _ -> (infos, None)

let rec valid_endorsements_loop cctxt unaccurate =
  Block_services.Mempool.monitor_operations
    cctxt
    ~chain:cctxt#chain
    ~applied:true
    ~refused:false
    ~branch_delayed:false
    ~branch_refused:false
    ()
  >>=? fun (ops_stream, _stopper) ->
  let op_stream = Lwt_stream.flatten ops_stream in
  Lwt_stream.fold
    (fun op (infos, l) ->
      let delay = Systime_os.now () in
      let (vv, news) = extract_endorsement infos op in
      (vv, Option.fold ~some:(fun x -> (delay, x) :: l) ~none:l news))
    op_stream
    (None, [])
  >>= fun out ->
  let next = valid_endorsements_loop cctxt false in
  match out with
  | (None, _) -> next
  | (Some (block, level), timed_ops) ->
      let level = Protocol.Alpha_context.Raw_level.to_int32 level in
      dump_my_current_endorsements
        cctxt
        ~unaccurate
        ~full:true
        block
        level
        []
        timed_ops
      >>=? fun () -> next

let errors_of_operation cctxt op =
  Block_services.Mempool.pending_operations cctxt ~chain:cctxt#chain ()
  >|=? fun { refused; branch_refused; branch_delayed; _ } ->
  let op_hash = Protocol.Alpha_context.Operation.hash_packed op in
  match Operation_hash.Map.find op_hash branch_delayed with
  | Some (_, err) -> err
  | None -> (
      match Operation_hash.Map.find op_hash branch_refused with
      | Some (_, err) -> err
      | None -> (
          match Operation_hash.Map.find op_hash refused with
          | Some (_, err) -> err
          | None -> [] ) )

let rec invalid_endorsements_loop cctxt =
  Block_services.Mempool.monitor_operations
    cctxt
    ~chain:cctxt#chain
    ~applied:false
    ~refused:true
    ~branch_delayed:true
    ~branch_refused:true
    ()
  >>=? fun (ops_stream, _stopper) ->
  let op_stream = Lwt_stream.flatten ops_stream in
  Lwt_stream.fold_s
    (fun op acc ->
      let delay = Systime_os.now () in
      match extract_endorsement None op with
      | (_, None) -> Lwt.return acc
      | (None, Some _) -> assert false
      | (Some (block, level), Some news) ->
          errors_of_operation cctxt op >>=? fun errors ->
          Lwt.return
            (acc >|? fun l -> (block, level, errors, (delay, news)) :: l))
    op_stream
    (ok [])
  >>=? fun out ->
  let next = invalid_endorsements_loop cctxt in
  List.iter_ep
    (fun (block, level, errors, endorsement) ->
      let level = Protocol.Alpha_context.Raw_level.to_int32 level in
      dump_my_current_endorsements
        cctxt
        ~full:false
        block
        level
        errors
        [endorsement])
    out
  >>=? fun () -> next

let blocks_loop cctxt =
  Shell_services.Monitor.valid_blocks cctxt ~chains:[cctxt#chain] ()
  >>=? fun (block_stream, _stopper) ->
  Lwt_stream.fold_s
    (fun ((chain_id, hash), header) _acc ->
      let reception_time = Systime_os.now () in
      Block_services.Operations.operations_in_pass
        cctxt
        ~chain:(`Hash chain_id)
        ~block:(`Hash (hash, 0))
        0
      >>=? fun ops ->
      let block_level = header.Block_header.shell.Block_header.level in
      (*_assert
          (check_level_in_endorsements (Int32.pred block_level) ops)
          __LOC__
          "Endorsement with the wrong level in "
        >>=? fun () ->*)
      let pks =
        List.filter_map
          (fun Block_services.{ receipt; _ } ->
            match receipt with
            | Some
                (Protocol.Apply_results.Operation_metadata
                  { contents =
                      Single_result
                        (Protocol.Apply_results.Endorsement_with_slot_result
                          (Tezos_raw_protocol_009_PsFLoren.Apply_results
                           .Endorsement_result { delegate; _ }))
                  }) ->
                Some delegate
            | _ -> None)
          ops
      in
      let () = Archiver.add_block block_level hash reception_time pks in
      return_unit)
    block_stream
    (ok ())

let print_failures f =
  f >|= function
  | Ok () -> ()
  | Error e -> Error_monad.pp_print_error Format.err_formatter e

let main cctxt prefix =
  let dumper = Archiver.launch prefix in
  let main =
    Lwt.join
      [ print_failures (valid_endorsements_loop cctxt true);
        print_failures (invalid_endorsements_loop cctxt);
        print_failures (blocks_loop cctxt) ]
    >>= fun () ->
    let () = Archiver.stop () in
    return_unit
  in
  dumper >>= fun () -> main

let group =
  { Clic.name = "teztale"; Clic.title = "A delegate operation monitor" }

let directory_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let commands =
  [ Clic.command
      ~group
      ~desc:"Go"
      Clic.no_options
      ( Clic.prefixes ["run"; "in"]
      @@ Clic.param
           ~name:"archive_path"
           ~desc:"folder in which to dump files"
           directory_parameter
      @@ Clic.stop )
      (fun () prefix cctxt -> main cctxt prefix) ]

let select_commands _ _ =
  return
    (List.map
       (Clic.map_command (new Protocol_client_context.wrap_full))
       commands)

let () = Client_main_run.run (module Client_config) ~select_commands
