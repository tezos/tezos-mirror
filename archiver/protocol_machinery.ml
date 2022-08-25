(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module type PROTOCOL_SERVICES = sig
  val hash : Protocol_hash.t

  type wrap_full

  val wrap_full : Tezos_client_base.Client_context.full -> wrap_full

  val endorsing_rights :
    wrap_full ->
    reference_level:Int32.t ->
    Int32.t ->
    Consensus_ops.rights tzresult Lwt.t

  val couple_ops_to_rights :
    (int * 'a) list ->
    Consensus_ops.rights ->
    (Signature.public_key_hash * 'a) list * Signature.public_key_hash list

  type block_id

  module BlockIdMap : Map.S with type key = block_id

  val consensus_operation_stream :
    wrap_full ->
    (((Operation_hash.t
      * ((block_id * Int32.t * Consensus_ops.operation_kind * Int32.t option)
        * int))
     * error trace option)
     Lwt_stream.t
    * RPC_context.stopper)
    tzresult
    Lwt.t

  val baking_right :
    wrap_full ->
    Block_hash.t ->
    int ->
    (Signature.public_key_hash * Time.Protocol.t option) tzresult Lwt.t

  val block_round : Block_header.t -> int tzresult

  val consensus_ops_info_of_block :
    wrap_full -> Block_hash.t -> Consensus_ops.block_op list tzresult Lwt.t

  val get_block_info :
    wrap_full ->
    Int32.t ->
    (Signature.public_key_hash * Time.Protocol.t * int * Block_hash.t) tzresult
    Lwt.t
end

module type MAIN_LOOPS = sig
  val protocol_hash : Protocol_hash.t

  val blocks_loop : Tezos_client_base.Client_context.full -> unit Lwt.t

  val endorsements_loop : Tezos_client_base.Client_context.full -> unit Lwt.t
end

module Make_main_loops
    (Protocol_services : PROTOCOL_SERVICES)
    (Archiver : Archiver.S) : MAIN_LOOPS = struct
  let print_failures f =
    let*! o = f in
    match o with
    | Ok () -> Lwt.return_unit
    | Error e ->
        let () = Error_monad.pp_print_trace Format.err_formatter e in
        Lwt.return_unit

  let aliases cctxt =
    let*! aliases_opt = Wallet.of_context cctxt in
    match aliases_opt with
    | Ok aliases -> return aliases
    | Error err ->
        let () = Error_monad.pp_print_trace Format.err_formatter err in
        return Wallet.empty

  let dump_my_current_endorsements cctxt ~full reference_level level ops =
    let cctxt' = Protocol_services.wrap_full cctxt in
    let* rights =
      Protocol_services.endorsing_rights cctxt' ~reference_level level
    in
    let* aliases = aliases cctxt in
    Archiver.add_rights ~level rights aliases ;
    (* We could slightly optimize the db-archiver by not coupling ops
       to rights. In this case we'd need two versions of
       [Archiver.add_received] (one for the json-archiver, and one for
       the db-archiver); and we'd need to change the query used by the
       db-archiver. *)
    let (items, missing) = Protocol_services.couple_ops_to_rights ops rights in
    let endorsements =
      if full then
        List.fold_left (fun acc delegate -> (delegate, []) :: acc) items missing
      else items
    in
    let unaccurate = if full then Some false else None in
    let () = Archiver.add_received ?unaccurate level endorsements in
    return_unit

  let rec pack_by_slot i e = function
    | ((i', l) as x) :: t ->
        if Int.equal i i' then (i, e :: l) :: t else x :: pack_by_slot i e t
    | [] -> [(i, [e])]

  let endorsements_recorder cctxt current_level =
    let cctxt' = Protocol_services.wrap_full cctxt in
    let* (op_stream, _stopper) =
      Protocol_services.consensus_operation_stream cctxt'
    in
    let*! out =
      Lwt_stream.fold
        (fun ((hash, ((block, level, kind, round), slot)), errors) acc ->
          let reception_time = Time.System.now () in
          let op =
            Consensus_ops.{op = {hash; kind; round}; errors; reception_time}
          in
          Protocol_services.BlockIdMap.update
            block
            (function
              | Some (_, l) -> Some (level, pack_by_slot slot op l)
              | None -> Some (level, [(slot, [op])]))
            acc)
        op_stream
        Protocol_services.BlockIdMap.empty
    in
    Protocol_services.BlockIdMap.iter_ep
      (fun _ (level, endorsements) ->
        let full = Compare.Int32.(current_level = level) in
        dump_my_current_endorsements
          cctxt
          ~full
          current_level
          level
          endorsements)
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
        let cctxt' = Protocol_services.wrap_full cctxt in
        Lwt_stream.iter_p
          (fun ((_chain_id, hash), header) ->
            let reception_time = Time.System.now () in
            let block_level = header.Block_header.shell.Block_header.level in
            let round = Protocol_services.block_round header in
            match round with
            | Error e ->
                Lwt.return (Error_monad.pp_print_trace Format.err_formatter e)
            | Ok round -> (
                let*! consensus_ops =
                  Protocol_services.consensus_ops_info_of_block cctxt' hash
                in
                match consensus_ops with
                | Error e ->
                    Lwt.return
                      (Error_monad.pp_print_trace Format.err_formatter e)
                | Ok consensus_ops -> (
                    let*! baking_rights =
                      Protocol_services.baking_right cctxt' hash round
                    in
                    match baking_rights with
                    | Error e ->
                        Error_monad.pp_print_trace Format.err_formatter e ;
                        Lwt.return_unit
                    | Ok (baker, _) ->
                        let timestamp =
                          header.Block_header.shell.Block_header.timestamp
                        in
                        Archiver.add_block
                          hash
                          ~level:block_level
                          ~round:(Int32.of_int round)
                          timestamp
                          reception_time
                          baker
                          consensus_ops ;
                        Lwt.return_unit)))
          block_stream

  let endorsements_loop cctxt =
    let*! head_stream = Shell_services.Monitor.heads cctxt cctxt#chain in
    match head_stream with
    | Error e ->
        let () = Error_monad.pp_print_trace Format.err_formatter e in
        Lwt.return_unit
    | Ok (head_stream, _stopper) ->
        Lwt_stream.iter_s
          (fun (_hash, header) ->
            let block_level = header.Block_header.shell.Block_header.level in
            print_failures (endorsements_recorder cctxt block_level))
          head_stream

  let protocol_hash = Protocol_services.hash
end

module type JSON_COMMANDS = sig
  val register_json_commands : unit -> unit
end

module type DB_COMMANDS = sig
  val register_db_commands : unit -> unit
end

let group = {Clic.name = "teztale"; Clic.title = "A delegate operation monitor"}

let await_protocol_activation cctxt protocol_hash =
  let* (_block_stream, stop) =
    Shell_services.Monitor.heads
      cctxt
      ~next_protocols:[protocol_hash]
      cctxt#chain
  in
  stop () ;
  return_unit

module Make_json_commands (Loops : MAIN_LOOPS) : JSON_COMMANDS = struct
  let main cctxt prefix =
    let* () = Client_confirmations.wait_for_bootstrapped cctxt in
    let* () = await_protocol_activation cctxt Loops.protocol_hash in
    let dumper = Json_archiver.launch cctxt prefix in
    let main =
      let*! () =
        Lwt.Infix.(Loops.blocks_loop cctxt <&> Loops.endorsements_loop cctxt)
      in
      let () = Json_archiver.stop () in
      Lwt.return_unit
    in
    let*! out = Lwt.join [dumper; main] in
    return out

  let directory_parameter =
    Clic.parameter (fun _ p ->
        if not (Sys.file_exists p && Sys.is_directory p) then
          failwith "Directory doesn't exist: '%s'" p
        else return p)

  let register_json_commands () =
    Tezos_client_commands.Client_commands.register Loops.protocol_hash (fun _ ->
        [
          Clic.command
            ~group
            ~desc:"run the json archiver"
            Clic.no_options
            (Clic.prefixes ["run"; "json-archiver"; "in"]
            @@ Clic.param
                 ~name:"archive_path"
                 ~desc:"folder in which to dump files"
                 directory_parameter
            @@ Clic.stop)
            (fun () prefix cctxt -> main cctxt prefix);
        ])
end

module Make_db_commands (Loops : MAIN_LOOPS) : DB_COMMANDS = struct
  let main cctxt db_path source =
    let* () = Client_confirmations.wait_for_bootstrapped cctxt in
    let* () = await_protocol_activation cctxt Loops.protocol_hash in
    let db = Sqlite3.db_open db_path in
    let () = Db.set_pragma_use_foreign_keys db in
    let dumper = Db_archiver.launch db source in
    let main =
      let*! () =
        Lwt.Infix.(Loops.blocks_loop cctxt <&> Loops.endorsements_loop cctxt)
      in
      let () = Db_archiver.stop () in
      Lwt.return_unit
    in
    let*! out = Lwt.join [dumper; main] in
    return out

  let new_file_parameter =
    Clic.parameter (fun _ p ->
        if Sys.file_exists p then failwith "File already exist: '%s'" p
        else return p)

  let path_parameter =
    Clic.parameter (fun _ p ->
        if not (Sys.file_exists p) then failwith "File does not exist: '%s'" p
        else return p)

  let register_db_commands () =
    Tezos_client_commands.Client_commands.register Loops.protocol_hash (fun _ ->
        [
          Clic.command
            ~group
            ~desc:"create empty Sqlite3 database"
            Clic.no_options
            (Clic.prefixes ["create"; "database"; "in"]
            @@ Clic.param
                 ~name:"db_path"
                 ~desc:"path to file in which to store the Sqlite3 database"
                 new_file_parameter
            @@ Clic.stop)
            (fun () db_path _cctxt ->
              Db.create_db db_path ;
              return_unit);
          Clic.command
            ~group
            ~desc:"run the db archiver"
            Clic.no_options
            (Clic.prefixes ["run"; "db-archiver"; "on"]
            @@ Clic.param
                 ~name:"db_path"
                 ~desc:"path to Sqlite3 database file where to store the data"
                 path_parameter
            @@ Clic.prefix "for"
            @@ Clic.param
                 ~name:"source"
                 ~desc:"name of the data source (i.e. of the node)"
                 (Clic.parameter (fun _ p -> return p))
            @@ Clic.stop)
            (fun () db_path source cctxt -> main cctxt db_path source);
        ])
end
