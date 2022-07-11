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

module Services = struct
  let level_arg = RPC_arg.int

  let level_range_arg =
    let max_range = 4096 in
    RPC_arg.make
      ~descr:
        (Format.asprintf
           "a level range in the format '<min_level>-<max_level>'; the maximum \
            range is set at %d levels"
           max_range)
      ~name:"level range"
      ~destruct:(fun str ->
        Format.printf "parsing '%s'@." str ;
        match String.split_on_char '-' str with
        | [str1; str2] -> (
            match (int_of_string_opt str1, int_of_string_opt str2) with
            | (Some l1, Some l2) ->
                if l1 > l2 || l1 < 0 then Error "not a valid range"
                else if l2 - l1 > max_range then
                  Error (Format.asprintf "range exceeds %d levels" max_range)
                else Ok (l1, l2)
            | _ -> Error "int_of_string failure")
        | _ -> Error "expecting a range")
      ~construct:(fun (l1, l2) -> Format.asprintf "%d-%d" l1 l2)
      ()

  let data_at_level () =
    RPC_service.get_service
      ~description:"export data at level"
      ~query:RPC_query.empty
      ~output:Data.encoding
      RPC_path.(open_root / "level" /: level_arg)

  let anomalies_at_levels () =
    RPC_service.get_service
      ~description:"export anomalies at levels"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Data.Anomaly.encoding)
      RPC_path.(open_root / "anomalies" /: level_range_arg)
end

module RPC_server = struct
  let register_data_at_level db ctxt dir =
    RPC_directory.register
      dir
      (Services.data_at_level ())
      (fun (_, level) () () -> Exporter.data_at_level db ctxt level)

  let register_anomalies_at_levels db dir =
    RPC_directory.register
      dir
      (Services.anomalies_at_levels ())
      (fun (_, (first_level, last_level)) () () ->
        let levels =
          Stdlib.List.init
            (last_level - first_level + 1)
            (fun i -> first_level + i)
        in
        Lwt_result_syntax.return @@ List.concat
        @@ List.map (Exporter.anomalies_at_level db) levels)

  let register_rpcs db ctxt =
    RPC_directory.empty
    |> register_data_at_level db ctxt
    |> register_anomalies_at_levels db

  let start (addr, port) dir =
    let open Lwt_syntax in
    let rpc_addr = P2p_addr.of_string_exn addr in
    let host = Ipaddr.V6.to_string rpc_addr in
    let server_mode = `TCP (`Port port) in
    let acl = RPC_server.Acl.default rpc_addr in
    Lwt.catch
      (fun () ->
        let* server =
          RPC_server.launch
            ~media_types:Media_type.all_media_types
            ~host
            ~acl
            server_mode
            dir
        in
        return_ok server)
      fail_with_exn

  let install_finalizer rpc_server db =
    let open Lwt_syntax in
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun _exit_status ->
    Format.printf "closing the database connection...@." ;
    let _closed = Sqlite3.db_close db in
    Format.printf "closing the RPC server...@." ;
    let* () = RPC_server.shutdown rpc_server in
    return ()
end

let set_pragma_use_wal_mode db =
  let cmd = "PRAGMA journal_mode = WAL" in
  match Sqlite3.exec db cmd with
  | Sqlite3.Rc.OK -> ()
  | _ -> Format.eprintf "Failed to exec \'%s\': %s@." cmd (Sqlite3.errmsg db)

let run ~db_path addr_port ctxt =
  let open Lwt_result_syntax in
  let db = Sqlite3.db_open ~mode:`READONLY db_path in
  set_pragma_use_wal_mode db ;
  let* rpc_server = RPC_server.(start addr_port (register_rpcs db ctxt)) in
  Format.printf "exporter RPC server started@." ;
  let _ = RPC_server.install_finalizer rpc_server db in
  Lwt_utils.never_ending ()
