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

  let data_at_level_service () =
    RPC_service.get_service
      ~description:"export data at level"
      ~query:RPC_query.empty
      ~output:Data.encoding
      RPC_path.(open_root /: level_arg)
end

module RPC_server = struct
  let register_data_at_level db dir =
    RPC_directory.register
      dir
      (Services.data_at_level_service ())
      (fun (_, level) () () ->
        Exporter.data_at_level db level |> Lwt_result_syntax.return)

  let register_rpcs db _ctxt = RPC_directory.empty |> register_data_at_level db

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
