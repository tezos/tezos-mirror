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

let usage_msg = "teztale-explorer -db <database> -level <level>"

let db_path = ref ""

let level = ref 0

let args =
  [
    ("-db", Arg.Set_string db_path, "path to the database");
    ("-level", Arg.Set_int level, "queried level");
  ]

let anon_args _str =
  Format.eprintf "usage: %s" usage_msg ;
  exit 1

let set_pragma_use_wal_mode db =
  let cmd = "PRAGMA journal_mode = WAL" in
  match Sqlite3.exec db cmd with
  | Sqlite3.Rc.OK -> ()
  | _ -> Format.eprintf "Failed to exec \'%s\': %s@." cmd (Sqlite3.errmsg db)

let main () =
  let db = Sqlite3.db_open ~mode:`READONLY !db_path in
  set_pragma_use_wal_mode db ;
  let () =
    Exporter.data_at_level db !level
    |> Data_encoding.Json.construct Data.encoding
    |> Format.printf "%a@." Data_encoding.Json.pp
  in
  let _closed = Sqlite3.db_close db in
  ()

let () =
  Arg.parse args anon_args usage_msg ;
  if String.equal !db_path "" || !level = 0 then print_endline usage_msg
  else main ()
