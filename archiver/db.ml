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

let schema =
  "\n\
  \  CREATE TABLE delegates(\n\
  \     id INTEGER PRIMARY KEY,\n\
  \     address BLOB UNIQUE NOT NULL,\n\
  \     alias TEXT);\n\
  \   CREATE TABLE nodes(\n\
  \     id INTEGER PRIMARY KEY,\n\
  \     name TEXT UNIQUE NOT NULL,\n\
  \     comment TEXT);\n\
  \   CREATE TABLE blocks(\n\
  \     id INTEGER PRIMARY KEY,\n\
  \     timestamp INTEGER NOT NULL, -- Unix time\n\
  \     hash BLOB UNIQUE NOT NULL,\n\
  \     level INTEGER NOT NULL,\n\
  \     round INTEGER NOT NULL,\n\
  \     baker INTEGER NOT NULL,\n\
  \     FOREIGN KEY (baker) REFERENCES delegates(id));\n\
  \   CREATE TABLE blocks_reception(\n\
  \     id INTEGER PRIMARY KEY,\n\
  \     timestamp TEXT NOT NULL, -- ISO8601 string\n\
  \     block INTEGER NOT NULL,\n\
  \     source INTEGER NOT NULL,\n\
  \     FOREIGN KEY (block) REFERENCES blocks(id),\n\
  \     FOREIGN KEY (source) REFERENCES nodes(id));\n\
  \   CREATE TABLE operations(\n\
  \     id INTEGER PRIMARY KEY,\n\
  \     hash BLOB UNIQUE NOT NULL,\n\
  \     endorsement INTEGER NOT NULL,\n\
  \     endorser INTEGER NOT NULL,\n\
  \     level INTEGER NOT NULL,\n\
  \     round INTEGER,\n\
  \     FOREIGN KEY (endorser) REFERENCES delegates(id));\n\
  \   CREATE TABLE operations_reception(\n\
  \     id INTEGER PRIMARY KEY,\n\
  \     timestamp TEXT NOT NULL, -- ISO8601 string\n\
  \     operation INTEGER NOT NULL,\n\
  \     source INTEGER NOT NULL,\n\
  \     errors BLOB,\n\
  \     FOREIGN KEY (operation) REFERENCES operations(id),\n\
  \     FOREIGN KEY (source) REFERENCES nodes(id));\n\
  \   CREATE TABLE operations_inclusion(\n\
  \      id INTEGER PRIMARY KEY,\n\
  \      block INTEGER NOT NULL,\n\
  \      operation INTEGER NOT NULL,\n\
  \      FOREIGN KEY (block) REFERENCES blocks(id),\n\
  \      FOREIGN KEY (operation) REFERENCES operations(id));\n\
  \   CREATE TABLE endorsing_rights(\n\
  \      id INTEGER PRIMARY KEY,\n\
  \      level INTEGER NOT NULL,\n\
  \      delegate INTEGER NOT NULL,\n\
  \      first_slot INTEGER NOT NULL,\n\
  \      endorsing_power INTEGER NOT NULL,\n\
  \      FOREIGN KEY (delegate) REFERENCES delegates(id),\n\
  \      UNIQUE (level, delegate));\n\
  \   CREATE INDEX endorsing_rights_level_idx ON endorsing_rights(level);\n\
  \   CREATE INDEX operations_level_idx ON operations(level);\n\
  \   CREATE INDEX operations_reception_operation_idx ON \n\
  \      operations_reception(operation);\n\
  \   CREATE INDEX operations_inclusion_operation_idx ON \n\
  \      operations_inclusion(operation);"

(* TRUE/FALSE literals were introduced in Sqlite 3.23 (and are represented as
   the integers 1 and 0. To support older versions, we convert booleans to
   integers. *)
let bool_to_int b = if b then 1 else 0

let exec db ?cb q =
  match Sqlite3.exec db ?cb q with
  | Sqlite3.Rc.OK -> ()
  | _ -> Format.eprintf "Failed to exec \'%s\': %s@." q (Sqlite3.errmsg db)

let set_pragma_use_foreign_keys db =
  let cmd = "PRAGMA foreign_keys = TRUE" in
  exec db cmd

let create_db path =
  let db = Sqlite3.db_open path in
  exec db schema ;
  assert (Sqlite3.db_close db)

let ensure_source_is_there db source =
  exec db ("INSERT OR IGNORE INTO nodes (name) VALUES (\'" ^ source ^ "\');")
