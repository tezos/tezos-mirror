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

(* TRUE/FALSE literals were introduced in Sqlite 3.23 (and are represented as
   the integers 1 and 0. To support older versions, we convert booleans to
   integers. *)
let bool_to_int b = if b then 1 else 0

let db_schema =
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

let maybe_insert_source source =
  "INSERT OR IGNORE INTO nodes (name) VALUES (\'" ^ source ^ "\');"

let maybe_insert_delegates_from_rights rights =
  Format.asprintf
    "INSERT OR IGNORE INTO delegates (address) VALUES %a;"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
       (fun f r ->
         Format.fprintf
           f
           "(x'%a')"
           Hex.pp
           (Signature.Public_key_hash.to_hex r.Consensus_ops.address)))
    rights

let maybe_insert_endorsing_rights ~level rights =
  Format.asprintf
    "INSERT OR IGNORE INTO endorsing_rights (level, delegate, first_slot, \
     endorsing_power) SELECT column1, delegates.id, column3, column4 FROM \
     delegates JOIN (VALUES %a) ON delegates.address = column2;"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
       (fun f Consensus_ops.{address; first_slot; power} ->
         Format.fprintf
           f
           "(%ld, x'%a', %d, %d)"
           level
           Hex.pp
           (Signature.Public_key_hash.to_hex address)
           first_slot
           power))
    rights

let maybe_insert_operations level extractor op_extractor l =
  Format.asprintf
    "INSERT INTO operations (hash, endorsement, endorser, level, round) SELECT \
     column1, column2, delegates.id, %ld, column4 FROM delegates JOIN (VALUES \
     %a) ON delegates.address = column3 WHERE column1 NOT IN (SELECT hash FROM \
     operations WHERE level = %ld);"
    level
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
       (fun f x ->
         let (delegate, ops) = extractor x in
         Format.pp_print_list
           ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
           (fun f y ->
             let op = op_extractor y in
             Format.fprintf
               f
               "(x'%a', %d, x'%a', %a)"
               Hex.pp
               (Operation_hash.to_hex op.Consensus_ops.hash)
               (bool_to_int (op.Consensus_ops.kind = Consensus_ops.Endorsement))
               Hex.pp
               (Signature.Public_key_hash.to_hex delegate)
               (Format.pp_print_option
                  ~none:(fun f () -> Format.pp_print_string f "NULL")
                  (fun f x -> Format.fprintf f "%li" x))
               op.Consensus_ops.round)
           f
           ops))
    l
    level

let maybe_insert_operations_from_block ~level operations =
  maybe_insert_operations
    level
    (fun op -> Consensus_ops.(op.delegate, [op.op]))
    (fun x -> x)
    operations

let maybe_insert_operations_from_received ~level operations =
  let operations = List.filter (fun (_, l) -> l <> []) operations in
  maybe_insert_operations
    level
    (fun (delegate, ops) -> (delegate, ops))
    (fun (op : Consensus_ops.received_operation) -> op.op)
    operations

let insert_block hash ~level ~round timestamp delegate =
  Format.asprintf
    "INSERT INTO blocks (timestamp, hash, level, round, baker) SELECT column1, \
     column2, %ld, column4, delegates.id FROM delegates JOIN (VALUES ('%a', \
     x'%a', x'%a', %ld)) ON delegates.address = column3;"
    level
    Time.Protocol.pp
    timestamp
    Hex.pp
    (Block_hash.to_hex hash)
    Hex.pp
    (Signature.Public_key_hash.to_hex delegate)
    round

let insert_received_operations ~source ~level operations =
  let operations = List.filter (fun (_, l) -> l <> []) operations in
  Format.asprintf
    "INSERT INTO operations_reception (timestamp, operation, source, errors) \
     SELECT column1, operations.id, nodes.id, column2 FROM operations, nodes, \
     (VALUES %a) ON operations.hash = column3 AND nodes.name = '%s' WHERE \
     operations.level = %ld;"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
       (fun f (_, l) ->
         (Format.pp_print_list
            ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
            (fun f (op : Consensus_ops.received_operation) ->
              Format.fprintf
                f
                "('%a', %a, x'%a')"
                Time.System.pp_hum
                op.reception_time
                (Format.pp_print_option
                   ~none:(fun f () -> Format.pp_print_string f "NULL")
                   (fun f errors ->
                     Format.fprintf
                       f
                       "x'%a'"
                       Hex.pp
                       (Hex.of_bytes
                          (Data_encoding.Binary.to_bytes_exn
                             (Data_encoding.list Error_monad.error_encoding)
                             errors))))
                op.errors
                Hex.pp
                (Operation_hash.to_hex op.op.hash)))
           f
           l))
    operations
    source
    level

let insert_included_operations block_hash ~level operations =
  Format.asprintf
    "INSERT INTO operations_inclusion (block, operation) SELECT blocks.id, \
     operations.id FROM operations, blocks, (VALUES %a) ON operations.hash = \
     column1 AND blocks.hash = x'%a' WHERE operations.level = %ld;"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
       (fun f (op : Consensus_ops.block_op) ->
         Format.fprintf
           f
           "(x'%a')"
           Hex.pp
           (Operation_hash.to_hex op.Consensus_ops.op.hash)))
    operations
    Hex.pp
    (Block_hash.to_hex block_hash)
    level

let insert_received_block ~source hash reception_time =
  Format.asprintf
    "INSERT INTO blocks_reception (timestamp, block, source) SELECT column1, \
     blocks.id, nodes.id FROM blocks JOIN (VALUES ('%a', x'%a')) ON \
     blocks.hash = column2 JOIN nodes ON nodes.name = '%s';"
    Time.System.pp_hum
    reception_time
    Hex.pp
    (Block_hash.to_hex hash)
    source
