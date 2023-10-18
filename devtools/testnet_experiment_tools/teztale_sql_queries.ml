(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Caqti custom types *)
let decode_error x =
  Result.map_error
    (fun e ->
      Format.asprintf "%a@." Tezos_error_monad.Error_monad.pp_print_trace e)
    x

let block_hash =
  Caqti_type.custom
    ~encode:(fun t -> Result.Ok (Tezos_crypto.Hashed.Block_hash.to_string t))
    ~decode:(fun s -> decode_error (Tezos_crypto.Hashed.Block_hash.of_string s))
    Caqti_type.octets

(* Create tables queries *)

let create_canonical_chain_table_query =
  Caqti_request.Infix.(Caqti_type.(unit ->. unit))
    {| CREATE TABLE IF NOT EXISTS canonical_chain(
         id INTEGER PRIMARY KEY,
         block_id INTEGER NOT NULL,
         predecessor INTEGER,
         FOREIGN KEY (block_id) REFERENCES blocks(id),
         FOREIGN KEY (predecessor) REFERENCES blocks(predecessor)) |}

let create_reorganised_blocks_table_query =
  Caqti_request.Infix.(Caqti_type.(unit ->. unit))
    {| CREATE TABLE IF NOT EXISTS reorganised_blocks(
         id INTEGER PRIMARY KEY,
         block_id INTEGER,
         block_hash TEXT NOT NULL,
         level INTEGER NOT NULL,
         round INTEGER NOT NULL,
         FOREIGN KEY (block_id) REFERENCES blocks(id)) |}

(* Get entries queries *)

let get_canonical_chain_head_id_query =
  Caqti_request.Infix.(Caqti_type.unit ->* Caqti_type.int)
    {| SELECT predecessor
       FROM blocks
       WHERE id = (
         SELECT predecessor
         FROM blocks
         WHERE level = (
           SELECT MAX(level)
           FROM blocks
         )
         LIMIT 1
       ) |}

let get_canonical_chain_entries_query =
  Caqti_request.Infix.(Caqti_type.int ->* Caqti_type.(tup2 int (option int)))
    {| WITH canonical_chain AS (
         SELECT id, predecessor
         FROM blocks
         WHERE id = ?

         UNION ALL

         SELECT b.id, b.predecessor
         FROM canonical_chain c
         JOIN blocks b ON c.predecessor = b.id
       )

       SELECT id, predecessor
       FROM canonical_chain |}

(* Retrieve all the blocks below threshold [level] provided as input
   which are at the same level as some block from the canonical_chain,
   but which have a strictly smaller round *)
let get_reorganised_blocks_entries_query =
  Caqti_request.Infix.(
    Caqti_type.int ->* Caqti_type.(tup4 int block_hash int int))
    {| SELECT b2.id, b2.hash, b2.level, b2.round
       FROM blocks b
       JOIN canonical_chain c ON b.id = c.block_id
       JOIN blocks b2 on b.level = b2.level
       WHERE b.round > b2.round 
         AND b.level <= ( SELECT level FROM blocks WHERE id = ? ) |}

(* Populate tables queries *)

let insert_canonical_chain_entry_query =
  Caqti_request.Infix.(Caqti_type.(tup2 int int ->. unit))
    {| INSERT INTO canonical_chain(block_id, predecessor) 
       VALUES ($1, $2) ON CONFLICT DO NOTHING |}

let insert_reorganised_blocks_entry_query =
  Caqti_request.Infix.(Caqti_type.(tup4 int string int int ->. unit))
    {| INSERT INTO reorganised_blocks(block_id, block_hash, level, round) 
       VALUES ($1, $2, $3, $4) ON CONFLICT DO NOTHING |}
