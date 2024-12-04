(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["dal_node"; "store"; "sqlite3"]

let applied_migration =
  declare_2
    ~section
    ~name:"store_applied_migration"
    ~msg:"Applied migration {name} to the store in {duration} second(s)"
    ~level:Info
    ("name", Data_encoding.string)
    ("duration", Data_encoding.float)

let migrations_from_the_future =
  declare_2
    ~section
    ~name:"migrations_from_the_future"
    ~msg:
      "Dal_store_sqlite3 has {applied} migrations applied but the DAL node is \
       only aware of {known}"
    ~level:Error
    ("applied", Data_encoding.int31)
    ("known", Data_encoding.int31)

let applied_migration ~name ~duration = emit applied_migration (name, duration)

let migrations_from_the_future ~applied ~known =
  emit migrations_from_the_future (applied, known)
