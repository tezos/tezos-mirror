(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section

let init_store =
  declare_0
    ~section
    ~name:"store_init"
    ~msg:"Store is being initialized for the first time"
    ~level:Notice
    ()

let assume_old_store =
  declare_0
    ~section
    ~name:"store_assume_old"
    ~msg:
      "A store already exists, provides the tables created by V0, but is \
       missing the migrations table. We assume it is correct."
    ~level:Warning
    ()

let applied_migration =
  declare_1
    ~section
    ~name:"store_applied_migration"
    ~msg:"Applied migration {name} to the store"
    ~level:Notice
    ("name", Data_encoding.string)

let migrations_from_the_future =
  declare_2
    ~section
    ~name:"migrations_from_the_future"
    ~msg:
      "Store has {applied} migrations applied but the EVM node is only aware \
       of {known}"
    ~level:Error
    ("applied", Data_encoding.int31)
    ("known", Data_encoding.int31)

let init_store () = emit init_store ()

let applied_migration name = emit applied_migration name

let assume_old_store () = emit assume_old_store ()

let migrations_from_the_future ~applied ~known =
  emit migrations_from_the_future (applied, known)
