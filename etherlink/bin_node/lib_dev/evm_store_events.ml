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
    ~msg:"store is being initialized for the first time"
    ~level:Notice
    ()

let applied_migration =
  declare_2
    ~section
    ~name:"store_applied_migration"
    ~msg:"applied migration {name} to the store in {time}"
    ~level:Notice
    ("name", Data_encoding.string)
    ("time", Time.System.Span.encoding)
    ~pp2:Time.System.Span.pp_hum

let migrations_from_the_future =
  declare_2
    ~section
    ~name:"migrations_from_the_future"
    ~msg:
      "store has {applied} migrations applied but the EVM node is only aware \
       of {known}"
    ~level:Error
    ("applied", Data_encoding.int31)
    ("known", Data_encoding.int31)

let no_l1_latest_level_to_catch_up =
  declare_0
    ~section
    ~name:"no_l1_latest_level"
    ~msg:"no l1 level processed, no EVM events catch up will occur"
    ~level:Warning
    ()

let init_store () = emit init_store ()

let applied_migration name time = emit applied_migration (name, time)

let migrations_from_the_future ~applied ~known =
  emit migrations_from_the_future (applied, known)

let no_l1_latest_level_to_catch_up () = emit no_l1_latest_level_to_catch_up ()
