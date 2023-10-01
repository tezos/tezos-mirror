(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Simple = struct
  include Internal_event.Simple

  let section = ["l2_store"]

  let starting_gc =
    declare_1
      ~section
      ~name:"l2_store_starting_gc"
      ~msg:"Starting GC for store {store_name}"
      ~level:Info
      ~pp1:Format.pp_print_string
      ("store_name", Data_encoding.string)

  let finished_gc =
    declare_1
      ~section
      ~name:"l2_store_finished_gc"
      ~msg:"Finished GC for store {store_name}"
      ~level:Info
      ~pp1:Format.pp_print_string
      ("store_name", Data_encoding.string)

  let ignore_gc =
    declare_1
      ~section
      ~name:"l2_store_ignore_gc"
      ~msg:
        "Ignore GC for store {store_name} because there is already an ongoing \
         one"
      ~level:Info
      ~pp1:Format.pp_print_string
      ("store_name", Data_encoding.string)

  let failed_gc =
    declare_2
      ~section
      ~name:"l2_store_failed_gc"
      ~msg:"[Warning] Failed GC for store {store_name} because {error}"
      ~level:Warning
        (* This event has level warning right now but may be changed in the
           future depending on if it appears in the wild. *)
      ~pp1:Format.pp_print_string
      ~pp2:pp_print_trace
      ("store_name", Data_encoding.string)
      ("error", trace_encoding)

  let missing_value_gc =
    declare_2
      ~section
      ~name:"l2_store_missing_value_gc"
      ~msg:
        "[Warning] GC could not retain key {key} of store {store_name} it is \
         not in the store any more."
      ~level:Warning
        (* This event has level warning right now but may be changed in the
           future depending on if it appears in the wild. *)
      ~pp1:Format.pp_print_string
      ~pp2:Format.pp_print_string
      ("store_name", Data_encoding.string)
      ("key", Data_encoding.string)
end

let starting_gc name = Simple.(emit starting_gc) name

let finished_gc name = Simple.(emit finished_gc) name

let ignore_gc name = Simple.(emit ignore_gc) name

let failed_gc name error = Simple.(emit failed_gc) (name, error)

let missing_value_gc name key = Simple.(emit missing_value_gc) (name, key)
