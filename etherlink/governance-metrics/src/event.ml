(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Simple = struct
  include Internal_event.Simple

  let section = ["etherlink_governance_observer"]

  let starting_observer =
    declare_0
      ~section
      ~level:Notice
      ~name:"starting_etherlink_governance_observer"
      ~msg:"Starting Etherlink's governance observer"
      ()
end

let starting_observer = Simple.(emit starting_observer)
