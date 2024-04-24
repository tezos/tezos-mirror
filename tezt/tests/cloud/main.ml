(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Declaration of tests using tezt cloud. *)
let () =
  Tezt_cloud.register ~tags:[Tag.cloud] ;
  Dal.register () ;
  Test.run ()
