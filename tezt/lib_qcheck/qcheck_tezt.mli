(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(* This library permits to run QCheck test with tezt to benefit from the
   advantages of the 2 libraries :
   - QCheck permits to do property-based testing and combinators to generate
     random values to run tests on.
   - Tezt has interesting features such as auto-balancing in the CI.
*)

(** Register a QCheck test.

    [Register ~__FILE__ ?title ~tags ?seed test] is same as
    [Tezt_core.Test.register ~__FILE__ ~title ~tags' ?seed f] with :
    - if [title] is not provided, the name of [test] is used.
    - [tags'] is ["qcheck" :: tags].*)
val register :
  __FILE__:string ->
  ?title:string ->
  tags:string list ->
  ?seed:Tezt_core.Test.seed ->
  QCheck2.Test.t ->
  unit
