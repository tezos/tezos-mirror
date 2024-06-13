(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

let register ~__FILE__ ?title ~tags ?seed (t : QCheck2.Test.t) : unit =
  let (QCheck2.Test.Test cell) = t in
  let title =
    match title with Some x -> x | None -> QCheck2.Test.get_name cell
  in
  let tags = "qcheck" :: tags in
  Tezt_core.Test.register ~__FILE__ ~title ~tags ?seed @@ fun () ->
  let rand = Random.get_state () in
  QCheck2.Test.check_cell_exn ~rand cell ;
  Tezt_core.Base.unit
