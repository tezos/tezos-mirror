(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [data_at_level_range conf db_pool boundaries] *)
val data_at_level_range :
  Config.t (* teztale server config *) ->
  ( Caqti_lwt.connection,
    ([> Caqti_error.call_or_retrieve] as 'a) )
  Caqti_lwt_unix.Pool.t
  (* DB connections pool *) ->
  Int32.t (* lowest included level *) * Int32.t (* highest included level *) ->
  (Data.batch, 'a) result Lwt.t
