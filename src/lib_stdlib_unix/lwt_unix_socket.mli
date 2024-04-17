(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type error +=
  | Socket_path_too_long of string
  | Socket_path_wrong_permission of string
  | Cannot_create_socket of string

val send : Lwt_io.output_channel -> 'a Data_encoding.t -> 'a -> unit Lwt.t

val recv : Lwt_io.input_channel -> 'a Data_encoding.t -> 'a Lwt.t

val recv_result :
  Lwt_io.input_channel -> 'a Data_encoding.t -> 'a tzresult Lwt.t

val create_socket_listen :
  canceler:Lwt_canceler.t ->
  max_requests:int ->
  socket_path:string ->
  Lwt_unix.file_descr tzresult Lwt.t

val create_socket_connect :
  canceler:Lwt_canceler.t ->
  socket_path:string ->
  Lwt_unix.file_descr tzresult Lwt.t
