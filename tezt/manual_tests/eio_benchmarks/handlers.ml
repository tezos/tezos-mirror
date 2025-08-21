(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

open Helpers
open Tezos_base
open TzPervasives

module MAKE (Worker : sig
  type infinite

  type 'a queue

  type 'a t
end) =
struct
  type self = Worker.infinite Worker.queue Worker.t

  type launch_error = error list

  let on_launch _ _ _ = Lwt.return @@ Ok ()

  let on_close _ = Lwt.return @@ ()

  let on_error _ _ _ _ = Lwt.return @@ Error []

  let on_completion (type a b) _w (_req : (a, b) Request.t) (_res : a) _status =
    Lwt.return @@ ()

  let on_no_request _ = Lwt.return @@ ()

  let on_request : type a b. self -> (a, b) Request.t -> (a, b) result =
   fun _w req ->
    match req with
    | Request.Check_signature (pk, signature, msg) ->
        Ok (Signature.check pk signature msg)

  let on_request w req = Lwt.return @@ on_request w req
end
