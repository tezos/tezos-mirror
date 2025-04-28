(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base
open TzPervasives

module Name = struct
  type t = string

  let encoding = Data_encoding.string

  let base = ["test"; "main"]

  let pp = Format.pp_print_string

  let equal = String.equal
end

module Request = struct
  type (_, _) t =
    | Check_signature :
        Signature.public_key * Signature.t * Bytes.t
        -> (bool, exn) t

  type view = Signature.public_key

  let encoding = Signature.Public_key.encoding

  let pp = Signature.Public_key.pp

  let view (type a b) (req : (a, b) t) : view =
    match req with Check_signature (pk, _signature, _msg) -> pk
end

module Types = struct
  type parameters = unit

  type state = unit
end

module Worker = Tezos_bees.Worker.MakeSingle (Name) (Request) (Types)

type t = Worker.infinite Worker.queue Worker.t

module Lwt_worker = Tezos_workers.Worker.MakeSingle (Name) (Request) (Types)

let table = Lwt_worker.create_table Lwt_worker.Queue
