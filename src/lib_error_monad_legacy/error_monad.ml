(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let return = Lwt_result.return

let return_unit = Lwt_result.return ()

let return_none = Lwt_result.return None

let return_some x = Lwt_result.return (Some x)

let return_nil = Lwt_result.return []

let return_true = Lwt_result.return true

let return_false = Lwt_result.return false

let ( >>= ) = Lwt.Infix.( >>= )

let ( >|= ) = Lwt.Infix.( >|= )

let ok = Result.ok

let error e = Error (Tezos_error_monad.TzTrace.make e)

let ( >>? ) = Result.bind

let ( >|? ) x f = Result.map f x

let fail e = Lwt.return (Error (Tezos_error_monad.TzTrace.make e))

let ( >>=? ) = Lwt_result.bind

let ( >|=? ) x f = Lwt_result.map f x

let ( >>?= ) x f = match x with Error _ as e -> Lwt.return e | Ok x -> f x

let ( >|?= ) x f =
  match x with Error _ as e -> Lwt.return e | Ok x -> Lwt.map Result.ok (f x)
