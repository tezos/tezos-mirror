(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

val return : 'a -> ('a, 'e) result Lwt.t

val return_unit : (unit, 'e) result Lwt.t

val return_none : ('a option, 'e) result Lwt.t

val return_some : 'a -> ('a option, 'e) result Lwt.t

val return_nil : ('a list, 'e) result Lwt.t

val return_true : (bool, 'e) result Lwt.t

val return_false : (bool, 'e) result Lwt.t

val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

val ok : 'a -> ('a, 'e) result

val error : 'e -> ('a, 'e Tezos_error_monad.TzTrace.trace) result

val ( >>? ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

val ( >|? ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

val fail : 'e -> ('a, 'e Tezos_error_monad.TzTrace.trace) result Lwt.t

val ( >>=? ) :
  ('a, 'e) result Lwt.t ->
  ('a -> ('b, 'e) result Lwt.t) ->
  ('b, 'e) result Lwt.t

val ( >|=? ) : ('a, 'e) result Lwt.t -> ('a -> 'b) -> ('b, 'e) result Lwt.t

val ( >>?= ) :
  ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

val ( >|?= ) : ('a, 'e) result -> ('a -> 'b Lwt.t) -> ('b, 'e) result Lwt.t
