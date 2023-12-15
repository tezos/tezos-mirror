(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error = {code : int; message : string}

(** [block_number evm_node] calls [eth_blockNumber]. *)
val block_number : Evm_node.t -> (int32, error) result Lwt.t

module Syntax : sig
  val ( let*@ ) : ('a, error) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t

  val ( let*@? ) : ('a, error) result Lwt.t -> (error -> 'c Lwt.t) -> 'c Lwt.t
end
