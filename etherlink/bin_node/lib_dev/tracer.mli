(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module CallTracerRead : sig
  (** [build_calltrace end_call get_next] rebuilds a call trace. 
      [end_call node list] adds a list of calls to a node, and 
      [get_next i] gets the data [(node,depth)] at line [i] in storage. 

      This is left relatively generic to facilitate testing the algorithm.
  *)
  val build_calltrace :
    ('a -> 'a list -> 'a) ->
    (int -> ('a * int) option tzresult Lwt.t) ->
    'a tzresult Lwt.t
end

(** [trace_transaction ~block_number ~transaction ~config] replays the block
    [block_number] and traces [transaction_hash] in it, with the given
    [config]. *)
val trace_transaction :
  (module Evm_execution.S) ->
  block_number:Ethereum_types.quantity ->
  transaction_hash:Ethereum_types.hash ->
  config:Tracer_types.config ->
  Tracer_types.output tzresult Lwt.t

(** [trace_call ~call ~block ~config] simulates and traces call
    [call] in block [block], with the given [config]. *)
val trace_call :
  (module Evm_execution.S) ->
  call:Ethereum_types.call ->
  block:Ethereum_types.Block_parameter.extended ->
  config:Tracer_types.config ->
  Tracer_types.output tzresult Lwt.t
