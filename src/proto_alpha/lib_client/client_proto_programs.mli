(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Tezos_micheline

module Program :
  Client_aliases.Alias
    with type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result

(* Parameters shared by both simulations (views, and contracts). *)
type simulation_params = {
  input : Michelson_v1_parser.parsed;
  unparsing_mode : Script_ir_unparser.unparsing_mode;
  now : Script_timestamp.t option;
  level : Script_int.n Script_int.num option;
  sender : Contract.t option;
  payer : Signature.public_key_hash option;
  gas : Gas.Arith.integral option;
}

(* Parameters specific to simulations of TZIP4 views *)
type run_view_params = {
  shared_params : simulation_params;
  contract : Contract_hash.t;
  entrypoint : Entrypoint.t;
}

(* Parameters specific to simulations of Michelson views *)
type run_script_view_params = {
  shared_params : simulation_params;
  contract : Contract_hash.t;
  view : string;
  unlimited_gas : bool;
}

(* Parameters specific to simulations of contract calls *)
type run_params = {
  shared_params : simulation_params;
  amount : Tez.t option;
  balance : Tez.t option;
  program : Michelson_v1_parser.parsed;
  storage : Michelson_v1_parser.parsed;
  entrypoint : Entrypoint.t option;
  self : Contract_hash.t option;
}

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.run_tzip4_view} *)
val run_view :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_view_params ->
  Script.expr tzresult Lwt.t

(** [run_script_view cctxt ~chain ~block params]
    executes {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.run_script_view},
    the RPC to run a Michelson view offchain and returns its value.
 *)
val run_script_view :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_script_view_params ->
  Script.expr tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.run_code} *)
val run :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_params ->
  (Script.expr
  * Apply_internal_results.packed_internal_operation list
  * Lazy_storage.diffs option)
  tzresult
  Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.trace_code} *)
val trace :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_params ->
  (Script.expr
  * Apply_internal_results.packed_internal_operation list
  * Script_typed_ir.execution_trace
  * Lazy_storage.diffs option)
  tzresult
  Lwt.t

val print_view_result :
  #Protocol_client_context.full ->
  Script_repr.expr tzresult ->
  unit tzresult Lwt.t

val print_run_result :
  #Protocol_client_context.full ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr
  * Apply_internal_results.packed_internal_operation list
  * Lazy_storage.diffs option)
  tzresult ->
  unit tzresult Lwt.t

val print_trace_result :
  #Protocol_client_context.full ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr
  * Apply_internal_results.packed_internal_operation list
  * Script_typed_ir.execution_trace
  * Lazy_storage.diffs option)
  tzresult ->
  unit tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.typecheck_data} *)
val typecheck_data :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Gas.Arith.integral ->
  ?legacy:bool ->
  data:Michelson_v1_parser.parsed ->
  ty:Michelson_v1_parser.parsed ->
  unit ->
  Gas.t tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.typecheck_code} *)
val typecheck_program :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Gas.Arith.integral ->
  ?legacy:bool ->
  show_types:bool ->
  Michelson_v1_parser.parsed ->
  (Script_tc_errors.type_map * Gas.t) tzresult Lwt.t

val print_typecheck_result :
  emacs:bool ->
  show_types:bool ->
  print_source_on_error:bool ->
  name:string option ->
  Michelson_v1_parser.parsed ->
  (Script_tc_errors.type_map * Gas.t) tzresult ->
  #Client_context.printer ->
  unit tzresult Lwt.t

(** Calls {!Tezos_protocol_plugin_alpha.Plugin.RPC.Scripts.script_size} *)
val script_size :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Gas.Arith.integral ->
  ?legacy:bool ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  unit ->
  int tzresult Lwt.t

val entrypoint_type :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Michelson_v1_parser.parsed ->
  entrypoint:Entrypoint.t ->
  Script.expr option tzresult Lwt.t

val print_entrypoint_type :
  #Protocol_client_context.full ->
  emacs:bool ->
  ?script_name:string ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  entrypoint:Entrypoint.t ->
  Script_repr.expr option tzresult ->
  unit tzresult Lwt.t

(** Calls {!Michelson_v1_entrypoints.list_entrypoints} *)
val list_entrypoints :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Michelson_v1_parser.parsed ->
  (string * Script.expr) list tzresult Lwt.t

val print_entrypoints_list :
  #Protocol_client_context.full ->
  emacs:bool ->
  ?script_name:string ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (string * Script.expr) list tzresult ->
  unit tzresult Lwt.t

val list_unreachables :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Michelson_v1_parser.parsed ->
  Michelson_v1_primitives.prim list list tzresult Lwt.t

(** Calls {!Michelson_v1_entrypoints.print_unreachables} *)
val print_unreachables :
  #Protocol_client_context.full ->
  emacs:bool ->
  ?script_name:string ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  Michelson_v1_primitives.prim list list tzresult ->
  unit tzresult Lwt.t
