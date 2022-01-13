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
  unparsing_mode : Script_ir_translator.unparsing_mode;
  now : Script_timestamp.t option;
  level : Script_int.n Script_int.num option;
  source : Contract.t option;
  payer : Contract.t option;
  gas : Gas.Arith.integral option;
}

(* Parameters specific to simulations of views *)
type run_view_params = {
  shared_params : simulation_params;
  contract : Contract.t;
  entrypoint : Entrypoint.t;
}

(* Parameters specific to simulations of contract calls *)
type run_params = {
  shared_params : simulation_params;
  amount : Tez.t option;
  balance : Tez.t;
  program : Michelson_v1_parser.parsed;
  storage : Michelson_v1_parser.parsed;
  entrypoint : Entrypoint.t option;
}

val run_view :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_view_params ->
  Script.expr tzresult Lwt.t

val run :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_params ->
  (Script.expr * packed_internal_operation list * Lazy_storage.diffs option)
  tzresult
  Lwt.t

val trace :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  run_params ->
  (Script.expr
  * packed_internal_operation list
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
  * packed_internal_operation list
  * Lazy_storage.diffs option)
  tzresult ->
  unit tzresult Lwt.t

val print_trace_result :
  #Protocol_client_context.full ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr
  * packed_internal_operation list
  * Script_typed_ir.execution_trace
  * Lazy_storage.diffs option)
  tzresult ->
  unit tzresult Lwt.t

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
  Michelson_v1_parser.parsed ->
  (Script_tc_errors.type_map * Gas.t) tzresult ->
  #Client_context.printer ->
  unit tzresult Lwt.t

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

val print_unreachables :
  #Protocol_client_context.full ->
  emacs:bool ->
  ?script_name:string ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  Michelson_v1_primitives.prim list list tzresult ->
  unit tzresult Lwt.t
