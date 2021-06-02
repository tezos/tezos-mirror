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

val run :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  ?source:Contract.t ->
  ?payer:Contract.t ->
  ?gas:Z.t ->
  ?entrypoint:string ->
  unit ->
  (Script.expr * packed_internal_operation list * Contract.big_map_diff option)
  tzresult
  Lwt.t

val trace :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  ?source:Contract.t ->
  ?payer:Contract.t ->
  ?gas:Z.t ->
  ?entrypoint:string ->
  unit ->
  (Script.expr
  * packed_internal_operation list
  * Script_interpreter.execution_trace
  * Contract.big_map_diff option)
  tzresult
  Lwt.t

val print_run_result :
  #Client_context.printer ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr
  * packed_internal_operation list
  * Contract.big_map_diff option)
  tzresult ->
  unit tzresult Lwt.t

val print_trace_result :
  #Client_context.printer ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr
  * packed_internal_operation list
  * Script_interpreter.execution_trace
  * Contract.big_map_diff option)
  tzresult ->
  unit tzresult Lwt.t

val typecheck_data :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Z.t ->
  data:Michelson_v1_parser.parsed ->
  ty:Michelson_v1_parser.parsed ->
  unit ->
  Gas.t tzresult Lwt.t

val typecheck_program :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Z.t ->
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

val entrypoint_type :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Michelson_v1_parser.parsed ->
  entrypoint:string ->
  Script.expr option tzresult Lwt.t

val print_entrypoint_type :
  #Client_context.printer ->
  emacs:bool ->
  ?script_name:string ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  entrypoint:string ->
  Script_repr.expr option tzresult ->
  unit tzresult Lwt.t

val list_entrypoints :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Michelson_v1_parser.parsed ->
  (string * Script.expr) list tzresult Lwt.t

val print_entrypoints_list :
  #Client_context.printer ->
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
  #Client_context.printer ->
  emacs:bool ->
  ?script_name:string ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  Michelson_v1_primitives.prim list list tzresult ->
  unit tzresult Lwt.t
