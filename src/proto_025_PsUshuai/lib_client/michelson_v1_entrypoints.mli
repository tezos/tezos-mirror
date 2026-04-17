(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Returns [Some type] if the contract has an entrypoint of type [type]. None if it does not exist.  *)
val script_entrypoint_type :
  #Protocol_client_context.rpc_context ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  Alpha_context.Script.expr ->
  entrypoint:Alpha_context.Entrypoint.t ->
  Alpha_context.Script.expr option tzresult Lwt.t

(** Returns [Some type] if the script has an entrypoint of type [type]. None if it does not exist.  *)
val contract_entrypoint_type :
  #Protocol_client_context.rpc_context ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  contract:Contract_hash.t ->
  entrypoint:Alpha_context.Entrypoint.t ->
  normalize_types:bool ->
  Alpha_context.Script.expr option tzresult Lwt.t

val print_entrypoint_type :
  #Client_context.printer ->
  ?on_errors:(error list -> unit tzresult Lwt.t) ->
  emacs:bool ->
  ?contract:Contract_hash.t ->
  ?script_name:string ->
  entrypoint:Alpha_context.Entrypoint.t ->
  Alpha_context.Script.expr option tzresult ->
  unit tzresult Lwt.t

(** List paths of unreachable parameters.
    Only useful to test the stitching, as no such parameter should be
    allowed in originated contracts.  *)
val list_contract_unreachables :
  #Protocol_client_context.rpc_context ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  contract:Contract_hash.t ->
  Michelson_v1_primitives.prim list list tzresult Lwt.t

val list_unreachables :
  #Protocol_client_context.rpc_context ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  Alpha_context.Script.expr ->
  Michelson_v1_primitives.prim list list tzresult Lwt.t

val print_unreachables :
  #Client_context.printer ->
  ?on_errors:(error list -> unit tzresult Lwt.t) ->
  emacs:bool ->
  ?contract:Contract_hash.t ->
  ?script_name:string ->
  Michelson_v1_primitives.prim list list tzresult ->
  unit tzresult Lwt.t

(** List the contract entrypoints with their types.
    If their is no explicit default, th type of default entrypoint will still be given.
*)
val list_contract_entrypoints :
  #Protocol_client_context.rpc_context ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  contract:Contract_hash.t ->
  normalize_types:bool ->
  (string * Alpha_context.Script.expr) list tzresult Lwt.t

(** List the script entrypoints with their types.  *)
val list_entrypoints :
  #Protocol_client_context.rpc_context ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  Alpha_context.Script.expr ->
  (string * Alpha_context.Script.expr) list tzresult Lwt.t

(** Print the contract entrypoints with their types.  *)
val print_entrypoints_list :
  #Client_context.printer ->
  ?on_errors:(error list -> unit tzresult Lwt.t) ->
  emacs:bool ->
  ?contract:Contract_hash.t ->
  ?script_name:string ->
  (string * Alpha_context.Script.expr) list tzresult ->
  unit tzresult Lwt.t
