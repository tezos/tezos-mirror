(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Dispatch tree *)
type 'prefix t

type 'prefix directory = 'prefix t

(** Empty list of dispatch trees *)
val empty : 'prefix directory

val map : ('a -> 'b Lwt.t) -> 'b directory -> 'a directory

val prefix : ('pr, 'p) RPC_path.path -> 'p directory -> 'pr directory

val merge :
  ?strategy:[`Raise | `Pick_left | `Pick_right] ->
  'a directory ->
  'a directory ->
  'a directory

(** Possible error while registering services. *)
type step =
  | Static of string
  | Dynamic of RPC_arg.descr
  | DynamicTail of RPC_arg.descr

type conflict =
  | CService of RPC_service.meth
  | CDir
  | CBuilder
  | CDynDescr of string * string
  | CTail
  | CTypes of RPC_arg.descr * RPC_arg.descr
  | CType of RPC_arg.descr * string list

exception Conflict of step list * conflict

(** Registering handler in service tree.

    The [chunked] parameter controls whether the answer to the RPC is chunk
    encoded (i.e., the serialisation is split and the caller receives the answer
    in multiple chunks) or not. Defaults to [false]. Set to [true] for RPCs that
    return potentially large collections (e.g., unbounded lists). *)
val register :
  chunked:bool ->
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  'prefix directory

val opt_register :
  chunked:bool ->
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output option tzresult Lwt.t) ->
  'prefix directory

val gen_register :
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> [< 'output RPC_answer.t] Lwt.t) ->
  'prefix directory

val lwt_register :
  chunked:bool ->
  'prefix directory ->
  ('meth, 'prefix, 'params, 'query, 'input, 'output) RPC_service.t ->
  ('params -> 'query -> 'input -> 'output Lwt.t) ->
  'prefix directory

(** Registering handler in service tree. Curryfied variant.  *)

val register0 :
  chunked:bool ->
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o tzresult Lwt.t) ->
  unit directory

val register1 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register2 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register3 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register4 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val register5 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o tzresult Lwt.t) ->
  'prefix directory

val opt_register0 :
  chunked:bool ->
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o option tzresult Lwt.t) ->
  unit directory

val opt_register1 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register2 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register3 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register4 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val opt_register5 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o option tzresult Lwt.t) ->
  'prefix directory

val gen_register0 :
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  unit directory

val gen_register1 :
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register2 :
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register3 :
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register4 :
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val gen_register5 :
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> [< 'o RPC_answer.t] Lwt.t) ->
  'prefix directory

val lwt_register0 :
  chunked:bool ->
  unit directory ->
  ('m, unit, unit, 'q, 'i, 'o) RPC_service.t ->
  ('q -> 'i -> 'o Lwt.t) ->
  unit directory

val lwt_register1 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, unit * 'a, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register2 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (unit * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register3 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register4 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

val lwt_register5 :
  chunked:bool ->
  'prefix directory ->
  ('m, 'prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o) RPC_service.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> 'o Lwt.t) ->
  'prefix directory

(** Registering dynamic subtree. *)
val register_dynamic_directory :
  ?descr:string ->
  'prefix directory ->
  ('prefix, 'a) RPC_path.t ->
  ('a -> 'a directory Lwt.t) ->
  'prefix directory
