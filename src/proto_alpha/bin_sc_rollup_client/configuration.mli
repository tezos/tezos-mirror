(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Client configuration. *)
type t = private {
  base_dir : string;
      (** [base_dir] is a directory where client user data is stored. *)
  endpoint : Uri.t;
      (** [endpoint] is used to communicate with the smart rollup
          node. *)
}

(** [parse argv] parses command-line arguments to return
   [(configuration, argv')] where [configuration] is deduced from the
   command-line arguments and [argv'] is the rest of the command-line
   arguments that have no meaning relatively to [Configuration]. *)
val parse : string list -> (t * string list) tzresult Lwt.t

(** [global_options ()] returns the list of options that have an
   influence on the configuration. *)
val global_options :
  unit -> (string option * Uri.t option, 'a) Tezos_clic.options

(** Instance of [Tezos_client_base.Client_context] that only handles IOs and
    RPCs. Can be used for keys and RPCs related commands. *)
class type sc_client_context =
  object
    inherit Tezos_client_base.Client_context.io_wallet

    inherit Tezos_rpc.Context.generic
  end

(** Instance of [sc_client_context] for linux systems. Relies on
    [Tezos_rpc_http_client_unix]. *)
class unix_sc_client_context :
  base_dir:string
  -> password_filename:string option
  -> rpc_config:Tezos_rpc_http_client_unix.RPC_client_unix.config
  -> sc_client_context

(** [make_unix_client_context config] generates a unix_sc_client_context from
    the client configuration. *)
val make_unix_client_context : t -> unix_sc_client_context
