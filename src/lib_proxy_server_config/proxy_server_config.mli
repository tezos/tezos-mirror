(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** This type corresponds to the content of the [CONFIG] file, as
    well as the aggregations of the corresponding command line arguments.
    Once it is built and, it is transformed into a value of type
    [runtime] which represents what the APIs actually needs. *)
type t = private {
  endpoint : Uri.t option;
      (** The node to delegate RPCs to. Name was chosen
          to be the same as in [octez-client] *)
  rpc_addr : Uri.t option;
      (** The address that the proxy server serves. Name
          was chosen to be the same as in [octez-node run] *)
  rpc_tls : string option;
      (** A string of the form ["crt,key"] where [crt] is the path
          to the TLS certificate to use and [key] is the path to the key
          to use. Name was chosen to be the same as in [octez-node run] *)
  sym_block_caching_time : Ptime.span option;
      (** The time during which data for a symbolic block identifier
          (like [head], [head~1]) is kept. Smaller values increase [endpoint]'s
          load but yield more up-to-date to clients. Higher values
          decrease [endpoint]'s load but make clients observe slightly deprecated
          values. If omitted, defaulted to [time_between_blocks].

          If you are not sure what to do, do not specify it; i.e.
          do not pass [--sym-block-caching-time]. If your node's load
          is too high, pass a bigger value than [time_between_blocks] (if
          your clients are fine seeing slightly deprecated values) *)
  data_dir : string option;
      (** Path to the data-dir of a running octez-node. If specified, we use
          the [context] subdirectory to obtain data instead of using the
          [../raw/bytes] RPC call (hereby reducing the node's IO). *)
}

(** Pretty printer for [t] *)
val pp : Format.formatter -> t -> unit

(** A value of type [t] after having been appropriately validated and
    prepared for being passed to the various APIs that need them
    (hence some types are slightly different than in [t]). *)
type runtime = private {
  endpoint : Uri.t;  (** The node to delegate RPCs to. *)
  rpc_server_address : P2p_addr.t;  (** The address of the server *)
  rpc_server_port : int;  (** The port of the server *)
  rpc_server_tls : (string * string) option;
      (** The paths to the certificate and key to use for TLS *)
  sym_block_caching_time : Ptime.span option;
      (** The duration during which data of symbolic blocks is kept *)
  data_dir : string option;
      (** Path to the data-dir of a running octez-node. If specified, we use
          the [context] subdirectory to obtain data instead of using the
          [../raw/bytes] RPC call (hereby reducing the node's IO). *)
}

(** [make endpoint rpc_addr rpc_tls sym_block_caching_time]
    creates an instance of [t] from the flags that are available
    on the command line. See [t] for the documentation of parameters. *)
val make :
  endpoint:Uri.t option ->
  rpc_addr:Uri.t option ->
  rpc_tls:string option ->
  sym_block_caching_time:Ptime.span option ->
  data_dir:string option ->
  t

(** Valid config file: if passed to [destruct_config], [Valid] is returned. *)
val example_config : string

(** Encoding of what may happen when deserializing a JSON value
    whose OCaml type is ['a] *)
type 'a destructed =
  | Valid of 'a  (** File could be parsed and validated *)
  | Invalid of string
      (** File could be parsed but not validated. Message is detailed. *)
  | CannotDeserialize  (** Deserializing the JSON failed. *)

(** [destruct_config json] returns a [t] from raw [json].
    Returns [Ok] in case of success, or [Error msg] if validation fails. *)
val destruct_config : Data_encoding.json -> t destructed

(** [merge_right_bias t1 t2] returns the union of data of [t1] and [t2]
    (taking the [Some] case on fields of type [option]). If data is
    present on both side, take data from [t2] *)
val union_right_bias : t -> t -> t

(** [to_runtime t] returns a [runtime] from a [t].
    Returns [Ok] in case of success, or [Error msg] if validation fails. *)
val to_runtime : t -> (runtime, string) result
