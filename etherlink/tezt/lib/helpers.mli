(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

(** Michelson type to use when originating the EVM rollup. *)
val evm_type : string

(** [no_0x s] removes the prefix [0x] of [s] if it exists. *)
val no_0x : string -> string

(** [normalize s] calls {!no_0x} and {!String.lowercase_ascii} on [s]. *)
val normalize : string -> string

(** [u16_to_bytes n] translate an int in a binary string of two bytes
    (little endian).
    NB: Ints greater than 2 bytes are truncated. *)
val u16_to_bytes : int -> string

(** [mapping_position key map_position] computes the storage position for
    a value in a mapping given its [key] and the position of the map
    itself.
    It computes this position as:
    [keccack(LeftPad32(key, 0), LeftPad32(map_position, 0))]
    as specified in
    https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getlogs
*)
val mapping_position : string -> int -> string

(** Transform an hexadecimal string to an integer using {!Z.of_bits}. *)
val hex_string_to_int : string -> int

(** [next_rollup_node_level ~sc_rollup_node ~node ~client] moves [sc_rollup_node] to
    the [node]'s next level. *)
val next_rollup_node_level :
  sc_rollup_node:Sc_rollup_node.t -> node:Node.t -> client:Client.t -> int Lwt.t

(** [next_evm_level ~evm_node ~sc_rollup_node ~node ~client] moves
    [evm_node] to the next L2 level. *)
val next_evm_level :
  evm_node:Evm_node.t ->
  sc_rollup_node:Sc_rollup_node.t ->
  node:Node.t ->
  client:Client.t ->
  int Lwt.t

(** Path to the directory containing sample inputs. *)
val kernel_inputs_path : string

(** [read_tx_from_file ()] reads a file containing 100 transactions.
    The list returned contains pairs of the shape [(tx_raw, tx_hash)].
*)
val read_tx_from_file : unit -> (string * string) list
