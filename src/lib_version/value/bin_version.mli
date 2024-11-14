(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** [version_string version] returns the version string containing the abbreviated hash,
    committer date and the current version.

    This string is suitable for binaries to show when e.g. a [--version] flag is given. *)
val version_string : Tezos_version_parser.t -> string

(** [version_string version] returns the version string
    containing only the current version and the abbreviated hash. *)
val simple_version_string : Tezos_version_parser.t -> string

(** [version_string] applied to the Version of Octez. *)
val octez_version_string : string

(** [simple_version_string] applied to the Version of Octez. *)
val octez_simple_version_string : string

(** [version_string] applied to the Version of the Octez EVM node. *)
val octez_evm_node_version_string : string

(** [simple_version_string] applied to the Version of Octez EVM node. *)
val octez_evm_node_simple_version_string : string
