(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  network : string;
      (** The url of the network, as passed by to the Octez node with the
          [--network] command-line argument. Fetched from the configuration
          file. *)
  snapshot : string option;
      (** The url of the snapshot to use to bootstrap the Octez
          node. Fetched from the configuration file.
          The snapshot can be omitted, the node will bootstrap
          itself instead. *)
  protocol : Protocol.t;
      (** The protocol run by the targeted testnet. Used to select
          which binaries to run, e.g., the smart rollup node. *)
  data_dir : string option;
      (** Optionally can start a node using an existing data-dir, instead
          of creating and bootstrapping one. *)
}

(** [get_testnet_config path] returns the configuration of the network
    to use to run the scenarios, based on the contents of the file
    stored under [path]. *)
val get_testnet_config : string -> t
