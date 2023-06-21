(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** {2 Command-line options} *)

(** This module declares the main commands of the DAL node. For each
    command, a set of options is recognized. The function [commands]
    can be used to register a function when the user invokes the
    command. *)

type options = {
  data_dir : string;  (** Directory containing files related to the DAL node. *)
  rpc_addr : P2p_point.Id.t;
      (** The endpoint on which the DAL node can be contacted for RPCs. *)
  expected_pow : float;  (** The expected proof of work for the P2P identity. *)
  listen_addr : P2p_point.Id.t;
      (** The endpoint on which the DAL node can be contacted by other DAL nodes. *)
  endpoint : Uri.t;  (** The endpoint on which to contact the L1 node. *)
  profile : Services.Types.profile option;
      (** Profile of the DAL node used for tracking shards. *)
  use_unsafe_srs_for_tests : bool;
  peers : P2p_point.Id.t list;  (** DAL nodes to connect to. *)
}

(** Subcommands that can be used by the DAL node. In the future this type
    could be generalized if a command recgonizes a different set of
    options. *)
type t = Run | Config_init

(** [commands ~run] attaches a callback to each subcommands of the DAL
    node. *)
val make : run:(t -> options -> 'res tzresult) -> 'res Cmdliner.Cmd.t
