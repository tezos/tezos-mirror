(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(* Client module for interacting with a Dac Node in Observer mode. This client
   should only used by components that are compiled with a protocol. *)

module Reveal_hash = Protocol.Sc_rollup_reveal_hash

type error +=
  | Failed_to_initialize_dac_plugin
  | Failed_to_fetch_missing_page_from_observer of Reveal_hash.t
  | Failed_to_verify_raw_data_with_hash of (Reveal_hash.t * Protocol_hash.t)
  | Timeout of Z.t
  | Wrong_hash of {found : Reveal_hash.t; expected : Reveal_hash.t}

type t

module Configuration : sig
  type t = {
    observer_endpoint : Uri.t;
    reveal_data_dir : string;
    timeout_seconds : Z.t option;
  }
end

(** [init configuration] initializes a [Dac_observer_client.t] from [configuration]. *)
val init : Configuration.t -> t tzresult Lwt.t

(** [fetch_preimage dac_observer_client hash] requests the preimage of [hash]
    from a Dac Observer Node.
*)
val fetch_preimage :
  t -> Protocol.Sc_rollup_reveal_hash.t -> string tzresult Lwt.t
