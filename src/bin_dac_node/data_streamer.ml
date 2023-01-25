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

(* Component for streaming published root page hashes to subscribers. *)

module type S = sig
  (* Protocol hash type used for reveal_preimage. *)
  type hash

  (* [publish hash] publishes a root page [hash] to all attached subscribers. *)
  val publish : hash -> unit tzresult Lwt.t

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4680
     Access Dac coordinator details via some [Dac_client.cctx].
  *)

  (* [subscribe coordinator_host coordinator_port] returns an Lwt_stream of hashes and
     a function to close the stream.
  *)
  val subscribe :
    coordinator_host:string ->
    coordinator_port:int ->
    hash Lwt_stream.t * Lwt_watcher.stopper
end

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4510
   Implement useful streaming.
*)
module Make (P : Dac_plugin.T) : S = struct
  type hash = P.hash

  let publish _hash = Lwt_result_syntax.return_unit

  let subscribe ~coordinator_host ~coordinator_port =
    let _unused = (coordinator_host, coordinator_port) in
    Lwt_watcher.create_fake_stream ()
end
