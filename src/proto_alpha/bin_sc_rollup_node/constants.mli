(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
open Alpha_context

(** Returns the address of the smart contract rollup tracked by the rollup node. 
    This function reads a reference that gets initialised by the [init] function. 
    Invoking [get_sc_rollup_address] before [init] will cause the rollup node 
    to crash and exit with code 1.
*)
val get_sc_rollup_address : unit -> Sc_rollup.t

(** Returns the address of the rollup node operator. 
    This function reads a reference that gets initialised by the [init] function. 
    Invoking [get_sc_rollup_node_operator] before [init] will cause the rollup 
    node to crash and exit with code 1.
*)
val get_sc_rollup_node_operator : unit -> Signature.Public_key_hash.t

(** Returns the origination level of the smart contract rollup tracked by the 
    rollup node. This function reads a reference that gets initialised by the 
    [init] function. Invoking [get_sc_rollp_initial_level] before [init] will 
    cause the rollup node to crash and exit with code 1.
*)
val get_sc_rollup_initial_level : unit -> Raw_level.t

(** [get_operator_keys cctxt] returns a triple [(pkh, pk, sk)] corresponding 
    to the address, public key, and secret key URI of the rollup node operator.
    The rollup node operator address is read from a reference that is 
    initialised by the [init] function. Invoking [get_operator_keys] before 
    [init] will cause the rollup node to crash and exit with code 1.
    The node will also crash if the public and secret key for the address 
    are not stored in the wallet of [cctxt].
*)
val get_operator_keys :
  Protocol_client_context.full ->
  (Signature.Public_key_hash.t * Signature.Public_key.t * Client_keys.sk_uri)
  tzresult
  Lwt.t

(* [init cctxt sc_rollup operator_pkh] initialises the global constants 
   provided by this module. The rollup origination level is fetched 
   via an RPC call to the layer1 node that [cctxt] uses for RPC requests.
*)
val init :
  Protocol_client_context.full ->
  Alpha_context.Sc_rollup.t ->
  Signature.Public_key_hash.t ->
  unit tzresult Lwt.t
