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

type error += No_identity_file of string

type error += Insufficient_proof_of_work of {expected : float}

type error += Existent_identity_file of string

(** [read ?expected_pow filename] reads an {!P2p_identity.t} from disk at
    location [filename]. If [expected_pow] is set, the function also checks that
    the read identity has the expected PoW.

    The function fails with:
    - {!No_identity_file} if the given file is not found;
    - {!Identity_mismatch} if the given identity is ill-formed (the id is not the hash of the public key);
    - {!Identity_keys_mismatch} if the the public and secret keys do not match;
    - {!Insufficient_proof_of_work} if the read identity doesn't have the
      expected PoW. *)
val read : ?expected_pow:float -> string -> P2p_identity.t tzresult Lwt.t

(** [write ~check_data_dir filename identity] writes [identity] into file
    [filename]. The function fails with {!Existent_identity_file} if the file
    already exists.

    See {!init} for [check_data_dir]. *)
val write :
  check_data_dir:(data_dir:string -> unit tzresult Lwt.t) ->
  string ->
  P2p_identity.t ->
  unit tzresult Lwt.t

(** [generate ~check_data_dir filename expected_pow] generates a fresh identity
    with the given [expected_pow] and saves it into [filename]. The function
    fails with [Existent_identity_file] if [filename] already exists.*)
val generate :
  check_data_dir:(data_dir:string -> unit tzresult Lwt.t) ->
  string ->
  float ->
  P2p_identity.t tzresult Lwt.t

(** [init ~check_data_dir ~identity_file ~expected_pow] returns the
    {!P2p_identity.t} read from [identity_file], if any. Otherwise, it generates
    a fresh one with the given [expected_pow], saves it into [identity_file] and
    returns it. In this latter case, the function [check_data_dir] can be used
    to enforce invariants on the "data dir" (the directory containing the
    identity file). *)
val init :
  check_data_dir:(data_dir:string -> unit tzresult Lwt.t) ->
  identity_file:string ->
  expected_pow:float ->
  P2p_identity.t tzresult Lwt.t
