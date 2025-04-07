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

(** [trusted_setup_candidate_directories] list directories in which
   the dal trusted setup could be. All folders are used when looking for it,
   the first one is the one selected when installing it. *)
val trusted_setup_candidate_directories :
  ?getenv_opt:(string -> string option) ->
  ?getcwd:(unit -> string) ->
  unit ->
  string list

(** [trusted_setup_preferred_directory] in which
   the dal trusted setup will be installed. *)
val trusted_setup_preferred_directory :
  ?getenv_opt:(string -> string option) ->
  ?getcwd:(unit -> string) ->
  unit ->
  string

(** [find_trusted_setup_files] returns the path of the two files
   necessary to initialize cryptographic primitives used by the
   DAL. See {!module:Tezos_crypto_dal.Cryptobox}. *)
val find_trusted_setup_files :
  ?getenv_opt:(string -> string option) ->
  ?getcwd:(unit -> string) ->
  ?file_exists:(string -> bool) ->
  unit ->
  (string * string) Error_monad.tzresult
